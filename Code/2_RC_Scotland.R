########################################################################################
########  An open source delineation and hierarchical classification 
#######     of UK retail agglomeration
######
#####     Jacob L. Macdonald, Les Dolega, Alex Singleton
####      Last Updated: October, 2021
###
##  SCOTLAND RETAIL CENTRE DELINEATION
##   
##  - Code for delineating the first iteration of retail agglomerations for Scotland. 
##    The retail points are sourced primarily from the cleaned OSM dataset and Land Use. 
##    This part of the code generates the complete set of retail agglomerations which are
##    eventually pruned, merged, cleaned and combined in the subsequent script.
##    
##  - Requires: OSM retail points and land use (SC_OSM_amenity/ shop/ LU.gpkg)
##              Functions.R code for spatial functions
##              h3 and sf packages
##    
##  Notes: 1) Exports the complete set of retail agglomerations to eventually be pruned. Two 
##            different sets are generated - an individual h3 level with all respective
##            h3 IDs and RC IDs; and a dissolved version at the retail agglomeration level.
## 


# LOAD PACKAGES; SET WD; READ IN FUNCTIONS
#------------------------------------------------------------------------------------
Packages <- c("tidyverse", "sf", "rgdal", "data.table", "h3", "tidygraph", "parallel", 
  "doSNOW", "igraph", "smoothr", "gtable", "gridExtra", "osmdata")
lapply(Packages, library, character.only = TRUE)
rm(Packages)
sf::sf_use_s2(FALSE)

# The folder structure for the project is laid out as follows
#    A working directory is set through a string 'wd' 
#
# < WORKING DIRECTORY > ("wd")
#      |
#      |--Code
#      |
#      |--Data
#      |   |--Boundaries and Lookups
#      |   |--OSM
#      |   |--VOA
#      |
#      |--Exports
#          |--Interim
#          |--Final

wd <- "<FILE PATH TO WORKING DIRECTORY>" 
# e.g. "/home/---/Retail Boundaries Project"

# Functions.R includes a set of functions related to h3 geometry tract connectivity,
#   infilling and spatial operations.
source(paste0(wd, "/Code/Functions.R"))
#------------------------------------------------------------------------------------


# READ IN OSM DATA 
#------------------------------------------------------------------------------------
osm.shopPT <- st_read(dsn = paste0(wd, "/Data/OSM/SC_OSM_shop_291020.gpkg"))
osm.shopPT$value <- NA
osm.amenityPT <- st_read(dsn = paste0(wd, "/Data/OSM/SC_OSM_amenity_291020.gpkg"))

osm.retailPT <- rbind(osm.amenityPT, osm.shopPT)
names(osm.retailPT)[names(osm.retailPT)=="geom"] <- c("geometry")
st_geometry(osm.retailPT) <- "geometry"

osm.retailLU <- st_read(dsn = paste0(wd, "/Data/OSM/SC_OSM_LU_291020.gpkg"))
#------------------------------------------------------------------------------------


# CONVERT OSM POINTS TO H3
#------------------------------------------------------------------------------------
osm.pt.h3 <- h3::h3_to_geo_boundary_sf(h3::geo_to_h3(st_coordinates(osm.retailPT)[,c(2,1)], res = 11))
osm.pt.h3$h3_resolution <- 11
osm.pt.h3$h3_address <- h3::geo_to_h3(st_coordinates(osm.retailPT)[,c(2,1)], res = 11)

osm.retailPT <- st_join(osm.retailPT, osm.pt.h3, join = st_within)
osm.retailPT <- osm.retailPT[!duplicated(osm.retailPT$osm_id),]
osm.retailPT$h3_resolution <- NULL

H3_counts <- as_tibble(osm.retailPT) %>%
  select(h3_address) %>%
  group_by(h3_address) %>%
  add_count(name = "retailN") %>%
  filter(!is.na(h3_address) & h3_address!="") %>% 
  unique()

osm.pt.h3 <- merge(osm.pt.h3, H3_counts, all.x=T, sort=F, by="h3_address")
osm.pt.h3 <- unique(osm.pt.h3)
osm.pt.h3$source <- "OSM_Points"
rm(H3_counts); gc()

hex_Graph <- get.tractID(osm.pt.h3$h3_address, osm.pt.h3$retailN, retail.count=TRUE, NAME="OSM_RetailID")
osm.pt.h3 <- osm.pt.h3 %>%
  left_join(as_tibble(hex_Graph)) %>%
  unique()
#------------------------------------------------------------------------------------


# CONVERT OSM POLYGONS TO H3
#------------------------------------------------------------------------------------
cl <- makeCluster(floor(detectCores()/2))
registerDoSNOW(cl)
H3_osm.retailLU.13 <- list()
H3_osm.retailLU.13 <- foreach(j = 1:dim(osm.retailLU)[1], .combine = c, .options.snow = list(progress = function(n) setTxtProgressBar(txtProgressBar(max = dim(osm.retailLU)[1], style = 3), n)), .packages=c("h3", "sf")) %dopar% {
  h3::polyfill(st_geometry(osm.retailLU[j,]), res = 13) }
stopCluster(cl)

H3_osm.retailLU.13 <- unique(H3_osm.retailLU.13)
H3_osm.retailLU.13 <- data.table(Child = H3_osm.retailLU.13)

osm.retailLUTR <- st_buffer(st_transform(osm.retailLU, crs = 27700), 10)
osm.retailLUTR <- st_transform(osm.retailLU, crs = 4326)

cl <- makeCluster(floor(detectCores()/2+8))
registerDoSNOW(cl)
H3_osm.retailLU.11 <- list()
H3_osm.retailLU.11 <- foreach(j = 1:dim(osm.retailLUTR)[1], .combine = c, .options.snow = list(progress = function(n) setTxtProgressBar(txtProgressBar(max = dim(osm.retailLUTR)[1], style = 3), n)), .packages=c("h3", "sf")) %dopar% {
  h3::polyfill(st_geometry(osm.retailLUTR[j,]), res = 11) }
stopCluster(cl)

H3_osm.retailLU.11 <- unique(H3_osm.retailLU.11)
H3_osm.retailLU.11 <- data.table(Parent = H3_osm.retailLU.11)

cl <- makeCluster(floor(detectCores()/2+8))
registerDoSNOW(cl)
tt <- list()
tt <- foreach(j = 1:length(H3_osm.retailLU.11$Parent), .combine = rbind, .options.snow = list(progress = function(n) setTxtProgressBar(txtProgressBar(max = length(H3_osm.retailLU.11$Parent), style = 3), n)), .packages=c("h3")) %dopar% {
  cbind(H3_osm.retailLU.11$Parent[j], h3::h3_to_children(H3_osm.retailLU.11$Parent[j], 13)) }
stopCluster(cl)
tt <- as.data.table(tt)
names(tt) <- c("Parent", "Child")

H3_osm.retailLU.13 <- merge(H3_osm.retailLU.13, tt, all.x=T, sort=F, by="Child")
H3_osm.retailLU.13$Child <- as.factor(H3_osm.retailLU.13$Child)
H3_osm.retailLU.13$Parent <- as.factor(H3_osm.retailLU.13$Parent)

H3_osm.retailLU.13.A <- H3_osm.retailLU.13[!is.na(H3_osm.retailLU.13$Parent),]

H3_osm.retailLU.13.B <- H3_osm.retailLU.13[is.na(H3_osm.retailLU.13$Parent),]
H3_osm.retailLU.13.B$Child <- as.character(H3_osm.retailLU.13.B$Child)
H3_osm.retailLU.13.B$Parent <- as.character(H3_osm.retailLU.13.B$Parent)
gc()

cl <- makeCluster(detectCores()-1)
registerDoSNOW(cl)
tt <- list()
tt <- foreach(j = 1:length(H3_osm.retailLU.13.B$Child), .combine = rbind, .options.snow = list(progress = function(n) setTxtProgressBar(txtProgressBar(max = length(H3_osm.retailLU.13.B$Child), style = 3), n)), .packages=c("h3")) %dopar% {
  cbind(H3_osm.retailLU.13.B$Child[j], h3::h3_to_parent(H3_osm.retailLU.13.B$Child[j], 11)) }
stopCluster(cl)
tt <- as.data.table(tt)
names(tt) <- c("Child", "Parent")

H3_osm.retailLU.13.B$Parent <- NULL
H3_osm.retailLU.13.B <- merge(H3_osm.retailLU.13.B, tt, all.x=T, sort=F, by="Child")
H3_osm.retailLU.13.B$Child <- as.factor(H3_osm.retailLU.13.B$Child)
H3_osm.retailLU.13.B$Parent <- as.factor(H3_osm.retailLU.13.B$Parent)

H3_osm.retailLU.13 <- rbind(H3_osm.retailLU.13.A, H3_osm.retailLU.13.B)

osm.pl.h3 <- h3::h3_to_geo_boundary_sf(H3_osm.retailLU.13$Parent)
osm.pl.h3$h3_resolution <- 11
osm.pl.h3$h3_address <- H3_osm.retailLU.13$Parent
osm.pl.h3$source <- "OSM_Poly"
osm.pl.h3$h3_address <- as.character(osm.pl.h3$h3_address)

osm.pl.h3 <- osm.pl.h3 %>%
  select(h3_address, h3_resolution, source) 
osm.pl.h3 <- unique(osm.pl.h3)

hex_Graph <- get.tractID(osm.pl.h3$h3_address, retail.count=FALSE, NAME="OSM_LandUseID")
osm.pl.h3 <- osm.pl.h3 %>%
  left_join(as_tibble(hex_Graph)) %>%
  unique()
rm(tt, osm.retailLUTR, H3_osm.retailLU.11, H3_osm.retailLU.13, H3_osm.retailLU.13.A, H3_osm.retailLU.13.B, cl, hex_Graph)
#------------------------------------------------------------------------------------


# PRUNING AND REMOVAL OF ISLANDS
#------------------------------------------------------------------------------------
osm.pt.h3$tractID <- NULL
osm.pt.h3$tractN <- NULL
osm.pt.h3$tr_retailN <- NULL
osm.pl.h3$tractID <- NULL
osm.pl.h3$tractN <- NULL
osm.pl.h3$retailN <- NA

retail_centres <- rbind(osm.pt.h3, osm.pl.h3[which(!(osm.pl.h3$h3_address %in% osm.pt.h3$h3_address)),])
retail_centres <- unique(retail_centres)


retail_centres$retailN <- NULL
H3_counts <- as_tibble(osm.retailPT) %>%
  select(h3_address) %>%
  group_by(h3_address) %>%
  add_count(name = "retailN") %>%
  filter(!is.na(h3_address) & h3_address!="") %>% 
  unique()
retail_centres <- merge(retail_centres, H3_counts, all.x=T, sort=F, by="h3_address")
retail_centres$retailN[is.na(retail_centres$retailN)] <- 0


hex_Graph <- get.tractID(retail_centres$h3_address, retail_centres$retailN, retail.count=TRUE, NAME="RC_SC", source=retail_centres$source)
retail_centres <- retail_centres %>%
  left_join(as_tibble(hex_Graph)) %>%
  unique()

retail_centres <- retail_centres[retail_centres$tractN >= 2 | retail_centres$retailN >= 10,]
retail_centres <- retail_centres[retail_centres$tractID %in% unique(retail_centres[which(retail_centres$tr_retailN >= 10),]$tractID),]
retail_centres <- retail_centres[!is.na(retail_centres$h3_address) | retail_centres$h3_address!="",]
retail_centres <- unique(retail_centres)
#------------------------------------------------------------------------------------


# DISSOLVE INTO UNIQUE RETAIL CENTRES
#------------------------------------------------------------------------------------
RC.dissolved <- retail_centres %>% 
  st_transform(crs=27700) %>%
  st_make_valid() %>%
  st_buffer(dist = 0.1) %>%
  dplyr::filter(!is.na(h3_address) & h3_address!="") %>% 
  dplyr::group_by(tractID) %>% 
  dplyr::summarize() %>%
  smoothr::fill_holes(threshold = units::set_units(2000, km^2)) %>%
  st_sf() %>%
  unique()

RC.x <- as_tibble(retail_centres) %>%
  select(tractID, tractN, tr_retailN, OSM_Points_N, OSM_Poly_N) %>%
  unique()

RC.dissolved <- left_join(RC.dissolved, RC.x)
rm(hex_Graph, RC.x); gc(verbose=FALSE)

st_write(RC.dissolved, dsn = paste0(wd, "/Exports/Interim/SC_RC_Complete.gpkg"), delete_layer = TRUE, driver = "gpkg")
st_write(retail_centres, dsn = paste0(wd, "/Exports/Interim/SC_H3_Complete.gpkg"), delete_layer = TRUE, driver = "gpkg")
#------------------------------------------------------------------------------------
