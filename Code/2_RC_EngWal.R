########################################################################################
########  An open source delineation and hierarchical classification 
#######     of UK retail agglomeration
######
#####     Jacob L. Macdonald, Les Dolega, Alex Singleton
####      Last Updated: October, 2021
###
##  ENGLAND & WALES RETAIL CENTRE DELINEATION
##   
##  - Code for delineating the first iteration of retail agglomerations for England and Wales. 
##    The retail points are sourced primarily from the cleaned VOA dataset and OSM Land Use. 
##    This part of the code generates the complete set of retail agglomerations which are
##    eventually pruned, merged, cleaned and combined in the subsequent script.
##   
##  - Requires: VOA retail points (Retail_Points.gpkg)
##              OSM retail points and land use (EW_OSM_amenity/ shop/ LU.gpkg)
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


# READ IN VOA AND OSM DATA 
#------------------------------------------------------------------------------------
voa.retail.PT <- st_read(paste0(wd, "/Data/VOA/Retail_Points.gpkg"))
names(voa.retail.PT)[names(voa.retail.PT)=="geom"] <- c("geometry")
st_geometry(voa.retail.PT) <- "geometry"

osm.shopPT <- st_read(dsn = paste0(wd, "/Data/OSM/EW_OSM_shop_301020.gpkg"))
osm.shopPT$value <- NA
osm.amenityPT <- st_read(dsn = paste0(wd, "/Data/OSM/EW_OSM_amenity_301020.gpkg"))

osm.retail.PT <- rbind(osm.amenityPT, osm.shopPT)
names(osm.retail.PT)[names(osm.retail.PT)=="geom"] <- c("geometry")
st_geometry(osm.retail.PT) <- "geometry"

osm.retail.LU <- st_read(dsn = paste0(wd, "/Data/OSM/EW_OSM_LU_301020.gpkg"))
#------------------------------------------------------------------------------------


# CONVERT VOA POINTS TO H3
#------------------------------------------------------------------------------------
voa.retail.h3 <- h3::h3_to_geo_boundary_sf(h3::geo_to_h3(st_coordinates(voa.retail.PT)[,c(2,1)], res = 11))
voa.retail.h3$h3_resolution <- 11
voa.retail.h3$h3_address <- h3::geo_to_h3(st_coordinates(voa.retail.PT)[,c(2,1)], res = 11)
voa.retail.PT <- st_join(voa.retail.PT, voa.retail.h3, join = st_within)
voa.retail.PT <- voa.retail.PT[!duplicated(voa.retail.PT$IDval_ID),]
voa.retail.PT$h3_resolution <- NULL

H3_IDS.pc <- bind_cols(voa.retail.h3, voa.retail.PT$Geo_Ref_Type) %>%
  dplyr::rename(Geo_Ref_Type = ...4) %>%
  select(-c(h3_resolution)) %>%
  st_drop_geometry() %>%
  as_tibble()

H3_IDS.pc <- H3_IDS.pc %>%
  group_by(h3_address, Geo_Ref_Type) %>%
  add_count()

H3_IDS.pc <- H3_IDS.pc %>%
  dplyr::group_by(h3_address, Geo_Ref_Type) %>%
  dplyr::summarise(Frequency = mean(n)) %>%
  tidyr::spread(Geo_Ref_Type, Frequency, fill=0)

H3_IDS.pc <- H3_IDS.pc %>%
  filter(((PCD > 0) & (ADD == 0))) %>%
  pull(h3_address)

voa.retail.h3 <- unique(voa.retail.h3)
voa.retail.h3$source <- ifelse(voa.retail.h3$h3_address %in% H3_IDS.pc, "VOA_Postcode", "VOA_Address")

H3_counts <- as_tibble(voa.retail.PT) %>%
  select(h3_address) %>%
  group_by(h3_address) %>%
  add_count(name = "retailN") %>%
  filter(!is.na(h3_address) & h3_address!="") %>% 
  unique()

voa.retail.h3 <- merge(voa.retail.h3, H3_counts, all.x=T, sort=F, by="h3_address")
rm(H3_IDS.pc, H3_counts); gc()
#------------------------------------------------------------------------------------


# CONVERT OSM POINTS TO H3
#------------------------------------------------------------------------------------
osm.retailPT.h3 <- h3::h3_to_geo_boundary_sf(h3::geo_to_h3(st_coordinates(osm.retail.PT)[,c(2,1)], res = 11))
osm.retailPT.h3$h3_resolution <- 11
osm.retailPT.h3$h3_address <- h3::geo_to_h3(st_coordinates(osm.retail.PT)[,c(2,1)], res = 11)
osm.retail.PT <- st_join(osm.retail.PT, osm.retailPT.h3, join = st_within)
osm.retail.PT <- osm.retail.PT[!duplicated(osm.retail.PT$osm_id),]
osm.retail.PT$h3_resolution <- NULL
osm.retailPT.h3 <- unique(osm.retailPT.h3)

H3_counts <- as_tibble(osm.retail.PT) %>%
  select(h3_address) %>%
  group_by(h3_address) %>%
  add_count(name = "retailN") %>%
  filter(!is.na(h3_address) & h3_address!="") %>% 
  unique()

osm.retailPT.h3 <- merge(osm.retailPT.h3, H3_counts, all.x=T, sort=F, by="h3_address")
osm.retailPT.h3 <- unique(osm.retailPT.h3)
osm.retailPT.h3$source <- "OSM_Retail"
rm(H3_counts, osm.shopPT, osm.amenityPT); gc()
#------------------------------------------------------------------------------------


# CONVERT OSM POLYGONS TO H3
#------------------------------------------------------------------------------------
# cl <- makeCluster(floor(detectCores()/2+10))
# registerDoSNOW(cl)
# H3_osm.retail.LU.13 <- list()
# H3_osm.retail.LU.13 <- foreach(j = 1:dim(osm.retail.LU)[1], .combine = c, .options.snow = list(progress = function(n) setTxtProgressBar(txtProgressBar(max = dim(osm.retail.LU)[1], style = 3), n)), .packages=c("h3", "sf")) %dopar% {
#   h3::polyfill(st_geometry(osm.retail.LU[j,]), res = 13) }
# stopCluster(cl)
# 
# H3_osm.retail.LU.13 <- unique(H3_osm.retail.LU.13)
# H3_osm.retail.LU.13 <- data.table(Child = H3_osm.retail.LU.13)
# 
# osm.retail.LUTR <- st_buffer(st_transform(osm.retail.LU, crs = 27700), 10)
# osm.retail.LUTR <- st_transform(osm.retail.LU, crs = 4326)
# 
# cl <- makeCluster(floor(detectCores()/2+10))
# registerDoSNOW(cl)
# H3_osm.retail.LU.11 <- list()
# H3_osm.retail.LU.11 <- foreach(j = 1:dim(osm.retail.LUTR)[1], .combine = c, .options.snow = list(progress = function(n) setTxtProgressBar(txtProgressBar(max = dim(osm.retail.LUTR)[1], style = 3), n)), .packages=c("h3", "sf")) %dopar% {
#   h3::polyfill(st_geometry(osm.retail.LUTR[j,]), res = 11) }
# stopCluster(cl)
# 
# H3_osm.retail.LU.11 <- unique(H3_osm.retail.LU.11)
# H3_osm.retail.LU.11 <- data.table(Parent = H3_osm.retail.LU.11)
# 
# cl <- makeCluster(floor(detectCores()/2+10))
# registerDoSNOW(cl)
# tt <- list()
# tt <- foreach(j = 1:length(H3_osm.retail.LU.11$Parent), .combine = rbind, .options.snow = list(progress = function(n) setTxtProgressBar(txtProgressBar(max = length(H3_osm.retail.LU.11$Parent), style = 3), n)), .packages=c("h3")) %dopar% {
#   cbind(H3_osm.retail.LU.11$Parent[j], h3::h3_to_children(H3_osm.retail.LU.11$Parent[j], 13)) }
# stopCluster(cl)
# tt <- as.data.table(tt)
# names(tt) <- c("Parent", "Child")
# 
# H3_osm.retail.LU.13 <- merge(H3_osm.retail.LU.13, tt, all.x=T, sort=F, by="Child")
# H3_osm.retail.LU.13$Child <- as.factor(H3_osm.retail.LU.13$Child)
# H3_osm.retail.LU.13$Parent <- as.factor(H3_osm.retail.LU.13$Parent)
# 
# H3_osm.retail.LU.13.A <- H3_osm.retail.LU.13[!is.na(H3_osm.retail.LU.13$Parent),]
# 
# H3_osm.retail.LU.13.B <- H3_osm.retail.LU.13[is.na(H3_osm.retail.LU.13$Parent),]
# H3_osm.retail.LU.13.B$Child <- as.character(H3_osm.retail.LU.13.B$Child)
# H3_osm.retail.LU.13.B$Parent <- as.character(H3_osm.retail.LU.13.B$Parent)
# gc()
# 
# cl <- makeCluster(detectCores()-1)
# registerDoSNOW(cl)
# tt <- list()
# tt <- foreach(j = 1:length(H3_osm.retail.LU.13.B$Child), .combine = rbind, .options.snow = list(progress = function(n) setTxtProgressBar(txtProgressBar(max = length(H3_osm.retail.LU.13.B$Child), style = 3), n)), .packages=c("h3")) %dopar% {
#   cbind(H3_osm.retail.LU.13.B$Child[j], h3::h3_to_parent(H3_osm.retail.LU.13.B$Child[j], 11)) }
# stopCluster(cl)
# tt <- as.data.table(tt)
# names(tt) <- c("Child", "Parent")
# 
# H3_osm.retail.LU.13.B$Parent <- NULL
# H3_osm.retail.LU.13.B <- merge(H3_osm.retail.LU.13.B, tt, all.x=T, sort=F, by="Child")
# H3_osm.retail.LU.13.B$Child <- as.factor(H3_osm.retail.LU.13.B$Child)
# H3_osm.retail.LU.13.B$Parent <- as.factor(H3_osm.retail.LU.13.B$Parent)
# 
# H3_osm.retail.LU.13 <- rbind(H3_osm.retail.LU.13.A, H3_osm.retail.LU.13.B)
# 
# osm.retailLU.h3 <- h3::h3_to_geo_boundary_sf(H3_osm.retail.LU.13$Parent)
# osm.retailLU.h3$h3_resolution <- 11
# osm.retailLU.h3$h3_address <- H3_osm.retail.LU.13$Parent
# osm.retailLU.h3$source <- "OSM_Poly"
# osm.retailLU.h3$h3_address <- as.character(osm.retailLU.h3$h3_address)
# 
# osm.retailLU.h3 <- osm.retailLU.h3 %>%
#   select(h3_address, h3_resolution, source)
# osm.retailLU.h3 <- unique(osm.retailLU.h3)
# rm(tt, osm.retail.LUTR, H3_osm.retail.LU.11, H3_osm.retail.LU.13, H3_osm.retail.LU.13.A, H3_osm.retail.LU.13.B, cl)
# 
# hex_Graph <- get.tractID(osm.retailLU.h3$h3_address, retail.count=FALSE, NAME="OSMPoly")
# osm.retailLU.h3 <- osm.retailLU.h3 %>%
#   left_join(as_tibble(hex_Graph)) %>%
#   unique()
# rm(hex_Graph)
# osm.retailLU.h3$retailN <- NA
# 
# st_write(osm.retailLU.h3, dsn = paste0(wd, "/Data/OSM/EW_OSM_H3LU.gpkg"), delete_layer = TRUE, layer="OSM_EW_pl", driver = "gpkg")

osm.retailLU.h3 <- st_read(dsn=paste0(wd, "/Data/OSM/EW_OSM_H3LU.gpkg"))
names(osm.retailLU.h3)[names(osm.retailLU.h3)=="geom"] <- c("geometry")
st_geometry(osm.retailLU.h3) <- "geometry"
#------------------------------------------------------------------------------------


# COMPILING THE FIRST MASTER SET OF RC H3s
#------------------------------------------------------------------------------------
H3_RC <- rbind(voa.retail.h3[voa.retail.h3$source!="VOA_Postcode",], osm.retailLU.h3[!(osm.retailLU.h3$h3_address %in% voa.retail.h3$h3_address), !(colnames(osm.retailLU.h3) %in% c("tractID", "tractN"))])
H3_RC <- rbind(H3_RC, osm.retailPT.h3[!(osm.retailPT.h3$h3_address %in% H3_RC$h3_address), !(colnames(osm.retailPT.h3) %in% c("tractID", "tractN", "tr_retailN"))])
H3_RC <- H3_RC[!is.na(H3_RC$h3_address) | H3_RC$h3_address!="",]
H3_RC <- unique(H3_RC)

retail_centres <- H3_RC
retail_centres <- retail_centres[retail_centres$retailN > 1 | retail_centres$source=="OSM_Poly",]
retail_centres <- retail_centres[!is.na(retail_centres$h3_address),]
#------------------------------------------------------------------------------------


# PRUNING AND REMOVAL OF ISLANDS
#------------------------------------------------------------------------------------
hex_Graph <- get.tractID(H3_ADDRESS=retail_centres$h3_address, RETAIL_N=retail_centres$retailN, retail.count=TRUE, NAME="RetailCentreID", source=retail_centres$source)
retail_centres <- retail_centres %>%
  left_join(hex_Graph, by="h3_address") %>%
  filter(!is.na(h3_address) & h3_address!="") %>%
  unique()

osm.retail.PT <- osm.retail.PT[,colnames(osm.retail.PT) %in% c("osm_id", "name", "value", "key")]
names(osm.retail.PT)[names(osm.retail.PT)=="geom"] <- c("geometry")
st_geometry(osm.retail.PT) <- "geometry"
osm.retail.PT <- st_join(osm.retail.PT, retail_centres, join = st_within)
osm.retail.PT <- osm.retail.PT[!duplicated(osm.retail.PT$osm_id),]
osm.retail.PT <- osm.retail.PT[,colnames(osm.retail.PT) %in% c("osm_id", "name", "value", "key", "h3_address")]

H3_counts <- as_tibble(osm.retail.PT) %>%
  select(h3_address) %>%
  group_by(h3_address) %>%
  add_count(name = "OSM_Point_N") %>%
  filter(!is.na(h3_address) & h3_address!="") %>% 
  unique()
retail_centres <- merge(retail_centres, H3_counts, all.x=T, sort=F, by="h3_address")

hex_Graph.osm <- get.tractID(retail_centres$h3_address, retail_centres$OSM_Point_N, retail.count=TRUE, NAME="RetailCentreID2")
hex_Graph.osm <- hex_Graph.osm[,colnames(hex_Graph.osm) %in% c("h3_address", "tr_retailN")]
names(hex_Graph.osm)[names(hex_Graph.osm)=="tr_retailN"] <- "tr_OSM_retailN"

retail_centres <- retail_centres %>%
  left_join(hex_Graph.osm, by="h3_address") %>%
  filter(!is.na(h3_address) & h3_address!="") %>%
  unique()


# We specify OSM tracts as large, mostly OSM land use tracts. So the overall VOA count has to be low and concentrated to only a few of the H3 with
#   most of the tract coming from OSM land use.
# So the only OSM type of tract that we keep will be those with land use surrounding a H3 hexagon (or a few hexagons) with VOA retail.
osm.tract <- unique(retail_centres$tractID[which(retail_centres$OSM_Poly_N/retail_centres$tractN > 0.75 & retail_centres$VOA_Address_N < 10 & retail_centres$tractN >= 3)])

retail_centres.osm <- retail_centres[retail_centres$tractID %in% osm.tract,]
retail_centres.osm <- retail_centres.osm[!is.na(retail_centres.osm$h3_address) | retail_centres.osm$h3_address!="",]
retail_centres.osm <- st_sf(as.data.frame(retail_centres.osm))
retail_centres.osm <- merge(retail_centres.osm, H3_counts, all.x=T, sort=F, by="h3_address")
retail_centres.osm <- unique(retail_centres.osm)

retail_centres.osm$OSM_Point_N.y <- NULL
names(retail_centres.osm)[names(retail_centres.osm)=="OSM_Point_N.x"] <- "OSM_Point_N"

retail_centres.osm <- retail_centres.osm[retail_centres.osm$tractID %in% unique(retail_centres.osm[which(!is.na(retail_centres.osm$OSM_Point_N)),]$tractID),]

retail_centres <- retail_centres[!(retail_centres$tractID %in% osm.tract),]
retail_centres$OSM_Point_N <- NULL
retail_centres1 <- retail_centres[retail_centres$tractN==1,]
retail_centres1 <- retail_centres1[retail_centres1$retailN >= 10 & retail_centres1$source=="VOA_Address",]
retail_centres <- retail_centres[retail_centres$tractN >= 2,]
retail_centres <- rbind(retail_centres, retail_centres1)
retail_centres <- merge(retail_centres, H3_counts, all.x=T, sort=F, by="h3_address")
retail_centres <- unique(retail_centres)
rm(retail_centres1)

retail_centres <- retail_centres[retail_centres$tractID %in% unique(retail_centres[which(retail_centres$tr_retailN>=10),]$tractID),]
retail_centres <- st_sf(as.data.frame(retail_centres))

retail_centres <- rbind(retail_centres, retail_centres.osm)
retail_centres <- retail_centres[!is.na(retail_centres$h3_address) | retail_centres$h3_address!="",]
retail_centres <- unique(retail_centres)

retail_centres$tractN <- NULL
retail_centres$tractID <- NULL
retail_centres$tr_retailN <- NULL
retail_centres$OSM_Amenity_N <- NULL
retail_centres$OSM_Poly_N <- NULL
retail_centres$VOA_Address_N <- NULL
retail_centres$OSM_Retail_N <- NULL
retail_centres$tr_OSM_retailN <- NULL

colnames(retail_centres)[colnames(retail_centres)=="retailN"] <- "h3_VOA_retailN"
colnames(retail_centres)[colnames(retail_centres)=="OSM_Point_N"] <- "h3_OSM_retailN"

hex_Graph <- get.tractID(retail_centres$h3_address, retail_centres$h3_VOA_retailN, retail.count=TRUE, NAME="RC_EW", source=retail_centres$source)
retail_centres <- retail_centres %>%
  left_join(as_tibble(hex_Graph)) %>%
  unique()
colnames(retail_centres)[colnames(retail_centres)=="tractN"] <- "H3_count"
colnames(retail_centres)[colnames(retail_centres)=="tr_retailN"] <- "tr_VOA_retailN"
colnames(retail_centres)[colnames(retail_centres)=="OSM_Poly_N"] <- "OSM_PolyH3"
colnames(retail_centres)[colnames(retail_centres)=="VOA_Address_N"] <- "VOA_AddressH3"
colnames(retail_centres)[colnames(retail_centres)=="OSM_Retail_N"] <- "OSM_PointH3"

hex_Graph2 <- get.tractID(retail_centres$h3_address, retail_centres$h3_OSM_retailN, retail.count=TRUE, NAME="RC_EW")
hex_Graph2 <- hex_Graph2[,c("h3_address", "tr_retailN")]
names(hex_Graph2) <- c("h3_address", "tr_OSM_retailN")

retail_centres <- retail_centres %>%
  left_join(as_tibble(hex_Graph2)) %>%
  unique()

retail_centres <- retail_centres[!(retail_centres$tractID %in% retail_centres$tractID[which(retail_centres$tr_VOA_retailN < 15 & retail_centres$tr_OSM_retailN < 10)]),]
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
  select(tractID, tr_VOA_retailN, tr_OSM_retailN, H3_count, OSM_PolyH3, VOA_AddressH3, OSM_PointH3) %>%
  unique()

RC.dissolved <- left_join(RC.dissolved, RC.x)
rm(hex_Graph, RC.x); gc(verbose=FALSE)

st_write(RC.dissolved, dsn = paste0(wd, "/Exports/Interim/EW_RC_Complete.gpkg"), delete_layer = TRUE, driver = "gpkg")
st_write(retail_centres, dsn = paste0(wd, "/Exports/Interim/EW_H3_Complete.gpkg"), delete_layer = TRUE, driver = "gpkg")
#------------------------------------------------------------------------------------
