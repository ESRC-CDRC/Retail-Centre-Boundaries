########################################################################################
########  An open source delineation and hierarchical classification 
#######     of UK retail agglomeration
######
#####     Jacob L. Macdonald, Les Dolega, Alex Singleton
####      Last Updated: October, 2021
###
##  BASE OSM DATA EXTRACTION AND CLEANING
##   
##  - Code for extracting and exporting raw OSM data to use in the delineation of
##    retail centres.
##  - Replicated for England and Wales, Scotland, and Northern Ireland
##
##  - Requires: 
##        Country Boundaries: Countries_(December_2018)_Boundaries_UK_BFC.shp
##        OSM Data Download API for R
##        Functions.R file with functions for splitting bounding large bounding boxes
##
##  Notes: 1) 3 files per country group exported to be used externally: Shops (Points), 
##            Amenities (Points), and Retail Land Use (Polygons)
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
# <WORKING DIRECTORY> ("wd")
#      |
#      |--Code
#      |
#      |--Data
#      |   |--Boundaries and Lookups
#      |   |--OSM
#      |   |--VOA
#      |
#      |--Exports
#      |   |--Interim
#      |   |--Final

# wd <- "<FILE PATH TO WORKING DIRECTORY>"

# wd <- "/Users/jake_mac02/Dropbox/Research/Retail Geography/Retail"
wd <- "/home/jacobmac/Dropbox/Research/Retail Geography/Retail"

# Functions.R includes a set of functions related to h3 geometry tract connectivity,
#   infilling and spatial operations.
source(paste0(wd, "/Code/Functions.R"))
#------------------------------------------------------------------------------------


# READ IN BOUNDARIES FOR CLIPPING
#------------------------------------------------------------------------------------
boundary <- st_read(paste0(wd, "/Data/Boundaries and Lookups/Countries_(December_2018)_Boundaries_UK_BFC.shp"))

ENGLAND_WALES <- boundary[boundary$ctry18nm!="Northern Ireland" & boundary$ctry18nm!="Scotland",]
ENGLAND_WALES <- st_transform(ENGLAND_WALES, crs=4326)

NORTHERN_IRELAND <- boundary[boundary$ctry18nm=="Northern Ireland",]
NORTHERN_IRELAND <- st_transform(NORTHERN_IRELAND, crs=4326)

SCOTLAND <- boundary[boundary$ctry18nm=="Scotland",]
SCOTLAND <- st_transform(SCOTLAND, crs=4326)
rm(boundary)
#------------------------------------------------------------------------------------


# List of OSM Amenity and Shop tags with respective counts and frequency
#   osm_tags <- fread(paste0(wd, "/Data/OSMtags_2021_02_03.csv"))


# ENGLAND AND WALES OSM POINTS - SHOPS AND AMENITIES
#------------------------------------------------------------------------------------
# We extract only a subset of amenities, and do so in sequence
# Given the size of the areas of interest, we use the bb.list() function
#   This function takes a bounding box and splits it into a list of smaller grids
#   which can be processed easier in parallel.
# To get the full set of unique (singular) points representing each shop, we need to 
#   extract OSM data represented by (centroid) points, by polygons and by nodes seperate.
#   Only a unique point representing the centroid is kept for each unique retail unit.

amenity.sub <- c("restaurant", "cafe", "fast_food", "bank", "pharmacy", "bar", "pub", "post_office",
  "marketplace", "nightclub", "bureau_de_change", "food_court")
amenity.points <- list()
for(i in 1:length(amenity.sub)){
  osm.amenity <- lapply(bb.list(getbb("England"), sq.N=5), function(x) {
    opq(bbox = x) %>%
      add_osm_feature(key = "amenity", value = amenity.sub[i]) %>%
      osmdata_sf()})
  
  osm.amenityPT <- osm.amenity[which(do.call(rbind, lapply(osm.amenity, function(y) dim(y$osm_points)[1]!=0)))]
  osm.amenityPT <- lapply(osm.amenityPT, function(x) x$osm_points)
  osm.amenityPT <- lapply(osm.amenityPT, function(x) x[, colnames(x) %in% Reduce(intersect, lapply(osm.amenityPT, names))])
  osm.amenityPT <- do.call(rbind, osm.amenityPT)
  if(is.null(osm.amenityPT$name)) { osm.amenityPT$name <- NA }
  osm.amenityPT <- unique(osm.amenityPT)
  t <- data.table(st_join(osm.amenityPT, ENGLAND_WALES[,c("ctry18nm")], join = st_intersects))[,c("osm_id", "ctry18nm")]
  osm.amenityPT <- osm.amenityPT[osm.amenityPT$osm_id %in% t[!is.na(t$ctry18nm), c("osm_id")]$osm_id,]
  osm.amenityPT$value <- amenity.sub[i]
  
  osm.amenityPL <- osm.amenity[which(do.call(rbind, lapply(osm.amenity, function(y) dim(y$osm_polygon)[1]!=0)))]
  osm.amenityPL <- lapply(osm.amenityPL, function(x) x$osm_polygon)
  osm.amenityPL <- lapply(osm.amenityPL, function(x) x[, colnames(x) %in% Reduce(intersect, lapply(osm.amenityPL, names))])
  osm.amenityPL <- do.call(rbind, osm.amenityPL)
  if(is.null(osm.amenityPL$name)) { osm.amenityPL$name <- NA }
  osm.amenityPL <- unique(osm.amenityPL)
  t <- data.table(st_join(osm.amenityPL, ENGLAND_WALES[,c("ctry18nm")], join = st_intersects))[,c("osm_id", "ctry18nm")]
  osm.amenityPL <- osm.amenityPL[osm.amenityPL$osm_id %in% t[!is.na(t$ctry18nm), c("osm_id")]$osm_id,]
  osm.amenityPL$value <- amenity.sub[i]
  
  osm.amenityPT.node <- osm.amenityPT[is.na(osm.amenityPT$name),]
  osm.amenityPT.node <- st_transform(st_buffer(st_transform(osm.amenityPT.node, crs=27700), 1.5), crs=4326)
  
  osm.amenityPT.2 <- osm.amenityPT[is.na(osm.amenityPT$name),]
  osm.amenityPT.2 <- osm.amenityPT.2[!(osm.amenityPT.2$osm_id %in% unique(st_intersection(osm.amenityPT.node, osm.amenityPL)$osm_id)),]
  
  osm.amenityPT.3 <- st_centroid(osm.amenityPL)
  
  osm.amenityPT.list <- list(osm.amenityPT[!is.na(osm.amenityPT$name),], osm.amenityPT.2, osm.amenityPT.3)
  osm.amenityPT.list <- lapply(osm.amenityPT.list, function(x) { x[, colnames(x) %in% Reduce(intersect, lapply(osm.amenityPT.list, names))] })
  amenity.points[[i]] <- do.call(rbind, osm.amenityPT.list)
  rm(t, osm.amenityPT, osm.amenityPL, osm.amenityPT.node, osm.amenityPT.2, osm.amenityPT.3, osm.amenityPT.list, osm.amenity); gc()
  
  cat("\r", i, " of", length(amenity.sub))
  flush.console()
}
osm.amenityPT <- do.call(plyr::rbind.fill, amenity.points[which(do.call(rbind, lapply(amenity.points, function(x) dim(x)[1]))!=0)])
osm.amenityPT$key <- "amenity"
osm.amenityPT <- st_as_sf(osm.amenityPT, crs=4326)
osm.amenityPT <- osm.amenityPT[,c("osm_id", "name", "value", "key")]
osm.amenityPT <- unique(osm.amenityPT)

# Replicating now for OSM shops - although we take the entire set of all 'shops'

osm.shop <- lapply(bb.list(getbb("England"), sq.N=6), function(x) {
  opq(bbox = x) %>%
    add_osm_feature(key = 'shop') %>%
    osmdata_sf()})

osm.shopPT <- osm.shop[which(do.call(rbind, lapply(osm.shop, function(y) dim(y$osm_points)[1]!=0)))]
osm.shopPT <- lapply(osm.shopPT, function(x) x$osm_points)
osm.shopPT <- lapply(osm.shopPT, function(x) x[, colnames(x) %in% Reduce(intersect, lapply(osm.shopPT, names))])
osm.shopPT <- do.call(rbind, osm.shopPT)
if(is.null(osm.shopPT$name)) { osm.shopPT$name <- NA }
osm.shopPT <- unique(osm.shopPT)
t <- data.table(st_join(osm.shopPT, ENGLAND_WALES[,c("ctry18nm")], join = st_intersects))[,c("osm_id", "ctry18nm")]
osm.shopPT <- osm.shopPT[osm.shopPT$osm_id %in% t[!is.na(t$ctry18nm), c("osm_id")]$osm_id,]
osm.shopPT$key <- "shop"
names(osm.shopPT)[names(osm.shopPT)=="shop"] <- "value"
osm.shopPT <- st_as_sf(osm.shopPT, crs=4326)
osm.shopPT <- osm.shopPT[,c("osm_id", "name", "value", "key")]
osm.shopPT <- unique(osm.shopPT)

osm.shopPL <- osm.shop[which(do.call(rbind, lapply(osm.shop, function(y) dim(y$osm_polygon)[1]!=0)))]
osm.shopPL <- lapply(osm.shopPL, function(x) x$osm_polygon)
osm.shopPL <- lapply(osm.shopPL, function(x) x[, colnames(x) %in% Reduce(intersect, lapply(osm.shopPL, names))])
osm.shopPL <- do.call(rbind, osm.shopPL)
if(is.null(osm.shopPL$name)) { osm.shopPL$name <- NA }
osm.shopPL <- unique(osm.shopPL)
t <- data.table(st_join(osm.shopPL, ENGLAND_WALES[,c("ctry18nm")], join = st_intersects))[,c("osm_id", "ctry18nm")]
osm.shopPL <- osm.shopPL[osm.shopPL$osm_id %in% t[!is.na(t$ctry18nm), c("osm_id")]$osm_id,]
osm.shopPL$key <- "shop"

osm.shopPT.node <- osm.shopPT[is.na(osm.shopPT$name),]
osm.shopPT.node <- st_transform(st_buffer(st_transform(osm.shopPT, crs=27700), 1.5), crs=4326)

osm.shopPT.2 <- osm.shopPT[is.na(osm.shopPT$name),]
osm.shopPT.2 <- osm.shopPT.2[!(osm.shopPT.2$osm_id %in% unique(st_intersection(osm.shopPT.node, osm.shopPL)$osm_id)),]

osm.shopPT.3 <- st_centroid(osm.shopPL)

osm.shopPT.list <- list(osm.shopPT[!is.na(osm.shopPT$name),], osm.shopPT.2, osm.shopPT.3)
osm.shopPT.list <- lapply(osm.shopPT.list, function(x) { x[, colnames(x) %in% Reduce(intersect, lapply(osm.shopPT.list, names))] })
osm.shopPT <- do.call(rbind, osm.shopPT.list)

osm.shopPT <- st_as_sf(osm.shopPT, crs=4326)
osm.shopPT <- osm.shopPT[,c("osm_id", "name", "key")]
osm.shopPT <- unique(osm.shopPT)
rm(t, osm.shopPL, osm.shopPT.node, osm.shopPT.2, osm.shopPT.3, osm.shopPT.list); gc()
#------------------------------------------------------------------------------------

# ENGLAND AND WALES OSM LAND USE
#------------------------------------------------------------------------------------
osm.retailLU <- lapply(bb.list(getbb("England"), sq.N=11), function(x) {
  opq(bbox = x) %>%
    add_osm_feature(key = "landuse", value="retail") %>%
    osmdata_sf()})
osm.retailLU <- osm.retailLU[which(do.call(rbind, lapply(osm.retailLU, function(y) dim(y$osm_polygons)[1]!=0)))]
osm.retailLU <- lapply(osm.retailLU, function(x) x$osm_polygons)
osm.retailLU <- lapply(osm.retailLU, function(x) x[, colnames(x) %in% Reduce(intersect, lapply(osm.retailLU, names))])
osm.retailLU <- do.call(rbind, osm.retailLU)
osm.retailLU <- unique(osm.retailLU)
t <- data.table(st_join(st_centroid(osm.retailLU), ENGLAND_WALES[,c("ctry18nm")], join = st_intersects))[,c("osm_id", "ctry18nm")]
osm.retailLU <- osm.retailLU[osm.retailLU$osm_id %in% t[!is.na(t$ctry18nm), c("osm_id")]$osm_id,]
rm(t)
#------------------------------------------------------------------------------------

# ENGLAND AND WALES OSM EXPORTS
#  - Print tag of 301020 are attached to the exports to represent the day they were
#    downloaded.
#------------------------------------------------------------------------------------
st_write(osm.amenityPT, dsn = paste0(wd, "/Data/OSM/EW_OSM_amenity.gpkg"), delete_layer = TRUE, driver = "gpkg")
st_write(osm.shopPT, dsn = paste0(wd, "/Data/OSM/EW_OSM_shop.gpkg"), delete_layer = TRUE, driver = "gpkg")
st_write(osm.retailLU, dsn = paste0(wd, "/Data/OSM/EW_OSM_LU.gpkg"), delete_layer = TRUE, driver = "gpkg")
#------------------------------------------------------------------------------------


# SCOTLAND OSM POINTS - SHOPS AND AMENITIES
#------------------------------------------------------------------------------------
amenity.sub <- c("restaurant", "cafe", "fast_food", "bank", "pharmacy", "bar", "pub", "post_office",
  "marketplace", "nightclub", "bureau_de_change", "food_court")
amenity.points <- list()
for(i in 1:length(amenity.sub)){
  osm.amenity <- lapply(bb.list(getbb("Scotland"), sq.N=3), function(x) {
    opq(bbox = x) %>%
      add_osm_feature(key = "amenity", value = amenity.sub[i]) %>%
      osmdata_sf()})

  osm.amenityPT <- osm.amenity[which(do.call(rbind, lapply(osm.amenity, function(y) dim(y$osm_points)[1]!=0)))]
  osm.amenityPT <- lapply(osm.amenityPT, function(x) x$osm_points)
  osm.amenityPT <- lapply(osm.amenityPT, function(x) x[, colnames(x) %in% Reduce(intersect, lapply(osm.amenityPT, names))])
  osm.amenityPT <- do.call(rbind, osm.amenityPT)
  if(is.null(osm.amenityPT$name)) { osm.amenityPT$name <- NA }
  osm.amenityPT <- unique(osm.amenityPT)
  t <- data.table(st_join(osm.amenityPT, SCOTLAND[,c("ctry18nm")], join = st_intersects))[,c("osm_id", "ctry18nm")]
  osm.amenityPT <- osm.amenityPT[osm.amenityPT$osm_id %in% t[t$ctry18nm=="Scotland", c("osm_id")]$osm_id,]
  osm.amenityPT$value <- amenity.sub[i]

  osm.amenityPL <- osm.amenity[which(do.call(rbind, lapply(osm.amenity, function(y) dim(y$osm_polygon)[1]!=0)))]
  osm.amenityPL <- lapply(osm.amenityPL, function(x) x$osm_polygon)
  osm.amenityPL <- lapply(osm.amenityPL, function(x) x[, colnames(x) %in% Reduce(intersect, lapply(osm.amenityPL, names))])
  osm.amenityPL <- do.call(rbind, osm.amenityPL)
  if(is.null(osm.amenityPL$name)) { osm.amenityPL$name <- NA }
  osm.amenityPL <- unique(osm.amenityPL)
  t <- data.table(st_join(osm.amenityPL, SCOTLAND[,c("ctry18nm")], join = st_intersects))[,c("osm_id", "ctry18nm")]
  osm.amenityPL <- osm.amenityPL[osm.amenityPL$osm_id %in% t[t$ctry18nm=="Scotland", c("osm_id")]$osm_id,]
  osm.amenityPL$value <- amenity.sub[i]

  osm.amenityPT.node <- osm.amenityPT[is.na(osm.amenityPT$name),]
  osm.amenityPT.node <- st_transform(st_buffer(st_transform(osm.amenityPT.node, crs=27700), 1.5), crs=4326)

  osm.amenityPT.2 <- osm.amenityPT[is.na(osm.amenityPT$name),]
  osm.amenityPT.2 <- osm.amenityPT.2[!(osm.amenityPT.2$osm_id %in% unique(st_intersection(osm.amenityPT.node, osm.amenityPL)$osm_id)),]

  osm.amenityPT.3 <- st_centroid(osm.amenityPL)

  osm.amenityPT.list <- list(osm.amenityPT[!is.na(osm.amenityPT$name),], osm.amenityPT.2, osm.amenityPT.3)
  osm.amenityPT.list <- lapply(osm.amenityPT.list, function(x) { x[, colnames(x) %in% Reduce(intersect, lapply(osm.amenityPT.list, names))] })
  amenity.points[[i]] <- do.call(rbind, osm.amenityPT.list)
  rm(t, osm.amenityPT, osm.amenityPL, osm.amenityPT.node, osm.amenityPT.2, osm.amenityPT.3, osm.amenityPT.list, osm.amenity); gc()

  cat("\r", i, " of", length(amenity.sub))
  flush.console()
}
osm.amenityPT <- do.call(plyr::rbind.fill, amenity.points[which(do.call(rbind, lapply(amenity.points, function(x) dim(x)[1]))!=0)])
osm.amenityPT$key <- "amenity"
osm.amenityPT <- st_as_sf(osm.amenityPT, crs=4326)
osm.amenityPT <- osm.amenityPT[,c("osm_id", "name", "value", "key")]
osm.amenityPT <- unique(osm.amenityPT)

# Replicating now for OSM shops - although we take the entire set of all 'shops'

osm.shop <- lapply(bb.list(getbb("Scotland"), sq.N=4), function(x) {
  opq(bbox = x) %>%
    add_osm_feature(key = "shop") %>%
    osmdata_sf()})

osm.shopPT <- osm.shop[which(do.call(rbind, lapply(osm.shop, function(y) dim(y$osm_points)[1]!=0)))]
osm.shopPT <- lapply(osm.shopPT, function(x) x$osm_points)
osm.shopPT <- lapply(osm.shopPT, function(x) x[, colnames(x) %in% Reduce(intersect, lapply(osm.shopPT, names))])
osm.shopPT <- do.call(rbind, osm.shopPT)
if(is.null(osm.shopPT$name)) { osm.shopPT$name <- NA }
osm.shopPT <- unique(osm.shopPT)
t <- data.table(st_join(osm.shopPT, SCOTLAND[,c("ctry18nm")], join = st_intersects))[,c("osm_id", "ctry18nm")]
osm.shopPT <- osm.shopPT[osm.shopPT$osm_id %in% t[t$ctry18nm=="Scotland", c("osm_id")]$osm_id,]
osm.shopPT$key <- "shop"
names(osm.shopPT)[names(osm.shopPT)=="shop"] <- "value"
osm.shopPT <- st_as_sf(osm.shopPT, crs=4326)
osm.shopPT <- osm.shopPT[,c("osm_id", "name", "value", "key")]
osm.shopPT <- unique(osm.shopPT)

osm.shopPL <- osm.shop[which(do.call(rbind, lapply(osm.shop, function(y) dim(y$osm_polygon)[1]!=0)))]
osm.shopPL <- lapply(osm.shopPL, function(x) x$osm_polygon)
osm.shopPL <- lapply(osm.shopPL, function(x) x[, colnames(x) %in% Reduce(intersect, lapply(osm.shopPL, names))])
osm.shopPL <- do.call(rbind, osm.shopPL)
if(is.null(osm.shopPL$name)) { osm.shopPL$name <- NA }
osm.shopPL <- unique(osm.shopPL)
t <- data.table(st_join(osm.shopPL, SCOTLAND[,c("ctry18nm")], join = st_intersects))[,c("osm_id", "ctry18nm")]
osm.shopPL <- osm.shopPL[osm.shopPL$osm_id %in% t[t$ctry18nm=="Scotland", c("osm_id")]$osm_id,]
osm.shopPL$key <- "shop"

osm.shopPT.node <- osm.shopPT[is.na(osm.shopPT$name),]
osm.shopPT.node <- st_transform(st_buffer(st_transform(osm.shopPT, crs=27700), 1.5), crs=4326)

osm.shopPT.2 <- osm.shopPT[is.na(osm.shopPT$name),]
osm.shopPT.2 <- osm.shopPT.2[!(osm.shopPT.2$osm_id %in% unique(st_intersection(osm.shopPT.node, osm.shopPL)$osm_id)),]

osm.shopPT.3 <- st_centroid(osm.shopPL)

osm.shopPT.list <- list(osm.shopPT[!is.na(osm.shopPT$name),], osm.shopPT.2, osm.shopPT.3)
osm.shopPT.list <- lapply(osm.shopPT.list, function(x) { x[, colnames(x) %in% Reduce(intersect, lapply(osm.shopPT.list, names))] })
osm.shopPT <- do.call(rbind, osm.shopPT.list)

osm.shopPT <- st_as_sf(osm.shopPT, crs=4326)
osm.shopPT <- osm.shopPT[,c("osm_id", "name", "key")]
osm.shopPT <- unique(osm.shopPT)
rm(t, osm.shopPL, osm.shopPT.node, osm.shopPT.2, osm.shopPT.3, osm.shopPT.list); gc()
#------------------------------------------------------------------------------------

# SCOTLAND OSM LAND USE
#------------------------------------------------------------------------------------
osm.retailLU <- opq(bbox = getbb("Scotland")) %>%
  add_osm_feature(key = "landuse", value = "retail") %>%
  osmdata_sf()
osm.retailLU <- osm.retailLU$osm_polygons
osm.retailLU <- unique(osm.retailLU)
t <- data.table(st_join(st_centroid(osm.retailLU), SCOTLAND[,c("ctry18nm")], join = st_intersects))[,c("osm_id", "ctry18nm")]
osm.retailLU <- osm.retailLU[osm.retailLU$osm_id %in% t[t$ctry18nm=="Scotland", c("osm_id")]$osm_id,]
rm(t)
#------------------------------------------------------------------------------------

# SCOTLAND OSM EXPORTS
#  - Print tag of 301020 are attached to the exports to represent the day they were
#    downloaded.
#------------------------------------------------------------------------------------
st_write(osm.amenityPT, dsn = paste0(wd, "/Data/OSM/SC_OSM_amenity.gpkg"), delete_layer = TRUE, driver = "gpkg")
st_write(osm.shopPT, dsn = paste0(wd, "/Data/OSM/SC_OSM_shop.gpkg"), delete_layer = TRUE, driver = "gpkg")
st_write(osm.retailLU, dsn = paste0(wd, "/Data/OSM/SC_OSM_LU.gpkg"), delete_layer = TRUE, driver = "gpkg")
#------------------------------------------------------------------------------------


# NORTHERN IRELAND OSM POINTS - SHOPS AND AMENITIES
#------------------------------------------------------------------------------------
amenity.sub <- c("restaurant", "cafe", "fast_food", "bank", "pharmacy", "bar", "pub", "post_office",
  "marketplace", "nightclub", "bureau_de_change", "food_court")
amenity.points <- list()
for(i in 1:length(amenity.sub)){
  osm.amenity <- lapply(bb.list(getbb("Northern Ireland"), sq.N=2), function(x) {
    opq(bbox = x) %>%
      add_osm_feature(key = "amenity", value = amenity.sub[i]) %>%
      osmdata_sf()})

  osm.amenityPT <- osm.amenity[which(do.call(rbind, lapply(osm.amenity, function(y) dim(y$osm_points)[1]!=0)))]
  osm.amenityPT <- lapply(osm.amenityPT, function(x) x$osm_points)
  osm.amenityPT <- lapply(osm.amenityPT, function(x) x[, colnames(x) %in% Reduce(intersect, lapply(osm.amenityPT, names))])
  osm.amenityPT <- do.call(rbind, osm.amenityPT)
  if(is.null(osm.amenityPT$name)) { osm.amenityPT$name <- NA }
  osm.amenityPT <- unique(osm.amenityPT)
  t <- data.table(st_join(osm.amenityPT, NORTHERN_IRELAND[,c("ctry18nm")], join = st_intersects))[,c("osm_id", "ctry18nm")]
  osm.amenityPT <- osm.amenityPT[osm.amenityPT$osm_id %in% t[t$ctry18nm=="Northern Ireland", c("osm_id")]$osm_id,]
  osm.amenityPT$value <- amenity.sub[i]

  osm.amenityPL <- osm.amenity[which(do.call(rbind, lapply(osm.amenity, function(y) dim(y$osm_polygon)[1]!=0)))]
  osm.amenityPL <- lapply(osm.amenityPL, function(x) x$osm_polygon)
  osm.amenityPL <- lapply(osm.amenityPL, function(x) x[, colnames(x) %in% Reduce(intersect, lapply(osm.amenityPL, names))])
  osm.amenityPL <- do.call(rbind, osm.amenityPL)
  if(is.null(osm.amenityPL$name)) { osm.amenityPL$name <- NA }
  osm.amenityPL <- unique(osm.amenityPL)
  t <- data.table(st_join(osm.amenityPL, NORTHERN_IRELAND[,c("ctry18nm")], join = st_intersects))[,c("osm_id", "ctry18nm")]
  osm.amenityPL <- osm.amenityPL[osm.amenityPL$osm_id %in% t[t$ctry18nm=="Northern Ireland", c("osm_id")]$osm_id,]
  osm.amenityPL$value <- amenity.sub[i]

  osm.amenityPT.node <- osm.amenityPT[is.na(osm.amenityPT$name),]
  osm.amenityPT.node <- st_transform(st_buffer(st_transform(osm.amenityPT.node, crs=27700), 1.5), crs=4326)

  osm.amenityPT.2 <- osm.amenityPT[is.na(osm.amenityPT$name),]
  osm.amenityPT.2 <- osm.amenityPT.2[!(osm.amenityPT.2$osm_id %in% unique(st_intersection(osm.amenityPT.node, osm.amenityPL)$osm_id)),]

  osm.amenityPT.3 <- st_centroid(osm.amenityPL)

  osm.amenityPT.list <- list(osm.amenityPT[!is.na(osm.amenityPT$name),], osm.amenityPT.2, osm.amenityPT.3)
  osm.amenityPT.list <- lapply(osm.amenityPT.list, function(x) { x[, colnames(x) %in% Reduce(intersect, lapply(osm.amenityPT.list, names))] })
  amenity.points[[i]] <- do.call(rbind, osm.amenityPT.list)
  rm(t, osm.amenityPT, osm.amenityPL, osm.amenityPT.node, osm.amenityPT.2, osm.amenityPT.3, osm.amenityPT.list, osm.amenity); gc()

  cat("\r", i, " of", length(amenity.sub))
  flush.console()
}
osm.amenityPT <- do.call(plyr::rbind.fill, amenity.points[which(do.call(rbind, lapply(amenity.points, function(x) dim(x)[1]))!=0)])
osm.amenityPT$key <- "amenity"
osm.amenityPT <- st_as_sf(osm.amenityPT, crs=4326)
osm.amenityPT <- osm.amenityPT[,c("osm_id", "name", "value", "key")]
osm.amenityPT <- unique(osm.amenityPT)

# Replicating now for OSM shops - although we take the entire set of all 'shops'

osm.shop <- lapply(bb.list(getbb("Northern Ireland"), sq.N=2), function(x) {
  opq(bbox = x) %>%
  add_osm_feature(key = "shop") %>%
  osmdata_sf()})

osm.shopPT <- osm.shop[which(do.call(rbind, lapply(osm.shop, function(y) dim(y$osm_points)[1]!=0)))]
osm.shopPT <- lapply(osm.shopPT, function(x) x$osm_points)
osm.shopPT <- lapply(osm.shopPT, function(x) x[, colnames(x) %in% Reduce(intersect, lapply(osm.shopPT, names))])
osm.shopPT <- do.call(rbind, osm.shopPT)
if(is.null(osm.shopPT$name)) { osm.shopPT$name <- NA }
osm.shopPT <- unique(osm.shopPT)
t <- data.table(st_join(osm.shopPT, NORTHERN_IRELAND[,c("ctry18nm")], join = st_intersects))[,c("osm_id", "ctry18nm")]
osm.shopPT <- osm.shopPT[osm.shopPT$osm_id %in% t[t$ctry18nm=="Northern Ireland", c("osm_id")]$osm_id,]
osm.shopPT$key <- "shop"
names(osm.shopPT)[names(osm.shopPT)=="shop"] <- "value"
osm.shopPT <- st_as_sf(osm.shopPT, crs=4326)
osm.shopPT <- osm.shopPT[,c("osm_id", "name", "value", "key")]
osm.shopPT <- unique(osm.shopPT)

osm.shopPL <- osm.shop[which(do.call(rbind, lapply(osm.shop, function(y) dim(y$osm_polygon)[1]!=0)))]
osm.shopPL <- lapply(osm.shopPL, function(x) x$osm_polygon)
osm.shopPL <- lapply(osm.shopPL, function(x) x[, colnames(x) %in% Reduce(intersect, lapply(osm.shopPL, names))])
osm.shopPL <- do.call(rbind, osm.shopPL)
if(is.null(osm.shopPL$name)) { osm.shopPL$name <- NA }
osm.shopPL <- unique(osm.shopPL)
t <- data.table(st_join(osm.shopPL, NORTHERN_IRELAND[,c("ctry18nm")], join = st_intersects))[,c("osm_id", "ctry18nm")]
osm.shopPL <- osm.shopPL[osm.shopPL$osm_id %in% t[t$ctry18nm=="Northern Ireland", c("osm_id")]$osm_id,]
osm.shopPL$key <- "shop"

osm.shopPT.node <- osm.shopPT[is.na(osm.shopPT$name),]
osm.shopPT.node <- st_transform(st_buffer(st_transform(osm.shopPT, crs=27700), 1.5), crs=4326)

osm.shopPT.2 <- osm.shopPT[is.na(osm.shopPT$name),]
osm.shopPT.2 <- osm.shopPT.2[!(osm.shopPT.2$osm_id %in% unique(st_intersection(osm.shopPT.node, osm.shopPL)$osm_id)),]

osm.shopPT.3 <- st_centroid(osm.shopPL)

osm.shopPT.list <- list(osm.shopPT[!is.na(osm.shopPT$name),], osm.shopPT.2, osm.shopPT.3)
osm.shopPT.list <- lapply(osm.shopPT.list, function(x) { x[, colnames(x) %in% Reduce(intersect, lapply(osm.shopPT.list, names))] })
osm.shopPT <- do.call(rbind, osm.shopPT.list)

osm.shopPT <- st_as_sf(osm.shopPT, crs=4326)
osm.shopPT <- osm.shopPT[,c("osm_id", "name", "key")]
osm.shopPT <- unique(osm.shopPT)
rm(t, osm.shopPL, osm.shopPT.node, osm.shopPT.2, osm.shopPT.3, osm.shopPT.list); gc()
#------------------------------------------------------------------------------------

# NORTHERN IRELAND OSM LAND USE
#------------------------------------------------------------------------------------
osm.retailLU <- opq(bbox = getbb("Northern Ireland")) %>%
  add_osm_feature(key = "landuse", value = "retail") %>%
  osmdata_sf()
osm.retailLU <- osm.retailLU$osm_polygons
osm.retailLU <- unique(osm.retailLU)
t <- data.table(st_join(st_centroid(osm.retailLU), NORTHERN_IRELAND[,c("ctry18nm")], join = st_intersects))[,c("osm_id", "ctry18nm")]
osm.retailLU <- osm.retailLU[osm.retailLU$osm_id %in% t[t$ctry18nm=="Northern Ireland", c("osm_id")]$osm_id,]
rm(t)
#------------------------------------------------------------------------------------

# NORTHERN IRELAND OSM EXPORTS
#  - Print tag of 301020 are attached to the exports to represent the day they were
#    downloaded.
#------------------------------------------------------------------------------------
st_write(osm.amenityPT, dsn = paste0(wd, "/Data/OSM/NI_OSM_amenity.gpkg"), delete_layer = TRUE, driver = "gpkg")
st_write(osm.shopPT, dsn = paste0(wd, "/Data/OSM/NI_OSM_shop.gpkg"), delete_layer = TRUE, driver = "gpkg")
st_write(osm.retailLU, dsn = paste0(wd, "/Data/OSM/NI_OSM_LU.gpkg"), delete_layer = TRUE, driver = "gpkg")
#------------------------------------------------------------------------------------