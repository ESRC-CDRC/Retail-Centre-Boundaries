########################################################################################
########  An open source delineation and hierarchical classification 
#######     of UK retail agglomeration
######
#####     Jacob L. Macdonald, Les Dolega, Alex Singleton
####      Last Updated: October, 2021
###
##  MANUAL EDITS OF THE COMPLETE RETAIL CENTRE LIST
##   
##  - Manual additions, deletions, splitting and merging of the core set of retail 
##    agglomerations is done to prune the comprehensive set. 
##  - A pruned collection of retail agglomerations and boundaries are exported
##    to the /Exports/Interim/ subfolder.
##   
##  - Requires: 
##        OSM and VOA points (with h3 reference)
##        EW/ SC/ NI Retail Boundaries and H3 Complete polygons
##    
##  Notes: 1) Individual EW/SC/NI agglomerations are combined here and pruned into a set of
##            UK cleaned retail agglomerations
##         2) Replicable IDs are generated during this process, and are swapped out
##            with the existing equivalent IDs in circulation using a look up.
##


# LOAD PACKAGES; SET WD; READ IN FUNCTIONS
#------------------------------------------------------------------------------------
Packages <- c("tidyverse", "sf", "rgdal", "data.table", "h3", "tidygraph", "parallel", 
  "doSNOW", "igraph", "smoothr", "gtable", "gridExtra", "osmdata", "dplyr")
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

# wd <- "<FILE PATH TO WORKING DIRECTORY>"

# wd <- "/Users/jake_mac02/Dropbox/Research/Retail Geography/Retail"
wd <- "/home/jacobmac/Dropbox/Research/Retail Geography/Retail"

# Functions.R includes a set of functions related to h3 geometry tract connectivity,
#   infilling and spatial operations.
source(paste0(wd, "/Code/Functions.R"))
#------------------------------------------------------------------------------------


# LOAD IN RETAIL POINTS
#     Can save time by keeping the voa.retail.PT and osm.retail.PT files from the previous step
#     The OSM files however have to be combined - it's really only the VOA that takes time.
#     Cleaned csv look up files are saved externally to speed up the process. 
#------------------------------------------------------------------------------------
# voa.retail.PT <- st_transform(st_read(paste0(wd, "/Data/VOA/Retail_Points.gpkg"))[,c("IDval_ID")], crs=4326)
# names(voa.retail.PT)[names(voa.retail.PT)=="geom"] <- c("geometry")
# st_geometry(voa.retail.PT) <- "geometry"
# voa.retail.h3 <- h3::h3_to_geo_boundary_sf(h3::geo_to_h3(st_coordinates(voa.retail.PT)[,c(2,1)], res = 11))
# voa.retail.h3$h3_resolution <- 11
# voa.retail.h3$h3_address <- h3::geo_to_h3(st_coordinates(voa.retail.PT)[,c(2,1)], res = 11)
# voa.retail.PT <- st_join(voa.retail.PT, voa.retail.h3, join = st_within)
# voa.retail.PT <- voa.retail.PT[!duplicated(voa.retail.PT$IDval_ID),]
# voa.retail.PT$h3_resolution <- NULL
# voa.retail.PT$latitude <- st_coordinates(voa.retail.PT)[,2]
# voa.retail.PT$longitude <- st_coordinates(voa.retail.PT)[,1]
# fwrite(data.table(voa.retail.PT)[,-c("geometry")], file=paste0(wd, "/Data/VOA/VOA_merged.csv"))
# rm(voa.retail.h3)

voa.retail.PT <- fread(paste0(wd, "/Data/VOA/VOA_merged.csv"))
voa.retail.PT <- st_as_sf(x = voa.retail.PT, coords = c("longitude", "latitude"), crs = 4326)

# osm.shopPT <- st_transform(st_read(dsn = paste0(wd, "/Data/OSM/EW_OSM_shop_301020.gpkg"))[,c("osm_id")], crs=4326)
# osm.retail.1 <- rbind(osm.amenityPT, osm.shopPT); rm(osm.amenityPT, osm.shopPT)
# osm.shopPT <- st_transform(st_read(dsn = paste0(wd, "/Data/OSM/SC_OSM_shop_291020.gpkg"))[,c("osm_id")], crs=4326)
# osm.amenityPT <- st_transform(st_read(dsn = paste0(wd, "/Data/OSM/SC_OSM_amenity_291020.gpkg"))[,c("osm_id")], crs=4326)
# osm.retail.2 <- rbind(osm.amenityPT, osm.shopPT); rm(osm.amenityPT, osm.shopPT)
# osm.shopPT <- st_transform(st_read(dsn = paste0(wd, "/Data/OSM/NI_OSM_shop_291020.gpkg"))[,c("osm_id")], crs=4326)
# osm.amenityPT <- st_transform(st_read(dsn = paste0(wd, "/Data/OSM/NI_OSM_amenity_291020.gpkg"))[,c("osm_id")], crs=4326)
# osm.retail.3 <- rbind(osm.amenityPT, osm.shopPT); rm(osm.amenityPT, osm.shopPT)
# osm.retail.PT <- rbind(osm.retail.1, osm.retail.2)
# osm.retail.PT <- rbind(osm.retail.PT, osm.retail.3)
# rm(osm.retail.1, osm.retail.2, osm.retail.3)
# names(osm.retail.PT)[names(osm.retail.PT)=="geom"] <- c("geometry")
# st_geometry(osm.retail.PT) <- "geometry"
# osm.retail.h3 <- h3::h3_to_geo_boundary_sf(h3::geo_to_h3(st_coordinates(osm.retail.PT)[,c(2,1)], res = 11))
# osm.retail.h3$h3_resolution <- 11
# osm.retail.h3$h3_address <- h3::geo_to_h3(st_coordinates(osm.retail.PT)[,c(2,1)], res = 11)
# osm.retail.PT <- st_join(osm.retail.PT, osm.retail.h3, join = st_within)
# osm.retail.PT <- osm.retail.PT[!duplicated(osm.retail.PT$osm_id),]
# osm.retail.PT$h3_resolution <- NULL
# osm.retail.PT$latitude <- st_coordinates(osm.retail.PT)[,2]
# osm.retail.PT$longitude <- st_coordinates(osm.retail.PT)[,1]
# fwrite(data.table(osm.retail.PT)[,-c("geometry")], file=paste0(wd, "/Data/OSM/OSM_merged.csv"))
# rm(osm.retail.h3)

osm.retail.PT <- fread(paste0(wd, "/Data/OSM/OSM_merged.csv"))
osm.retail.PT <- st_as_sf(x = osm.retail.PT, coords = c("longitude", "latitude"), crs = 4326)
#------------------------------------------------------------------------------------


# READ IN AND CLEAN DISSOLVED COMPLETE SET OF RETAIL CENTRES
#    We have two forms of the retail agglomerations. The dissolved boundaries which 
#     are read in here, and the individual H3s which are loaded in next.
#------------------------------------------------------------------------------------
RC_EW <- st_transform(st_read(paste0(wd, "/Exports/Interim/EW_RC_Complete.gpkg")), crs=27700)
names(RC_EW)[names(RC_EW)=="tractID"] <- c("RC_ID")
names(RC_EW)[names(RC_EW)=="geom"] <- c("geometry")
st_geometry(RC_EW) <- "geometry"
RC_EW$RC_ID <- as.character(RC_EW$RC_ID)
RC_EW <- RC_EW[,names(RC_EW) %in% c("RC_ID", "tr_VOA_retailN", "tr_OSM_retailN", "H3_count")]
RC_EW <- RC_EW[!is.na(RC_EW$RC_ID),]

RC_SC <- st_transform(st_read(paste0(wd, "/Exports/Interim/SC_RC_Complete.gpkg")), crs=27700)
names(RC_SC)[names(RC_SC)=="geom"] <- c("geometry")
names(RC_SC)[names(RC_SC)=="tr_retailN"] <- "tr_OSM_retailN"
names(RC_SC)[names(RC_SC)=="tractN"] <- "H3_count"
names(RC_SC)[names(RC_SC)=="tractID"] <- "RC_ID"
st_geometry(RC_SC) <- "geometry"
RC_SC$RC_ID <- as.character(RC_SC$RC_ID)
RC_SC$tr_VOA_retailN <- NA
RC_SC <- RC_SC[,names(RC_SC) %in% c("RC_ID", "tr_VOA_retailN", "tr_OSM_retailN", "H3_count")]
RC_SC <- RC_SC[!is.na(RC_SC$RC_ID),]

RC_NI <- st_transform(st_read(paste0(wd, "/Exports/Interim/NI_RC_Complete.gpkg")), crs=27700)
names(RC_NI)[names(RC_NI)=="geom"] <- c("geometry")
names(RC_NI)[names(RC_NI)=="tr_retailN"] <- "tr_OSM_retailN"
names(RC_NI)[names(RC_NI)=="tractN"] <- "H3_count"
names(RC_NI)[names(RC_NI)=="tractID"] <- c("RC_ID")
RC_NI$RC_ID <- as.character(RC_NI$RC_ID)
st_geometry(RC_NI) <- "geometry"
RC_NI$tr_VOA_retailN <- NA
RC_NI <- RC_NI[,names(RC_NI) %in% c("RC_ID", "tr_VOA_retailN", "tr_OSM_retailN", "H3_count")]
RC_NI <- RC_NI[!is.na(RC_NI$RC_ID),]

RC_UK <- rbind(RC_EW, RC_SC)
RC_UK <- rbind(RC_UK, RC_NI)
rm(RC_EW, RC_SC, RC_NI); gc()
RC_UK$area_km2 <- as.numeric(st_area(RC_UK))*0.000001

# We replace the old interim ID with an updated new one (from a replicable seed)
set.seed(2021)
ID.lookup <- data.table(cbind(unique(RC_UK$RC_ID), 
  paste0("RC_Z", sample(c(9991000:9999999), size=length(unique(RC_UK$RC_ID)), replace =F))))
names(ID.lookup) <- c("RC_ID", "ID_New")
RC_UK <- merge(RC_UK, ID.lookup, all.x=T, sort=F, by="RC_ID")
rm(ID.lookup)
#------------------------------------------------------------------------------------


# READ IN AND CLEAN GRANULAR H3 COMPONENTS OF RETAIL CENTRES
#------------------------------------------------------------------------------------
RC_EW <- st_transform(st_read(paste0(wd, "/Exports/Interim/EW_H3_Complete.gpkg")), crs=27700)
names(RC_EW)[names(RC_EW)=="tractID"] <- "RC_ID"
names(RC_EW)[names(RC_EW)=="geom"] <- "geometry"
st_geometry(RC_EW) <- "geometry"
RC_EW <- RC_EW[,names(RC_EW) %in% c("h3_address", "RC_ID", "h3_VOA_retailN", "h3_OSM_retailN", "tr_VOA_retailN", "tr_OSM_retailN", "H3_count")]
RC_EW$RC_ID <- as.character(RC_EW$RC_ID)

RC_SC <- st_transform(st_read(paste0(wd, "/Exports/Interim/SC_H3_Complete.gpkg")), crs=27700)
names(RC_SC)[names(RC_SC)=="geom"] <- "geometry"
names(RC_SC)[names(RC_SC)=="tr_retailN"] <- "tr_OSM_retailN"
names(RC_SC)[names(RC_SC)=="tractN"] <- "H3_count"
names(RC_SC)[names(RC_SC)=="tractID"] <- "RC_ID"
names(RC_SC)[names(RC_SC)=="retailN"] <- "h3_OSM_retailN"
st_geometry(RC_SC) <- "geometry"
RC_SC$RC_ID <- as.character(RC_SC$RC_ID)
RC_SC$tr_VOA_retailN <- NA
RC_SC$h3_VOA_retailN <- NA
RC_SC <- RC_SC[,names(RC_SC) %in% c("h3_address", "RC_ID", "h3_VOA_retailN", "h3_OSM_retailN", "tr_VOA_retailN", "tr_OSM_retailN", "H3_count")]
RC_SC <- RC_SC[!is.na(RC_SC$RC_ID),]

RC_NI <- st_transform(st_read(paste0(wd, "/Exports/Interim/NI_H3_Complete.gpkg")), crs=27700)
names(RC_NI)[names(RC_NI)=="geom"] <- "geometry"
names(RC_NI)[names(RC_NI)=="tr_retailN"] <- "tr_OSM_retailN"
names(RC_NI)[names(RC_NI)=="tractN"] <- "H3_count"
names(RC_NI)[names(RC_NI)=="tractID"] <- "RC_ID"
names(RC_NI)[names(RC_NI)=="retailN"] <- "h3_OSM_retailN"
RC_NI$RC_ID <- as.character(RC_NI$RC_ID)
st_geometry(RC_NI) <- "geometry"
RC_NI$tr_VOA_retailN <- NA
RC_NI$h3_VOA_retailN <- NA
RC_NI <- RC_NI[,names(RC_NI) %in% c("h3_address", "RC_ID", "h3_VOA_retailN", "h3_OSM_retailN", "tr_VOA_retailN", "tr_OSM_retailN", "H3_count")]
RC_NI <- RC_NI[!is.na(RC_NI$RC_ID),]

H3_UK <- rbind(RC_EW, RC_SC)
H3_UK <- rbind(H3_UK, RC_NI)
rm(RC_EW, RC_SC, RC_NI); gc()
H3_UK$area_km2 <- as.numeric(st_area(H3_UK))*0.000001

H3_UK <- merge(H3_UK, data.table(RC_UK)[,c("RC_ID", "ID_New")], all.x=T, sort=F, by="RC_ID")

H3_UK$RC_ID <- NULL
RC_UK$RC_ID <- NULL
names(H3_UK)[names(H3_UK)=="ID_New"] <- "RC_ID"
names(RC_UK)[names(RC_UK)=="ID_New"] <- "RC_ID"
#------------------------------------------------------------------------------------


# MANUAL CLEANING - DELETIONS
#------------------------------------------------------------------------------------
# DELETIONS
# - RC_Z9992725   - Single hex; Claughton Rd., Birkenhead
# - RC_Z9992224   - Single hex; Camborne
# - RC_Z9992336   - Car dealership area; Dudley
# - RC_Z9991051   - Car dealership area; Norwich
# - RC_Z9998842   - Car showroom/industrial
H3_UK <- H3_UK[!(H3_UK$RC_ID %in% 
  c("RC_Z9992725", "RC_Z9992224", "RC_Z9992336", "RC_Z9991051", "RC_Z9991477", "RC_Z9998842")),] 
#------------------------------------------------------------------------------------


# MANUAL CLEANING - SPLITS
#------------------------------------------------------------------------------------
# SPLIT: Dereham Rd, Norwich (RC_Z9997531)
H3_UK <- H3_UK[!(H3_UK$h3_address %in% c("8b194105a56bfff", "8b194105a540fff", 
  "8b194105a192fff", "8b194105a56cfff", "8b194105a568fff", "8b194105a56efff", "8b194105a560fff", 
  "8b194105a56afff", "8b194105a563fff", "8b194105a54cfff", "8b194105a545fff", "8b194105a562fff", 
  "8b194105a541fff", "8b194105a54efff", "8b194105a543fff", "8b194105a542fff", "8b194105a56dfff")),]

# SPLIT: London Rd, Brighton (RC_Z9995169)
H3_UK <- H3_UK[!(H3_UK$h3_address %in% c("8b194a7252ecfff", "8b194a7252eefff")),]

# SPLIT: Islington and Highbury in London (RC_Z9995256)
H3_UK <- H3_UK[!(H3_UK$h3_address %in% c("8b195da6bb53fff", "8b195da6bb51fff", "8b195da6bb42fff")),]
#------------------------------------------------------------------------------------


# MANUAL CLEANING - MERGERS
#------------------------------------------------------------------------------------
# t1 <- H3_UK[H3_UK$RC_ID %in% c("RC_EW_3443", "RC_EW_3444"),]
# t2 <- h3::h3_to_geo_boundary_sf(h3::polyfill(st_as_sfc(st_bbox(t1)), res = 11))
# t2$h3_address <- h3::polyfill(st_as_sfc(st_bbox(t1)), res = 11)
# st_write(t2, dsn = paste0(wd, "/Exports/England_Wales/RC_Merge.gpkg"), delete_layer = TRUE, driver = "gpkg")

mergers <- list()
# MERGE: Liverpool Region
#    "RC_Z9996999", "RC_Z9999437", "RC_Z9995948", "RC_Z9991131" (Crosby)
mergers[[1]] <- c("8b1951028516fff", "8b1951028510fff", "8b1951028511fff", "8b195102851cfff", "8b19510285a5fff", 
  "8b1951028512fff", "8b1951028513fff")

#    "RC_EW_5554", "RC_EW_5555", "RC_EW_5634" (Orrell Park)
mergers[[2]] <- c("8b19510749a3fff", "8b195115a292fff", "8b195115a293fff")

#    "RC_EW_5532", "RC_EW_5533" (County Road)
mergers[[3]] <- c("8b1951070913fff", "8b1951070911fff")

#    "RC_EW_5404", "RC_EW_5402" (Smithdown Road)
mergers[[4]] <- c("8b19510442e1fff", "8b19510442e5fff")

#    "RC_EW_5415", "RC_EW_5412" (Allerton Road)
mergers[[5]] <- c("8b1951045c18fff", "8b1951045c1dfff", "8b1951045c0afff", "8b1951045c0bfff", "8b1951045c09fff")

#    "RC_EW_5467", "RC_EW_11868" (Church Road)
mergers[[6]] <- c("8b1951058d0cfff", "8b1951058d2bfff")

#    "RC_EW_5604", "RC_EW_5596" (Station Road)
mergers[[7]] <- c("8b195111d909fff", "8b195111d956fff", "8b195111d952fff", "8b195111d950fff", "8b195111d953fff")

#    "RC_EW_12148", "RC_EW_5461" (Albert Dock)
mergers[[8]] <- c("8b19510558e0fff", "8b19510558e1fff", "8b1951055852fff", "8b1951055850fff")

#    "RC_EW_5427", "RC_EW_5426", "RC_EW_5392" (Lodge Land)
mergers[[9]] <- c("8b1951046b5dfff", "8b1951046b4efff", "8b1951046a61fff", "8b1951046a6efff", "8b1951046a6cfff", 
  "8b1951046a68fff")

#    "RC_EW_5589", "RC_EW_5595" ( )
mergers[[10]] <- c("8b1951101526fff", "8b1951101c99fff", "8b1951101c8afff", "8b1951101c9dfff")

#    "RC_EW_5611", "RC_EW_5610" (Southport)
mergers[[11]] <- c("8b195112335cfff", "8b1951123351fff")


# MERGE: City Centres and High Streets
#    "RC_EW_5668", "RC_EW_5662" (Chester)
mergers[[12]] <- c("8b195123429bfff", "8b1951234298fff", "8b195123429efff", "8b1951234291fff", "8b1951234290fff", 
  "8b1951234296fff")

#    "RC_EW_1299", "RC_EW_1298", "RC_EW_1301", "RC_EW_1303"  (Bury)
mergers[[13]] <- c("8b194248b608fff", "8b194248b60efff", "8b194248b6e2fff", "8b194248b6f5fff", "8b194248b6d3fff", 
  "8b194248b6d0fff", "8b194248b6d4fff")

#    "RC_EW_1983", "RC_EW_1996", "RC_EW_2018"  (Leeds)
mergers[[14]] <- c("8b1942c59d2efff", "8b1942c59d23fff", "8b1942c5d780fff", "8b1942c5d781fff", "8b1942c5d78cfff", 
  "8b1942c5d78dfff", "8b1942c5d7abfff", "8b1942c5d7a9fff", "8b1942c5d7a8fff", "8b1942c5d7acfff", "8b1942c5d713fff")

#    "RC_EW_7027", "RC_EW_7029"  (Bristol)
mergers[[15]] <- c("8b19583922a1fff", "8b19583922acfff", "8b1958392213fff", "8b1958392211fff", "8b195839221efff", 
  "8b195839221cfff", "8b1958392203fff", "8b1958392201fff", "8b19583922a9fff", "8b195839221afff", "8b1958392218fff", 
  "8b19583922f4fff", "8b1958392219fff")


# MERGE: Others based on feedback: 
#    "RC_EW_3289", "RC_EW_3277" 
mergers[[16]] <- c("8b194a722b4bfff", "8b194a722b48fff")

#    "RC_EW_7543", "RC_EW_7542" 
mergers[[17]] <- c("8b1959024113fff", "8b19590241adfff", "8b1959024021fff", "8b195902402cfff", "8b1959024156fff", 
  "8b1959024152fff", "8b1959024025fff")

#    "RC_EW_10076", "RC_EW_10075" 
mergers[[18]] <- c("8b195d09ad83fff", "8b195d09ad8dfff", "8b195d09ac36fff", "8b195d09ac33fff", "8b195d09ac28fff")

#    "RC_EW_10477", "RC_EW_10452" 
mergers[[19]] <- c("8b195d5592d3fff", "8b195d5592d0fff", "8b195d5592d6fff", "8b195d55928bfff")

#    "RC_EW_7037", "RC_EW_7036" 
mergers[[20]] <- c("8b19583926e1fff", "8b1958392652fff", "8b19583926eefff")

#    "RC_EW_88", "RC_EW_90" 
mergers[[21]] <- c("8b18746ea369fff", "8b18746ebcb4fff", "8b18746ea36bfff", "8b18746ebcb6fff")

#    "RC_EW_9079", "RC_EW_9073" 
mergers[[22]] <- c("8b195c341c9cfff", "8b195c341c83fff")

#    "RC_EW_8006", "RC_EW_8005" 
mergers[[23]] <- c("8b195a118658fff", "8b195a11874efff", "8b195a118748fff")

#    "RC_EW_6700", "RC_EW_6699" 
mergers[[24]] <- c("8b19550d6014fff", "8b19550d6016fff")

#    "RC_EW_6447", "RC_EW_12003" (Levenshulme retail centre in Manchester)
mergers[[25]] <- c("8b1951b62a8bfff", "8b1951b62a89fff")

#    "RC_EW_6180", "RC_EW_6186", "RC_EW_6185" (St. Helen's padding)
mergers[[26]] <- c("8b1951ab0899fff", "8b1951ab088afff", "8b1951ab089dfff", "8b1951ab088efff", "8b1951ab08d4fff", 
  "8b1951ab08f2fff")

#    "RC_EW_6125", "RC_EW_6126" (St. Helen's padding)
mergers[[27]] <- c("8b1951a8a242fff", "8b1951a8a25cfff", "8b1951a8a258fff", "8b1951a8a25bfff", "8b1951a8b5b5fff", 
  "8b1951a8b5b4fff")

#    "RC_Z9999451", "RC_Z9998476" (Yates)
mergers[[28]] <- c("8b195805d251fff")

#    "RC_Z9996641", "RC_Z9999428" (Knightsbridge, London; major town centre - include Harrods)
mergers[[29]] <- c("8b194ad16d9afff", "8b194ad16cb4fff", "8b194ad16d9bfff", "8b194ad16cb0fff", 
  "8b194ad16cb5fff", "8b194ad16ca6fff", "8b194ad16cb1fff", "8b194ad16ca2fff", "8b194ad16ca0fff")

#    "RC_Z9995678", "RC_Z9994514" (Fratton Road; Fratton; Portsmouth (South East; England) 
mergers[[30]] <- c("8b1959aa0d94fff", "8b1959aa0db3fff", "8b1959aa0db1fff")

#    "RC_Z9992627", "RC_Z9992795", "RC_Z9992306", "RC_Z9995119", "RC_Z9995616" (North Wales)
mergers[[31]] <- c("8b19512b00b3fff", "8b19512b00b0fff", "8b19512b00b6fff", "8b19512b0569fff", "8b19512b00b4fff", 
  "8b19512b019bfff", "8b19512b0198fff", "8b19512b019dfff")

#    "RC_Z9991559", "RC_Z9992564" (South East - Palmerston Road)
mergers[[32]] <- c("8b1959aa2a01fff", "8b1959aa2a2afff", "8b1959aa2a0cfff")


# MERGE: Scotland: 
#    "RC_SC_1642", "RC_SC_1643" (Scotland)
mergers[[33]] <- c("8b1972761089fff", "8b19727610d4fff")

#    "RC_SC_1656", "RC_SC_1647" (Scotland)
mergers[[34]] <- c("8b197276174afff", "8b1972761748fff")

#    "RC_SC_1712", "RC_SC_1320" (Old Town Retail Centres in Edinburgh)
mergers[[35]] <- c("8b1972765384fff", "8b1972765385fff", "8b197276538cfff", "8b19727653aafff", "8b19727653a8fff", 
  "8b19727653a9fff")


# MERGE: Add-in: 
mergers[[36]] <- c("8b1951a8a2c3fff", "8b1951a8a2c0fff", "8b1951a8a2c4fff", "8b1951a8a2e2fff", "8b1951a8a2e4fff", 
  "8b1951a8a2e0fff", "8b1951a8a2e3fff", "8b1951a8a2c5fff", "8b1951a8a2c1fff", "8b1951a8a2ccfff", "8b1951a8a2eafff", 
  "8b1951a8a2eefff", "8b1951a8a2e1fff", "8b1951a8a2e5fff", "8b1951a8a256fff", "8b1951a8a252fff", "8b1951a8a2ecfff", 
  "8b1951a8a2e8fff", "8b1951a8a2ebfff", "8b1951a8a2cdfff", "8b1951a8b5b6fff", "8b1951a8a2e9fff", "8b1951a8a2edfff", 
  "8b1951a8a253fff", "8b1951a8a250fff", "8b1951a8a251fff", "8b1951a8a25efff", "8b1951a8a25afff", "8b1951a8b5b4fff", 
  "8b1951a8b5b5fff", "8b1951a8a25bfff", "8b1951a8a258fff", "8b1951a8a25cfff", "8b1951a8a242fff")


mergers1 <- unlist(mergers, recursive = FALSE)
mergers <- h3::h3_to_geo_boundary_sf(mergers1)
mergers$h3_address <- mergers1
rm(mergers1)

H3_counts <- as_tibble(voa.retail.PT) %>%
  select(h3_address) %>%
  group_by(h3_address) %>%
  add_count(name = "h3_VOA_retailN") %>%
  filter(!is.na(h3_address) & h3_address!="") %>% 
  unique()
mergers <- merge(mergers, H3_counts, all.x=T, sort=F, by="h3_address")

H3_counts <- as_tibble(osm.retail.PT) %>%
  select(h3_address) %>%
  group_by(h3_address) %>%
  add_count(name = "h3_OSM_retailN") %>%
  filter(!is.na(h3_address) & h3_address!="") %>% 
  unique()
mergers <- merge(mergers, H3_counts, all.x=T, sort=F, by="h3_address")
mergers <- unique(mergers)
mergers$h3_OSM_retailN[is.na(mergers$h3_OSM_retailN)] <- 0
mergers$h3_VOA_retailN[is.na(mergers$h3_VOA_retailN)] <- 0
mergers <- st_transform(mergers, crs=27700)
mergers$area_km2 <- as.numeric(st_area(mergers))*0.000001
rm(H3_counts)
#------------------------------------------------------------------------------------


# SECOND ITERATION OF GENERATING/ DISSOLVING RETAIL CENTRES
# 
# We run a second iteration of the new set of h3 geometries - with the splits, mergers, 
#   and deletions having taken place. The new centres and then given an updated retail
#   agglomeration ID.
#------------------------------------------------------------------------------------
H3_UK$RC_ID <- NULL
H3_UK$tr_VOA_retailN <- NULL
H3_UK$tr_OSM_retailN <- NULL
H3_UK$H3_count <- NULL

H3_UK <- rbind(H3_UK, mergers)
H3_UK <- H3_UK[!is.na(H3_UK$h3_address) | H3_UK$h3_address!="",]
H3_UK$h3_address <- as.character(H3_UK$h3_address)

hex_Graph <- get.tractID(H3_UK$h3_address, H3_UK$h3_VOA_retailN, retail.count=TRUE, NAME="RC_UK")
H3_UK <- H3_UK %>%
  left_join(as_tibble(hex_Graph)) %>%
  unique()
colnames(H3_UK)[colnames(H3_UK)=="tractN"] <- "H3_count"
colnames(H3_UK)[colnames(H3_UK)=="tr_retailN"] <- "tr_VOA_retailN"

hex_Graph2 <- get.tractID(H3_UK$h3_address, H3_UK$h3_OSM_retailN, retail.count=TRUE, NAME="RC_UK")
hex_Graph2 <- hex_Graph2[,c("h3_address", "tr_retailN")]
names(hex_Graph2) <- c("h3_address", "tr_OSM_retailN")
H3_UK <- H3_UK %>%
  left_join(as_tibble(hex_Graph2)) %>%
  unique()
rm(hex_Graph, hex_Graph2, mergers); gc(verbose=FALSE)
names(H3_UK)[names(H3_UK)=="tractID"] <-"RC_ID"

RC.dissolved <- H3_UK %>% 
  st_transform(crs=27700) %>%
  st_make_valid() %>%
  st_buffer(dist = 0.1) %>%
  dplyr::filter(!is.na(h3_address) & h3_address!="") %>% 
  dplyr::group_by(RC_ID) %>% 
  dplyr::summarize() %>%
  smoothr::fill_holes(threshold = units::set_units(2000, km^2)) %>%
  st_sf() %>%
  unique()

RC.x <- as_tibble(H3_UK) %>%
  select(RC_ID, tr_VOA_retailN, tr_OSM_retailN, H3_count) %>%
  unique()
RC.dissolved <- left_join(RC.dissolved, RC.x)

# We again replace the old ID with an updated new one (from a replicable seed)
set.seed(2021)
ID.lookup <- data.table(cbind(unique(RC.dissolved$RC_ID),
  paste0("RC_Q", sample(c(91000:99999), size=length(unique(RC.dissolved$RC_ID)), replace =F))))
names(ID.lookup) <- c("RC_ID", "ID_New")

RC_UK <- merge(RC.dissolved, ID.lookup, all.x=T, sort=F, by="RC_ID")
RC_UK$RC_ID <- NULL
names(RC_UK)[names(RC_UK)=="ID_New"] <- "RC_ID"
rm(ID.lookup, RC.x, RC.dissolved)

RC_UK$area_km2 <- as.numeric(st_area(RC_UK))*0.000001
#------------------------------------------------------------------------------------



# GENERATED CORRECT IDs AND EXPORT
#
# The versions of the retail agglomeration IDs published differ from the cleaned/ replicable
#   versions used and generated within these codes. We use a generated lookup to swap
#   in the conventionally published codes (making sure the boundaries are identical).
#------------------------------------------------------------------------------------
id.lookup <- fread(paste0(wd, "/Exports/Interim/ID_Lookup.csv"), na.strings = c("", " "))
id.lookup <- id.lookup[!(is.na(id.lookup$ID_Final)),]

RC_UK <- merge(RC_UK, id.lookup, all.x=T, sort=F, by.x="RC_ID", by.y="ID_Interim")
RC_UK$RC_ID <- NULL
names(RC_UK)[names(RC_UK)=="ID_Final"] <- "RC_ID"

H3_UK <- merge(H3_UK, id.lookup, all.x=T, sort=F, by.x="RC_ID", by.y="ID_Interim")
H3_UK$RC_ID <- NULL
names(H3_UK)[names(H3_UK)=="ID_Final"] <- "RC_ID"

st_write(RC_UK, dsn = paste0(wd, "/Exports/Interim/UK_RC_Pruned.gpkg"), delete_layer = TRUE, driver = "gpkg")
st_write(H3_UK, dsn = paste0(wd, "/Exports/Interim/UK_H3_Pruned.gpkg"), delete_layer = TRUE, driver = "gpkg")
#------------------------------------------------------------------------------------