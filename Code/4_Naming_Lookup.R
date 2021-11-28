########################################################################################
########  An open source delineation and hierarchical classification 
#######     of UK retail agglomeration
######
#####     Jacob L. Macdonald, Les Dolega, Alex Singleton
####      Last Updated: October, 2021
###
##  CONVENTIONAL NAMING LOOKUP FILE FOR EACH RETAIL CENTRE
##   
##  - Collecting and cleaning a series of conventional place names and administrative
##    lookups for each retail agglomeration.
##   
##  - Requires: 
##        ONS, OS and Street level naming files/ administrative boundaries.
##    
##  Notes: 1) Generates a lookup of all conventional names associated to each retail
##            agglomeration as sourced from OSM, OS and VOA conventional location names.
##         2) Files exported to the /Exports/Interim/ subfolder
##


# LOAD PACKAGES; SET WD; READ IN FUNCTIONS
#------------------------------------------------------------------------------------
Packages <- c("sf", "data.table", "stringr", "tidyverse", "parallel", "doSNOW")
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


# READ IN RETAIL CENTRES
#------------------------------------------------------------------------------------
RC_UK <- st_transform(st_read(paste0(wd, "/Exports/Interim/UK_RC_Pruned.gpkg")), crs=27700)
names(RC_UK)[names(RC_UK)=="geom"] <- c("geometry")
st_geometry(RC_UK) <- "geometry"
RC_UK <- RC_UK[,names(RC_UK) %in% c("RC_ID", "tr_VOA_retailN", "tr_OSM_retailN", "H3_count")]
RC_UK$RC_ID <- as.character(RC_UK$RC_ID)
#------------------------------------------------------------------------------------


# ONS NAMING - LOCAL AUTHORITY LEVEL BREAKDOWN
#------------------------------------------------------------------------------------
UK.LAD <- st_read(paste0(wd, "/Data/Boundaries and Lookups/Local_Authority_Districts_(December_2020)_UK_BUC.shp"))[,c("LAD20CD", "LAD20NM")]
UK.LAD$LocalAuthorityTY <- NA
UK.LAD$LocalAuthorityTY[grepl("E0", UK.LAD$LAD20CD)] <- "LAD20CD"
UK.LAD$LocalAuthorityTY[grepl("W06", UK.LAD$LAD20CD)] <- "LAD20CD"
UK.LAD$LocalAuthorityTY[grepl("S12", UK.LAD$LAD20CD)] <- "CouncilArea20CD"
UK.LAD$LocalAuthorityTY[grepl("N09", UK.LAD$LAD20CD)] <- "LGD14CD"
UK.LAD <- st_transform(UK.LAD, crs=st_crs(RC_UK))

RC.int <- st_intersection(RC_UK, UK.LAD)[,c("RC_ID", "LAD20CD", "LAD20NM")]
RC.int$area_km2 <- as.numeric(st_area(RC.int))*0.000001

t <- data.table(RC.int)[,.(N=.N, Size=sum(area_km2)), by=.(RC_ID)]

RC.int1 <- RC.int[RC.int$RC_ID %in% t[t$N==1,]$RC_ID,]
RC.int1 <- data.table(RC.int1)[,-c("geometry", "area_km2")]
names(RC.int1) <- c("RC_ID", "LocalAuthorityCD", "LocalAuthorityNM")
RC.int1$Prop <- 1

RC.int2 <- RC.int[RC.int$RC_ID %in% t[t$N>1,]$RC_ID,]; rm(t)
RC.int2 <- data.table(RC.int2)[,-c("geometry")]
RC.int2[,Tarea:=sum(area_km2), by=RC_ID][,Prop:=area_km2/Tarea, by=RC_ID]
RC.int2 <- RC.int2[as.numeric(RC.int2$Prop)>0.1,]

t <- RC.int2[!(duplicated(RC.int2$RC_ID) | duplicated(RC.int2$RC_ID, fromLast=T)), c("RC_ID", "LAD20CD", "LAD20NM", "Prop")]
names(t) <- c("RC_ID", "LocalAuthorityCD", "LocalAuthorityNM", "Prop")
RC.LADs <- rbind(RC.int1, t); rm(RC.int, RC.int1, t)

t <- RC.int2[(duplicated(RC.int2$RC_ID) | duplicated(RC.int2$RC_ID, fromLast=T)), c("RC_ID", "LAD20CD", "LAD20NM", "Prop")]
names(t) <- c("RC_ID", "LocalAuthorityCD", "LocalAuthorityNM", "Prop")
t <- t[order(t$RC_ID, -t$Prop),]
t$RC_ID <- as.character(t$RC_ID)
t$LocalAuthorityCD <- as.character(t$LocalAuthorityCD)
t$LocalAuthorityNM <- as.character(t$LocalAuthorityNM)
t$Number <- as.numeric(do.call(rbind, lapply(str_split(make.unique(as.character(t$RC_ID), sep = "-"), "-"), function(x) x[2])))
t$Number[is.na(t$Number)] <- 0
t$Number <- t$Number + 1

t1 <- t[t$Number==1, c("RC_ID", "LocalAuthorityCD", "LocalAuthorityNM", "Prop")]
names(t1) <- c("RC_ID", "LocalAuthorityCD_1", "LocalAuthorityNM_1", "LocalAuthority_1_Prop")

t2 <- t[t$Number==2, c("RC_ID", "LocalAuthorityCD", "LocalAuthorityNM", "Prop")]
names(t2) <- c("RC_ID", "LocalAuthorityCD_2", "LocalAuthorityNM_2", "LocalAuthority_2_Prop")

t3 <- t[t$Number==3, c("RC_ID", "LocalAuthorityCD", "LocalAuthorityNM", "Prop")]
names(t3) <- c("RC_ID", "LocalAuthorityCD_3", "LocalAuthorityNM_3", "LocalAuthority_3_Prop")

t <- merge(t1, t2, all.x=T, sort=F, by="RC_ID")
t <- merge(t, t3, all.x=T, sort=F, by="RC_ID")

names(RC.LADs) <- c("RC_ID", "LocalAuthorityCD_1", "LocalAuthorityNM_1", "LocalAuthority_1_Prop")
RC.LADs <- rbind(RC.LADs, t, fill=T); rm(t1, t2, t3, t)

RC.LADs <- merge(RC.LADs, unique(data.table(UK.LAD)[,c("LAD20CD", "LocalAuthorityTY")]), all.x=T, sort=F, by.x="LocalAuthorityCD_1", by.y="LAD20CD")
RC.LADs <- RC.LADs[,c("RC_ID", "LocalAuthorityTY", "LocalAuthorityCD_1", "LocalAuthorityNM_1", "LocalAuthority_1_Prop", "LocalAuthorityCD_2", "LocalAuthorityNM_2", "LocalAuthority_2_Prop", "LocalAuthorityCD_3", "LocalAuthorityNM_3", "LocalAuthority_3_Prop")]
#------------------------------------------------------------------------------------


# ONS NAMING - {CEREMONIAL} COUNTY BREAKDOWN
#------------------------------------------------------------------------------------
GB.CC <- st_read(paste0(wd, "/Data/Boundaries and Lookups/Boundary-line-ceremonial-counties_region.shp"))
t1 <- fread(paste0(wd, "/Data/Boundaries and Lookups/Local_Authority_District_to_County_(April_2020)_Lookup_in_England.csv"))
GB.CC <- merge(GB.CC, unique(t1[,c("CTY20CD", "CTY20NM")]), all.x=T, sort=F, by.x="NAME", by.y="CTY20NM"); rm(t1)
GB.CC$C_CCTY <- paste0("CC_", 1:length(GB.CC$CTY20CD))
GB.CC[is.na(GB.CC$CTY20CD),]$CTY20CD <- GB.CC[is.na(GB.CC$CTY20CD),]$C_CCTY
GB.CC$C_CCTY <- NULL
GB.CC$DESCRIPTIO <- gsub(" ", "_", GB.CC$DESCRIPTIO)
GB.CC <- st_transform(GB.CC, crs=st_crs(RC_UK))
names(GB.CC) <- c("CountyNM", "CountyTY", "CountyCD", "geometry")

RC.int <- st_intersection(RC_UK, GB.CC)[,c("RC_ID", "CountyCD", "CountyNM")]
RC.int$area_km2 <- as.numeric(st_area(RC.int))*0.000001

t <- data.table(RC.int)[,.(N=.N, Size=sum(area_km2)), by=.(RC_ID)]

RC.int1 <- RC.int[RC.int$RC_ID %in% t[t$N==1,]$RC_ID,]
RC.int1 <- data.table(RC.int1)[,-c("geometry", "area_km2")]
names(RC.int1) <- c("RC_ID", "CountyCD", "CountyNM")
RC.int1$Prop <- 1

RC.int2 <- RC.int[RC.int$RC_ID %in% t[t$N>1,]$RC_ID,]; rm(t)
RC.int2 <- data.table(RC.int2)[,-c("geometry")]
RC.int2[,Tarea:=sum(area_km2), by=RC_ID][,Prop:=area_km2/Tarea, by=RC_ID]
RC.int2 <- RC.int2[as.numeric(RC.int2$Prop)>0.1,]

t <- RC.int2[!(duplicated(RC.int2$RC_ID) | duplicated(RC.int2$RC_ID, fromLast=T)), c("RC_ID", "CountyCD", "CountyNM", "Prop")]
names(t) <- c("RC_ID", "CountyCD", "CountyNM", "Prop")
RC.County <- rbind(RC.int1, t); rm(RC.int, RC.int1, t)

t <- RC.int2[(duplicated(RC.int2$RC_ID) | duplicated(RC.int2$RC_ID, fromLast=T)), c("RC_ID", "CountyCD", "CountyNM", "Prop")]
names(t) <- c("RC_ID", "CountyCD", "CountyNM", "Prop")
t <- t[order(t$RC_ID, -t$Prop),]
t$RC_ID <- as.character(t$RC_ID)
t$CountyCD <- as.character(t$CountyCD)
t$CountyNM <- as.character(t$CountyNM)
t$Number <- as.numeric(do.call(rbind, lapply(str_split(make.unique(as.character(t$RC_ID), sep = "-"), "-"), function(x) x[2])))
t$Number[is.na(t$Number)] <- 0
t$Number <- t$Number + 1

t1 <- t[t$Number==1, c("RC_ID", "CountyCD", "CountyNM", "Prop")]
names(t1) <- c("RC_ID", "CountyCD_1", "CountyNM_1", "County_1_Prop")

t2 <- t[t$Number==2, c("RC_ID", "CountyCD", "CountyNM", "Prop")]
names(t2) <- c("RC_ID", "CountyCD_2", "CountyNM_2", "County_2_Prop")

t <- merge(t1, t2, all.x=T, sort=F, by="RC_ID")

names(RC.County) <- c("RC_ID", "CountyCD_1", "CountyNM_1", "County_1_Prop")
RC.County <- rbind(RC.County, t, fill=T); rm(t1, t2, t)

RC.County <- merge(RC.County, unique(data.table(GB.CC)[,c("CountyCD", "CountyTY")]), all.x=T, sort=F, by.x="CountyCD_1", by.y="CountyCD")
RC.County <- RC.County[,c("RC_ID", "CountyTY", "CountyCD_1", "CountyNM_1", "County_1_Prop", "CountyCD_2", "CountyNM_2", "County_2_Prop")]
#------------------------------------------------------------------------------------


# ONS NAMING - CITY LEVEL BREAKDOWN
#------------------------------------------------------------------------------------
SCT.CITY <- st_read(paste0(wd, "/Data/Boundaries and Lookups/Scotland/Localities2016_MHW.shp"))[,c("code", "name")]
SCT.CITY$CityTY <- "Localities16CD"
names(SCT.CITY) <- c("CityCD", "CityNM", "geometry", "CityTY")
SCT.CITY <- st_transform(SCT.CITY, crs=st_crs(RC_UK))

ENG.CITY <- st_read(paste0(wd, "/Data/Boundaries and Lookups/England Wales/Built-up_Areas_(December_2011)_Boundaries_V2.shp"))[,c("bua11cd", "bua11nm")]
ENG.CITY$CityTY <- "BUA11CD"
names(ENG.CITY) <- c("CityCD", "CityNM", "geometry", "CityTY")
ENG.CITY <- st_transform(ENG.CITY, crs=st_crs(RC_UK))

NIR.CITY <- st_transform(st_read(paste0(wd, "/Data/Boundaries and Lookups/Northern Ireland/settlements-2015-above-500-threshold.shp"))[,c("Code", "Name")], crs=st_crs(ENG.CITY))
NIR.CITY$CityTY <- "Settlements15CD"
names(NIR.CITY) <- c("CityCD", "CityNM", "geometry", "CityTY")
NIR.CITY <- st_transform(NIR.CITY, crs=st_crs(RC_UK))

UK.CITY <- rbind(ENG.CITY, SCT.CITY)
UK.CITY <- rbind(UK.CITY, NIR.CITY); rm(SCT.CITY, ENG.CITY, NIR.CITY)

RC.int <- st_intersection(RC_UK, UK.CITY)[,c("RC_ID", "CityCD", "CityNM")]
RC.int$area_km2 <- as.numeric(st_area(RC.int))*0.000001

t <- data.table(RC.int)[,.(N=.N, Size=sum(area_km2)), by=.(RC_ID)]

RC.int1 <- RC.int[RC.int$RC_ID %in% t[t$N==1,]$RC_ID,]
RC.int1 <- data.table(RC.int1)[,-c("geometry", "area_km2")]
names(RC.int1) <- c("RC_ID", "CityCD", "CityNM")
RC.int1$Prop <- 1

RC.int2 <- RC.int[RC.int$RC_ID %in% t[t$N>1,]$RC_ID,]; rm(t)
RC.int2 <- data.table(RC.int2)[,-c("geometry")]
RC.int2[,Tarea:=sum(area_km2), by=RC_ID][,Prop:=area_km2/Tarea, by=RC_ID]
RC.int2 <- RC.int2[as.numeric(RC.int2$Prop)>0.1,]

t <- RC.int2[!(duplicated(RC.int2$RC_ID) | duplicated(RC.int2$RC_ID, fromLast=T)), c("RC_ID", "CityCD", "CityNM", "Prop")]
names(t) <- c("RC_ID", "CityCD", "CityNM", "Prop")
RC.City <- rbind(RC.int1, t); rm(RC.int, RC.int1, t)

t <- RC.int2[(duplicated(RC.int2$RC_ID) | duplicated(RC.int2$RC_ID, fromLast=T)), c("RC_ID", "CityCD", "CityNM", "Prop")]
names(t) <- c("RC_ID", "CityCD", "CityNM", "Prop")
t <- t[order(t$RC_ID, -t$Prop),]
t$RC_ID <- as.character(t$RC_ID)
t$CityCD <- as.character(t$CityCD)
t$CityNM <- as.character(t$CityNM)
t$Number <- as.numeric(do.call(rbind, lapply(str_split(make.unique(as.character(t$RC_ID), sep = "-"), "-"), function(x) x[2])))
t$Number[is.na(t$Number)] <- 0
t$Number <- t$Number + 1

t1 <- t[t$Number==1, c("RC_ID", "CityCD", "CityNM", "Prop")]
names(t1) <- c("RC_ID", "CityCD_1", "CityNM_1", "City_1_Prop")

t2 <- t[t$Number==2, c("RC_ID", "CityCD", "CityNM", "Prop")]
names(t2) <- c("RC_ID", "CityCD_2", "CityNM_2", "City_2_Prop")

t <- merge(t1, t2, all.x=T, sort=F, by="RC_ID")

names(RC.City) <- c("RC_ID", "CityCD_1", "CityNM_1", "City_1_Prop")
RC.City <- rbind(RC.City, t, fill=T); rm(t1, t2, t)

RC.City <- merge(RC.City, unique(data.table(UK.CITY)[,c("CityCD", "CityTY")]), all.x=T, sort=F, by.x="CityCD_1", by.y="CityCD")
RC.City <- RC.City[,c("RC_ID", "CityTY", "CityCD_1", "CityNM_1", "City_1_Prop", "CityCD_2", "CityNM_2", "City_2_Prop")]
#------------------------------------------------------------------------------------


# ONS NAMING - NEIGHBOURHOOD LEVEL BREAKDOWN
#------------------------------------------------------------------------------------
SCT.NBHD <- st_read(paste0(wd, "/Data/Boundaries and Lookups/Scotland/SG_IntermediateZone_Bdry_2011.shp"))[,c("InterZone", "Name")]
SCT.NBHD$NbhdTY <- "IZ11CD"
names(SCT.NBHD) <- c("NbhdCD", "NbhdNM", "geometry", "NbhdTY")
SCT.NBHD <- st_transform(SCT.NBHD, crs=st_crs(RC_UK))

ENG.NBHD <- st_read(paste0(wd, "/Data/Boundaries and Lookups/England Wales/Middle_Layer_Super_Output_Areas_(December_2011)_Boundaries_Generalised_Clipped_(BGC)_EW_V3.shp"))[,c("MSOA11CD", "MSOA11NM")]
ENG.NBHD$NbhdTY <- "MSOA11CD"
names(ENG.NBHD) <- c("NbhdCD", "NbhdNM", "geometry", "NbhdTY")
ENG.NBHD <- st_transform(ENG.NBHD, crs=st_crs(RC_UK))

NIR.NBHD <- st_transform(st_read(paste0(wd, "/Data/Boundaries and Lookups/Northern Ireland/SOA2011.shp"))[,c("SOA_CODE", "SOA_LABEL")], crs=st_crs(ENG.NBHD))
NIR.NBHD$NbhdTY <- "SOA11CD"
names(NIR.NBHD) <- c("NbhdCD", "NbhdNM", "geometry", "NbhdTY")
NIR.NBHD <- st_transform(NIR.NBHD, crs=st_crs(RC_UK))

UK.NBHD <- rbind(ENG.NBHD, SCT.NBHD)
UK.NBHD <- rbind(UK.NBHD, NIR.NBHD); rm(SCT.NBHD, ENG.NBHD, NIR.NBHD)

RC.int <- st_intersection(RC_UK, st_buffer(UK.NBHD, 0))[,c("RC_ID", "NbhdCD", "NbhdNM")]
RC.int$area_km2 <- as.numeric(st_area(RC.int))*0.000001

t <- data.table(RC.int)[,.(N=.N, Size=sum(area_km2)), by=.(RC_ID)]

RC.int1 <- RC.int[RC.int$RC_ID %in% t[t$N==1,]$RC_ID,]
RC.int1 <- data.table(RC.int1)[,-c("geometry", "area_km2")]
names(RC.int1) <- c("RC_ID", "NbhdCD", "NbhdNM")
RC.int1$Prop <- 1

RC.int2 <- RC.int[RC.int$RC_ID %in% t[t$N>1,]$RC_ID,]; rm(t)
RC.int2 <- data.table(RC.int2)[,-c("geometry")]
RC.int2[,Tarea:=sum(area_km2), by=RC_ID][,Prop:=area_km2/Tarea, by=RC_ID]
RC.int2 <- RC.int2[as.numeric(RC.int2$Prop)>0.1,]

t <- RC.int2[!(duplicated(RC.int2$RC_ID) | duplicated(RC.int2$RC_ID, fromLast=T)), c("RC_ID", "NbhdCD", "NbhdNM", "Prop")]
names(t) <- c("RC_ID", "NbhdCD", "NbhdNM", "Prop")
RC.Nbhd <- rbind(RC.int1, t); rm(RC.int, RC.int1, t)

t <- RC.int2[(duplicated(RC.int2$RC_ID) | duplicated(RC.int2$RC_ID, fromLast=T)), c("RC_ID", "NbhdCD", "NbhdNM", "Prop")]
names(t) <- c("RC_ID", "NbhdCD", "NbhdNM", "Prop")
t <- t[order(t$RC_ID, -t$Prop),]
t$RC_ID <- as.character(t$RC_ID)
t$NbhdCD <- as.character(t$NbhdCD)
t$NbhdNM <- as.character(t$NbhdNM)
t$Number <- as.numeric(do.call(rbind, lapply(str_split(make.unique(as.character(t$RC_ID), sep = "-"), "-"), function(x) x[2])))
t$Number[is.na(t$Number)] <- 0
t$Number <- t$Number + 1

t1 <- t[t$Number==1, c("RC_ID", "NbhdCD", "NbhdNM", "Prop")]
names(t1) <- c("RC_ID", "NbhdCD_1", "NbhdNM_1", "Nbhd_1_Prop")

t2 <- t[t$Number==2, c("RC_ID", "NbhdCD", "NbhdNM", "Prop")]
names(t2) <- c("RC_ID", "NbhdCD_2", "NbhdNM_2", "Nbhd_2_Prop")

t3 <- t[t$Number==3, c("RC_ID", "NbhdCD", "NbhdNM", "Prop")]
names(t3) <- c("RC_ID", "NbhdCD_3", "NbhdNM_3", "Nbhd_3_Prop")

t <- merge(t1, t2, all.x=T, sort=F, by="RC_ID")
t <- merge(t, t3, all.x=T, sort=F, by="RC_ID")

names(RC.Nbhd) <- c("RC_ID", "NbhdCD_1", "NbhdNM_1", "Nbhd_1_Prop")
RC.Nbhd <- rbind(RC.Nbhd, t, fill=T); rm(t1, t2, t3, t, RC.int2)

RC.Nbhd <- merge(RC.Nbhd, unique(data.table(UK.NBHD)[,c("NbhdCD", "NbhdTY")]), all.x=T, sort=F, by.x="NbhdCD_1", by.y="NbhdCD")
RC.Nbhd <- RC.Nbhd[,c("RC_ID", "NbhdTY", "NbhdCD_1", "NbhdNM_1", "Nbhd_1_Prop")]
#------------------------------------------------------------------------------------


# ONS NAMING - MERGE ON REGIONAL STUFF & COMBINE TO MASTER DATASET
#------------------------------------------------------------------------------------
RC.Naming <- merge(RC.Nbhd, RC.City, all.x=T, sort=F, by="RC_ID")
RC.Naming <- merge(RC.Naming, RC.LADs, all.x=T, sort=F, by="RC_ID")
RC.Naming <- merge(RC.Naming, RC.County, all.x=T, sort=F, by="RC_ID")

t <- fread(paste0(wd, "/Data/Boundaries and Lookups/England Wales/Local_Authority_District_to_Region__December_2020__Lookup_in_England.csv"))
t <- unique(t[,c("LAD20CD", "RGN20CD", "RGN20NM")])
names(t) <- c("LAD20CD", "RegionCD", "RegionNM")
t$Country <- "England"

RC.Naming <- merge(RC.Naming, t, all.x=T, sort=F, by.x="LocalAuthorityCD_1", by.y="LAD20CD"); rm(t)
RC.Naming$Country[grepl("W", RC.Naming$LocalAuthorityCD_1)] <- "Wales"
RC.Naming$Country[grepl("S", RC.Naming$LocalAuthorityCD_1)] <- "Scotland"
RC.Naming$Country[grepl("N", RC.Naming$LocalAuthorityCD_1)] <- "Northern Ireland"
RC.Naming$RegionNM[grepl("W", RC.Naming$LocalAuthorityCD_1)] <- "Wales"
RC.Naming$RegionNM[grepl("S", RC.Naming$LocalAuthorityCD_1)] <- "Scotland"
RC.Naming$RegionNM[grepl("N", RC.Naming$LocalAuthorityCD_1)] <- "Northern Ireland"
RC.Naming$RegionCD[grepl("W", RC.Naming$LocalAuthorityCD_1)] <- "W92000004"
RC.Naming$RegionCD[grepl("S", RC.Naming$LocalAuthorityCD_1)] <- "S92000003"
RC.Naming$RegionCD[grepl("N", RC.Naming$LocalAuthorityCD_1)] <- "N92000002"

RC.Naming$RegionNM[RC.Naming$LocalAuthorityCD_1 %in% c("E07000244", "E07000245", "E07000246")] <- "East of England"
RC.Naming$RegionCD[RC.Naming$LocalAuthorityCD_1 %in% c("E07000244", "E07000245", "E07000246")] <- "E12000006"
RC.Naming$RegionNM[RC.Naming$LocalAuthorityCD_1 %in% c("E06000059", "E06000058")] <- "South West"
RC.Naming$RegionCD[RC.Naming$LocalAuthorityCD_1 %in% c("E06000059", "E06000058")] <- "E12000009"
RC.Naming$RegionNM[RC.Naming$LocalAuthorityCD_1 %in% c("E06000060")] <- "South East"
RC.Naming$RegionCD[RC.Naming$LocalAuthorityCD_1 %in% c("E06000060")] <- "E12000008"
RC.Naming$Country[RC.Naming$LocalAuthorityCD_1 %in% c("E07000244", "E07000245", "E07000246", "E06000059", "E06000058", "E06000060")] <- "England"

RC.Naming <- RC.Naming[,c("RC_ID", "Country", "RegionCD", "RegionNM", "NbhdTY", "NbhdCD_1", "NbhdNM_1", "Nbhd_1_Prop", "NbhdCD_2", "NbhdNM_2", "Nbhd_2_Prop", 
  "NbhdCD_3", "NbhdNM_3", "Nbhd_3_Prop", "CityTY", "CityCD_1", "CityNM_1", "City_1_Prop", "CityCD_2", "CityNM_2", "City_2_Prop", 
  "LocalAuthorityTY", "LocalAuthorityCD_1", "LocalAuthorityNM_1", "LocalAuthority_1_Prop", "LocalAuthorityCD_2", "LocalAuthorityNM_2", "LocalAuthority_2_Prop", 
  "LocalAuthorityCD_3", "LocalAuthorityNM_3", "LocalAuthority_3_Prop", "CountyTY", "CountyCD_1", "CountyNM_1", "County_1_Prop", "CountyCD_2", "CountyNM_2", "County_2_Prop")]

RC.Naming$RegionNM[RC.Naming$RC_ID %in% c("RC_EW_2656")] <- "East of England"
RC.Naming$RegionCD[RC.Naming$RC_ID %in% c("RC_EW_2656")] <- "E12000006"
RC.Naming$RegionNM[RC.Naming$RC_ID %in% c("RC_EW_1437", "RC_EW_1543")] <- "South East"
RC.Naming$RegionCD[RC.Naming$RC_ID %in% c("RC_EW_1437", "RC_EW_1543")] <- "E12000008"
RC.Naming$RegionNM[RC.Naming$RC_ID %in% c("RC_EW_34", "RC_EW_26", "RC_EW_81", "RC_EW_6000")] <- "South West"
RC.Naming$RegionCD[RC.Naming$RC_ID %in% c("RC_EW_34", "RC_EW_26", "RC_EW_81", "RC_EW_6000")] <- "E12000009"
RC.Naming$RegionNM[RC.Naming$RC_ID %in% c("RC_EW_1883", "RC_EW_6001", "RC_EW_6002")] <- "London"
RC.Naming$RegionCD[RC.Naming$RC_ID %in% c("RC_EW_1883", "RC_EW_6001", "RC_EW_6002")] <- "E12000007"
RC.Naming$RegionNM[RC.Naming$RC_ID %in% c("RC_EW_3322")] <- "North West"
RC.Naming$RegionCD[RC.Naming$RC_ID %in% c("RC_EW_3322")] <- "E12000002"
RC.Naming$RegionNM[RC.Naming$RC_ID %in% c("RC_EW_3361", "RC_EW_5623", "RC_EW_5624", "RC_EW_5628")] <- "Wales"
RC.Naming$RegionCD[RC.Naming$RC_ID %in% c("RC_EW_3361", "RC_EW_5623", "RC_EW_5624", "RC_EW_5628")] <- "W92000004"

RC.Naming$Country[RC.Naming$RC_ID %in% c("RC_SC_6", "RC_SC_1642", "RC_SC_1656", "RC_SC_1712")] <- "Scotland"
RC.Naming$RegionNM[RC.Naming$RC_ID %in% c("RC_SC_6", "RC_SC_1642", "RC_SC_1656", "RC_SC_1712")] <- "Scotland"
RC.Naming$RegionCD[RC.Naming$RC_ID %in% c("RC_SC_6", "RC_SC_1642", "RC_SC_1656", "RC_SC_1712")] <- "S92000003"

RC.Naming$Country[RC.Naming$RC_ID %in% c("RC_EW_6001", "RC_EW_6002", "RC_EW_6000", "RC_EW_2656", "RC_EW_1437", 
  "RC_EW_1543", "RC_EW_34", "RC_EW_26", "RC_EW_81", "RC_EW_1883", "RC_EW_3322")] <- "England"
RC.Naming$Country[RC.Naming$RC_ID %in% c("RC_EW_3361", "RC_EW_5623", "RC_EW_5624", "RC_EW_5628")] <- "Wales"
RC.Naming$Country[RC.Naming$RC_ID %in% c("RC_SC_6")] <- "Scotland"

RC.Naming <- rbind(RC.Naming, cbind(RC_ID="RC_EW_5951", unique(RC.Naming[RC.Naming$NbhdCD_1=="E02000808" & RC.Naming$Nbhd_1_Prop==1, -c("RC_ID")])))

RC.Naming.ONS <- RC.Naming
rm(RC.Naming, RC.Nbhd, RC.City, RC.LADs, RC.County)
#------------------------------------------------------------------------------------


# OS NAMING - PLACE NAME EXTRACTION (NEAREST)
#------------------------------------------------------------------------------------
n <- names(fread(paste0(wd, "/Data/Boundaries and Lookups/opname_csv_gb/DOC/OS_Open_Names_Header.csv")))
t <- mclapply(as.list(list.files(paste0(wd, "/Data/Boundaries and Lookups/opname_csv_gb/DATA"), full.names=T)), function(x) fread(x, na.string=c("", "NA", "NULL"), header=F), mc.cores=20)
t <- mclapply(t, function(x) {names(x) <- n; return(x) }, mc.cores=20)
t <- mclapply(t, function(x) {x <- x[x$TYPE=="populatedPlace" | x$TYPE=="transportNetwork", c("NAME1", "TYPE", "LOCAL_TYPE", "GEOMETRY_X", "GEOMETRY_Y")]; return(x) }, mc.cores=20)
open.names <- do.call(rbind, t); rm(t, n)
open.names <- st_as_sf(open.names, coords = c("GEOMETRY_X", "GEOMETRY_Y"), crs = 27700)

names.dists <- mclapply(as.list(1:length(unique(RC_UK$RC_ID))), function(G){
  RC_buffer <- st_buffer(RC_UK[G,], dist=10000)
  RC_buffer <- st_intersection(open.names, RC_buffer)
  RC.cent <- st_centroid(RC_UK[G,])
  FF <- cbind(City=as.character(RC_buffer[RC_buffer$LOCAL_TYPE=="City",][which.min(st_distance(RC.cent, RC_buffer[RC_buffer$LOCAL_TYPE=="City",])),]$NAME1),
    CityD=min(st_distance(RC.cent, RC_buffer[RC_buffer$LOCAL_TYPE=="City",])),
    Village=as.character(RC_buffer[RC_buffer$LOCAL_TYPE=="Village",][which.min(st_distance(RC.cent, RC_buffer[RC_buffer$LOCAL_TYPE=="Village",])),]$NAME1),
    VillageD=min(st_distance(RC.cent, RC_buffer[RC_buffer$LOCAL_TYPE=="Village",])),
    Town=as.character(RC_buffer[RC_buffer$LOCAL_TYPE=="Town",][which.min(st_distance(RC.cent, RC_buffer[RC_buffer$LOCAL_TYPE=="Town",])),]$NAME1),
    TownD=min(st_distance(RC.cent, RC_buffer[RC_buffer$LOCAL_TYPE=="Town",])),
    Suburban=as.character(RC_buffer[RC_buffer$LOCAL_TYPE=="Suburban Area",][which.min(st_distance(RC.cent, RC_buffer[RC_buffer$LOCAL_TYPE=="Suburban Area",])),]$NAME1),
    SuburbanD=min(st_distance(RC.cent, RC_buffer[RC_buffer$LOCAL_TYPE=="Suburban Area",])),
    Settlement=as.character(RC_buffer[RC_buffer$LOCAL_TYPE=="Other Settlement",][which.min(st_distance(RC.cent, RC_buffer[RC_buffer$LOCAL_TYPE=="Other Settlement",])),]$NAME1),
    SettlementD=min(st_distance(RC.cent, RC_buffer[RC_buffer$LOCAL_TYPE=="Other Settlement",])),
    Hamlet=as.character(RC_buffer[RC_buffer$LOCAL_TYPE=="Hamlet",][which.min(st_distance(RC.cent, RC_buffer[RC_buffer$LOCAL_TYPE=="Hamlet",])),]$NAME1),
    HamletD=min(st_distance(RC.cent, RC_buffer[RC_buffer$LOCAL_TYPE=="Hamlet",])))
  return(FF) }, mc.cores = 25)

names.dists2 <- lapply(as.list(1:length(unique(RC_UK$RC_ID))), function(x) { 
  f <- as.data.frame(names.dists[[x]]); 
  if(dim(as.data.frame(names.dists[[x]]))[1]==0){
    f <- data.table()
    f$RC_ID <- unique(RC_UK$RC_ID)[[x]]
  } else {
    f$RC_ID <- unique(RC_UK$RC_ID)[[x]]
  } 
  return(f) })
names.dists2 <- rbindlist(names.dists2, fill=TRUE)
names.dists2$CityD <- as.numeric(as.character(names.dists2$CityD))
names.dists2$CityD[names.dists2$CityD==Inf] <- NA
names.dists2$VillageD <- as.numeric(as.character(names.dists2$VillageD))
names.dists2$VillageD[names.dists2$VillageD==Inf] <- NA
names.dists2$TownD <- as.numeric(as.character(names.dists2$TownD))
names.dists2$TownD[names.dists2$TownD==Inf] <- NA
names.dists2$SuburbanD <- as.numeric(as.character(names.dists2$SuburbanD))
names.dists2$SuburbanD[names.dists2$SuburbanD==Inf] <- NA
names.dists2$SettlementD <- as.numeric(as.character(names.dists2$SettlementD))
names.dists2$SettlementD[names.dists2$SettlementD==Inf] <- NA
names.dists2$HamletD <- as.numeric(as.character(names.dists2$HamletD))
names.dists2$HamletD[names.dists2$HamletD==Inf] <- NA


j1 <- RC_UK[RC_UK$RC_ID %in% names.dists2[is.na(names.dists2$Hamlet),]$RC_ID,]
j2 <- open.names[open.names$LOCAL_TYPE=="Hamlet",]
Hamlet.FILL <- mclapply(as.list(1:length(unique(j1$RC_ID))), function(G){
  names.distsFILL <- sf::st_distance(st_centroid(j1[G,]), j2, by_element=FALSE)
  FF <- cbind(Hamlet=as.character(j2[which.min(names.distsFILL),]$NAME1),
    HamletD=min(names.distsFILL))
  return(FF) }, mc.cores = 25)
Hamlet.FILL <- lapply(as.list(1:length(Hamlet.FILL)), function(x) { 
  f <- as.data.frame(Hamlet.FILL[[x]]); 
  if(dim(as.data.frame(Hamlet.FILL[[x]]))[1]==0){
    f <- data.table()
    f$RC_ID <- unique(j1$RC_ID)[[x]]
  } else {
    f$RC_ID <- unique(j1$RC_ID)[[x]]
  } 
  return(data.table(f)) })
Hamlet.FILL <- rbindlist(Hamlet.FILL, fill=TRUE)
Hamlet.FILL$HamletD <- as.numeric(as.character(Hamlet.FILL$HamletD))


j1 <- RC_UK[RC_UK$RC_ID %in% names.dists2[is.na(names.dists2$Settlement),]$RC_ID,]
j2 <- open.names[open.names$LOCAL_TYPE=="Other Settlement",]
Settlement.FILL <- mclapply(as.list(1:length(unique(j1$RC_ID))), function(G){
  names.distsFILL <- sf::st_distance(st_centroid(j1[G,]), j2, by_element=FALSE)
  FF <- cbind(Settlement=as.character(j2[which.min(names.distsFILL),]$NAME1),
    SettlementD=min(names.distsFILL))
  return(FF) }, mc.cores = 25)
Settlement.FILL <- lapply(as.list(1:length(Settlement.FILL)), function(x) { 
  f <- as.data.frame(Settlement.FILL[[x]]); 
  if(dim(as.data.frame(Settlement.FILL[[x]]))[1]==0){
    f <- data.table()
    f$RC_ID <- unique(j1$RC_ID)[[x]]
  } else {
    f$RC_ID <- unique(j1$RC_ID)[[x]]
  } 
  return(data.table(f)) })
Settlement.FILL <- rbindlist(Settlement.FILL, fill=TRUE)
Settlement.FILL$SettlementD <- as.numeric(as.character(Settlement.FILL$SettlementD))

j1 <- RC_UK[RC_UK$RC_ID %in% names.dists2[is.na(names.dists2$Suburban),]$RC_ID,]
j2 <- open.names[open.names$LOCAL_TYPE=="Suburban Area",]
Suburban.FILL <- mclapply(as.list(1:length(unique(j1$RC_ID))), function(G){
  names.distsFILL <- sf::st_distance(st_centroid(j1[G,]), j2, by_element=FALSE)
  FF <- cbind(Suburban=as.character(j2[which.min(names.distsFILL),]$NAME1),
    SuburbanD=min(names.distsFILL))
  return(FF) }, mc.cores = 25)
Suburban.FILL <- lapply(as.list(1:length(Suburban.FILL)), function(x) { 
  f <- as.data.frame(Suburban.FILL[[x]]); 
  if(dim(as.data.frame(Suburban.FILL[[x]]))[1]==0){
    f <- data.table()
    f$RC_ID <- unique(j1$RC_ID)[[x]]
  } else {
    f$RC_ID <- unique(j1$RC_ID)[[x]]
  } 
  return(data.table(f)) })
Suburban.FILL <- rbindlist(Suburban.FILL, fill=TRUE)
Suburban.FILL$SuburbanD <- as.numeric(as.character(Suburban.FILL$SuburbanD))


j1 <- RC_UK[RC_UK$RC_ID %in% names.dists2[is.na(names.dists2$Town),]$RC_ID,]
j2 <- open.names[open.names$LOCAL_TYPE=="Town",]
Town.FILL <- mclapply(as.list(1:length(unique(j1$RC_ID))), function(G){
  names.distsFILL <- sf::st_distance(st_centroid(j1[G,]), j2, by_element=FALSE)
  FF <- cbind(Town=as.character(j2[which.min(names.distsFILL),]$NAME1),
    TownD=min(names.distsFILL))
  return(FF) }, mc.cores = 25)
Town.FILL <- lapply(as.list(1:length(Town.FILL)), function(x) { 
  f <- as.data.frame(Town.FILL[[x]]); 
  if(dim(as.data.frame(Town.FILL[[x]]))[1]==0){
    f <- data.table()
    f$RC_ID <- unique(j1$RC_ID)[[x]]
  } else {
    f$RC_ID <- unique(j1$RC_ID)[[x]]
  } 
  return(data.table(f)) })
Town.FILL <- rbindlist(Town.FILL, fill=TRUE)
Town.FILL$TownD <- as.numeric(as.character(Town.FILL$TownD))

j1 <- RC_UK[RC_UK$RC_ID %in% names.dists2[is.na(names.dists2$City),]$RC_ID,]
j2 <- open.names[open.names$LOCAL_TYPE=="City",]
City.FILL <- mclapply(as.list(1:length(unique(j1$RC_ID))), function(G){
  names.distsFILL <- sf::st_distance(st_centroid(j1[G,]), j2, by_element=FALSE)
  FF <- cbind(City=as.character(j2[which.min(names.distsFILL),]$NAME1),
    CityD=min(names.distsFILL))
  return(FF) }, mc.cores = 25)
City.FILL <- lapply(as.list(1:length(City.FILL)), function(x) { 
  f <- as.data.frame(City.FILL[[x]]); 
  if(dim(as.data.frame(City.FILL[[x]]))[1]==0){
    f <- data.table()
    f$RC_ID <- unique(j1$RC_ID)[[x]]
  } else {
    f$RC_ID <- unique(j1$RC_ID)[[x]]
  } 
  return(data.table(f)) })
City.FILL <- rbindlist(City.FILL, fill=TRUE)
City.FILL$CityD <- as.numeric(as.character(City.FILL$CityD))


j1 <- RC_UK[RC_UK$RC_ID %in% names.dists2[is.na(names.dists2$Village),]$RC_ID,]
j2 <- open.names[open.names$LOCAL_TYPE=="Village",]
Village.FILL <- mclapply(as.list(1:length(unique(j1$RC_ID))), function(G){
  names.distsFILL <- sf::st_distance(st_centroid(j1[G,]), j2, by_element=FALSE)
  FF <- cbind(Village=as.character(j2[which.min(names.distsFILL),]$NAME1),
    VillageD=min(names.distsFILL))
  return(FF) }, mc.cores = 25)
Village.FILL <- lapply(as.list(1:length(Village.FILL)), function(x) { 
  f <- as.data.frame(Village.FILL[[x]]); 
  if(dim(as.data.frame(Village.FILL[[x]]))[1]==0){
    f <- data.table()
    f$RC_ID <- unique(j1$RC_ID)[[x]]
  } else {
    f$RC_ID <- unique(j1$RC_ID)[[x]]
  } 
  return(data.table(f)) })
Village.FILL <- rbindlist(Village.FILL, fill=TRUE)
Village.FILL$VillageD <- as.numeric(as.character(Village.FILL$VillageD))


names.dists2 <- merge(names.dists2, Hamlet.FILL, all.x=T, sort=F, by="RC_ID")
names.dists2[is.na(names.dists2$Hamlet.x),]$Hamlet.x <- names.dists2[is.na(names.dists2$Hamlet.x),]$Hamlet.y
names.dists2[is.na(names.dists2$HamletD.x),]$HamletD.x <- names.dists2[is.na(names.dists2$HamletD.x),]$HamletD.y
names.dists2$Hamlet.y <- NULL; names.dists2$HamletD.y <- NULL
names(names.dists2)[names(names.dists2)=="Hamlet.x"] <- "Hamlet"
names(names.dists2)[names(names.dists2)=="HamletD.x"] <- "HamletD"

names.dists2 <- merge(names.dists2, Settlement.FILL, all.x=T, sort=F, by="RC_ID")
names.dists2[is.na(names.dists2$Settlement.x),]$Settlement.x <- names.dists2[is.na(names.dists2$Settlement.x),]$Settlement.y
names.dists2[is.na(names.dists2$SettlementD.x),]$SettlementD.x <- names.dists2[is.na(names.dists2$SettlementD.x),]$SettlementD.y
names.dists2$Settlement.y <- NULL; names.dists2$SettlementD.y <- NULL
names(names.dists2)[names(names.dists2)=="Settlement.x"] <- "Settlement"
names(names.dists2)[names(names.dists2)=="SettlementD.x"] <- "SettlementD"

names.dists2 <- merge(names.dists2, Suburban.FILL, all.x=T, sort=F, by="RC_ID")
names.dists2[is.na(names.dists2$Suburban.x),]$Suburban.x <- names.dists2[is.na(names.dists2$Suburban.x),]$Suburban.y
names.dists2[is.na(names.dists2$SuburbanD.x),]$SuburbanD.x <- names.dists2[is.na(names.dists2$SuburbanD.x),]$SuburbanD.y
names.dists2$Suburban.y <- NULL; names.dists2$SuburbanD.y <- NULL
names(names.dists2)[names(names.dists2)=="Suburban.x"] <- "Suburban"
names(names.dists2)[names(names.dists2)=="SuburbanD.x"] <- "SuburbanD"

names.dists2 <- merge(names.dists2, Town.FILL, all.x=T, sort=F, by="RC_ID")
names.dists2[is.na(names.dists2$Town.x),]$Town.x <- names.dists2[is.na(names.dists2$Town.x),]$Town.y
names.dists2[is.na(names.dists2$TownD.x),]$TownD.x <- names.dists2[is.na(names.dists2$TownD.x),]$TownD.y
names.dists2$Town.y <- NULL; names.dists2$TownD.y <- NULL
names(names.dists2)[names(names.dists2)=="Town.x"] <- "Town"
names(names.dists2)[names(names.dists2)=="TownD.x"] <- "TownD"

names.dists2 <- merge(names.dists2, Village.FILL, all.x=T, sort=F, by="RC_ID")
names.dists2[is.na(names.dists2$Village.x),]$Village.x <- names.dists2[is.na(names.dists2$Village.x),]$Village.y
names.dists2[is.na(names.dists2$VillageD.x),]$VillageD.x <- names.dists2[is.na(names.dists2$VillageD.x),]$VillageD.y
names.dists2$Village.y <- NULL; names.dists2$VillageD.y <- NULL
names(names.dists2)[names(names.dists2)=="Village.x"] <- "Village"
names(names.dists2)[names(names.dists2)=="VillageD.x"] <- "VillageD"

names.dists2 <- merge(names.dists2, City.FILL, all.x=T, sort=F, by="RC_ID")
names.dists2[is.na(names.dists2$City.x),]$City.x <- names.dists2[is.na(names.dists2$City.x),]$City.y
names.dists2[is.na(names.dists2$CityD.x),]$CityD.x <- names.dists2[is.na(names.dists2$CityD.x),]$CityD.y
names.dists2$City.y <- NULL; names.dists2$CityD.y <- NULL
names(names.dists2)[names(names.dists2)=="City.x"] <- "City"
names(names.dists2)[names(names.dists2)=="CityD.x"] <- "CityD"

RC.Naming.OS <- names.dists2
rm(names.dists2, Hamlet.FILL, Settlement.FILL, Suburban.FILL, Town.FILL, Village.FILL, City.FILL, j1, j2)
#------------------------------------------------------------------------------------


# OS NAMING - PLACE NAME EXTRACTION (INTERNAL)
#------------------------------------------------------------------------------------
RC_buffer <- st_buffer(RC_UK, dist=100)
RC_buffer <- st_intersection(RC_buffer, open.names)

t <- RC_buffer[RC_buffer$LOCAL_TYPE %in% c("Village", "Town", "Suburban Area", "Other Settlement", "City", "Hamlet"),]
t$LOCAL_TYPE <- factor(t$LOCAL_TYPE)
t <- data.table(t)[,c("RC_ID", "NAME1", "LOCAL_TYPE")]
t$NAME1 <- as.character(t$NAME1)
t$LOCAL_TYPE <- as.character(t$LOCAL_TYPE)
t$RC_ID <- as.character(t$RC_ID)

t <- dcast(t[, lapply(.SD, paste, collapse = "; "), by = c("RC_ID", "LOCAL_TYPE")], RC_ID ~ LOCAL_TYPE, value.var="NAME1")
names(t) <- c("RC_ID", "City", "Hamlet", "Settlement", "Suburban", "Town", "Village")

Suburban.dup <- open.names[open.names$LOCAL_TYPE=="Suburban Area" & open.names$NAME1 %in% do.call(c, strsplit(t$Suburban[!is.na(t$Suburban)][grepl(";", t$Suburban[!is.na(t$Suburban)])], "; ")),]
Suburban.dup <- Suburban.dup[!is.na(Suburban.dup$NAME1),]
Suburban.rep <- t[which(grepl(";", t$Suburban)),][,c("RC_ID", "Suburban")]
sub.fix <- lapply(split(Suburban.rep, Suburban.rep$RC_ID), function(x) {
  h <- st_centroid(RC_UK[RC_UK$RC_ID==x$RC_ID,])
  k <- Suburban.dup[Suburban.dup$NAME1 %in% do.call(c, strsplit(Suburban.rep[Suburban.rep$RC_ID==x$RC_ID,]$Suburban, "; ")),]
  return(k[which.min(st_distance(h, k)),]$NAME1) })
Suburban.rep <- cbind(Suburban.rep, Suburban2=do.call(c, sub.fix))

t <- merge(t, Suburban.rep[,c("RC_ID", "Suburban2")], all.x=T, sort=F, by="RC_ID")
t$Suburban[!is.na(t$Suburban2)] <- t$Suburban2[!is.na(t$Suburban2)]
t$Suburban2 <- NULL

Settlement.dup <- open.names[open.names$LOCAL_TYPE=="Other Settlement" & open.names$NAME1 %in% do.call(c, strsplit(t$Settlement[!is.na(t$Settlement)][grepl(";", t$Settlement[!is.na(t$Settlement)])], "; ")),]
Settlement.dup <- Settlement.dup[!is.na(Settlement.dup$NAME1),]
Settlement.rep <- t[which(grepl(";", t$Settlement)),][,c("RC_ID", "Settlement")]
sub.fix <- lapply(split(Settlement.rep, Settlement.rep$RC_ID), function(x) {
  h <- st_centroid(RC_UK[RC_UK$RC_ID==x$RC_ID,])
  k <- Settlement.dup[Settlement.dup$NAME1 %in% do.call(c, strsplit(Settlement.rep[Settlement.rep$RC_ID==x$RC_ID,]$Settlement, "; ")),]
  return(k[which.min(st_distance(h, k)),]$NAME1) })
Settlement.rep <- cbind(Settlement.rep, Settlement2=do.call(c, sub.fix))

t <- merge(t, Settlement.rep[,c("RC_ID", "Settlement2")], all.x=T, sort=F, by="RC_ID")
t$Settlement[!is.na(t$Settlement2)] <- t$Settlement2[!is.na(t$Settlement2)]
t$Settlement2 <- NULL

Village.dup <- open.names[open.names$LOCAL_TYPE=="Village" & open.names$NAME1 %in% do.call(c, strsplit(t$Village[!is.na(t$Village)][grepl(";", t$Village[!is.na(t$Village)])], "; ")),]
Village.dup <- Village.dup[!is.na(Village.dup$NAME1),]
Village.rep <- t[which(grepl(";", t$Village)),][,c("RC_ID", "Village")]
sub.fix <- lapply(split(Village.rep, Village.rep$RC_ID), function(x) {
  h <- st_centroid(RC_UK[RC_UK$RC_ID==x$RC_ID,])
  k <- Village.dup[Village.dup$NAME1 %in% do.call(c, strsplit(Village.rep[Village.rep$RC_ID==x$RC_ID,]$Village, "; ")),]
  return(k[which.min(st_distance(h, k)),]$NAME1) })
Village.rep <- cbind(Village.rep, Village2=do.call(c, sub.fix))

t <- merge(t, Village.rep[,c("RC_ID", "Village2")], all.x=T, sort=F, by="RC_ID")
t$Village[!is.na(t$Village2)] <- t$Village2[!is.na(t$Village2)]
t$Village2 <- NULL

rm(Settlement.dup, Suburban.dup, Village.dup, sub.fix, Village.rep, Settlement.rep)

RC.Naming.OS_2 <- t; rm(t)
#------------------------------------------------------------------------------------


# STREET NAMING - FREQUENCY OF VOA POINTS (USES LDC FOR SCOTLAND AND NORTHERN IRELAND)
#------------------------------------------------------------------------------------
voa.retailEN <- st_read(paste0(wd, "/Data/VOA/Retail_Points.gpkg"))
names(voa.retailEN)[names(voa.retailEN)=="geom"] <- c("geometry")
st_geometry(voa.retailEN) <- "geometry"
voa.retailEN <- voa.retailEN[,c("address", "street", "town_city", "postal_district", "oa11", "postcode")]

voa.retailEN <- st_transform(voa.retailEN, st_crs(RC_UK))
voa.retailEN <- st_join(voa.retailEN, RC_UK[,c("RC_ID")], join = st_within)
voa.retailEN <- voa.retailEN[!is.na(voa.retailEN$RC_ID),]
retail_VOA <- voa.retailEN; rm(voa.retailEN)

proportions <- as_tibble(retail_VOA) %>%
  group_by(RC_ID, street) %>%
  filter(!is.na(street)) %>%
  filter(street!="") %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  filter(!is.na(RC_ID))
proportions$street[proportions$street==""] <- NA
proportions$street <- as.factor(proportions$street)
proportions <- as.data.table(proportions)

cl <- makeCluster(detectCores()/2+2)
registerDoSNOW(cl)
t <- list()
t <- foreach(j = 1:length(unique(proportions$RC_ID)), .combine=rbind, .errorhandling = "remove", .options.snow = list(progress = function(n) setTxtProgressBar(txtProgressBar(max = length(unique(proportions$RC_ID)), style = 3), n)), .packages=c("data.table")) %dopar% {
  t[[j]] <- do.call(cbind, split(proportions[proportions$RC_ID %in% unique(proportions$RC_ID)[j] & !is.na(proportions$street),][order(proportions[proportions$RC_ID %in% unique(proportions$RC_ID)[j] & !is.na(proportions$street),]$freq, decreasing=T),][1:3,-c(3)], 1:3))
  return(t[[j]]) }
stopCluster(cl)
names(t) <- c("RC_ID", "Str_OS1", "Str_OS1_P", "RC_ID2", "Str_OS2", "Str_OS2_P", "RC_ID3", "Str_OS3", "Str_OS3_P")
t$RC_ID2 <- NULL
t$RC_ID3 <- NULL
t$Str_OS3 <- NULL
t$Str_OS3_P <- NULL

RC.Naming.Streets <- t
rm(t, cl); gc(verbose=F)

## We do the same with secured LDC data to get street name proportions from this address listing
##  which is then exported. This is used in Scotland and N. Ireland where no VOA data is available.
#
# LDC.2018 <- fread(paste0(wd, "/Data/LDC/2018_Retail_Unit_Data_Results.csv"), na.strings=c("", "NA", "NULL"))
# LDC.2018 <- LDC.2018[is.na(LDC.2018$ClosedDate),]
# LDC.2018 <- LDC.2018[!is.na(LDC.2018$Longitude),]
# LDC.2018 <- LDC.2018[!is.na(LDC.2018$Latitude),]
# LDC.2018 <- unique(LDC.2018[,c("Latitude", "Longitude", "Street")])
# LDC.2018 <- st_as_sf(x = LDC.2018, coords = c("Longitude", "Latitude"), crs = 4326)
# LDC.2018 <- st_transform(LDC.2018, st_crs(RC_UK))
# 
# LDC.2018 <- st_join(LDC.2018, RC_UK[,c("RC_ID")], join = st_within)
# LDC.2018 <- LDC.2018[!is.na(LDC.2018$RC_ID),]
# 
# proportions <- as_tibble(LDC.2018) %>%
#   group_by(RC_ID, Street) %>%
#   filter(!is.na(Street)) %>%
#   filter(Street!="") %>%
#   summarise (n = n()) %>%
#   mutate(freq = n / sum(n)) %>%
#   filter(!is.na(RC_ID))
# proportions$Street[proportions$Street==""] <- NA
# proportions$Street <- as.factor(proportions$Street)
# proportions <- as.data.table(proportions)
# 
# cl <- makeCluster(detectCores()/2+2)
# registerDoSNOW(cl)
# t <- list()
# t <- foreach(j = 1:length(unique(proportions$RC_ID)), .combine=rbind, .errorhandling = "remove", .options.snow = list(progress = function(n) setTxtProgressBar(txtProgressBar(max = length(unique(proportions$RC_ID)), style = 3), n)), .packages=c("data.table")) %dopar% {
#   t[[j]] <- do.call(cbind, split(proportions[proportions$RC_ID %in% unique(proportions$RC_ID)[j] & !is.na(proportions$Street),][order(proportions[proportions$RC_ID %in% unique(proportions$RC_ID)[j] & !is.na(proportions$Street),]$freq, decreasing=T),][1:3,-c(3)], 1:3))
#   return(t[[j]]) }
# stopCluster(cl)
# names(t) <- c("RC_ID", "Str_LDC1", "Str_LDC1_P", "RC_ID2", "Str_LDC2", "Str_LDC2_P", "RC_ID3", "Str_LDC3", "Str_LDC3_P")
# t$RC_ID2 <- NULL
# t$RC_ID3 <- NULL
# t$Str_LDC3 <- NULL
# t$Str_LDC3_P <- NULL
# 
# RC.Naming.Streets <- merge(RC.Naming.Streets, t, all=T, sort=F, by="RC_ID")
# # table(substr(RC.Naming.Streets$RC_ID, 1, 5))
# rm(t, cl); gc(verbose=F)
#------------------------------------------------------------------------------------


# RETAIL PARK FLAG/NAME (NOT AVAILABLE IN RECENT LDC RELEASES)
#------------------------------------------------------------------------------------
## Similarly we use a previous release of LDC data where retail parks and 
##   corresponding names are classified. 
#
# LDC.2016 <- fread(paste0(wd, "/Data/LDC/LDC2016_PC_XY.csv"), na.strings=c("", "NA", "NULL"))
# names(LDC.2016) <- gsub(".x", "", names(LDC.2016))
# LDC.2016 <- LDC.2016[!is.na(LDC.2016$ShoppingAr) | !is.na(LDC.2016$Shopping_1),]
# LDC.2016 <- unique(LDC.2016[,c("Latitude", "Longitude", "ShoppingAr", "Shopping_1")])
# LDC.2016 <- st_as_sf(x = LDC.2016, coords = c("Longitude", "Latitude"), crs = 4326)
# LDC.2016 <- st_transform(LDC.2016, st_crs(RC_UK))
# 
# t <- unique(data.table(st_intersection(RC_UK, LDC.2016))[,c("RC_ID", "Shopping_1")])
# t$Type <- "Retail_Centre"
# t$Type[duplicated(t$RC_ID)] <- paste0(t$Type[duplicated(t$RC_ID)], "_2")
# 
# t <- dcast(t[, lapply(.SD, paste, collapse = "; "), by = c("RC_ID", "Type")], RC_ID ~ Type, value.var=c("Shopping_1"))
# names(t) <- c("RC_ID", "RetailParkNM_1", "RetailParkNM_2")
# retailPark.names <- t; rm(t)
# 
# RC.Naming.Streets <- merge(RC.Naming.Streets, retailPark.names, all.x=T, sort=F, by="RC_ID")
# 
# RC.Naming.Streets[,(names(RC.Naming.Streets)[sapply(RC.Naming.Streets, is.character)]) := lapply(.SD, as.factor), .SDcols = names(RC.Naming.Streets)[sapply(RC.Naming.Streets, is.character)]]
#------------------------------------------------------------------------------------


# EXPORT GENERATED NAMING LOOKUP FILES TO BE USED EXTERNALLY
#------------------------------------------------------------------------------------
RC.Naming.Streets <- merge(RC.Naming.Streets, unique(RC.Naming.ONS[,c("RC_ID", "Country", "RegionCD", "RegionNM")]), all.x=T, sort=F, by="RC_ID")
RC.Naming.Streets <- data.table(RC.Naming.Streets)[,c("RC_ID", "Country", "RegionCD", "RegionNM", "Str_OS1", "Str_OS1_P", "Str_OS2", "Str_OS2_P", "Str_OS3", "Str_OS3_P",
  "Str_LDC1", "Str_LDC1_P", "Str_LDC2", "Str_LDC2_P", "Str_LDC3", "Str_LDC3_P", "RetailParkNM_1", "RetailParkNM_2")]
fwrite(RC.Naming.Streets[order(RC.Naming.Streets$RegionNM, RC.Naming.Streets$RC_ID),], file=paste0(wd, "/Exports/Interim/RC_Naming_Streets.csv"))

RC.Naming.ONS <- data.table(RC.Naming.ONS)[,c("RC_ID", "Country", "RegionCD", "RegionNM",
  "NbhdTY", "NbhdCD_1", "NbhdNM_1", "Nbhd_1_Prop", "NbhdCD_2", "NbhdNM_2", "Nbhd_2_Prop", "NbhdCD_3", "NbhdNM_3", "Nbhd_3_Prop", 
  "CityTY", "CityCD_1", "CityNM_1", "City_1_Prop", "CityCD_2", "CityNM_2", "City_2_Prop", 
  "LocalAuthorityTY", "LocalAuthorityCD_1", "LocalAuthorityNM_1", "LocalAuthority_1_Prop", "LocalAuthorityCD_2", "LocalAuthorityNM_2", "LocalAuthority_2_Prop", 
    "LocalAuthorityCD_3", "LocalAuthorityNM_3", "LocalAuthority_3_Prop",
  "CountyTY", "CountyCD_1", "CountyNM_1", "County_1_Prop", "CountyCD_2", "CountyNM_2", "County_2_Prop")]
fwrite(RC.Naming.ONS[order(RC.Naming.ONS$RegionNM, RC.Naming.ONS$RC_ID),], file=paste0(wd, "/Exports/Interim/RC_Naming_ONS.csv"))

RC.Naming.OS <- merge(RC.Naming.OS, unique(RC.Naming.ONS[,c("RC_ID", "Country", "RegionCD", "RegionNM")]), all.x=T, sort=F, by="RC_ID")
RC.Naming.OS <- data.table(RC.Naming.OS)[,c("RC_ID", "Country", "RegionCD", "RegionNM", "City", "CityD", "Village", "VillageD", "Town", 
  "TownD", "Suburban", "SuburbanD", "Settlement", "SettlementD", "Hamlet", "HamletD")]
fwrite(RC.Naming.OS[order(RC.Naming.OS$RegionNM, RC.Naming.OS$RC_ID),], file=paste0(wd, "/Exports/Interim/RC_Naming_OS.csv"))

RC.Naming.OS_2 <- merge(RC.Naming.OS_2, unique(RC.Naming.ONS[,c("RC_ID", "Country", "RegionCD", "RegionNM")]), all.x=T, sort=F, by="RC_ID")
RC.Naming.OS_2 <- data.table(RC.Naming.OS_2)[,c("RC_ID", "Country", "RegionCD", "RegionNM", "City", "Village", "Town", "Suburban", 
  "Settlement", "Hamlet", "Harbour", "Port", "Railway", "Road", "Road_Sec")]
fwrite(RC.Naming.OS_2[order(RC.Naming.OS_2$RegionNM, RC.Naming.OS_2$RC_ID),], file=paste0(wd, "/Exports/Interim/RC_Naming_OS_Internal.csv"))
#------------------------------------------------------------------------------------
