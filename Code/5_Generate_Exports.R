########################################################################################
########  An open source delineation and hierarchical classification 
#######     of UK retail agglomeration
######
#####     Jacob L. Macdonald, Les Dolega, Alex Singleton
####      Last Updated: October, 2021
###
##  GENERATING HIERARCHY, NAMES AND EXPORTING FINAL RETAIL CENTRE FILES
##   
##  - Relative size within administrative units; and conventional place names are used
##    do develop the classification hierarchy and retail agglomeration naming.
##   
##  - Requires: 
##        Naming Lookup file; retail counts by agglomeration
##        
##  Notes: 1) Final Retail_Boundaries_UK.gpkg is exported to the /Exports/Final/ subfolder
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

# wd <- "<FILE PATH TO WORKING DIRECTORY>"

# Functions.R includes a set of functions related to h3 geometry tract connectivity,
#   infilling and spatial operations.
source(paste0(wd, "/Code/Functions.R"))
#------------------------------------------------------------------------------------


# READ IN RETAIL CENTRES; NAMING LOOKUPS; (EXTERNAL) LDC RETAIL COUNTS
#------------------------------------------------------------------------------------
RC_UK <- st_transform(st_read(paste0(wd, "/Exports/Interim/UK_RC_Pruned.gpkg")), crs=27700)
names(RC_UK)[names(RC_UK)=="geom"] <- c("geometry")
st_geometry(RC_UK) <- "geometry"
RC_UK <- RC_UK[,names(RC_UK) %in% c("RC_ID", "tr_VOA_retailN", "tr_OSM_retailN", "H3_count")]
RC_UK$RC_ID <- as.character(RC_UK$RC_ID)

# Secured LDC counts used for this section
#
# LDC.counts <- fread(paste0(wd, "/Data/_____.csv"))[,c("RC_ID", "Retail")]
# names(LDC.counts) <- c("RC_ID", "tr_LDC_retailN")
# RC_UK <- merge(RC_UK, LDC.counts, all.x=T, sort=F, by="RC_ID")
# rm(LDC.counts)

RC.Naming.Streets <- fread(file=paste0(wd, "/Exports/Interim/RC_Naming_Streets.csv"))
RC.Naming.ONS <- fread(file=paste0(wd, "/Exports/Interim/RC_Naming_ONS.csv"))
RC.Naming.OS <- fread(file=paste0(wd, "/Exports/Interim/RC_Naming_OS.csv"))
RC.Naming.OS_2 <- fread(file=paste0(wd, "/Exports/Interim/RC_Naming_OS_Internal.csv"))
#------------------------------------------------------------------------------------


# GENERATE THE HIERARCHY OF CLASSIFICATION
#------------------------------------------------------------------------------------
H_Classification <- merge(data.table(RC_UK)[,-c("geometry")], RC.Naming.ONS[,c("RC_ID", "RegionCD", "CountyCD_1", "LocalAuthorityCD_1", "CityCD_1")], all.x=T, sort=F)
H_Classification$CountyCD_1[is.na(H_Classification$CountyCD_1)] <- "NA"
H_Classification$LocalAuthorityCD_1[is.na(H_Classification$LocalAuthorityCD_1)] <- "NA"
H_Classification$CityCD_1 <- as.character(H_Classification$CityCD_1)
H_Classification$CityCD_1[is.na(H_Classification$CityCD_1)] <- "NA"

H_Classification <- do.call(rbind, lapply(split(H_Classification, H_Classification$RegionCD), function(x) { x$RGN_rank <- rank(-x$tr_LDC_retailN, na.last="keep", ties.method="first"); return(x) }))
H_Classification <- do.call(rbind, lapply(split(H_Classification, H_Classification$CountyCD_1), function(x) { x$CTY_rank <- rank(-x$tr_LDC_retailN, na.last="keep", ties.method="first"); return(x) }))
H_Classification <- do.call(rbind, lapply(split(H_Classification, H_Classification$LocalAuthorityCD_1), function(x) { x$LAD_rank <- rank(-x$tr_LDC_retailN, na.last="keep", ties.method="first"); return(x) }))
H_Classification <- do.call(rbind, lapply(split(H_Classification, H_Classification$CityCD_1), function(x) { x$BUA_rank <- rank(-x$tr_LDC_retailN, na.last="keep", ties.method="first"); return(x) }))
H_Classification <- H_Classification[order(H_Classification$RegionCD, H_Classification$RC_ID),]

H_Classification$RGN_rank[is.na(H_Classification$RGN_rank)] <- max(H_Classification$RGN_rank, na.rm=T)
H_Classification$CTY_rank[is.na(H_Classification$CTY_rank)] <- max(H_Classification$CTY_rank, na.rm=T)
H_Classification$LAD_rank[is.na(H_Classification$LAD_rank)] <- max(H_Classification$LAD_rank, na.rm=T)
H_Classification$BUA_rank[is.na(H_Classification$BUA_rank)] <- max(H_Classification$BUA_rank, na.rm=T)

H_Classification$Classification <- NA
H_Classification$tr_retailN <- pmax(H_Classification$tr_LDC_retailN, H_Classification$tr_VOA_retailN, na.rm=T)

## Up to town centre use LDC points; below that we take the maximum of the two LDC/VOA counts (tr_retailN)
#
# REGIONAL CENTRE: The main centre in each region (in the NW Liverpool plus Manchester - add in Liverpool manually)
criteria <- is.na(H_Classification$Classification) & 
  (H_Classification$RGN_rank==1 | H_Classification$RC_ID %in% c("RC_EW_2751", "RC_SC_280"))
H_Classification$Classification[criteria] <- "Regional Centre"

# MAJOR TOWN CENTRE: The main centre in each ceremonial county (if not already regional) AND are above 300 units 
criteria <- is.na(H_Classification$Classification) & 
  ((H_Classification$CTY_rank==1 & H_Classification$RGN_rank!=1) & (!is.na(H_Classification$tr_retailN) & H_Classification$tr_LDC_retailN >= 350))
H_Classification$Classification[criteria] <- "Major Town Centre"

# TOWN CENTRE: The main centre in each LA (if not regional or semi regional) OR 250-300 units
criteria <- is.na(H_Classification$Classification) & 
  ((H_Classification$CTY_rank==1 & H_Classification$RGN_rank!=1) | (H_Classification$LAD_rank==1 & H_Classification$RGN_rank!=1 & H_Classification$CTY_rank!=1)) |
  (!is.na(H_Classification$tr_retailN) & H_Classification$tr_LDC_retailN >= 250 & H_Classification$tr_LDC_retailN < 350)
H_Classification$Classification[criteria] <- "Town Centre"


# MARKET TOWN CENTRE: The major centre within each BUA (if the above conditions not met) AND 100-250 units (no others around)
criteria <- is.na(H_Classification$Classification) & 
  (H_Classification$BUA_rank==1 & H_Classification$LAD_rank!=1 & H_Classification$RGN_rank!=1 & H_Classification$CTY_rank!=1) & 
  (!is.na(H_Classification$tr_retailN) & H_Classification$tr_retailN >= 150 & H_Classification$tr_retailN < 250)
H_Classification$Classification[criteria] <- "Market Town"

# DISTRICT CENTRE: can be the same size as Market Town, but potentially in larger urban areas with potentially a regional or semi
criteria <- is.na(H_Classification$Classification) & 
  (H_Classification$BUA_rank!=1 & H_Classification$LAD_rank!=1 & H_Classification$RGN_rank!=1 & H_Classification$CTY_rank!=1) & 
  (!is.na(H_Classification$tr_retailN) & H_Classification$tr_retailN >= 150)
H_Classification$Classification[criteria] <- "District Centre"

# LOCAL CENTRE: 
criteria <- ( is.na(H_Classification$Classification) & (!is.na(H_Classification$tr_retailN) & H_Classification$tr_retailN < 150) )
H_Classification$Classification[criteria] <- "Local Centre"

# SMALL LOCAL CENTRE: 
criteria <- (H_Classification$Classification=="Local Centre") & 
  (H_Classification$Classification=="Local Centre" & !is.na(H_Classification$tr_retailN) & H_Classification$tr_retailN<100)
H_Classification$Classification[criteria] <- "Small Local Centre"

H_Classification$Classification[is.na(H_Classification$Classification) & !is.na(H_Classification$tr_LDC_retailN)] <- "Market Town"

criteria <- is.na(H_Classification$Classification)
H_Classification$Classification[criteria] <- "Small Local Centre"

# RETAIL PARKS
criteria <- H_Classification$RC_ID %in% as.character(RC.Naming.Streets[!is.na(RC.Naming.Streets$RetailParkNM_1),]$RC_ID) & H_Classification$Classification=="Small Local Centre"
H_Classification$Classification[criteria] <- "Retail Park"

H_Classification$Classification[H_Classification$Classification=="Retail Park" & H_Classification$tr_retailN < 50] <- "Small Retail Park"
H_Classification$Classification[H_Classification$Classification=="Retail Park" & H_Classification$tr_retailN >= 50] <- "Large Retail Park"
#------------------------------------------------------------------------------------


# MANUAL EDITS TO CLASSIFICATIONS/ SHOPPING OUTLETS
#------------------------------------------------------------------------------------
H_Classification$Classification[H_Classification$RC_ID=="RC_EW_2920"] <- "Market Town"
H_Classification$Classification[H_Classification$RC_ID=="RC_SC_332"] <- "Major Town Centre"
H_Classification$Classification[H_Classification$RC_ID=="RC_EW_876"] <- "Major Town Centre"
H_Classification$Classification[H_Classification$RC_ID=="RC_EW_5564"] <- "Major Town Centre"
H_Classification$Classification[H_Classification$RC_ID=="RC_EW_61"] <- "Major Town Centre"
H_Classification$Classification[H_Classification$RC_ID=="RC_EW_3096"] <- "Major Town Centre"
H_Classification$Classification[H_Classification$RC_ID=="RC_EW_3254"] <- "Major Town Centre"
H_Classification$Classification[H_Classification$RC_ID=="RC_EW_3301"] <- "Major Town Centre"
H_Classification$Classification[H_Classification$RC_ID=="RC_EW_3522"] <- "Regional Centre"
H_Classification$Classification[H_Classification$RC_ID=="RC_EW_3476"] <- "Major Town Centre"

# Reclassify a manual list of agglomerations to upgrade to Major Town Centres
RC_manual <- fread(paste0(wd, "/Exports/Interim/Manual_Hierarchy.csv"))
RC_manual <- as.character(unique(RC_manual[RC_manual$H_Classification=="Major Town Centre",]$RC_ID))
RC_manual <- RC_manual[!is.na(RC_manual)]
H_Classification$Classification[H_Classification$RC_ID %in% as.character(H_Classification[H_Classification$RC_ID %in% RC_manual & H_Classification$Classification=="Town Centre",]$RC_ID)] <- "Major Town Centre"
rm(RC_manual)

# Add in Manual classification of Shopping Outlets based on their location/ overlap with existing Retail Agglomerations

outlets <- data.table(rbind(
  cbind("Regional Shopping Centre", "Westfield London", 51.50768597440922, -0.22088352838801395),
  cbind("Regional Shopping Centre", "Metrocentre", 54.957214904105754, -1.6674536751658506),
  cbind("Regional Shopping Centre", "Trafford Centre", 53.465100417291744, -2.347637851106119),
  cbind("Regional Shopping Centre", "Westfield Stratford City", 51.54425552636472, -0.004332458383472109),
  cbind("Regional Shopping Centre", "Bluewater", 51.43912203176112, 0.27160383738290644),
  cbind("Regional Shopping Centre", "intu Merry Hill", 52.48138718476229, -2.110658395511001),
  cbind("Regional Shopping Centre", "Lakeside Shopping Centre", 51.489406902239956, 0.2853764470235825),
  cbind("Regional Shopping Centre", "Meadowhall", 53.41411230589209, -1.4111151687565566),
  cbind("Regional Shopping Centre", "East Kilbride Shopping Centre", 55.76021412051323, -4.17708950101207),
  cbind("Regional Shopping Centre", "Silverburn Centre", 55.82112954772368, -4.341253330960348),
  cbind("Regional Shopping Centre", "intu Braehead", 55.87600450967236, -4.364642953985305),
  cbind("Regional Shopping Centre", "White Rose Centre", 53.75868081616114, -1.5734907774777223),
  cbind("Large Outlet Shopping Centre", "Ashford Designer Outlet", 51.137387301260496, 0.8801687441992894),
  cbind("Large Outlet Shopping Centre", "Astle Park West Bromwich", 52.520103339621166, -1.9960259925402608),
  cbind("Large Outlet Shopping Centre", "Bicester Outlet Village", 51.89237714202758, -1.1560770883204268),
  cbind("Large Outlet Shopping Centre", "Bideford Atlantic Village", 51.00755545946937, -4.237327761297502),
  cbind("Large Outlet Shopping Centre", "Burberry Factory Outlet London", 51.54651001026377, -0.05093197052890261),
  cbind("Large Outlet Shopping Centre", "Castleford Junction 32 Outlet", 53.712463797168354, -1.3388364244282458),
  cbind("Large Outlet Shopping Centre", "Chatham Dockside Outlet Centre", 51.40160304040388, 0.5377104489852201),
  cbind("Large Outlet Shopping Centre", "Cheshire Oaks Designer Outlet", 53.26539272808919, -2.8809029974792315),
  cbind("Large Outlet Shopping Centre", "Clacton Factory Outlet", 51.81858769266023, 1.168209463308007),
  cbind("Large Outlet Shopping Centre", "Clarks Village Outlet Somerset", 51.12833201493458, -2.741588552823589),
  cbind("Large Outlet Shopping Centre", "Dalton Park Outlet", 54.81537131857434, -1.3739226162513756),
  cbind("Large Outlet Shopping Centre", "Doncaster Lakeside Village", 53.50769203205623, -1.1135984485990036),
  cbind("Large Outlet Shopping Centre", "Dover De Bradelei Wharf Outlet", 51.12211019226935, 1.3141697547138849),
  cbind("Large Outlet Shopping Centre", "East Midlands Outlet Junction 28", 53.108083884731556, -1.3126583496100666),
  cbind("Large Outlet Shopping Centre", "Freeport Braintree", 51.870070700810075, 0.5714801267433955),
  cbind("Large Outlet Shopping Centre", "Freeport Fleetwood Outlet", 53.91786152727727, -3.0118505404061744),
  cbind("Large Outlet Shopping Centre", "Freeport Talke Stoke-on-Trent", 53.070597467076176, -2.263078253210015),
  cbind("Large Outlet Shopping Centre", "Gloucester Quays Outlet", 51.85953205593224, -2.2516156614946916),
  cbind("Large Outlet Shopping Centre", "Hertfordshire Galleria Outlet Shopping", 51.76141229905345, -0.24021860677316503),
  cbind("Large Outlet Shopping Centre", "Hornsea Freeport Outlet", 53.898485374967684, -0.1724229262407349),
  cbind("Large Outlet Shopping Centre", "Kilver Court Designer Village", 51.19063584650441, -2.5353243560244905),
  cbind("Large Outlet Shopping Centre", "London Designer Outlet Wembley", 51.55646778421662, -0.28320440479433673),
  cbind("Large Outlet Shopping Centre", "Lowry Outlet Salford Quays", 53.47092253822526, -2.292514874626797),
  cbind("Large Outlet Shopping Centre", "Newcastle Royal Quays Outlet", 54.99532901611218, -1.4642812945442476),
  cbind("Large Outlet Shopping Centre", "Portsmouth Gunwharf Quays", 50.79546395254288, -1.103824062300293),
  cbind("Large Outlet Shopping Centre", "Springfields Spalding Outlet", 52.799438891276665, -0.126329033767297),
  cbind("Large Outlet Shopping Centre", "Swindon Designer Outlet", 51.56272470333755, -1.7971899666540831),
  cbind("Large Outlet Shopping Centre", "The Mill Outlet Batley", 53.7129199988173, -1.6251337290018772),
  cbind("Large Outlet Shopping Centre", "York Designer Outlet", 53.92233415376806, -1.0768705880781113),
  cbind("Large Outlet Shopping Centre", "Banbridge Village Outlet", 54.33172185692118, -6.276147653115943),
  cbind("Large Outlet Shopping Centre", "Cribbs Causeway", 51.524552488774034, -2.5955220725284307),
  cbind("Large Outlet Shopping Centre", "Bridgend Designer Outlets Wales", 51.532410963682594, -3.576839020076668),
  cbind("Large Outlet Shopping Centre", "Gretna Gateway Outlet Village", 54.99688074646182, -3.056523264134829),
  cbind("Large Outlet Shopping Centre", "Livingston Designer Outlet", 55.88310933797727, -3.520137944032474),
  cbind("Large Outlet Shopping Centre", "Sterling Mills Outlet", 56.14927287877639, -3.740053433726151)))
names(outlets) <- c("SubType", "Name", "Lat", "Lon")
outlets$Lat <- as.numeric(as.character(outlets$Lat))
outlets$Lon <- as.numeric(as.character(outlets$Lon))
outlets$Type <- "Out of Town Shopping Centres"

outlets <- st_as_sf(x = outlets, coords = c("Lon", "Lat"), crs = 4326)
outlets <- st_transform(outlets, st_crs(RC_UK))
outlets1 <- st_intersection(outlets, RC_UK[,c("RC_ID")])
outlets1 <- rbind(outlets1,
  cbind(outlets[outlets$Name=="Ashford Designer Outlet",], RC_ID="RC_EW_1312"),
  cbind(outlets[outlets$Name=="Chatham Dockside Outlet Centre",], RC_ID="RC_EW_1529"),
  cbind(outlets[outlets$Name=="Doncaster Lakeside Village",], RC_ID="RC_EW_762"),
  cbind(outlets[outlets$Name=="Swindon Designer Outlet",], RC_ID="RC_EW_3751"),
  cbind(outlets[outlets$Name=="Springfields Spalding Outlet",], RC_ID="RC_EW_955"))

H_Classification <- merge(H_Classification, data.table(outlets1)[,c("Type", "RC_ID")], all.x=T, sort=F, by="RC_ID")
H_Classification$Classification[!is.na(H_Classification$Type)] <- H_Classification$Type[!is.na(H_Classification$Type)]
H_Classification$Type <- NULL
#------------------------------------------------------------------------------------
 
 
# IDENTIFY CONVENTIAL SET OF NAMES
#------------------------------------------------------------------------------------
Master.Names <- merge(RC.Naming.Streets, RC.Naming.ONS[,-c("Country", "RegionCD", "RegionNM")], all=T, sort=F, by="RC_ID")
Master.Names <- merge(Master.Names, RC.Naming.OS[,-c("Country", "RegionCD", "RegionNM")], all=T, sort=F, by="RC_ID")
Master.Names <- merge(Master.Names, H_Classification[,c("RC_ID", "Classification")], all.x=T, sort=F, by="RC_ID")

Level_1 <- Master.Names[,c("RC_ID", "Str_OS1", "Str_OS1_P", "Str_OS2", "Str_OS2_P", "Str_LDC1", "Str_LDC1_P", "Str_LDC2", "Str_LDC2_P")]
Level_1$Street <- ifelse(Level_1$Str_OS1_P > 0.31, as.character(Level_1$Str_OS1), 
  ifelse(Level_1$Str_OS2_P > 0.31, paste0(as.character(Level_1$Str_OS1), " & ", as.character(Level_1$Str_OS2)), NA))
Level_1$Street2 <- ifelse(Level_1$Str_LDC1_P > 0.31, as.character(Level_1$Str_LDC1), 
  ifelse(Level_1$Str_LDC2_P > 0.31, paste0(as.character(Level_1$Str_LDC1), " & ", as.character(Level_1$Str_LDC2)), NA))
Level_1$Street[is.na(Level_1$Street)] <- Level_1$Street2[is.na(Level_1$Street)]
Level_1$Street2 <- NULL
Level_1$Street <- str_to_title(Level_1$Street)

Level_2 <- Master.Names[,c("RC_ID", "City", "CityD", "Village", "VillageD", "Town", "TownD", "Suburban", "SuburbanD", "Settlement", "SettlementD", "Hamlet", "HamletD")]
Level_2$min.label <- c("City", "Village", "Town", "Suburban", "Settlement", "Hamlet")[apply(Master.Names[,c("CityD", "VillageD", "TownD", "SuburbanD", "SettlementD", "HamletD")], 1, FUN=which.min)]
nearest <- list()
for(i in 1:length(Level_2$RC_ID)){
  nearest[[i]] <- as.character(Level_2[i,which(names(Level_2[i,])==Level_2[i,]$min.label),with=F][[1]])
}
Level_2$nearest <- as.character(do.call(rbind, nearest))

Level_2_inner <- RC.Naming.OS_2[,c("RC_ID", "City", "Village", "Town", "Suburban", "Settlement", "Hamlet")]
Level_2_inner$City[Level_2_inner$RC_ID=="RC_SC_54"] <- "St Enochs"
Level_2_inner$Village[Level_2_inner$RC_ID=="RC_SC_54"] <- "Blythswood New Town"
Level_2_inner$Town[Level_2_inner$RC_ID=="RC_SC_54"] <- "Merchant City"
Level_2_inner$Suburban[Level_2_inner$RC_ID=="RC_SC_54"] <- "Garnethill"
Level_2_inner$Settlement[Level_2_inner$RC_ID=="RC_SC_54"] <- "Broomielaw"

Level_2_inner$nearest <- NA
Level_2_inner$nearest <- ifelse(is.na(Level_2_inner$nearest), ifelse(is.na(Level_2_inner$City), NA, as.character(Level_2_inner$City)), Level_2_inner$nearest)
Level_2_inner$nearest <- ifelse(is.na(Level_2_inner$nearest), ifelse(is.na(Level_2_inner$Village), NA, as.character(Level_2_inner$Village)), Level_2_inner$nearest)
Level_2_inner$nearest <- ifelse(is.na(Level_2_inner$nearest), ifelse(is.na(Level_2_inner$Town), NA, as.character(Level_2_inner$Town)), Level_2_inner$nearest)
Level_2_inner$nearest <- ifelse(is.na(Level_2_inner$nearest), ifelse(is.na(Level_2_inner$Suburban), NA, as.character(Level_2_inner$Suburban)), Level_2_inner$nearest)
Level_2_inner$nearest <- ifelse(is.na(Level_2_inner$nearest), ifelse(is.na(Level_2_inner$Settlement), NA, as.character(Level_2_inner$Settlement)), Level_2_inner$nearest)
Level_2_inner$nearest <- ifelse(is.na(Level_2_inner$nearest), ifelse(is.na(Level_2_inner$Hamlet), NA, as.character(Level_2_inner$Hamlet)), Level_2_inner$nearest)
names(Level_2_inner)[names(Level_2_inner)=="nearest"] <- "inner"

Level_2 <- merge(Level_2, Level_2_inner[,c("RC_ID", "inner")], all.x=T, sort=F, by="RC_ID")
Level_2$nearest[which(!Level_2$nearest==Level_2$inner)] <- do.call(c, lapply(strsplit(Level_2[which(!Level_2$nearest==Level_2$inner),]$inner, ";"), function(x) x[1]))

Level_2 <- merge(Level_2, RC.Naming.ONS[,c("RC_ID", "NbhdNM_1")], all.x=T, sort=F, by="RC_ID")
Level_2[grepl("RC_NI", Level_2$RC_ID),]$nearest <- gsub("_*[[:digit:]]", "", as.character(Level_2[grepl("RC_NI", Level_2$RC_ID),]$NbhdNM_1))


Naming <- merge(Master.Names[,c("RC_ID", "LocalAuthorityNM_1", "Country", "RegionNM", "Classification")], Level_1[,c("RC_ID", "Street")], all.x=T, sort=F, by="RC_ID")
Naming <- merge(Naming, Level_2[,c("RC_ID", "nearest")], all.x=T, sort=F, by="RC_ID")
names(Naming) <- c("RC_ID", "LocalAuthority", "Country", "Region", "Classification", "Street", "Neighbourhood")
rm(Level_1, Level_2)

Naming$RC_Name1 <- paste(ifelse(is.na(Naming$Street), "", as.character(Naming$Street)),
  ifelse(is.na(Naming$Neighbourhood), "", as.character(Naming$Neighbourhood)),
  ifelse(is.na(Naming$LocalAuthority), "", as.character(Naming$LocalAuthority)),
  ifelse(is.na(Naming$Region), "", paste0("(", as.character(Naming$Region))),
  ifelse(is.na(Naming$Country), "", paste0(as.character(Naming$Country), ")")), sep = "; ")
Naming$RC_Name1 <- gsub("; ; ; ; ; ", "; ", Naming$RC_Name1)
Naming$RC_Name1 <- gsub("; ; ; ; ", "; ", Naming$RC_Name1)
Naming$RC_Name1 <- gsub("; ; ; ", "; ", Naming$RC_Name1)
Naming$RC_Name1 <- gsub("; ; ", "; ", Naming$RC_Name1)
Naming$RC_Name1[startsWith(Naming$RC_Name1, "; ")] <- gsub("^; ", "", Naming$RC_Name1[startsWith(Naming$RC_Name1, "; ")])
Naming$RC_Name1[endsWith(Naming$RC_Name1, "; ")] <- gsub("; $", "", Naming$RC_Name1[endsWith(Naming$RC_Name1, "; ")])

Naming$RC_Name2 <- paste(ifelse(is.na(Naming$Neighbourhood), "", as.character(Naming$Neighbourhood)),
  ifelse(is.na(Naming$LocalAuthority), "", as.character(Naming$LocalAuthority)),
  ifelse(is.na(Naming$Region), "", paste0("(", as.character(Naming$Region))),
  ifelse(is.na(Naming$Country), "", paste0(as.character(Naming$Country), ")")), sep = "; ")
Naming$RC_Name2 <- gsub("; ; ; ; ; ", "; ", Naming$RC_Name2)
Naming$RC_Name2 <- gsub("; ; ; ; ", "; ", Naming$RC_Name2)
Naming$RC_Name2 <- gsub("; ; ; ", "; ", Naming$RC_Name2)
Naming$RC_Name2 <- gsub("; ; ", "; ", Naming$RC_Name2)
Naming$RC_Name2[startsWith(Naming$RC_Name2, "; ")] <- gsub("^; ", "", Naming$RC_Name2[startsWith(Naming$RC_Name2, "; ")])
Naming$RC_Name2[endsWith(Naming$RC_Name2, "; ")] <- gsub("; $", "", Naming$RC_Name2[endsWith(Naming$RC_Name2, "; ")])

Naming$RC_Name <- ifelse(Naming$Classification %in% c("Regional Centre", "Major Town Centre", "Town Centre"), Naming$RC_Name2, Naming$RC_Name1)

name1 <- data.table(retailPark.names[retailPark.names$RC_ID %in% Naming[Naming$Classification=="Retail Park",]$RC_ID,])[,c("RC_ID", "RetailParkNM_1")]
name2 <- data.table(outlets1[outlets1$RC_ID %in% Naming[Naming$Classification=="Out of Town Shopping Centres",]$RC_ID,])[,c("RC_ID", "Name")]
name.replace <- Naming[Naming$RC_ID %in% c(name1$RC_ID, name2$RC_ID),]
name.replace <- merge(name.replace, name1, all.x=T, sort=F, by="RC_ID")
name.replace <- merge(name.replace, name2, all.x=T, sort=F, by="RC_ID"); rm(name1, name2)
name.replace$NAME <- ifelse(name.replace$Classification=="Retail Park", name.replace$RetailParkNM_1, name.replace$Name)

name.replace$RC_Name <- paste(ifelse(is.na(name.replace$NAME), "", as.character(name.replace$NAME)),
  ifelse(is.na(name.replace$LocalAuthority), "", as.character(name.replace$LocalAuthority)),
  ifelse(is.na(name.replace$Region), "", paste0("(", as.character(name.replace$Region))),
  ifelse(is.na(name.replace$Country), "", paste0(as.character(name.replace$Country), ")")), sep = "; ")
name.replace$RC_Name <- gsub("; ; ; ; ; ", "; ", name.replace$RC_Name)
name.replace$RC_Name <- gsub("; ; ; ; ", "; ", name.replace$RC_Name)
name.replace$RC_Name <- gsub("; ; ; ", "; ", name.replace$RC_Name)
name.replace$RC_Name <- gsub("; ; ", "; ", name.replace$RC_Name)
name.replace$RC_Name[startsWith(name.replace$RC_Name, "; ")] <- gsub("^; ", "", name.replace$RC_Name[startsWith(name.replace$RC_Name, "; ")])
name.replace$RC_Name[endsWith(name.replace$RC_Name, "; ")] <- gsub("; $", "", name.replace$RC_Name[endsWith(name.replace$RC_Name, "; ")])
names(name.replace)[names(name.replace)=="RC_Name"] <- "Shopping_Name"

Naming <- merge(Naming, name.replace[,c("RC_ID", "Shopping_Name")], all.x=T, sort=F)

Naming$RC_Name[!is.na(Naming$Shopping_Name)] <- Naming$Shopping_Name[!is.na(Naming$Shopping_Name)]
#------------------------------------------------------------------------------------



# NAMING CLEAN UP
#------------------------------------------------------------------------------------
Naming$RC_Name <- gsub("Scotland; Scotland", "Scotland", Naming$RC_Name)
Naming$RC_Name <- gsub("Wales; Wales", "Wales", Naming$RC_Name)
Naming$RC_Name <- gsub("Northern Ireland; Northern Ireland", "Northern Ireland", Naming$RC_Name)
Naming$RC_Name <- gsub("; \\(", " (", Naming$RC_Name)

Naming$RC_Name <- gsub("Bristol, City of", "City of Bristol", Naming$RC_Name)
Naming$RC_Name <- gsub("Kingston upon Hull, City of", "City of Kingston upon Hull", Naming$RC_Name)

d1 <- Naming[!grepl('\\)$', Naming$RC_Name),c("RC_ID", "RC_Name")]
d1$City <- do.call(c, lapply(strsplit(Naming$RC_Name[!grepl('\\)$', Naming$RC_Name)], "; "), function(x) x[2]))
d2 <- data.table(rbind(cbind(c("North East, England"), c("North Tyneside")),
  cbind(c("West Midlands, England"), c("Birmingham", "Coventry", "East Staffordshire", "Tamworth")),
  cbind(c("South West, England"), c("South Gloucestershire", "Sedgemoor", "Somerset West and Taunton")),
  cbind(c("East of England, England"), c("Luton", "South Cambridgeshire", "Basildon", "Tendring", "Breckland", 
    "King's Lynn and West Norfolk", "Norwich", "South Norfolk", "West Suffolk")),
  cbind(c("South East, England"), c("West Berkshire", "Reading", "Wokingham", "Isle of Wight", "East Hampshire", "Winchester", "Canterbury")),
  cbind(c("North West, England"), c("Cheshire West and Chester", "Lancaster")),
  cbind(c("East Midlands, England"), c("Charnwood", "South Northamptonshire", "Wellingborough", "High Peak")),
  cbind(c("Wales"), c("Conwy", "Swansea")),
  cbind(c("Scotland"), c("Aberdeenshire", "East Lothian", "East Renfrewshire", "City of Edinburgh",
  "Na h-Eileanan Siar", "Fife", "Glasgow City", "Highland", "Moray", "Perth and Kinross", 
  "Scottish Borders", "South Lanarkshire")),
  cbind(c("Northern Ireland"), c("Derry City and Strabane", "Belfast", "Mid and East Antrim", "Newry, Mourne and Down"))))
names(d2) <- c("Region2", "City")
d1 <- merge(d1, d2, all.x=T, sort=F, by="City"); rm(d2)
d1$RC_Name <- paste0(d1$RC_Name, " (", d1$Region2, ")")
d1 <- d1[,c("RC_ID", "RC_Name")]
names(d1) <- c("RC_ID", "RC_Name_fill")

Naming <- merge(Naming, d1, all.x=T, sort=F, by="RC_ID")
Naming$RC_Name[!is.na(Naming$RC_Name_fill)] <- Naming$RC_Name_fill[!is.na(Naming$RC_Name_fill)]
Naming$RC_Name_fill <- NULL
Naming$RC_Name[Naming$RC_Name==""] <- NA
Naming$RC_Name <- gsub("; \\(", " (", Naming$RC_Name)

Naming$Street_Flag <- as.numeric(do.call(rbind, lapply(str_split(make.unique(Naming$RC_Name, sep = "_"), "_"), function(x) as.numeric(x[2]))))
Naming$RC_Name[!is.na(Naming$Street_Flag)] <- paste0(Naming$RC_Name[!is.na(Naming$Street_Flag)], " - ", Naming$Street_Flag[!is.na(Naming$Street_Flag)])
Naming$Street_Flag <- NULL
Naming$RC_Name1 <- NULL
Naming$RC_Name2 <- NULL

Naming$RC_Name[Naming$RC_ID=="RC_EW_3507"] <- "Regent Street; Kingswood; South Gloucestershire (South West; England)"
Naming$RC_Name <- gsub("Herefordshire, County of", "County of Herefordshire", Naming$RC_Name)
#------------------------------------------------------------------------------------




# FINAL GENERATION OF FILES AND EXPORTS
#------------------------------------------------------------------------------------
hierarchy.portraits <- data.table(rbind(
  c("Regional Centre", "#882255", 
    "Largest retail centre in the broader administrative region"),
  c("Major Town Centre", "#332288",
    "Significantly large retail centres. Largest in the cer-emonial county with a minimum of 350 retail units"),
  c("Town Centre", "#117733",
    "Large retail centre (largest in the local authority or within the ceremonial county) with at least 250-350 units"),
  c("Market Town", "#44AA99",
    "Largest centre in the respective built up area with at least 150 retail units"),
  c("District Centre", "#999933",
    "Not the largest centre within a built up area, but has at least 150 retail units"),
  c("Local Centre", "#DDCC77",
    "Retail centres with under 150 units"),
  c("Small Local Centre", "#88CCEE",
    "Retail centres with under 100 units"),
  c("Large Retail Park", "#CC6677",
    "Small Local Retail Centres which are designated as Retail Parks with 50-100 units"),
  c("Small Retail Park", "#E0A1AB",
    "Small Local Retail Centres which are designated as Retail Parks with under 50 units"),
  c("Large Shopping Centre", "#AA4499",
    "Larger out of town shopping centres and outlets"),
  c("Small Shopping Centre", "#CD83C1",
    "Smaller out of town shopping centres and outlets with under 50 units")))
names(hierarchy.portraits) <- c("Classification", "Hex_Col", "Summary")
fwrite(hierarchy.portraits, file=paste0(wd, "/Exports/Final/Classification_Hierarchy.csv"))


Retail_Centres <- merge(RC_UK, Naming[,c("RC_ID", "RC_Name")], all.x=T, sort=F, by="RC_ID")
Retail_Centres <- merge(Retail_Centres, H_Classification[,c("RC_ID", "Classification")], all.x=T, sort=F, by="RC_ID")

Retail_Centres$Area_km2 <- as.numeric(st_area(Retail_Centres))*0.000001

st_write(Retail_Centres[,c("RC_ID", "RC_Name", "Classification", "Country", "Region_NM", "H3_count", "Area_km2", "tr_retailN")], 
  dsn = paste0(wd, "/Exports/Final/Retail_Boundaries_UK.gpkg"), delete_layer = TRUE, driver = "gpkg")
#------------------------------------------------------------------------------------