########################################################################################
########  An open source delineation and hierarchical classification 
#######     of UK retail agglomeration
######
#####     Jacob L. Macdonald, Les Dolega, Alex Singleton
####      Last Updated: October, 2021
###
##  BASE VOA DATA CLEANING
##   
##  - Code for cleaning the raw VOA data and formatting into an appropriate structure
##    for use in the eventual delineation of retail centres.
##  - Subsets VOA into retail specific components and geocodes addresses.
##
##  - Requires: 
##        uk-englandwales-ndr-2017-summaryvaluations-compiled-epoch-0019-baseline-csv.csv
##        uk-englandwales-ndr-2017-listentries-compiled-epoch-0019-baseline-csv.csv
##        NSPL_FEB_2020_UK.csv
##        cleaned .RDS file of the already geocoded addresses using the HERE API (retail_pcd.rds)
##
##  Notes: 1) Exports a cleaned and flat database for all retail related VOA observations
##         2) Makes use of the HERE API geocoder - requires an API key(s)
##


# LOAD PACKAGES; SET WD; READ IN FUNCTIONS
#------------------------------------------------------------------------------------
Packages <- c("tidyverse", "sf", "rgdal", "data.table", "treemap", "bit64", 
  "magrittr", "RColorBrewer")
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


# READ AND CLEAN RAW VOA POINTS DATA - SUMMARY VALUATIONS & RATING LIST ENTRIES
#
#   - VOA ratings list available from: https://voaratinglists.blob.core.windows.net/html/rlidata.htm
#   - Important to read the T&C first: https://www.tax.service.gov.uk/business-rates-find/terms-and-conditions
#   - Technical guidance on data structure: https://voaratinglists.blob.core.windows.net/html/rlidata.htm#technicalguidance
#------------------------------------------------------------------------------------
# Import 2017 non domestic rating summary valuations - details of the valuation
VOA <- fread(paste0(wd, "/Data/VOA/uk-englandwales-ndr-2017-summaryvaluations-compiled-epoch-0019-baseline-csv.csv"), 
  sep="*", stringsAsFactors=FALSE, header=FALSE, fill=TRUE)

VOA %<>%
  as_tibble() %>%
  rename(R_Type = V1, IDval = V2, 
    IDbus= V3, LA_ID= V4, 
    firm_name = V5, number_name = V6,
    sub_street_Lv3 = V7, sub_street_Lv2 = V8,
    sub_street_Lv1 = V9, street = V10, 
    town_city = V11, postal_district = V12, 
    county = V13, postcode = V14, 
    scheme_ref = V15, description = V16,
    area_m2 = V17, sub_total_value = V18, 
    total_value = V19, value_rated = V20, 
    list_year = V21, LA_name = V22, 
    LA_ref = V23, VO_ref = V24,
    start_date = V25, end_date = V26, 
    SCAT = V27, unit_measurement = V28, 
    unadjusted_price = V29) 

# Unique IDs for each unit is given by IDval which is 10 numbers in length with a 
#   number of different items associated to each (which make up the valuation components)
# We extract these individual units and fill in the IDs for all the records
# An alternative would be to use the R_Type variable where values == 1
VOA %<>%  
  as_tibble() %>%
  mutate(IDval_ID = case_when(grepl("^[0-9]{10}",IDval) ~ IDval)) %>%
  fill(IDval_ID, .direction = "down") 


# Import 2017 non domestic rating list entries - summary data of each hereditament
VOA_base <- fread(paste0(wd, "/Data/VOA/uk-englandwales-ndr-2017-listentries-compiled-epoch-0019-baseline-csv.csv"), 
  sep="*", stringsAsFactors=FALSE, header=FALSE, fill=TRUE)

VOA_base %<>%
  as_tibble() %>%
  rename(LA_ID = V2, NDR_Code = V3,
  LA_ref = V4, desc_code = V5,
  description = V6, IDbus = V7,
  address=V8, firm_name = V9,
  number_name=V10, street= V11,
  town_city= V12, postal_district = V13,
  county = V14, pcd = V15,
  effective_date = V16, comp_indicator = V17,
  value_rated = V18, appeal_code = V19,
  IDval_ID=V20, list_alteration = V21,
  SCAT = V22, sub_street_Lv3 = V23,
  sub_street_Lv2 = V24, sub_street_Lv1 = V25, 
  case_number = V26, current_from =V27) %>%
  select(-V1,-V28,-V29)
#------------------------------------------------------------------------------------


# SPLIT TABLES INTO SUB-TABLES AND CLEAN STRUCTURE
#
# It is necessary to split the records into two sub tables as they have slightly 
#   different structure. This uses the "R_Type" to filter the records; and 
#   renames some columns.
#------------------------------------------------------------------------------------
# Create a table of the main valuations
val_table <- VOA %>%
  select(c(1:7,"IDval_ID")) %>%
  filter((R_Type == "2")) %>%
  rename(R_Type = R_Type,
    ID = IDval, Floor= IDbus, 
    Description_Floor= LA_ID, 
    Area_M_2 = firm_name, 
    Price_M_2 = number_name,
    Sub_Total_Price = sub_street_Lv3,
    IDval_ID = IDval_ID) %>%
  mutate(IDval_ID = as.integer64((IDval_ID)))

# Create a table of the additional details
val_table_added <- VOA %>%
  select(c(1:7,"IDval_ID")) %>%
  filter( !(R_Type %in% c("1","2"))) %>%
  rename( R_Type = R_Type,
    Description_Added = IDval, 
    Area_M_2 = IDbus, 
    Price_M_2 = LA_ID, 
    Sub_Total_Price = firm_name, 
    X = number_name,
    XX = sub_street_Lv3,
    IDval_ID = IDval_ID)%>%
  mutate(IDval_ID = as.integer64(IDval_ID))

# Fix some columns
val_table %<>%
  mutate(Area_M_2 = ifelse(Area_M_2 ==  "N/A", NA,Area_M_2)) %>%
  mutate(Area_M_2 = as.numeric(Area_M_2),
    Price_M_2 = as.numeric(Price_M_2),
    Sub_Total_Price = as.numeric(Sub_Total_Price))
#------------------------------------------------------------------------------------


# VOA DESCRIPTOR CATEGORIES AND RETAIL DEFINITIONS
# 
# Identifying retail, leisure and pub/bar amenities
#------------------------------------------------------------------------------------
VOA_base %>%
  group_by(description)%>%
  summarize(n=n(),.groups="drop")%>%
  mutate(pct=round(n/sum(n)*100,2))  %>%
  arrange(-n)

retail_descriptions <- c("SHOP AND PREMISES","STORE AND PREMISES","RESTAURANT AND PREMISES",
  "HAIRDRESSING SALON AND PREMISES","CAFE AND PREMISES","RETAIL WAREHOUSE AND PREMISES",
  "HOTEL AND PREMISES","STORE","KIOSK AND PREMISES","MARKET STALL AND PREMISES",
  "BETTING SHOP AND PREMISES","SHOP","SUPERSTORE AND PREMISES","MARKET STALL",
  "POST OFFICE AND PREMISES","BEAUTY SALON AND PREMISES","SHOP & PREMISES","LAUNDERETTE AND PREMISES",
  "SHOP, OFFICE AND PREMISES","PHARMACY AND PREMISES","MARKET STALL & PREMISES",
  "AMUSEMENT ARCADE AND PREMISES","DENTAL SURGERY AND PREMISES","STORES","SHOP AND PREMISES",
  "SHOP, STORE AND PREMISES","STALL AND PREMISES","HAIRDRESSING SALON","SUPERMARKET AND PREMISES",
  "SHOP, WORKSHOP AND PREMISES","TREATMENT ROOM AND PREMISES","BEAUTY SALON","DOG GROOMING PARLOUR",
  "TAKEAWAY AND PREMISES","DOG GROOMING PARLOUR AND PREMISES","RETAIL UNIT AND PREMISES","MARKET STALLS",
  "CAFE","STORE & PREMISES","MARKET STALLS AND PREMISES","CAFE & PREMISES","RESTAURANT & PREMISES",
  "LARGE FOODSTORE","BEAUTY SALON & PREMISES","STORE, STORE AND PREMISES","CAFE BAR AND PREMISES",
  "TAKE AWAY AND PREMISES","FUNERAL PARLOUR","TATTOO STUDIO AND PREMISES","BUREAU DE CHANGE",
  "SHOP AND PREMISES.","VETERINARY SURGERY & PREMISES","HAIR SALON AND PREMISES","TEA ROOM AND PREMISES",
  "BARBERS SHOP AND PREMISES","SALON","YOGA STUDIO AND PREMISES","FOOD COURT AND PREMISES","RETAIL UNIT",
  "DENTAL SURGERY","HAIRDRESSING SALON & PREMISES","COFFEE SHOP AND PREMISES","SHOP AND PREMISES",
  "SHOP AND PREMISE","LARGE FOODSTORE AND PREMISES","PHARMACY & PREMISES","TATTOO PARLOUR AND PREMISES",
  "BARBER SHOP AND PREMISES","RESTAURANT","VETERINARY SURGERY","SHOP (AND PREMISES)","SALON & PREMISES",
  "SHOP, CAFE AND PREMISES","SHOP, POST OFFICE AND PREMISES","SHOP CAFE AND PREMISES",
  "DOG GROOMING PARLOUR & PREMISES","SHOP POST OFFICE AND PREMISES","POST OFFICE","PET GROOMING PARLOUR",
  "PET GROOMING PARLOUR AND PREMISES","BEAUTY ROOM AND PREMISES","FITNESS CENTRE")

leisure_descriptions <- c("GYMNASIUM AND PREMISES","MUSEUM AND PREMISES","GYM AND PREMISES","THEATRE AND PREMISES",
  "LIBRARY AND PREMISES","LEISURE CENTRE AND PREMISES","MUSEUM AND PREMISES","SPORTS CENTRE AND PREMISES",
  "CINEMA AND PREMISES","SWIMMING POOL AND PREMISES","DANCE STUDIO AND PREMISES",
  "DANCE SCHOOL AND PREMISES","GYMNASIUM & PREMISES","BINGO HALL AND PREMISES",
  "FITNESS CENTRE AND PREMISES","GALLERY AND PREMISES","BOWLING ALLEY AND PREMISES",
  "NIGHTCLUB AND PREMISES","GYMNASIUM","FITNESS STUDIO AND PREMISES","GYM & PREMISES",
  "HEALTH & FITNESS CLUB AND PREMISES","INDOOR BOWLING CENTRE AND PREMISES","CASINO AND PREMISES",
  "HEALTH AND FITNESS CLUB AND PREMISES","HEALTH CLUB AND PREMISES","GYM","FITNESS CENTRE & PREMISES",
  "HEALTH AND FITNESS CENTRE AND PREMISES","FITNESS STUDIO","DANCE STUDIO & PREMISES",
  "ART GALLERY AND PREMISES","ART GALLERY","GALLERY AND PREMISES","THEATRE")

# Missing are pubs and bars which do not have the same breakdown of valuation as other retail / leisure
# Valuations instead based on: annual level of trade (excluding VAT) that a pub is expected to achieve
# We identify pubs and bars using a series of string searches


pub_bar_descriptions <- VOA_base %>%
  filter(str_detect(description, '\\bPUBLIC\\b|\\bPUB\\b|\\bBAR\\b'))  %>%
  filter(!str_detect(description, 
    '\\bCONVENIENCE\\b|\\bCONVENIENCES\\b|\\bTELEPHONE\\b|\\bBROADBAND\\b|\\bSNACK\\b|\\bNAIL\\b|\\bSNACK\\b|\\bTOILETS\\b|\\bTOILET\\b|\\bSANDWICH\\b|\\bJUICE\\b|\\bJUICE\\b|\\bOCNVENIENCE\\b|\\bTAKE AWAY\\b|\\bCAFE\\b|\\bCONVIENIENCE\\b|\\bCONVENIENCIES\\b|\\bGARDEN\\b|\\bDELI\\b|\\bBRAODBAND\\b|\\bBROADBRAND\\b|\\bCONCENIENCE\\b|\\bCONENIENCE\\b|\\bCONVENCIENCES\\b|\\bCONVENIANCES\\b|\\bCONVIENCIES\\b|\\bCOVENIENCES\\b|\\bBEAUTICIAN\\b|\\bBURGER\\b|\\bSTYLE\\b|\\bFISH\\b|\\bCRAZY\\b|\\bKAROKE\\b|\\bMOTORCYCLE\\b|\\bOFFICES\\b|\\bPRIVATE\\b|\\bSCHOOL\\b|\\bWEDDING\\b|\\bWATERSPORTS\\b|\\bTAPAS\\b|\\bSUSHI\\b|\\SNOOKER\\b|\\bSANWICH\\b|\\bSAUNA\\b|\\bSHISHA\\b|\\bTPOILET\\b|\\bWC\\b|\\bWCS\\b|\\bREFRESHMENT\\b|\\bWEIGHBRIDGE\\b|\\bTICKET\\b|\\bLIBRARY\\b|\\bCARAVAN\\b|\\bCATERING\\b|\\bCPONVENIENCES\\b|\\bCONVIENCE\\b|\\bCONVIENCES\\b|\\bCONVENENIENCES\\b|\\bCONVENIEMCES\\b|\\bPARK\\b|\\bCONVEINCE\\b|\\bCONVEINENCE\\b|\\bCONVEINIENCE\\b|\\bCONVINIENCE\\b|\\bCONVIENIENCES\\b|\\bCONVINIENCES\\b|\\bENQUIRY\\b|\\bHALL\\b|\\bOYSTER\\b|\\bCONCENIENCES\\b|\\bCONVEIENCE\\b|\\bCONVEMIENCES\\b|\\bCONVENEINCES\\b|\\bCONVENICENCE\\b|\\bCONVENIECE\\b|\\bCONVENIECES\\b|\\bCOMMUNICATION\\b|\\bCOFFEE\\b|\\bBEDROOMS\\b|\\bCONVEINIENCES\\b|\\bCONVEIENCES\\b|\\bCONVENIENC\\b|\\bCONVENIENCENCES\\b|\\bCONVENIENECE\\b|\\bCONVEINENCES\\b|\\bTEA\\b|\\bBROADCAST\\b|\\bCONVNIENCE\\b|\\bCONVIENENCE\\b|\\bCONVIENECES\\b|\\bHOTEL\\b|\\bHOSTEL\\b|\\bGUEST\\b|\\bMORTUARY\\b|\\bDESSERT\\b|\\bDOCKBED\\b|\\bCINEMA\\b|\\bWORSHIP\\b|\\bBOWLING\\b|\\bBISTRO\\b|\\bRESTAURANT\\b|\\bWORKSHOP\\b|\\bSERVERY\\b|\\bMUSIC\\b|\\bAPERA\\b|\\bSHOP\\b|\\bMOTEL\\b|\\bB&B\\b|\\bTAKEAWAY\\b|\\bINFORMATION\\b|\\bHOLIDAY\\b|\\bPOST\\b|\\bLODGE\\b|\\bCOVENIENCE\\b|\\bCOVENIENCE\\b|\\bPERFORMANCE\\b|\\bPIZZA\\b|\\bSTORE\\b')) %>%
  group_by(description)%>%
  summarize(n=n(),.groups="drop")%>%
  mutate(pct=round(n/sum(n)*100,2))  %>%
  arrange(-n)

pub_bar_descriptions
pub_bar_descriptions  <- unique(as.character(pub_bar_descriptions$description))
#------------------------------------------------------------------------------------


# IDENTIFYING VOA RETAIL POINTS
#------------------------------------------------------------------------------------
# Get a list of the valuation ids
ids <- VOA_base %>%
  filter(description %in% retail_descriptions) %>%
  select("IDval_ID")

# Print the frequency table
val_table %>%
  filter(IDval_ID  %in% ids$IDval_ID) %>%
  group_by(Description_Floor)%>%
  summarize(n=n(),.groups="drop")%>%
  mutate(pct=round(n/sum(n)*100,2))  %>%
  arrange(-n)

core_retail <- c("Retail Zone A","Retail Zone B","Retail Zone C","Remaining Retail Zone",
  "Retail Area","Ground Floor Sales","Restaurant","Retail Zone D","Surgery",
  "Retail Zone E","Retail Zone F","Amusement Arcade")

# Create a subset of retail
retail_VOA <- val_table %>%
  filter( (Description_Floor %in% core_retail)) %>%
  select(IDval_ID) %>%
  distinct() %>%
  left_join(VOA_base[,c("IDval_ID","IDbus","address","pcd","description")])

core_retail_IDs <- retail_VOA$IDval_ID 
#------------------------------------------------------------------------------------


# REMOVE SOME NON-RETAIL TYPE UNITS
#------------------------------------------------------------------------------------
rm <- c("OFFICES AND PREMISES","OFFICE AND PREMISES","OFFICE & PREMISES","OFFICE","OFFICES",
  "OFFICES & PREMISES","OFFICES, OFFICE AND PREMISES","OFFICE  AND PREMISES",
  "OFFICES, WORKSHOP AND PREMISES","OFFICE AND PREMS","OFFICES AND PREMISES (DABW)",
  "OFFICES AND PREMISE","OFFICES CAFE AND PREMISES","SITE OFFICE","OFFICE AND PREMISES.",
  "OFFICE AND PREMSIES","OFFICES SHOP AND PREMISES","OFFICES STORES AND PREMISES","OFFICES, SHOP AND PREMISES",
  "OFFICES, STUDIO AND PREMISES","OFFICES, WORKSHOPS AND PREMISES","CAB OFFICE","COMMUNITY OFFICE",
  "COMMUNITY OFFICES","COMMUNITY POST OFFICE","COMMUNITY SUPPORT OFFICE","FIRE MARSHALL OFFICE","OFFICE , WORKSHOP & PREMISES",
  "OFFICE (AND PREMISES)","OFFICE / SHOP & PREMISES","OFFICE & PREMISE","OFFICE & PREMSES","OFFICE & STORAGE",
  "OFFICE &PREMISES","OFFICE AND PREMESIS","OFFICE AND PREMISEES","OFFICE AND PREMISIS","OFFICE AND PRESMIES",
  "OFFICE AND PRMISES","OFFICE AND RETAIL UNIT","OFFICE AND TRAINING ROOMS","OFFICE CLINIC AND PREMISES",
  "OFFICE GYM & PREMISES","OFFICE LICENCE AND PREMISES","OFFICE ND PREMISES","OFFICE SHOP WORKSHOP AND PREMISES",
  "OFFICE STORE AND PREMSIES","OFFICE TRAINING CENTRE & PREMISES","OFFICE USED AS SHOP","OFFICE WORKSHOP AND PREMISES",
  "OFFICE, CAR SPACES AND PREMISES","OFFICE, HALL AND PREMISES","OFFICE, SHOP AND PREMISES","OFFICE, SHOWROOM AND PREMISES",
  "OFFICE, STORE AND PREMISES","OFFICE, STORES AND PREMISES","OFFICE, WORKSHOP & PREMISES","OFFICE/SHOWROOM AND PREMISES",
  "OFFICE/TRAINING CENTRE AND PREMISES","OFFICES  AND PREMISES","OFFICES 'AND PREMISES","OFFICES & PREIMES","OFFICES & PREMIES",
  "OFFICES AND 9 PARKING SPACES","OFFICES AND OFFICES","OFFICES AND PREMISES (PART EXEMPT)",
  "OFFICES AND PREMISES (PARTIALLY EXEMPT)","OFFICES AND PREMISES PART","OFFICES AND PREMISES.",
  "OFFICES AND PREMSIES","OFFICES AND STORES","OFFICES CRECHE AND PREMISES","OFFICES FARM SHOP AND PREMISES",
  "OFFICES MUSEUM AND PREMISES","OFFICES STUDIOS AND PREMISES","OFFICES USED AS SURGERY AND PREMISES",
  "OFFICES VALUED AS SHOP AND PREMISES","OFFICES, BANK AND PREMISES","OFFICES, CAFE, HALL AND PREMISES",
  "OFFICES, CAR SPACES (264) AND PREMISES","OFFICES, CAR SPACES AND PREMISES","OFFICES, CHECK-IN DESKS AND PREMISES",
  "OFFICES, COMMUNITY CENTRE AND PREMISES","OFFICES, GYM, HEALTH SUITE, RESTAURANT AND PREMISES",
  "OFFICES, LAUNDRETTE AND PREMISES","OFFICES, LEISURE CENTRE, LIBRARY AND PREMISES","OFFICES, SALON AND PREMISES",
  "OFFICES, SELF-CATERING APARTMENTS AND PREMISES","OFFICES, SHOP, HALL AND PREMISES","OFFICES, sTOREs AND PREMISES",
  "OFFICES, WAREHOUSE AND PREMISES","OFFICES, WORKSHOP, MOORINGS AND PREMISES","OFFICES,CHAPEL & PREMISES","OFFICESAND PREMISES",
  "RSPCA OFFICE","SALES OFFICE AND PREMISESE","SALES OFFICE SHOWHOME AND PREMISES","SALES OFFICE WORKSHOP AND PREMISES",
  "SALES PITCH USED AS OFFICE AND PREMISES","SECURITY OFFICE AND PREMISES","STAFFROOM & SECURITY OFFICE AND PREMISES",
  "STOCK ROOM AND OFFICE","STORAGE LAND,OFFICES AND PREMISES","STORE AND OFFICE","STORE SHOP CAFE OFFICE AND PREMISES",
  "STORE, OFFICES AND PREMISES","STORE, POST OFFICE, SELF CATERING UNIT AND PREMISES","STORES, RETAIL, OFFICES",
  "STORES, SHOP, OFFICES AND PREMISES","STORY TELLERS GARDEN,OFFICE AND PREMISES","STUDIO, OFFICES AND PREMISES",
  "TEMP SITE OFFICE AND PREMISES","CAR AUCTION AND PREMISES","CAR AUCTION SITE AND PREMISES","CAR SHOWROOM, WORKSHOP AND PREMISES",
  "CAR AUCTION & PREMISES","CAR AUCTION","CAR SALES SITE AND PREMISES","DISPLAY AREA","CAR AUCTION AND PREMISES","BUILDERS MERCHANTS AND PREMISES",
  "BUILDERS MERCHANT AND PREMISES","BUILDERS MERCHANTS & PREMISES","BUILDERS MERCHANT & PREMISES","BUILDERS MERCHANTS",
  "BUILDERS' MERCHANTS & PREMISES","BUILDERS' MERCHANTS AND PREMISES","FACTORY AND PREMISES","SELF CATERING HOLIDAY UNIT AND PREMISES",
  "PROPERTY BEYOND ECONOMIC REPAIR","OFFICES, STORE AND PREMISES","BUILDING UNDERGOING REDEVELOPMENT","SITE OF PHOTO BOOTH",
  "COLD STORE AND PREMISES","MILL AND PREMISES","MARKETING SUITE","CHAPEL OF REST","SITE OF SALES PITCH & PREMISES","LAND COVERED BY WATER USED FOR FISHING",
  "RESEARCH CENTRE AND PREMISES","UNDERGOING WORKS","UNDER RECONSTRUCTION","UNDER REDEVELOPMENT","SORTING OFFICE & PREMISES","POTTERY AND PREMISES","MOORING",
  "MORTUARY AND PREMISES","AMUSEMENT PARK SITE","PORTAKABIN","MESS ROOM AND PREMISES","HIGH DEPENDENCY UNIT AND PREMISES","OFFCE AND PREMISES",
  "OFFCIES AND PREMISES","OFFFICE & PREMISES","OFFFICE AND PREMISES","OFFICE AND PREMISE","OFFICES, CAFE AND PREMISES","OFFICES, STORE AND PREMISES","PLAY AREA",
  "SEA CADET CENTRE","BUILDERS YARD AND PREMISES","BUILDING MERCHANTS AND PREMISES","BUILDERS MERCHANT","BUILDERS MERCHANTGS & PREMISES","BUILDERS MERCHANTS & LAND",
  "BUILDERS MERCHATS & PREMESIS","BUILDERS YARD & PREMISES","BUILDERS' MERCHANT & PREMISES.","BUILDERS' MERCHANT AND PREMISES","ADVERTISING AREA",
  "ADVERTISING RIGHT AND PREMISES","LAND USED FOR STORAGE AND PREMISES","SCHOOL AND PREMISES")

rm2 <- "\\bPETROL\\b|\\bUSED AS\\b|\\bUSE AS\\b\\bHOLIDAY\\b|\\bSCHOOL\\b|\\bFACTORY\\b|\\bGOLF\\b|\\bUNDER RECONSTRUCTION\\b|\\bADVERTISING\\b|\\bLAND USED FOR\\b|\\bLORRY\\b|\\bBOATYARD\\b|\\bHOSPICE\\b|\\bEQUESTRIAN\\b|\\bMASONIC\\b|\\bLIFEBOAT\\b|\\bKENNELS\\b|\\bCATTERY\\b|\\bRECORDING STUDIO\\b|\\bPAVILLION\\b|\\bABATTOIR\\b|\\bABBATOIR\\b|\\bANIMAL SANCTUARY\\b|\\bANIMAL SHELTER\\b|\\bBANQUETING HALL\\b|\\bBANQUETING SUITE\\b|\\bBOAT\\b|\\bBUSINESS UNIT\\b|\\bBED & BREAKFAST\\b|\\bGUESTHOUSE\\b|\\bGUEST HOUSE\\b|\\bCARAVAN\\b|\\bCARDIAC\\b|\\bCHAPEL\\b|\\bCHURCH\\b|\\bFIRE DAMAGE\\b|\\bFIRE DAMAGED\\b|\\bFIRST AID\\b|\\bGOLF COURSE\\b|\\bGUEST HOUSE\\b|\\bHOMELESS\\b|\\bMEDIA CENTRE\\b|\\bREHABILITATION\\b|\\bRENAL\\b|\\bRETIREMENT\\b|\\bRIDING SCHOOL\\b|\\bSAWMILL\\b|\\bSCAT\\b|\\bSLAUGHTER\\b|\\bSLAUGHTERHOUSE\\b|\\bSTUDENT\\b|\\bUNIVERSITY\\b|\\bVISITOR CENTRE \\b|\\bFOOTBALL GROUND\\b|\\bFORMER\\b|\\bHOLIDAY\\b|\\bLAND COVERED BY WATER\\b|\\bLECTURE\\b|\\bMOTOR AUCTION\\b|\\bMOTORWAY\\b|\\bNURSERY\\b|\\bPLAYING FIELD\\b|\\bPROMOTIONAL\\b|\\bCONVERTING\\b|\\bRAILWAY\\b|\\bSEATING AREA\\b|\\bSHOWCASE\\b"

retail_descriptions_ex <- retail_VOA %>%
  select(description) %>%
  filter( (!description %in% rm) ) %>%
  filter( (!str_detect(description, rm2)) )

# Create expanded retail list
retail_descriptions_ex  <- unique(as.character(retail_descriptions_ex$description))

retail_VOA <- VOA_base %>%
  filter( ((description %in% retail_descriptions_ex) & (IDval_ID %in% core_retail_IDs)) | 
  (description %in% pub_bar_descriptions) | (description %in% leisure_descriptions))
#------------------------------------------------------------------------------------


# POSTCODE GEOCODING THE RETAIL UNITS
#
#   - Requires the NSPL available from: https://geoportal.statistics.gov.uk/datasets/4f71f3e9806d4ff895996f832eb7aacf
#   - Requires Lookup tables
#
# Geocoding at the postcode level can easily be done with the NSPL, however this gives 
#   a fairly broad location and a finer resolution is desired The first step we take 
#   though is to merge on the correct associated postcode information from the 
#   NSPL - latitude, longitude, geographic IDs.
#------------------------------------------------------------------------------------
nspl.lookup <- fread(paste0(wd, "/Data/Boundaries and Lookups/NSPL_FEB_2020_UK.csv"), header=T, na.strings=c("", "NA", "-", "- -", ".", ",")) %>%
  as_tibble() %>%
  select(pcd, oseast1m, osnrth1m, lat, long, usertype, oa11, msoa11, laua) %>%
  mutate(pcd_merge=gsub("[[:space:]]", "", pcd))
nspl.lookup$pcd <- NULL

nspl.lookup <- nspl.lookup[nspl.lookup$lat > 49.82380908513249 & nspl.lookup$lat < 59.478568831926395 & nspl.lookup$long > -10.8544921875 & nspl.lookup$long < 2.021484375,]

retail_VOA$pcd_merge <- gsub("[[:space:]]", "", retail_VOA$pcd)

retail_VOA %<>%
  left_join(nspl.lookup, by="pcd_merge") 
names(retail_VOA)[names(retail_VOA)=="lat"] <- "lat_PC"
names(retail_VOA)[names(retail_VOA)=="long"] <- "lon_PC"

retail_VOA$pcd_Area <- as.vector(do.call(rbind, lapply(strsplit(do.call(rbind, lapply(strsplit(retail_VOA$pcd, " "), function(x) x[1])), split = "(?<=[a-zA-Z])\\s*(?=[0-9])", perl = TRUE), function(y) y[1])))
retail_VOA$pcd_District <- as.vector(do.call(rbind, lapply(strsplit(do.call(rbind, lapply(strsplit(retail_VOA$pcd, " "), function(x) x[1])), split = "(?<=[a-zA-Z])\\s*(?=[0-9])", perl = TRUE), function(y) y[2])))
retail_VOA$pcd_Sector <- substr(lapply(strsplit(retail_VOA$pcd, " "), function(x) x[2]), 1, 1)
retail_VOA$pcd_Unit <- substr(lapply(strsplit(retail_VOA$pcd, " "), function(x) x[2]), 2, 3)


lau.lookup <- fread(paste0(wd, "/Data/Boundaries and Lookups/Local_Authority_District_to_County_(April_2020)_Lookup_in_England.csv"), header=T, na.strings=c("", "NA", "-", "- -", ".", ","))
retail_VOA <- merge(retail_VOA, lau.lookup[,c("LAD20CD", "LAD20NM", "CTY20NM")], all.x=T, sort=F, by.x="laua", by.y="LAD20CD")
names(retail_VOA)[names(retail_VOA)=="LAD20NM"] <- c("la.name")
names(retail_VOA)[names(retail_VOA)=="CTY20NM"] <- c("cnt.name")

msoa.lookup <- fread(paste0(wd, "/Data/Boundaries and Lookups/Middle_Layer_Super_Output_Area__2011__to_Major_Towns_and_Cities__December_2015__Lookup_in_England_and_Wales.csv"), header=T, na.strings=c("", "NA", "-", "- -", ".", ","))
retail_VOA <- merge(retail_VOA, msoa.lookup[,c("MSOA11CD", "MSOA11NM", "TCITY15NM")], all.x=T, sort=F, by.x="msoa11", by.y="MSOA11CD")
names(retail_VOA)[names(retail_VOA)=="MSOA11NM"] <- c("msoa.name")
names(retail_VOA)[names(retail_VOA)=="TCITY15NM"] <- c("twn.name")

bua.lookup <- fread(paste0(wd, "/Data/Boundaries and Lookups/Middle_Layer_Super_Output_Area__2011__to_Built-up_Area_Sub_Division_to_Built-up_Area_to_Local_Authority_District_to_Region__December_2011__Lookup_in_England_and_Wales.csv"), header=T, na.strings=c("", "NA", "-", "- -", ".", ","))
retail_VOA <- merge(retail_VOA, bua.lookup[,c("MSOA11CD", "BUASD11NM", "BUA11NM", "RGN11NM")], all.x=T, sort=F, by.x="msoa11", by.y="MSOA11CD")
names(retail_VOA)[names(retail_VOA)=="BUASD11NM"] <- c("buaSD.name")
names(retail_VOA)[names(retail_VOA)=="BUA11NM"] <- c("bua.name")
names(retail_VOA)[names(retail_VOA)=="RGN11NM"] <- c("rgn.name")

retail_VOA$laua <- NULL
retail_VOA$msoa11 <- NULL
rm(lau.lookup, msoa.lookup, bua.lookup); gc(verbose=F)
#------------------------------------------------------------------------------------


# 'HERE' GEOCODING THE RETAIL UNITS
#
#   - Uses the HERE Geocoder available from: https://developer.here.com/
#
# A higher resolution geocode required to differentiate between locations of heradetiments 
#   within a postcode. The results from this geocoding are saved into  retail_pcd.rds
# 
# Running this section will require a set of API keys to use, depending on the size of the
#   data to geocode. These are used in the set_key() function.
#------------------------------------------------------------------------------------
# 
# install.packages("hereR")
# require(hereR)
# 
# # Creates the address table
# adds_all<- retail_VOA %>%
#   mutate(Full_Address = paste0(address,",",postcode,",UK")) %>%
#   select(IDval_ID,Full_Address) %>%
#   as.data.table()
# 
# # Geocode 1 - 250k
# set_key("------") #
# adds_1_250GC <- list()
# adds_1_250 <- adds_all[1:250000,]
# adds_1_250_split <- split(adds_1_250, rep(1:25, length.out = nrow(adds_1_250), each = nrow(adds_1_250)/25)) # Split in lists
# 
# for (i in 22:length(adds_1_250_split)){
#   gc <- geocode(adds_1_250_split[[i]][,Full_Address]) # Get the addresses and geocode
#   base_gc <- as_tibble(adds_1_250_split[[i]][,"IDval_ID"][,id := .I]) # get the valuation ID
#   adds_1_250GC[[i]] <-  base_gc %>%
#     left_join(gc,by = "id") # create output table
#   rm(list = c("gc","base_gc"))
#   print(i)
# }
# saveRDS(adds_1_250GC, file = "adds_1_250.rds")
# unset_key()
# 
# # Geocode 250k - 500k
# set_key("------") 
# adds_250_500GC <- list()
# adds_250_500 <- adds_all[250001:500000]
# adds_250_500_split <- split(adds_250_500, rep(1:25, length.out = nrow(adds_250_500), each = nrow(adds_250_500)/25)) # Split in lists
# 
# for (i in 1:length(adds_250_500_split)){
#   gc <- geocode(adds_250_500_split[[i]][,Full_Address]) # Get the addresses and geocode
#   base_gc <- as_tibble(adds_250_500_split[[i]][,"IDval_ID"][,id := .I]) # get the valuation ID
#   adds_250_500GC[[i]] <-  base_gc %>%
#     left_join(gc,by = "id") # create output table
#   rm(list = c("gc","base_gc"))
#   print(i)
# }
# saveRDS(adds_250_500GC, file = "adds_250_500.rds")
# unset_key()
# 
# # Geocode 500k - End
# set_key("------")
# adds_500_EndGC <- list()
# adds_500_End <- adds_all[500001:nrow(adds_all)] 
# adds_500_End_split <- split(adds_500_End, rep(1:25, length.out = nrow(adds_500_End), each = nrow(adds_500_End)/25)) # Split in lists
# 
# for (i in 1:length(adds_500_End_split)){
#   gc <- geocode(adds_500_End_split[[i]][,Full_Address]) # Get the addresses and geocode
#   base_gc <- as_tibble(adds_500_End_split[[i]][,"IDval_ID"][,id := .I]) # get the valuation ID
#   adds_500_EndGC[[i]] <-  base_gc %>%
#     left_join(gc,by = "id") # create output table
#   rm(list = c("gc","base_gc"))
#   print(i)
# }
# saveRDS(adds_500_EndGC, file = "adds_500_End.rds")
# unset_key()
# 
# adds_1_250GC_DT <- rbindlist(adds_1_250GC, use.names=TRUE)
# adds_250_500GC_DT <- rbindlist(adds_250_500GC, use.names=TRUE)
# adds_500_EndGC_DT <- rbindlist(adds_500_EndGC, use.names=TRUE)
# geocoded_raw <- rbindlist(list(adds_1_250GC_DT, adds_250_500GC_DT, adds_500_EndGC_DT))
# 
# # Append 4326 geometry into the table; remove old geometry (the CRS is invalid, so needs to be re-created)
# geocoded_raw %<>%
#   as_tibble() %>%
#   mutate(x_add = unlist(map(.$geometry,1)),y_add = unlist(map(.$geometry,2)))  
# 
# # Create SF - this will drop some results where there is no address geocode
# geocoded_raw <- st_as_sf(x = geocoded_raw[!is.na(geocoded_raw$x_add),], 
#   coords = c("x_add", "y_add"), crs = 4326)
# 
# # Set and store CRS OSGB + remove any non England and Wales address geocodes
# geocoded_raw %<>%
#   st_transform(27700) %>%
#   mutate(x_add = unlist(map(.$geometry,1)),y_add = unlist(map(.$geometry,2))) %>%
#   filter(state %in% c("England","Wales"))
# 
# output <- geocoded_raw %>%
#   select(IDval_ID,x_add,y_add)
# 
# # Append the postcode co-ordinates
# retail_pcd <- retail_VOA %>%
#   select(IDval_ID,oa11,oseast1m, osnrth1m)%>%
#   as_tibble() 
# 
# retail_pcd %<>%
#   left_join(output, by= "IDval_ID")
# 
# # Where there are no address geocode matches, we use the postcode geocode
# retail_pcd %<>%
#   mutate(Final_E = ifelse(is.na(x_add),oseast1m,x_add)) %>%
#   mutate(Final_N = ifelse(is.na(y_add),osnrth1m,y_add)) %>%
#   mutate(Geo_Ref_Type = ifelse(is.na(y_add),"PCD","ADD"))
# 
# # Create output geocode lookup
# retail_pcd %<>%
#   select(IDval_ID,Final_E,Final_N,Geo_Ref_Type)
# 
# saveRDS(retail_pcd, file = paste0(wd, "/Data/VOA/retail_pcd.rds"))

retail_pcd <- readRDS(paste0(wd, "/Data/VOA/retail_pcd.rds"))
retail_pcd <- st_as_sf(x = retail_pcd[!is.na(retail_pcd$Final_E),], 
  coords = c("Final_E", "Final_N"), crs = 27700)
retail_pcd <- st_transform(retail_pcd, crs = 4326)
retail_pcd <- data.table(cbind(retail_pcd, st_coordinates(retail_pcd)))[,-c("geometry")]
names(retail_pcd)[names(retail_pcd)=="Y"] <- "lat_Geo"
names(retail_pcd)[names(retail_pcd)=="X"] <- "lon_Geo"

retail_VOA <- merge(retail_pcd, retail_VOA, by="IDval_ID", all.x=T, sort=F)
rm(retail_pcd); gc(verbose = F)
#------------------------------------------------------------------------------------


## IDENTIFYING GEOCODED MISMATCHES
#
# We compare geocoded locations with postal locations for validity to identify those which
#   are located in different postal sectors
#------------------------------------------------------------------------------------
geo.mismatch <- data.table(retail_VOA)[,c("IDval_ID", "description", "address", "firm_name", "number_name", "street", "town_city", "pcd", "pcd_Area", "pcd_District", "pcd_Sector", "pcd_Unit", "lat_Geo", "lon_Geo", "lat_PC", "lon_PC")]

post.sct <- st_read(paste0(wd, "/Data/Boundaries and Lookups/PostalSector.shp"))
post.sct <- st_transform(post.sct, crs=4326)

t.geo <- st_as_sf(geo.mismatch, coords = c("lon_Geo", "lat_Geo"), crs = 4326)
t.geo$lon_PC <- NULL
t.geo$lat_PC <- NULL
t1 <- as.data.frame(st_join(t.geo, post.sct, join = st_intersects))[c("PostArea", "DistNum", "SecNum")]
t.geo$pcd_Area_Geo <- t1$PostArea
t.geo$pcd_District_Geo <- t1$DistNum
t.geo$pcd_Sector_Geo <- t1$SecNum

t.pc <- st_as_sf(geo.mismatch[!is.na(geo.mismatch$lon_PC) | !is.na(geo.mismatch$lat_PC),], coords = c("lon_PC", "lat_PC"), crs = 4326)
t.pc$lon_Geo <- NULL
t.pc$lat_Geo <- NULL
t1 <- as.data.frame(st_join(t.pc, post.sct, join = st_intersects))[c("PostArea", "DistNum", "SecNum")]
t.pc$pcd_Area_PC <- t1$PostArea
t.pc$pcd_District_PC <- t1$DistNum
t.pc$pcd_Sector_PC <- t1$SecNum

geo.mismatch <- merge(geo.mismatch, data.table(t.geo)[,c("IDval_ID", "pcd_Area_Geo", "pcd_District_Geo", "pcd_Sector_Geo")], all.x=T, sort=F, by="IDval_ID")
geo.mismatch <- merge(geo.mismatch, data.table(t.pc)[,c("IDval_ID", "pcd_Area_PC", "pcd_District_PC", "pcd_Sector_PC")], all.x=T, sort=F, by="IDval_ID")

geo.mismatch$pcd_Sector_PC <- paste0(as.character(geo.mismatch$pcd_Area_PC), as.character(geo.mismatch$pcd_District_PC), " ", as.character(geo.mismatch$pcd_Sector_PC))
geo.mismatch$pcd_Sector_Geo <- paste0(as.character(geo.mismatch$pcd_Area_Geo), as.character(geo.mismatch$pcd_District_Geo), " ", as.character(geo.mismatch$pcd_Sector_Geo))
geo.mismatch$pcd_District_PC <- paste0(as.character(geo.mismatch$pcd_Area_PC), as.character(geo.mismatch$pcd_District_PC))
geo.mismatch$pcd_District_Geo <- paste0(as.character(geo.mismatch$pcd_Area_Geo), as.character(geo.mismatch$pcd_District_Geo))
geo.mismatch$pcd_Area_PC <- as.character(geo.mismatch$pcd_Area_PC)
geo.mismatch$pcd_Area_Geo <- as.character(geo.mismatch$pcd_Area_Geo)

geo.mismatch$pcd_Area_match <- ifelse(as.character(geo.mismatch$pcd_Area_Geo)!=as.character(geo.mismatch$pcd_Area_PC), 0, 1)
geo.mismatch$pcd_Sector_match <- ifelse(as.character(geo.mismatch$pcd_Sector_Geo)!=as.character(geo.mismatch$pcd_Sector_PC), 0, 1)
geo.mismatch$pcd_District_match <- ifelse(as.character(geo.mismatch$pcd_District_Geo)!=as.character(geo.mismatch$pcd_District_PC), 0, 1)
#------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------
dim(geo.mismatch[geo.mismatch$lau_match==0,])
dim(geo.mismatch[geo.mismatch$pcd_Area_match==0,])
dim(geo.mismatch[geo.mismatch$pcd_District_match==0,])
dim(geo.mismatch[geo.mismatch$pcd_Sector_match==0,])
dim(geo.mismatch[geo.mismatch$pcd_Area_match==0 | geo.mismatch$pcd_District_match==0 | geo.mismatch$pcd_Sector_match==0,])

geo.mismatch <- geo.mismatch[geo.mismatch$pcd_Area_match==0 | geo.mismatch$pcd_District_match==0 | geo.mismatch$pcd_Sector_match==0,]
geo.mismatch <- geo.mismatch[!is.na(geo.mismatch$lat_PC),]

geo.mismatch.Geo <- st_as_sf(geo.mismatch[,c("lon_Geo", "lat_Geo"),], coords = c("lon_Geo", "lat_Geo"), crs = 4326)
geo.mismatch.PC <- st_as_sf(geo.mismatch[,c("lon_PC", "lat_PC"),], coords = c("lon_PC", "lat_PC"), crs = 4326)

t <- st_distance(geo.mismatch.Geo, geo.mismatch.PC, by_element = TRUE)

geo.mismatch$dist.descrep <- as.numeric(t)
rm(t); gc(verbose = F)

round(quantile(geo.mismatch$dist.descrep, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.975, 0.99, 0.995, 1))/1000, digits=3)
#------------------------------------------------------------------------------------


## CORRECTING GEOCODED MISMATCHES
#
# Taking a 5 km threshold, 575 properties are at a significant distance between geocodings. 
#   We want to correct the geocoding for those properties which are over 5 km away between 
#   their respective HERE and NSPL points, and also which do not have a match on the postcode area. 
#   We want to only correct those which have a different postal area for now.
#------------------------------------------------------------------------------------
PC.replace <- geo.mismatch[geo.mismatch$dist.descrep>5000 & geo.mismatch$pcd_Area_match==0 & geo.mismatch$pcd_Area_PC==geo.mismatch$pcd_Area,]$IDval_ID

geo.mismatch2 <- geo.mismatch[!(geo.mismatch$IDval_ID %in% PC.replace),]

summary(geo.mismatch2[,c("pcd_Area_match", "pcd_District_match", "pcd_Sector_match", "dist.descrep")])
round(quantile(geo.mismatch2$dist.descrep, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.975, 0.99, 0.995, 1))/1000, digits=3)

PC.replace <- unique(c(PC.replace, geo.mismatch2[geo.mismatch2$dist.descrep>10000 & geo.mismatch2$pcd_District_match==0,]$IDval_ID))
rm(geo.mismatch2); gc(verbose = F)

geo.mismatch <- retail_VOA[retail_VOA$IDval_ID %in% PC.replace,]
geo.mismatch$latitude <- geo.mismatch$lat_PC
geo.mismatch$longitude <- geo.mismatch$lon_PC
geo.mismatch$Geo_Ref_Type <- "PCD"
geo.mismatch$lat_Geo <- NULL
geo.mismatch$lat_PC <- NULL
geo.mismatch$lon_Geo <- NULL
geo.mismatch$lon_PC <- NULL

retail_VOA <- retail_VOA[!(retail_VOA$IDval_ID %in% PC.replace),]
retail_VOA$latitude <- retail_VOA$lat_Geo
retail_VOA$longitude <- retail_VOA$lon_Geo
retail_VOA$lat_Geo <- NULL
retail_VOA$lat_PC <- NULL
retail_VOA$lon_Geo <- NULL
retail_VOA$lon_PC <- NULL

retail_VOA <- rbind(retail_VOA, geo.mismatch)

for (j in which(sapply(retail_VOA, is.character))) set(retail_VOA, i = grep("^$|^ $", retail_VOA[[j]]), j = j, value = NA_character_)
retail_VOA <- retail_VOA[, names(retail_VOA)[which(sapply(retail_VOA, class)=="character")] := lapply(.SD, as.factor), .SDcols = which(sapply(retail_VOA, class)=="character")]
#------------------------------------------------------------------------------------


## EXPORT AND SAVE VOA RETAIL POINTS
#------------------------------------------------------------------------------------
retail_VOA$lat <- retail_VOA$latitude
retail_VOA$long <- retail_VOA$longitude

names(retail_VOA)[names(retail_VOA)=="pcd"] <- "postcode"

retail_VOA$pcd_merge <- NULL
retail_VOA$oseast1m <- NULL
retail_VOA$osnrth1m <- NULL
retail_VOA$firm_name <- NULL
retail_VOA$number_name <- NULL
retail_VOA$twn.name <- NULL
retail_VOA$SCAT <- NULL
retail_VOA$list_alteration <- NULL
retail_VOA$appeal_code <- NULL

retail_VOA <- st_as_sf(x = retail_VOA, coords = c("long", "lat"), crs = 4326)

st_write(retail_VOA, dsn = paste0(wd, "/Data/VOA/Retail_Points.gpkg"), delete_layer = TRUE, layer="Retail_Points.gpkg", driver = "gpkg")
# fwrite(data.table(retail_VOA)[,-c("geometry")], file=paste0(wd, "/Data/VOA/Retail_Points.csv"))
# saveRDS(retail_VOA, paste0(wd, "/Data/VOA/Retail_Points.rds"))
#------------------------------------------------------------------------------------