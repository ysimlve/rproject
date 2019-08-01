######################################################Remark###############################################################################
#Functionality: Use MarketLink source data as input, to go through Standardization, Rejection and GeoAPI process.
#Input: MarketLink source data file, contains columns:
#       CLIENT_PROPERTY_CODE: Unique Property ID
#       PROPERTY_NAME: Name of a Property
#       ADDRESS_1: Address of a Property
#       DISTRICT_NAME
#       CITY
#       COUNTRY
#       PROPERTY_NAME_LC
#       ADDRESS_LC
#       DISTRICT_NAME_LC
#       CITY_LC
#       COUNTRY_LC
#       GEO_LAT
#       GEO_LONG
#Output: A data set of rejected records
#        A data set of cleaned records
########################################################Start#############################################################################
setwd("C:/YuanLe/R/RWkDir/01. MDM_RegExp")
#ls()
#rm(list=ls())
#install.packages("bitops")

options(stringsAsFactors = FALSE)
options(java.parameters = "-Xmx4g")
library(readxl)            ##import data from Excel
library(dplyr)             ##data manipulation, e.g. select, filter
library(stringdist)        ##string distance calculation and approximate string matching
library(RCurl)             ##use for processing Google Geocoding API and JSON file
library(RJSONIO)           ##use for processing Google Geocoding API and JSON file
library(xlsx)

##############################Step 1:Import Source Data & Reference Data##############################
#####Function - sourceDataImport
#######Usage: import excel source into a data frame
#######Input Parameter: 
##########Excel Source Name
##########Excel Spreedsheet name
#######Output: A data frame
sourceDataImport <- function(sourceXLSX,spreedsheet){
  #DS_Source1 <- read_excel("Andy's Data.xlsx", sheet = "MarketLinkSourceData")
  DS_Source <- read.xlsx(sourceXLSX,sheetName = spreedsheet,header=TRUE,encoding = "UTF-8")
  return(DS_Source)
}


DS_Source <- sourceDataImport('Andy\'s Data.xlsx','ML Source Data')
DS_CityList <- sourceDataImport('Andy\'s Data.xlsx','CityList')
DS_DistrictList <- sourceDataImport('Andy\'s Data.xlsx','DistrictList')

#View(DS_Source)
#View(DS_CityList)
#View(DS_DistrictList)


##############################Step 2:Format Standardization Process##############################
#######common definition
tone_sign <- c("ā","á","ă","à","ō","ó","ŏ","ò","ē","é","ĕ","è","ī","í","ĭ","ì","ū","ú","ŭ","ù","ǖ","ǘ","ǚ","ǜ")
tone_sign_fix <- c("a","a","a","a","o","o","o","o","e","e","e","e","i","i","i","i","u","u","u","u","u","u","u","u")

address_type_abbr <- c("[R][Dd]", "S[Tt][Rr]?",  "A[Vv][Ee]", "D[Rr]", "L[Nn]", "N",     "S",     "E",    "W",    "E[Xx][Pp][Yy]", "H[Ww][Yy]", "P[Kk][Ww][Yy]", "C[Ii][Rr]", "W[Yy]", "B[Ll][Vv][Dd]", "P[Ll][Zz]", "C[Tt][Rr]", "C[Tt]")
address_type_full <- c("Road",    "Street", "Avenue",    "Drive", "Lane",  "North", "South", "East", "West", "Expressway",    "Highway",   "Parkway",       "Circle",    "Way",   "Boulevard",     "Plaza",     "Center",    "Court")


###########Format Standardization Rule - Property Name
###########Rule 2: The first charactor of a seperate work should be in UPPER case. e.g. 'Lujiazui information tower' -> Lujiazui Information Tower'
###########Rule 4: abbreviation translation. e.g. Rd -> Road
###########Rule 5: tone sign process, e.g. 8 Xīn Yuán Road -> 8 Xin Yuan Road
###########Rule 3: tow or more space should be replaced by one
###########Rule 1: ',' should be replaced by ' '. e.g Lujiazui,Information,Tower -> Lujiazui Information Tower
SdProcess_Property <-function(vec_Property, vec_Property_LC){
  vec_Property <- gsub("^\\s{1,}", "", vec_Property)
  vec_Property <- gsub("\\s{1,}$", "", vec_Property)
  #Rule 1
  pattern_comma <- ','
  vec_Property <- gsub(pattern_comma, ' ' , vec_Property)
  
  #Rule 2
  for(lc in letters){
    pattern_UC <- paste0('\\s(',lc,')')
    vec_Property <- gsub(pattern_UC, paste0(' ', toupper(lc)) , vec_Property)
  }
  
  #Rule3
  pattern_tp <- '\\s{2,}'
  vec_Property <- gsub(pattern_tp, ' ' , vec_Property)
  
  #Rule4
  Abbr <- c('BLDG', 'TWR', 'HQ', 'C[Tt][Rr]', 'CT', 'APT')
  Abbr_Trans <- c('Building', 'Tower', 'Headquarters', 'Center', 'Court', 'Apartment')
  for(i in 1:length(Abbr)){
    pattern_Abbr <- paste0('(\\s|^)(',Abbr[i],')(\\s|$)')
    vec_Property <- gsub(pattern_Abbr, paste0(' ', Abbr_Trans[i], ' ') , vec_Property)
  }
  
  #Rule5
  for(i in 1:length(tone_sign_fix)){
    pattern_ts <- tone_sign[i]
    vec_Property <- gsub(pattern_ts,tone_sign_fix[i],vec_Property)
  }
  
  vec_Property <- gsub("^\\s{1,}", "", vec_Property)
  vec_Property <- gsub("\\s{1,}$", "", vec_Property)
  vec_Property_LC <- gsub("^\\s{1,}", "", vec_Property_LC)
  vec_Property_LC <- gsub("\\s{1,}$", "", vec_Property_LC)
  
  return(data.frame(Property_Name = vec_Property, Property_Name_LC = vec_Property_LC, stringsAsFactors = FALSE))
  
}




###########Format Standardization Rule - Address
###########Rule 1: ',' need to be changed to ' ', e.g. '2 Tianhe, Road'
###########Rule 2: two or more ' ' need to be replaced by one ' '
###########Rule 3: District, City & Country shouldn't apprea in address
###########Rule 4: NO. at front of Address should be removed
###########Rule 5: 4-1 should be converted to 1-4, 1-1 should be converted to 1
###########Rule 6: The first charactor of a seperate work should be in UPPER case. e.g. 'Lujiazui information tower' -> Lujiazui Information Tower'
###########Rule 7: tone sign process, e.g. 8 Xīn Yuán Road -> 8 Xin Yuan Road
###########Rule 8: abbreviation replacement
###########Rule 9: For Chinese Address, add “号” at the end of number. E.g. “解放南路123” -> “解放南路123号”
SdProcess_Address <- function(vec_Address, vec_Address_LC){
  vec_Address <- gsub("^\\s{1,}", "", vec_Address)
  vec_Address <- gsub("\\s{1,}$", "", vec_Address)
  
  #Rule 1
  pattern_comma <- ','
  vec_Address <- gsub(pattern_comma, ' ' , vec_Address)
  
  #Rule 2
  pattern_tp <- '\\s{2,}'
  vec_Address <- gsub(pattern_tp, ' ' , vec_Address)
  
  #Rule 3 - Country
  pattern_cntr <- '\\s[Cc](hina)$'
  vec_Address <- gsub(pattern_cntr, '' , vec_Address)
  
  #Rule 3 - City
  cityList <- unique(DS_CityList$City)
  for(ct in cityList){
    pattern_city <- paste0('\\s(', ct, ')(\\s[Cc][Ii][Tt][Yy])?$')
    vec_Address <- gsub(pattern_city, '' , vec_Address)
  }
  
  #Rule 3 - District
  districtList <- unique(DS_DistrictList$District)
  for(ds in districtList){
    pattern_dist <- paste0('\\s(', ds, ')\\s?([Dd]istrict|DISTRICT)?$')
    vec_Address <- gsub(pattern_dist, '' , vec_Address)
  }
  
  #Rule 4
  pattern_NO <- '^N[Oo][^A-Za-z0-9]'
  vec_Address <- gsub(pattern_NO, "" ,  vec_Address)
  vec_Address <- gsub("^\\s{1,}","",vec_Address)
  
  #Rule 5
  pattern_NoRng <- '(^|\\s)[0-9]{1,}(-)[0-9]{1,}\\s'
  
  vec_Addr <- vec_Address
  match_start <- regexpr(pattern_NoRng, vec_Addr)
  match_len <- attr(match_start, "match.length")
  vec_Addr_rng <- gsub(' ', '', substr(vec_Addr, match_start, match_len + match_start - 1))
  vec_Addr_rng_1 <- as.numeric(substr(vec_Addr_rng, 1 ,regexpr('-', vec_Addr_rng) - 1))
  vec_Addr_rng_2 <- as.numeric(substr(vec_Addr_rng, regexpr('-', vec_Addr_rng) + 1, 10000L))
  vec_Addr_rng_dif <- vec_Addr_rng_2 - vec_Addr_rng_1
  
  vec_Addr_rng_fixed <- vec_Addr_rng
  vec_Addr_fixed <- vec_Addr
  for(i in 1:length(vec_Addr)){
    if(is.na(vec_Addr_rng_dif[i])){
      vec_Addr_rng_fixed[i] <- ""
    } else if(vec_Addr_rng_1[i] == 0 | vec_Addr_rng_2[i] == 0){
      vec_Addr_rng_fixed[i] <- vec_Addr_rng[i]
    }else if(vec_Addr_rng_dif[i] == 0 & vec_Addr_rng_1[i] != 0 & vec_Addr_rng_2[i] != 0){
      vec_Addr_rng_fixed[i] <- as.character(vec_Addr_rng_1[i])  
    }else if(vec_Addr_rng_dif[i] < 0 & vec_Addr_rng_1[i] != 0 & vec_Addr_rng_2[i] != 0){
      vec_Addr_rng_fixed[i] <- paste0(as.character(vec_Addr_rng_2[i]), "-", as.character(vec_Addr_rng_1[i]))
    }else {
      vec_Addr_rng_fixed[i] <- vec_Addr_rng[i]
    }
    
    if(vec_Addr_rng_fixed[i] != vec_Addr_rng[i]){
      vec_Addr_fixed[i] <- gsub(vec_Addr_rng[i], vec_Addr_rng_fixed[i], vec_Addr[i])
    }
  }
  
  vec_Address <- vec_Addr_fixed
  
  #Rule 6
  for(lc in letters){
    pattern_UC <- paste0('\\s(',lc,')')
    vec_Address <- gsub(pattern_UC, paste0(' ', toupper(lc)) , vec_Address)
  }
  vec_Address <- gsub("\\s(Of)\\s"," of ", vec_Address)
  
  
  #Rule 7 
  for(i in 1:length(tone_sign_fix)){
    pattern_ts <- tone_sign[i]
    vec_Address <- gsub(pattern_ts,tone_sign_fix[i],vec_Address)
  }
  
  #Rule 8
  for(i in 1:length(address_type_abbr)){
    pattern_abbr <- paste0("(^|\\s)", address_type_abbr[i], "(\\s|$|\\.?)")
    vec_Address <- gsub(pattern_abbr,paste0(" ", address_type_full[i], " "),vec_Address)
    vec_Address <- gsub("^\\s", "", vec_Address)
    vec_Address <- gsub("\\s$", "", vec_Address)
  }
  
  vec_Address <- gsub("^\\s{1,}", "", vec_Address)
  vec_Address <- gsub("\\s{1,}$", "", vec_Address)
  vec_Address_LC <- gsub("^\\s{1,}", "", vec_Address_LC)
  vec_Address_LC <- gsub("\\s{1,}$", "", vec_Address_LC)
  
  #Rule 9 
  pattern_hao <- '[0-9]$'   ####1446 vec_Address_LC[1446:1447] vec_hao_number[1446:1447] NA > 0
  vec_hao_number <- regexpr(pattern_hao,vec_Address_LC)
  for(i in 1:length(vec_Address_LC)){
    if(vec_hao_number[i] < 0 | is.na(vec_hao_number[i])){
      vec_Address_LC[i] <- vec_Address_LC[i] 
    }else{
      vec_Address_LC[i] <- paste0(vec_Address_LC[i], "号")
    }
  }
  
  return(data.frame(Address_1 = vec_Address, Address_LC = vec_Address_LC, stringsAsFactors = FALSE))

}

SdProcess_Address_LC <- function(v_address_lc){
  #Rule1: Remove all space(" ")
  v_address_lc <- gsub("\\s{1,}", "", v_address_lc)
  
  #Rule2: Add "号" at the end of the string
  pattern_hao <- '[0-9]$'  
  vec_hao_number <- regexpr(pattern_hao,v_address_lc)
  for(i in 1:length(v_address_lc)){
    if(vec_hao_number[i] < 0 | is.na(vec_hao_number[i])){
      v_address_lc[i] <- v_address_lc[i] 
    }else{
      v_address_lc[i] <- paste0(v_address_lc[i], "号")
    }
  }
  return(v_address_lc)
}


###########Format Standardization Rule - District
###########Rule 1: District should be removed
###########Rule 2: all space need to be removed
###########Rule 3: District Name conversion
###########Rule 4: tone sign process, e.g. 8 Xīn Yuán Road -> 8 Xin Yuan Road
###########Rule 5: approximate matching
###########Rule 6: update DISTRICT_LC according to DISTRICT
SdProcess_District <- function(vec_District, vec_District_LC) {
  vec_District <- gsub("^\\s{1,}", "", vec_District)
  vec_District <- gsub("\\s{1,}$", "", vec_District)
  
  #Rule 1
  pattern_disrm <- "\\s(DISTRICT|[Dd]istrict)"
  vec_District <- gsub(pattern_disrm, "", vec_District)
  
  #Rule 2
  pattern_space <- "\\s{1,}"
  vec_District <- gsub(pattern_space, "", vec_District)
  
  #Rule 3
  vec_dis_from <- c("Jinqiao","Luwan","Lujiazui","Nanhui","Pudongothers","Tangqiao","Zhabei","Zhangjiang","Zhuyuan")
  vec_dis_to <- c("Pudong","Huangpu","Pudong","Pudong","Pudong","Pudong","Jing'an","Pudong","Pudong")
  for(i in 1:length(vec_dis_from)){
    pattern_disfrom <- vec_dis_from[i]
    vec_District <- gsub(pattern_disfrom, vec_dis_to[i], vec_District, ignore.case = TRUE)
  }
  
  #Rule 4
  for(i in 1:length(tone_sign_fix)){
    pattern_ts <- tone_sign[i]
    vec_District <- gsub(pattern_ts,tone_sign_fix[i],vec_District)
  }
  
  
  #Rule 5
  index_district <- amatch(vec_District,DS_DistrictList$District,maxDist = 7, nomatch = 0)
  vec_District <- DS_DistrictList$District[index_district]
  
  #Rule 6
  ds_district_sc <- data.frame(District = vec_District,DISTRICT_LC = vec_District_LC, stringsAsFactors = FALSE)
  ds_district_sd <- DS_DistrictList[,c("District","District_CHN")]
  
  ind_mch <- match(ds_district_sc$District, ds_district_sd$District)
  ds_district_sc[ , "DISTRICT_LC"] <- ds_district_sd[ind_mch, "District_CHN"]
  
  return(data.frame(vec_District_Fixed = vec_District, vec_District_LC_Fixed = gsub("^\\s{1,}", "", gsub("\\s{1,}$", "", ds_district_sc$DISTRICT_LC)), stringsAsFactors = FALSE))
}






###########Format Standardization Rule - City
###########Rule 1: shi or city should be removed
###########Rule 2: all space need to be removed
###########Rule 3: approximate matching
###########Rule 4: update CITY_LC according to CITY

SdProcess_City <- function(vec_City,vec_City_LC){
  vec_City <- gsub("^\\s", "", vec_City)
  vec_City <- gsub("\\s$", "", vec_City)
  
  #Rule 1
  pattern_shi <- "\\s([Ss]hi|[Cc][Ii][Tt][Yy])$"
  vec_City <- gsub(pattern_shi, "", vec_City)
  
  #Rule 2
  pattern_space <- "\\s{1,}"
  vec_City[vec_City != "Hong Kong"] <- gsub(pattern_space, "", vec_City[vec_City != "Hong Kong"])
  
  #Rule 3
  index_city <- amatch(vec_City,DS_CityList$City,maxDist = 5, nomatch = 0)
  
  vec_City <- DS_CityList$City[index_city]
  
  #Rule 4
  ds_city_sc <- data.frame(City = vec_City,CITY_LC = vec_City_LC, stringsAsFactors = FALSE)
  ds_city_sd <- DS_CityList[,c("City","Chinese")]
  
  ind_mch <- match(ds_city_sc$City, ds_city_sd$City)
  ds_city_sc[ , "CITY_LC"] <- ds_city_sd[ind_mch, "Chinese"]
  ds_city_sc$CITY_LC <- gsub("市$","",ds_city_sc$CITY_LC)

  
  return(data.frame(vec_City_Fixed = vec_City, vec_City_LC_Fixed = ds_city_sc$CITY_LC, stringsAsFactors = FALSE))
  
}



##########Standardization Processing
SdProcess_All <- function(DS_Source){
  ds_Property_Fixed <- SdProcess_Property(DS_Source$PROPERTY_NAME, DS_Source$PROPERTY_NAME_LC)
  vec_Property_Fixed <- ds_Property_Fixed$Property_Name
  vec_Property_LC_Fixed <- ds_Property_Fixed$Property_Name_LC
  
  ds_Address_Fixed <- SdProcess_Address(DS_Source$ADDRESS_1,DS_Source$ADDRESS_LC)
  vec_Address_Fixed <- ds_Address_Fixed$Address_1
  vec_Address_LC_Fixed <- ds_Address_Fixed$Address_LC
  
  ds_District_Fixed <- SdProcess_District(DS_Source$DISTRICT_NAME,DS_Source$DISTRICT_NAME_LC)
  vec_District_Fixed <- ds_District_Fixed$vec_District_Fixed
  vec_District_LC_Fixed <- ds_District_Fixed$vec_District_LC_Fixed
  
  ds_City_Fixed <- SdProcess_City(DS_Source$CITY,DS_Source$CITY_LC)
  vec_City_Fixed <- ds_City_Fixed$vec_City_Fixed
  vec_City_LC_Fixed <- ds_City_Fixed$vec_City_LC_Fixed
  
  DS_Source_Fixed <- data.frame(CLIENT_PROPERTY_CODE = DS_Source$CLIENT_PROPERTY_CODE,
                                PROPERTY_NAME = vec_Property_Fixed,
                                ADDRESS_1 = vec_Address_Fixed,
                                DISTRICT_NAME = vec_District_Fixed,
                                CITY = vec_City_Fixed,
                                COUNTRY = rep("China",length(DS_Source$CLIENT_PROPERTY_CODE)),
                                PROPERTY_NAME_CN = vec_Property_LC_Fixed,
                                ADDRESS_LC = vec_Address_LC_Fixed,
                                DISTRICT_NAME_LC = vec_District_LC_Fixed,
                                CITY_LC = vec_City_LC_Fixed,
                                COUNTRY_LC = rep("中国",length(DS_Source$CLIENT_PROPERTY_CODE)),
                                GEO_LAT = DS_Source$GEO_LAT,
                                GEO_LONG = DS_Source$GEO_LONG,
                                stringsAsFactors = FALSE)
  
  return(DS_Source_Fixed)
}

DS_Source_Fixed <- SdProcess_All(DS_Source)
#xlsx::write.xlsx(DS_Source_Fixed,"out_put_sd_marketlink.xlsx",sheetName = "Standardized Set")


##############################Step 3:Run Rejection Process##############################
##########Rejection Rules for PROPERTY_NAME
##############1. Invalid Value: "", " "Unknow, Null, NA, TBD
##############2. Contains special character: Chinese, ~, ^ etc.
##########Rejection Rules for ADDRESS_1
##############1. Invalid Value: "", " "Unknow, Null, NA, TBD
##############2. Value without digit or street number is 0(0-)
##############3. Value without any one of the key word: Road, Avenue, Street, Lane
##########Rejection Rules for DISTRICT_NAME
##############1. Invalid Value: "", " "Unknow, Null, NA, TBD
##############2. value can't be found in standard list of district
##########Rejection Rules for CITY
##############1. Invalid Value: "", " "Unknow, Null, NA, TBD
##############2. value can't be found in standard list of CITY
##########Rejection Rules for PROPERTY_NAME_CN
##############1. Invalid Value: "", " ", Null, NA, TBD
##############2. length of value should greater than 2
##########Rejection Rules for ADDRESS_LC
##############1. Invalid Value: "", " "Unknow, Null, NA, TBD
##############2. Value without digit or street number is 0(0-) 
##############3. Value without any one of the key word: 路，道，街，巷，弄
##########Rejection Rules for DISTRICT_NAME_LC
##############1. Invalid Value: "", " "Unknow, Null, NA, TBD
##############2. value can't be found in standard list of district
##########Rejection Rules for CITY_LC
##############1. Invalid Value: "", " "Unknow, Null, NA, TBD
##############2. value can't be found in standard list of CITY
##########Rejection Rules for LAT/LONG
##############1. Invalid Value: "", " "Unknow, Null, NA, TBD

RejProcess_All <- function(DS_Source_Fixed) {
  DS_Reject <- DS_Source_Fixed
  DS_Reject$RejReason <- rep("", nrow(DS_Source_Fixed))
  
  #Property_Name Rule 1
  DS_Reject[DS_Reject$PROPERTY_NAME == "" | DS_Reject$PROPERTY_NAME == " " | DS_Reject$PROPERTY_NAME == "NULL" | DS_Reject$PROPERTY_NAME == "Null" | is.na(DS_Reject$PROPERTY_NAME), "RejReason"] <- "PROPERTY_NAME - value is NULL; "
  DS_Reject[DS_Reject$PROPERTY_NAME == "Unknow" | DS_Reject$PROPERTY_NAME == "NA" | DS_Reject$PROPERTY_NAME == "TBD" | DS_Reject$PROPERTY_NAME == "TBC", "RejReason"] <- paste0(DS_Reject[DS_Reject$PROPERTY_NAME == "Unknow" | DS_Reject$PROPERTY_NAME == "NA" | DS_Reject$PROPERTY_NAME == "TBD" | DS_Reject$PROPERTY_NAME == "TBC", "RejReason"], "PROPERTY_NAME - value is invalid; ")
  
  #Property_Name Rule 2
  vec_property <- DS_Reject$PROPERTY_NAME
  vec_property <- gsub("\\s{1,}", "", vec_property)
  pattern_splc <- "[^A-Za-z0-9'.&\\-]"
  DS_Reject[which(regexpr(pattern_splc, vec_property) > 0), "RejReason"] <- paste0(DS_Reject[which(regexpr(pattern_splc, vec_property) > 0), "RejReason"], "PROPERTY_NAME - value contains special charactors; ")
  
  #ADDRESS_1 Rule 1
  DS_Reject[DS_Reject$ADDRESS_1 == "" | DS_Reject$ADDRESS_1 == " " | DS_Reject$ADDRESS_1 == "NULL" | DS_Reject$ADDRESS_1 == "Null" | is.na(DS_Reject$ADDRESS_1), "RejReason"] <- paste0(DS_Reject[DS_Reject$ADDRESS_1 == "" | DS_Reject$ADDRESS_1 == " " | DS_Reject$ADDRESS_1 == "NULL" | DS_Reject$ADDRESS_1 == "Null" | is.na(DS_Reject$ADDRESS_1), "RejReason"], "ADDRESS_1 - value is NULL; ")
  DS_Reject[DS_Reject$ADDRESS_1 == "Unknow" | DS_Reject$ADDRESS_1 == "NA" | DS_Reject$ADDRESS_1 == "TBD" | DS_Reject$ADDRESS_1 == "TBC", "RejReason"] <- paste0(DS_Reject[DS_Reject$ADDRESS_1 == "Unknow" | DS_Reject$ADDRESS_1 == "NA" | DS_Reject$ADDRESS_1 == "TBD" | DS_Reject$ADDRESS_1 == "TBC", "RejReason"], "ADDRESS_1 - value is invalid; ")
  
  #ADDRESS_1 Rule 2
  DS_Reject[which(regexpr("[0-9]", DS_Reject$ADDRESS_1) == -1), "RejReason"] <- paste0(DS_Reject[which(regexpr("[0-9]", DS_Reject$ADDRESS_1) == -1), "RejReason"], "ADDRESS_1 - value doesn't contain digit; ")
  DS_Reject[which(regexpr("^0(\\s|-)", DS_Reject$ADDRESS_1) > 0), "RejReason"] <- paste0(DS_Reject[which(regexpr("^0\\s", DS_Reject$ADDRESS_1) > 0), "RejReason"], "ADDRESS_1 - the street number is 0; ")
  
  #ADDRESS_1 Rule 3
  DS_Reject[which(regexpr("(Road|Avenue|Street|Lane)", DS_Reject$ADDRESS_1) == -1), "RejReason"] <- paste0(DS_Reject[which(regexpr("(Road|Avenue|Street|Lane)", DS_Reject$ADDRESS_1) == -1), "RejReason"], "ADDRESS_1 - value doen't contain any of the key word Road, Avenue, Street or Lane; ")
  
  #DISTRICT_NAME Rule 1
  DS_Reject[DS_Reject$DISTRICT_NAME == "" | DS_Reject$DISTRICT_NAME == " " | DS_Reject$DISTRICT_NAME == "NULL" | DS_Reject$DISTRICT_NAME == "Null" | is.na(DS_Reject$DISTRICT_NAME), "RejReason"] <- paste0(DS_Reject[DS_Reject$DISTRICT_NAME == "" | DS_Reject$DISTRICT_NAME == " " | DS_Reject$DISTRICT_NAME == "NULL" | DS_Reject$DISTRICT_NAME == "Null" | is.na(DS_Reject$DISTRICT_NAME), "RejReason"], "DISTRICT_NAME - value is NULL; ")
  DS_Reject[DS_Reject$DISTRICT_NAME == "Unknow" | DS_Reject$DISTRICT_NAME == "NA" | DS_Reject$DISTRICT_NAME == "TBD" | DS_Reject$DISTRICT_NAME == "TBC", "RejReason"] <- paste0(DS_Reject[DS_Reject$DISTRICT_NAME == "Unknow" | DS_Reject$DISTRICT_NAME == "NA" | DS_Reject$DISTRICT_NAME == "TBD" | DS_Reject$DISTRICT_NAME == "TBC", "RejReason"], "DISTRICT_NAME - value is invalid; ")
  
  #DISTRICT_NAME Rule 2
  DS_Reject[which(is.na(match(DS_Reject$DISTRICT_NAME, DS_DistrictList$District))), "RejReason"] <- paste0(DS_Reject[which(is.na(match(DS_Reject$DISTRICT_NAME, DS_DistrictList$District))), "RejReason"], "DISTRICT_NAME - value can't be found in standard list of District; ")
  
  #CITY Rule 1
  DS_Reject[DS_Reject$CITY == "" | DS_Reject$CITY == " " | DS_Reject$CITY == "NULL" | DS_Reject$CITY == "Null" | is.na(DS_Reject$CITY), "RejReason"] <- paste0(DS_Reject[DS_Reject$CITY == "" | DS_Reject$CITY == " " | DS_Reject$CITY == "NULL" | DS_Reject$CITY == "Null" | is.na(DS_Reject$CITY), "RejReason"], "CITY - value is NULL; ")
  DS_Reject[DS_Reject$CITY == "Unknow" | DS_Reject$CITY == "NA" | DS_Reject$CITY == "TBD" | DS_Reject$CITY == "TBC", "RejReason"] <- paste0(DS_Reject[DS_Reject$CITY == "Unknow" | DS_Reject$CITY == "NA" | DS_Reject$CITY == "TBD" | DS_Reject$CITY == "TBC", "RejReason"], "CITY - value is invalid; ")
  
  #CITY Rule 2
  DS_Reject[which(is.na(match(DS_Reject$CITY, DS_DistrictList$City))), "RejReason"] <- paste0(DS_Reject[which(is.na(match(DS_Reject$CITY, DS_DistrictList$City))), "RejReason"], "CITY - value can't be found in standard list of CITY; ")
  
  #PROPERTY_NAME_CN Rule 1
  DS_Reject[DS_Reject$PROPERTY_NAME_CN == "" | DS_Reject$PROPERTY_NAME_CN == " " | DS_Reject$PROPERTY_NAME_CN == "NULL" | DS_Reject$PROPERTY_NAME_CN == "Null" | is.na(DS_Reject$PROPERTY_NAME_CN), "RejReason"] <- paste0(DS_Reject[DS_Reject$PROPERTY_NAME_CN == "" | DS_Reject$PROPERTY_NAME_CN == " " | DS_Reject$PROPERTY_NAME_CN == "NULL" | DS_Reject$CITY == "Null" | is.na(DS_Reject$PROPERTY_NAME_CN), "RejReason"], "PROPERTY_NAME_CN - value is NULL; ")
  DS_Reject[DS_Reject$PROPERTY_NAME_CN == "Unknow" | DS_Reject$PROPERTY_NAME_CN == "NA" | DS_Reject$PROPERTY_NAME_CN == "TBD" | DS_Reject$PROPERTY_NAME_CN == "TBC", "RejReason"] <- paste0(DS_Reject[DS_Reject$PROPERTY_NAME_CN == "Unknow" | DS_Reject$PROPERTY_NAME_CN == "NA" | DS_Reject$PROPERTY_NAME_CN == "TBD" | DS_Reject$PROPERTY_NAME_CN == "TBC", "RejReason"], "PROPERTY_NAME_CN - value is invalid; ")
  
  #PROPERTY_NAME_CN Rule 2
  DS_Reject[which(nchar(DS_Reject$PROPERTY_NAME_CN) == 2), "RejReason"] <- paste0(DS_Reject[which(nchar(DS_Reject$PROPERTY_NAME_CN) == 2), "RejReason"], "PROPERTY_NAME_NC - The length of value is less than 3; ")
  
  #ADDRESS_LC Rule 1
  DS_Reject[DS_Reject$ADDRESS_LC == "" | DS_Reject$ADDRESS_LC == " " | DS_Reject$ADDRESS_LC == "NULL" | DS_Reject$ADDRESS_LC == "Null" | is.na(DS_Reject$ADDRESS_LC), "RejReason"] <- paste0(DS_Reject[DS_Reject$ADDRESS_LC == "" | DS_Reject$ADDRESS_LC == " " | DS_Reject$ADDRESS_LC == "NULL" | DS_Reject$ADDRESS_LC == "Null" | is.na(DS_Reject$ADDRESS_LC), "RejReason"], "ADDRESS_LC - value is NULL; ")
  #DS_Reject[DS_Reject$ADDRESS_LC == "Unknow" | DS_Reject$ADDRESS_LC == "NA" | DS_Reject$ADDRESS_LC == "TBD" | DS_Reject$ADDRESS_LC == "TBC", "RejReason"] <- paste0(DS_Reject[DS_Reject$ADDRESS_LC == "Unknow" | DS_Reject$ADDRESS_LC == "NA" | DS_Reject$ADDRESS_LC == "TBD" | DS_Reject$ADDRESS_LC == "TBC", "RejReason"], "ADDRESS_LC - value is invalid; ")
  
  
  #ADDRESS_LC Rule 2
  DS_Reject[which(regexpr("[0-9]", DS_Reject$ADDRESS_LC) == -1), "RejReason"] <- paste0(DS_Reject[which(regexpr("[0-9]", DS_Reject$ADDRESS_LC) == -1), "RejReason"], "ADDRESS_LC - value doesn't contain digit; ")
  DS_Reject[which(regexpr("[^0-9]0(号|-)", DS_Reject$ADDRESS_LC) > 0), "RejReason"] <- paste0(DS_Reject[which(regexpr("[^0-9]0(号|-)", DS_Reject$ADDRESS_LC) > 0), "RejReason"] , "ADDRESS_LC - the street number is 0; ")
  
  #ADDRESS_LC Rule 3
  DS_Reject[which(regexpr("(路|道|街|巷|弄)", DS_Reject$ADDRESS_LC) == -1), "RejReason"] <- paste0(DS_Reject[which(regexpr("(路|道|街|巷|弄)", DS_Reject$ADDRESS_LC) == -1), "RejReason"], "ADDRESS_LC - value doen't contain any of the key word 路，道，街，巷，弄; ")
  
  #DISTRICT_NAME_LC Rule 1
  DS_Reject[DS_Reject$DISTRICT_NAME_LC == "" | DS_Reject$DISTRICT_NAME_LC == " " | DS_Reject$DISTRICT_NAME_LC == "NULL" | DS_Reject$DISTRICT_NAME_LC == "Null" | is.na(DS_Reject$DISTRICT_NAME_LC), "RejReason"] <- paste0(DS_Reject[DS_Reject$DISTRICT_NAME_LC == "" | DS_Reject$DISTRICT_NAME_LC == " " | DS_Reject$DISTRICT_NAME_LC == "NULL" | DS_Reject$DISTRICT_NAME_LC == "Null" | is.na(DS_Reject$DISTRICT_NAME), "RejReason"], "DISTRICT_NAME_LC - value is NULL; ")
  DS_Reject[DS_Reject$DISTRICT_NAME_LC == "Unknow" | DS_Reject$DISTRICT_NAME_LC == "NA" | DS_Reject$DISTRICT_NAME_LC == "TBD" | DS_Reject$DISTRICT_NAME_LC == "TBC", "RejReason"] <- paste0(DS_Reject[DS_Reject$DISTRICT_NAME_LC == "Unknow" | DS_Reject$DISTRICT_NAME_LC == "NA" | DS_Reject$DISTRICT_NAME_LC == "TBD" | DS_Reject$DISTRICT_NAME_LC == "TBC", "RejReason"], "DISTRICT_NAME_LC - value is invalid; ")
  
  #DISTRICT_NAME_LC Rule 2
  DS_Reject[which(is.na(match(DS_Reject$DISTRICT_NAME_LC, DS_DistrictList$District_CHN))), "RejReason"] <- paste0(DS_Reject[which(is.na(match(DS_Reject$DISTRICT_NAME_LC, DS_DistrictList$District_CHN))), "RejReason"], "DISTRICT_NAME_LC - value can't be found in standard list of DISTRICT_NAME_LC; ")
  
  #CITY_LC Rule 1
  DS_Reject[DS_Reject$CITY_LC == "" | DS_Reject$CITY_LC == " " | DS_Reject$CITY_LC == "NULL" | DS_Reject$CITY_LC == "Null" | is.na(DS_Reject$CITY_LC), "RejReason"] <- paste0(DS_Reject[DS_Reject$CITY_LC == "" | DS_Reject$CITY_LC == " " | DS_Reject$CITY_LC == "NULL" | DS_Reject$CITY_LC == "Null" | is.na(DS_Reject$CITY_LC), "RejReason"], "CITY_LC - value is NULL; ")
  DS_Reject[DS_Reject$CITY_LC == "Unknow" | DS_Reject$CITY_LC == "NA" | DS_Reject$CITY_LC == "TBD" | DS_Reject$CITY_LC == "TBC", "RejReason"] <- paste0(DS_Reject[DS_Reject$CITY_LC == "Unknow" | DS_Reject$CITY_LC == "NA" | DS_Reject$CITY_LC == "TBD" | DS_Reject$CITY_LC == "TBC", "RejReason"], "CITY_LC - value is invalid; ")
  
  #CITY_LC Rule 2
  DS_Reject[which(is.na(match(DS_Reject$CITY_LC, DS_DistrictList$City_LC))), "RejReason"] <- paste0(DS_Reject[which(is.na(match(DS_Reject$CITY_LC, DS_DistrictList$City_LC))), "RejReason"], "CITY_LC - value can't be found in standard list of CITY_LC; ")
  
  #LAT Rule 1
  DS_Reject[DS_Reject$GEO_LAT == "" | DS_Reject$GEO_LAT == " " | DS_Reject$GEO_LAT == "NULL" | DS_Reject$GEO_LAT == "Null" | is.na(DS_Reject$GEO_LAT), "RejReason"] <- paste0(DS_Reject[DS_Reject$GEO_LAT == "" | DS_Reject$GEO_LAT == " " | DS_Reject$GEO_LAT == "NULL" | DS_Reject$GEO_LAT == "Null" | is.na(DS_Reject$GEO_LAT), "RejReason"], "GEO_LAT - value is NULL; ")
  DS_Reject[DS_Reject$GEO_LAT == "Unknow" | DS_Reject$GEO_LAT == "NA" | DS_Reject$GEO_LAT == "TBD" | DS_Reject$GEO_LAT == "TBC", "RejReason"] <- paste0(DS_Reject[DS_Reject$GEO_LAT == "Unknow" | DS_Reject$GEO_LAT == "NA" | DS_Reject$GEO_LAT == "TBD" | DS_Reject$GEO_LAT == "TBC", "RejReason"], "GEO_LAT - value is invalid; ")
  
  #LONG Rule 1
  DS_Reject[DS_Reject$GEO_LONG == "" | DS_Reject$GEO_LONG == " " | DS_Reject$GEO_LONG == "NULL" | DS_Reject$GEO_LONG == "Null" | is.na(DS_Reject$GEO_LONG), "RejReason"] <- paste0(DS_Reject[DS_Reject$GEO_LONG == "" | DS_Reject$GEO_LONG == " " | DS_Reject$GEO_LONG == "NULL" | DS_Reject$GEO_LONG == "Null" | is.na(DS_Reject$GEO_LONG), "RejReason"], "GEO_LONG - value is NULL; ")
  DS_Reject[DS_Reject$GEO_LONG == "Unknow" | DS_Reject$GEO_LONG == "NA" | DS_Reject$GEO_LONG == "TBD" | DS_Reject$GEO_LONG == "TBC", "RejReason"] <- paste0(DS_Reject[DS_Reject$GEO_LONG == "Unknow" | DS_Reject$GEO_LONG == "NA" | DS_Reject$GEO_LONG == "TBD" | DS_Reject$GEO_LONG == "TBC", "RejReason"], "GEO_LONG - value is invalid; ")
  
  
  return(DS_Reject)
}

DS_Source_PostRej <- RejProcess_All(DS_Source_Fixed)

DS_Source_Rejected <- filter(DS_Source_PostRej, RejReason != "")

xlsx::write.xlsx(DS_Source_Rejected,"out_put_Rj_marketlink.xlsx",sheetName = "Rejected Set")

DS_Source_Qualified <- filter(DS_Source_PostRej, RejReason == "")[, -ncol(DS_Source_PostRej)]


##############################Step 4: Google API search##############################
########################Google API function
keys <- c(#"AIzaSyA8Dmo33Ao1BwqTYthLhzmBRsv0Y9EtcBY"
          "AIzaSyDWDVo9Sy-Yk2eqjF5nS88hOPuiZpTOmD0"
          ,"AIzaSyBcqmigDl8Tuo06kWc7Ib-SrddUPRfLa5E"
          ,"AIzaSyA4-cljqh4KkfNWtziQgaAKPUG2XudfLfI"
          ,"AIzaSyClo0crDJDNwvEn5iMohXx_G6fInbbApXw"
          ,"AIzaSyD8ylAoVNwWJ8M4Xz8wKjXgoK-eyYLI4ew"
          ,"AIzaSyBqfG4uVkgVsqm-KGhsFXi_Bm87Ozg5Tbw"   
          ,"AIzaSyAOlyJl-HvmCgKIWtsRi0xBAjjPpO3vqXg"   
          ,"AIzaSyCLd9r79jmVTzO-rTot1vHsQ92-8xIYb1Y"   
          ,"AIzaSyDUGcoXWCXEEiAPcC-HzJl7GOS3xXZe6Xs"   
          ,"AIzaSyADoa11-g4vkv6Vf0QDaVYRHheSmkuJ1tg"   
          ,"AIzaSyBEMDfkNhwlQusKIQifo_OjrYl9gRq1ozs"   
          ,"AIzaSyAPXKtsC7bnUwQhIwGqK_Xs34S91TnKWeg"
          ,"AIzaSyBW97apAnfAxJRXAGCfztwBqV4uErV_llQ"
          ,"AIzaSyA_C99YFdu8tdZHO2XrTvCwswck3xPAviA"
          ,"AIzaSyApru4GSxXTt-HHEXSg2dnR-R9yoPxCM68"
          ,"AIzaSyAjsIquTPJph_8kBUjFsAdPceQLw0-dNbA"
          ,"AIzaSyCA5qZD8c68C9Lzj7JBEgXulqA9shRhq8s"
          ,"AIzaSyAX4IM8_H7sEAWVNvgZj2UmYohpKGc12Bo"
          ,"AIzaSyDfm86KYWGutSe5qPRcR8B22yD9EkNXD0Y"
          ,"AIzaSyCrd3k-ZG9ba7S9qNiO40F-DJmXVV209hg"
          ,"AIzaSyDIyk3QGB2TWO7tQp912pd3E04-gEZXb9A"
          ,"AIzaSyAkZ_5XMfguMICHGenTYDbT7vpdtYlYo0s"
          ,"AIzaSyASmGO6SgW7snetVl7B81UjT27aBZVdUvw"
          ,"AIzaSyDH8iGmWWSkYb7dfAJQdhx2YXS2N72F9GY"
          ,"AIzaSyDIhOhId0EVpyebvnSBvmkNQqFRAT335X8"
          ,"AIzaSyAOuD9IZPIOehoeV6Yb9E_zqNKAxmG4trM"
          ,"AIzaSyDWJt73kRLVrhkpB4y3dDsWjFqTDcV6ZPY"
          ,"AIzaSyBzgdu1JGlZ3eT2zAYnvnM0ZP8kKAyH5Ec"
          ,"AIzaSyDPPcLO6hIyQUS9AOTBQ861D-VO9kxbK0A"
          ,"AIzaSyCRkj0P5n2eHnOLmUDCgK4QZP2nnDt3IC0"
          ,"AIzaSyDXdk5X4ldrEcrD5zxiSb_FTWnu2Ptab_c"
          ,"AIzaSyBHLusUqoqkFnKoHsu-Yuz8OaizfTqvKWA"
          ,"AIzaSyDfIU6Hbi_Z5e86HIiIVJCeL81AX-ddUzM"
          ,"AIzaSyD_jphifEC1p1jvxv5e5sLYOc_QsZPFnTU"
          ,"AIzaSyC7k6p4UMHRVxWlnbyprpGWtg72BL7zGyk"
          ,"AIzaSyDPdMOZNWGkCpSVcU9AKsPB7Rqrw5sxNRQ")

keys <- c("AIzaSyCdeheCQP7K4abGbnXduKjtxocQEfhX3s4"
          ,"AIzaSyBr9KLDJbE7YiAqcwrrkTsenrZJusD9S44"
          ,"AIzaSyArYyiHgQJDIZYWPaWMAGCeTn4MTMnNeME"
          ,"AIzaSyCHGUo37HTZ-mORyi78cPgCp-MeDXBH6OM"
          ,"AIzaSyD6AJi4tjV0xK1tPW7hO-__00rJ6WfqXrE"
          ,"AIzaSyDkinkKUqsQ29o4nkxduHp1BKOnx5wPzFk"
          ,"AIzaSyD6-YuO_vMYns9U3JVBCY3zobtKxFbL-WY"
          ,"AIzaSyDHWrZLRpzUe1RDGXyVAtJRzDKPNcUYRi0"
          ,"AIzaSyCIbOPps5BBU1rjsBRM2SMW86jyO5WpKog"
          ,"AIzaSyDe4TVXfbYIaoIKsqNg1cGiefzJllpCK_8")

#google.api <- function(query, language, key= "AIzaSyADoa11-g4vkv6Vf0QDaVYRHheSmkuJ1tg"){
  query_1 <- paste0(query[1],",",query[2],",",query[3],",",query[4])
  root <- "https://maps.googleapis.com/maps/api/place/textsearch/json"
  u <- paste0(root, "?query=", query_1, "&language=", language, "&key=", key)
  doc <- getURL(URLencode(u))
  x <- fromJSON(doc,simplify = FALSE)
  
  PROPERTY_NAME_CN_GAPI <- ""
  ADDRESS_LC_GAPI <- ""
  DISTRICT_NAME_LC_GAPI <- ""
  CITY_LC_GAPI <- ""
  GEO_LAT_GAPI <- 0.0
  GEO_LONG_GAPI <- 0.0
  
  if(x$status=="OK"){
    result <- x$results
    #####judge if the result type is establishment or street_address
    result_type <- "TBC"
    result_num <- 0
    for(i in 1:length(result)){
      v_type <- c("")
      for(j in 1:length(result[[i]]$types)){
        v_type[j] <- result[[i]]$types[[j]]
      }
      
      if(any(v_type == "establishment")){
        result_type <- "establishment"
        result_num <- i
        break
      }
      if(any(v_type == "premise")){
        result_type <- "premise"
        result_num <- i
        break
      }
      if(any(v_type == "street_address")){
        result_type <- "street_address"
        result_num <- i
        break
      }
    }
    
    if(result_type == "establishment" | result_type == "premise"){
      result_1 <- result[[result_num]]
      PROPERTY_NAME_CN_GAPI <- result_1$name
      formatted_address <- result_1$formatted_address
      if(regexpr("\\s", formatted_address) > 0){
        formatted_address <- substr(formatted_address,1,regexpr("\\s", formatted_address) - 1)
      }
      formatted_address <- gsub("^中国", "", formatted_address)
      v_prov <- unique(as.character(DS_DistrictList$Province_LC))
      for(n in 1:length(v_prov)){
        formatted_address <- gsub(paste0("^" ,v_prov[n], "省"), "", formatted_address) 
      }
      v_city <- unique(as.character(DS_DistrictList$City_LC))
      for(m in 1:length(v_city)){
        if(regexpr(paste0("^" ,v_city[m], "市"),formatted_address) > 0){
          CITY_LC_GAPI <- v_city[m]
          formatted_address <- gsub(paste0("^" ,v_city[m], "市"), "", formatted_address) 
        }
      }
      if(CITY_LC_GAPI == ""){
        CITY_LC_GAPI <- query[3]
      }
      v_dist <- unique(as.character(DS_DistrictList$District_CHN))
      for(k in 1:length(v_dist)){
        if(regexpr(paste0("^" ,v_dist[k], "区"),formatted_address) > 0){
          DISTRICT_NAME_LC_GAPI <- v_dist[k]
          formatted_address <- gsub(paste0("^" ,v_dist[k], "区"), "", formatted_address) 
        }
      }
      if(DISTRICT_NAME_LC_GAPI == ""){
        DISTRICT_NAME_LC_GAPI <- query[2]
      }
      ADDRESS_LC_GAPI <- formatted_address
      GEO_LAT_GAPI <- result_1$geometry$location$lat
      GEO_LONG_GAPI <- result_1$geometry$location$lng
    }
    
    if(result_type == "street_address"){
      result_1 <- result[[result_num]]
      ADDRESS_LC_GAPI <- result_1$name
      formatted_address <- result_1$formatted_address
      if(regexpr("\\s", formatted_address) > 0){
        formatted_address <- substr(formatted_address,1,regexpr("\\s", formatted_address) - 1)
      }
      formatted_address <- gsub("^中国", "", formatted_address)
      v_prov <- unique(as.character(DS_DistrictList$Province_LC))
      for(n in 1:length(v_prov)){
        formatted_address <- gsub(paste0("^" ,v_prov[n], "省"), "", formatted_address) 
      }
      v_city <- unique(as.character(DS_DistrictList$City_LC))
      for(m in 1:length(v_city)){
        if(regexpr(paste0("^" ,v_city[m], "市"),formatted_address) > 0){
          CITY_LC_GAPI <- v_city[m]
          formatted_address <- gsub(paste0("^" ,v_city[m], "市"), "", formatted_address) 
        }
      }
      if(CITY_LC_GAPI == ""){
        CITY_LC_GAPI <- query[3]
      }
      v_dist <- unique(as.character(DS_DistrictList$District_CHN))
      for(k in 1:length(v_dist)){
        if(regexpr(paste0("^" ,v_dist[k], "区"),formatted_address) > 0){
          DISTRICT_NAME_LC_GAPI <- v_dist[k]
          formatted_address <- gsub(paste0("^" ,v_dist[k], "区"), "", formatted_address) 
        }
      }
      if(DISTRICT_NAME_LC_GAPI == ""){
        DISTRICT_NAME_LC_GAPI <- query[2]
      }
      if(regexpr(paste0(ADDRESS_LC_GAPI, "$"),formatted_address) > 1){
        PROPERTY_NAME_CN_GAPI <- substr(formatted_address,1, regexpr(paste0(ADDRESS_LC_GAPI, "$"),formatted_address) - 1)
      }
      GEO_LAT_GAPI <- result_1$geometry$location$lat
      GEO_LONG_GAPI <- result_1$geometry$location$lng
    }
    
  }
  
  return(data.frame(PROPERTY_NAME = PROPERTY_NAME_CN_GAPI
                    , ADDRESS_LC = ADDRESS_LC_GAPI 
                    , DISTRICT_NAME_LC = DISTRICT_NAME_LC_GAPI
                    , CITY_LC = CITY_LC_GAPI
                    , GEO_LAT = GEO_LAT_GAPI
                    , GEO_LONG = GEO_LONG_GAPI
                    , stringsAsFactors = FALSE))
}

google.api <- function(query, language, key= "AIzaSyADoa11-g4vkv6Vf0QDaVYRHheSmkuJ1tg"){
  query_1 <- paste0(query[1],",",query[2],",",query[3],",",query[4])
  root <- "https://maps.googleapis.com/maps/api/place/textsearch/json"
  u <- paste0(root, "?query=", query_1, "&language=", language, "&key=", key)
  doc <- getURL(URLencode(u))
  x <- fromJSON(doc,simplify = FALSE)
  
  DS_RESULT <- data.frame(PROPERTY_NAME = ""
                          , ADDRESS = ""  
                          , DISTRICT_NAME = ""
                          , CITY = ""
                          , GEO_LAT = ""
                          , GEO_LONG = ""
                          , stringsAsFactors = FALSE)
  
  if(language == "zh-CN"){
    PROPERTY_NAME_CN_GAPI <- ""
    ADDRESS_LC_GAPI <- ""
    DISTRICT_NAME_LC_GAPI <- ""
    CITY_LC_GAPI <- ""
    GEO_LAT_GAPI <- 0.0
    GEO_LONG_GAPI <- 0.0
    
    if(x$status=="OK"){
      result <- x$results
      #####judge if the result type is establishment or street_address
      result_type <- "TBC"
      result_num <- 0
      for(i in 1:length(result)){
        v_type <- c("")
        for(j in 1:length(result[[i]]$types)){
          v_type[j] <- result[[i]]$types[[j]]
        }
        
        if(any(v_type == "establishment")){
          result_type <- "establishment"
          result_num <- i
          break
        }
        if(any(v_type == "premise")){
          result_type <- "premise"
          result_num <- i
          break
        }
        if(any(v_type == "street_address")){
          result_type <- "street_address"
          result_num <- i
          break
        }
      }
      
      if(result_type == "establishment" | result_type == "premise"){
        result_1 <- result[[result_num]]
        PROPERTY_NAME_CN_GAPI <- result_1$name
        formatted_address <- result_1$formatted_address
        if(regexpr("\\s", formatted_address) > 0){
          formatted_address <- substr(formatted_address,1,regexpr("\\s", formatted_address) - 1)
        }
        formatted_address <- gsub("^中国", "", formatted_address)
        v_prov <- unique(as.character(gv_china_division_list$PROVINCE_LC)) 
        for(n in 1:length(v_prov)){
          formatted_address <- gsub(paste0("^" ,v_prov[n], "省"), "", formatted_address) 
        }
        v_city <- unique(as.character(gv_china_division_list$CITY_LC)) 
        for(m in 1:length(v_city)){
          if(regexpr(paste0("^" ,v_city[m], "市"),formatted_address) > 0){
            CITY_LC_GAPI <- v_city[m]
            formatted_address <- gsub(paste0("^" ,v_city[m], "市"), "", formatted_address) 
          }
        }
        if(CITY_LC_GAPI == ""){
          CITY_LC_GAPI <- query[3]
        }
        v_dist <- unique(as.character(gv_china_division_list$DISTRICT_LC))
        for(k in 1:length(v_dist)){
          if(regexpr(paste0("^" ,v_dist[k], "区"),formatted_address) > 0){
            DISTRICT_NAME_LC_GAPI <- v_dist[k]
            formatted_address <- gsub(paste0("^" ,v_dist[k], "区"), "", formatted_address) 
          }
        }
        if(DISTRICT_NAME_LC_GAPI == ""){
          DISTRICT_NAME_LC_GAPI <- query[2]
        }
        ADDRESS_LC_GAPI <- formatted_address
        GEO_LAT_GAPI <- result_1$geometry$location$lat
        GEO_LONG_GAPI <- result_1$geometry$location$lng
      }
      
      if(result_type == "street_address"){
        result_1 <- result[[result_num]]
        ADDRESS_LC_GAPI <- result_1$name
        formatted_address <- result_1$formatted_address
        if(regexpr("\\s", formatted_address) > 0){
          formatted_address <- substr(formatted_address,1,regexpr("\\s", formatted_address) - 1)
        }
        formatted_address <- gsub("^中国", "", formatted_address)
        v_prov <- unique(as.character(gv_china_division_list$PROVINCE_LC))
        for(n in 1:length(v_prov)){
          formatted_address <- gsub(paste0("^" ,v_prov[n], "省"), "", formatted_address) 
        }
        v_city <- unique(as.character(gv_china_division_list$CITY_LC))
        for(m in 1:length(v_city)){
          if(regexpr(paste0("^" ,v_city[m], "市"),formatted_address) > 0){
            CITY_LC_GAPI <- v_city[m]
            formatted_address <- gsub(paste0("^" ,v_city[m], "市"), "", formatted_address) 
          }
        }
        if(CITY_LC_GAPI == ""){
          CITY_LC_GAPI <- query[3]
        }
        v_dist <- unique(as.character(gv_china_division_list$DISTRICT_LC))
        for(k in 1:length(v_dist)){
          if(regexpr(paste0("^" ,v_dist[k], "区"),formatted_address) > 0){
            DISTRICT_NAME_LC_GAPI <- v_dist[k]
            formatted_address <- gsub(paste0("^" ,v_dist[k], "区"), "", formatted_address) 
          }
        }
        if(DISTRICT_NAME_LC_GAPI == ""){
          DISTRICT_NAME_LC_GAPI <- query[2]
        }
        if(regexpr(paste0(ADDRESS_LC_GAPI, "$"),formatted_address) > 1){
          PROPERTY_NAME_CN_GAPI <- substr(formatted_address,1, regexpr(paste0(ADDRESS_LC_GAPI, "$"),formatted_address) - 1)
        }
        GEO_LAT_GAPI <- result_1$geometry$location$lat
        GEO_LONG_GAPI <- result_1$geometry$location$lng
      }
      
    }
    
    DS_RESULT <- data.frame(PROPERTY_NAME = PROPERTY_NAME_CN_GAPI
                            , ADDRESS = ADDRESS_LC_GAPI 
                            , DISTRICT_NAME = DISTRICT_NAME_LC_GAPI
                            , CITY = CITY_LC_GAPI
                            , GEO_LAT = GEO_LAT_GAPI
                            , GEO_LONG = GEO_LONG_GAPI
                            , stringsAsFactors = FALSE)
  } 
  
  if(language == "en"){
    PROPERTY_NAME_EN_GAPI <- ""
    ADDRESS_EN_GAPI <- ""
    DISTRICT_NAME_EN_GAPI <- ""
    CITY_EN_GAPI <- ""
    GEO_LAT_EN_GAPI <- 0.0
    GEO_LONG_EN_GAPI <- 0.0
    
    if(x$status=="OK"){
      result <- x$results
      #####judge if the result type is establishment or street_address
      result_type <- "TBC"
      result_num <- 0
      for(i in 1:length(result)){
        v_type <- c("")
        for(j in 1:length(result[[i]]$types)){
          v_type[j] <- result[[i]]$types[[j]]
        }
        
        if(any(v_type == "establishment")){
          result_type <- "establishment"
          result_num <- i
          break
        }
        if(any(v_type == "premise")){
          result_type <- "premise"
          result_num <- i
          break
        }
        if(any(v_type == "street_address")){
          result_type <- "street_address"
          result_num <- i
          break
        }
      }
      
      if(result_num != 0){
        result_1 <- result[[result_num]]
        place_id <- result_1$place_id
        root_pid <- "https://maps.googleapis.com/maps/api/place/details/json?placeid="
        u_pid <- paste0(root_pid, place_id, "&key=", key)
        doc_pid <- getURL(URLencode(u_pid))
        x_pid <- fromJSON(doc_pid,simplify = FALSE)
        
        if(x_pid$status == "OK"){
          result_1_pid <- x_pid$result
          
          PROPERTY_NAME_EN_GAPI <- result_1_pid$name
          GEO_LAT_EN_GAPI <- result_1_pid$geometry$location$lat
          GEO_LONG_EN_GAPI <- result_1_pid$geometry$location$lng
          
          if(!is.null(result_1_pid$address_components)){
            address_components <- result_1_pid$address_components
            street_number <- ""
            route <- ""
            district <- ""
            city <- ""
            
            for(i in 1:length(address_components)){
              if(any(address_components[[i]]$types == "street_number")){
                street_number <-  address_components[[i]]$long_name
              }else if(any(address_components[[i]]$types == "route")){
                route <- address_components[[i]]$long_name
              }else if(any(address_components[[i]]$types == "sublocality")){
                district <- address_components[[i]]$long_name
                district <- ifelse(regexpr("\\s", district)>0,
                                   substr(district,1,regexpr("\\s", district) - 1),
                                   district) 
              }else if(any(address_components[[i]]$types == "locality")){
                city <- address_components[[i]]$long_name
                city <- ifelse(regexpr("\\s", city)>0,
                               substr(city,1,regexpr("\\s", city) - 1),
                               city) 
              }
              
            }
            
            ADDRESS_EN_GAPI <- paste(street_number, route)
            DISTRICT_NAME_EN_GAPI <- district
            CITY_EN_GAPI <- city
          }
          
        }
      }
    }
    
    DS_RESULT <- data.frame(PROPERTY_NAME = PROPERTY_NAME_EN_GAPI
                            , ADDRESS = ADDRESS_EN_GAPI 
                            , DISTRICT_NAME = DISTRICT_NAME_EN_GAPI
                            , CITY = CITY_EN_GAPI
                            , GEO_LAT = GEO_LAT_EN_GAPI
                            , GEO_LONG = GEO_LONG_EN_GAPI
                            , stringsAsFactors = FALSE)
    
  }
  
  
  return(DS_RESULT)
  
}


query <- c("西门口广场", "荔湾",             "广州"      ,       "中国" )
language <- "zh-CN" #zh-CN， en
key <- "AIzaSyA8Dmo33Ao1BwqTYthLhzmBRsv0Y9EtcBY"
ds_g_t <- google.api(query,language)
ds_g_t$PROPERTY_NAME
ds_g_t$ADDRESS

#######################Use Google API for searching
#Data_Search_Google_CN <- function(DS_PreAPI,key){
  DS_PreAPI_Google <- DS_PreAPI
  rownum <- nrow(DS_PreAPI_Google)
  DS_PreAPI_Google$PROPERTY_NAME_CN_GAPI <- rep("", rownum)
  DS_PreAPI_Google$ADDRESS_LC_GAPI <- rep("", rownum)
  DS_PreAPI_Google$DISTRICT_NAME_LC_GAPI <- rep("", rownum)
  DS_PreAPI_Google$CITY_LC_GAPI <- rep("", rownum)
  DS_PreAPI_Google$GEO_LAT_GAPI <- rep("", rownum)
  DS_PreAPI_Google$GEO_LONG_GAPI <- rep("", rownum)
  
  for(i in 1:rownum){
    print(i)
    print(as.character(DS_PreAPI_Google$CLIENT_PROPERTY_CODE[i]))
    
    
    qele_1_1 <- ifelse(is.null(DS_PreAPI_Google$PROPERTY_NAME_CN[i]) | DS_PreAPI_Google$PROPERTY_NAME_CN[i] == "NULL", "" ,DS_PreAPI_Google$PROPERTY_NAME_CN[i])
    qele_1_1 <- ifelse(is.na(qele_1_1) | nchar(qele_1_1) < 3, "", qele_1_1)
    
    qele_1_2 <- ifelse(is.null(DS_PreAPI_Google$PROPERTY_NAME[i]) | DS_PreAPI_Google$PROPERTY_NAME[i] == "NULL", "" ,DS_PreAPI_Google$PROPERTY_NAME[i])
    qele_1_2 <- ifelse(is.na(qele_1_2) | nchar(qele_1_2) < 3, "", qele_1_2)
    
    qele_1_3 <- ifelse(is.null(DS_PreAPI_Google$ADDRESS_LC[i]) | DS_PreAPI_Google$ADDRESS_LC[i] == "NULL", "" ,DS_PreAPI_Google$ADDRESS_LC[i])
    qele_1_3 <- ifelse(is.na(qele_1_3) | nchar(qele_1_3) < 5, "", qele_1_3)
    
    qele_1_4 <- ifelse(is.null(DS_PreAPI_Google$ADDRESS_1[i]) | DS_PreAPI_Google$ADDRESS_1[i] == "NULL", "" ,DS_PreAPI_Google$ADDRESS_1[i])
    qele_1_4 <- ifelse(is.na(qele_1_4) | nchar(qele_1_4) < 5, "", qele_1_4)
    
    qele_2_1 <- ifelse(is.null(DS_PreAPI_Google$DISTRICT_NAME_LC[i]) | DS_PreAPI_Google$DISTRICT_NAME_LC[i] == "NULL", "", DS_PreAPI_Google$DISTRICT_NAME_LC[i]) 
    qele_2_1 <- ifelse(is.na(qele_2_1) | nchar(qele_2_1) < 2, "", qele_2_1)
    
    qele_2_2 <- ifelse(is.null(as.character(DS_PreAPI_Google$DISTRICT_NAME[i])) | as.character(DS_PreAPI_Google$DISTRICT_NAME[i]) == "NULL", "", as.character(DS_PreAPI_Google$DISTRICT_NAME[i]))
    qele_2_2 <- ifelse(is.na(qele_2_2) | nchar(qele_2_2) < 4, "", qele_2_2)
    if(qele_2_2 != ""){
      DS_unq_Dist <- unique(DS_DistrictList[,1:2])
      qele_2_2 <- as.character(DS_unq_Dist$District_CHN[which(unique(DS_unq_Dist$District) ==  qele_2_2)]) 
    }
    
    qele_3_1 <- ifelse(is.null(DS_PreAPI_Google$CITY_LC[i]) | DS_PreAPI_Google$CITY_LC[i] == "NULL", "", DS_PreAPI_Google$CITY_LC[i]) 
    qele_3_1 <- ifelse(is.na(qele_3_1) | nchar(qele_3_1) < 2, "", qele_3_1)
    
    qele_3_2 <- ifelse(is.null(as.character(DS_PreAPI_Google$CITY[i])) | as.character(DS_PreAPI_Google$CITY[i]) == "NULL", "", as.character(DS_PreAPI_Google$CITY[i]))
    qele_3_2 <- ifelse(is.na(qele_3_2) | nchar(qele_3_2) < 4, "", qele_3_2)
    if(qele_3_2 != ""){
      DS_unq_City <- unique(DS_DistrictList[,3:4])
      qele_3_2 <- as.character(DS_unq_City$City_LC[which(unique(DS_unq_City$City) ==  qele_3_2)]) 
    }
    
    
    query <- rep("",4)
    qele_1 <- ifelse(ifelse(ifelse(qele_1_1 == "", qele_1_2, qele_1_1) == "", qele_1_3, ifelse(qele_1_1 == "", qele_1_2, qele_1_1)) == "", 
                     qele_1_4, 
                     ifelse(ifelse(qele_1_1 == "", qele_1_2, qele_1_1) == "", qele_1_3, ifelse(qele_1_1 == "", qele_1_2, qele_1_1))) 
    
    qele_2 <- ifelse(qele_2_1 == "", qele_2_2, qele_2_1)
    qele_3 <- ifelse(qele_3_1 == "", qele_3_2, qele_3_1)
    qele_4 <- "中国"
    
    query[1] <- qele_1
    query[2] <- qele_2
    query[3] <- qele_3
    query[4] <- qele_4
    
    #call google api
    print(query)
    DS_GAPI_Return <- google.api(query, language = "zh-CN", key = key)
    print(DS_GAPI_Return$PROPERTY_NAME)
    if(!is.null(DS_GAPI_Return))
    {
      DS_PreAPI_Google$PROPERTY_NAME_CN_GAPI[i] <- DS_GAPI_Return$PROPERTY_NAME[1]
      DS_PreAPI_Google$ADDRESS_LC_GAPI[i] <- DS_GAPI_Return$ADDRESS_LC[1]
      DS_PreAPI_Google$DISTRICT_NAME_LC_GAPI[i] <- DS_GAPI_Return$DISTRICT_NAME_LC[1]
      DS_PreAPI_Google$CITY_LC_GAPI[i] <- DS_GAPI_Return$CITY_LC[1]
      DS_PreAPI_Google$GEO_LAT_GAPI[i] <- DS_GAPI_Return$GEO_LAT[1]
      DS_PreAPI_Google$GEO_LONG_GAPI[i] <- DS_GAPI_Return$GEO_LONG[1]
    }
    
  }
  
  return(DS_PreAPI_Google)
  
}
Data_Search_Google <- function(DS_PreAPI,key,language){
  DS_PreAPI_Google <- DS_PreAPI
  rownum <- nrow(DS_PreAPI_Google)
  DS_PreAPI_Google$PROPERTY_NAME_GAPI <- rep("", rownum)
  DS_PreAPI_Google$ADDRESS_GAPI <- rep("", rownum)
  DS_PreAPI_Google$DISTRICT_NAME_GAPI <- rep("", rownum)
  DS_PreAPI_Google$CITY_GAPI <- rep("", rownum)
  DS_PreAPI_Google$GEO_LAT_GAPI <- rep("", rownum)
  DS_PreAPI_Google$GEO_LONG_GAPI <- rep("", rownum)

  for(i in 1:rownum){
    print(as.character(DS_PreAPI_Google$CLIENT_PROPERTY_CODE[i]))
    
    
    qele_1_1 <- ifelse(is.null(DS_PreAPI_Google$PROPERTY_NAME_CN[i]) | DS_PreAPI_Google$PROPERTY_NAME_CN[i] == "NULL", "" ,DS_PreAPI_Google$PROPERTY_NAME_CN[i])
    qele_1_1 <- ifelse(is.na(qele_1_1) | nchar(qele_1_1) < 3, "", qele_1_1)
    
    qele_1_2 <- ifelse(is.null(DS_PreAPI_Google$PROPERTY_NAME[i]) | DS_PreAPI_Google$PROPERTY_NAME[i] == "NULL", "" ,DS_PreAPI_Google$PROPERTY_NAME[i])
    qele_1_2 <- ifelse(is.na(qele_1_2) | nchar(qele_1_2) < 3, "", qele_1_2)
    
    qele_1_3 <- ifelse(is.null(DS_PreAPI_Google$ADDRESS_LC[i]) | DS_PreAPI_Google$ADDRESS_LC[i] == "NULL", "" ,DS_PreAPI_Google$ADDRESS_LC[i])
    qele_1_3 <- ifelse(is.na(qele_1_3) | nchar(qele_1_3) < 5, "", qele_1_3)
    
    qele_1_4 <- ifelse(is.null(DS_PreAPI_Google$ADDRESS_1[i]) | DS_PreAPI_Google$ADDRESS_1[i] == "NULL", "" ,DS_PreAPI_Google$ADDRESS_1[i])
    qele_1_4 <- ifelse(is.na(qele_1_4) | nchar(qele_1_4) < 5, "", qele_1_4)
    
    qele_2_1 <- ifelse(is.null(DS_PreAPI_Google$DISTRICT_NAME_LC[i]) | DS_PreAPI_Google$DISTRICT_NAME_LC[i] == "NULL", "", DS_PreAPI_Google$DISTRICT_NAME_LC[i]) 
    qele_2_1 <- ifelse(is.na(qele_2_1) | nchar(qele_2_1) < 2, "", qele_2_1)
    
    qele_2_2 <- ifelse(is.null(as.character(DS_PreAPI_Google$DISTRICT_NAME[i])) | as.character(DS_PreAPI_Google$DISTRICT_NAME[i]) == "NULL", "", as.character(DS_PreAPI_Google$DISTRICT_NAME[i]))
    qele_2_2 <- ifelse(is.na(qele_2_2) | nchar(qele_2_2) < 4, "", qele_2_2)
    if(qele_2_2 != ""){
      DS_unq_Dist <- unique(DS_DistrictList[,1:2])
      qele_2_2 <- as.character(DS_unq_Dist$District_CHN[which(unique(DS_unq_Dist$District) ==  qele_2_2)]) 
    }
    
    qele_3_1 <- ifelse(is.null(DS_PreAPI_Google$CITY_LC[i]) | DS_PreAPI_Google$CITY_LC[i] == "NULL", "", DS_PreAPI_Google$CITY_LC[i]) 
    qele_3_1 <- ifelse(is.na(qele_3_1) | nchar(qele_3_1) < 2, "", qele_3_1)
    
    qele_3_2 <- ifelse(is.null(as.character(DS_PreAPI_Google$CITY[i])) | as.character(DS_PreAPI_Google$CITY[i]) == "NULL", "", as.character(DS_PreAPI_Google$CITY[i]))
    qele_3_2 <- ifelse(is.na(qele_3_2) | nchar(qele_3_2) < 4, "", qele_3_2)
    if(qele_3_2 != ""){
      DS_unq_City <- unique(DS_DistrictList[,3:4])
      qele_3_2 <- as.character(DS_unq_City$City_LC[which(unique(DS_unq_City$City) ==  qele_3_2)]) 
    }
    
    
    query <- rep("",4)
    qele_1 <- ifelse(ifelse(ifelse(qele_1_1 == "", qele_1_2, qele_1_1) == "", qele_1_3, ifelse(qele_1_1 == "", qele_1_2, qele_1_1)) == "", 
                     qele_1_4, 
                     ifelse(ifelse(qele_1_1 == "", qele_1_2, qele_1_1) == "", qele_1_3, ifelse(qele_1_1 == "", qele_1_2, qele_1_1))) 
    
    qele_2 <- ifelse(qele_2_1 == "", qele_2_2, qele_2_1)
    qele_3 <- ifelse(qele_3_1 == "", qele_3_2, qele_3_1)
    qele_4 <- ifelse(language == "en", "China", "中国") 
    
    query[1] <- qele_1
    query[2] <- qele_2
    query[3] <- qele_3
    query[4] <- qele_4
    
    #call google api
    print(query)
    DS_GAPI_Return <- google.api(query, language = language, key = key)
    
    if(!is.null(DS_GAPI_Return))
    {
      DS_PreAPI_Google$PROPERTY_NAME_GAPI[i] <- DS_GAPI_Return$PROPERTY_NAME[1]
      DS_PreAPI_Google$ADDRESS_GAPI[i] <- DS_GAPI_Return$ADDRESS[1]
      DS_PreAPI_Google$DISTRICT_NAME_GAPI[i] <- DS_GAPI_Return$DISTRICT_NAME[1]
      DS_PreAPI_Google$CITY_GAPI[i] <- DS_GAPI_Return$CITY[1]
      DS_PreAPI_Google$GEO_LAT_GAPI[i] <- DS_GAPI_Return$GEO_LAT[1]
      DS_PreAPI_Google$GEO_LONG_GAPI[i] <- DS_GAPI_Return$GEO_LONG[1]
    }
    
  }
  
  return(DS_PreAPI_Google)
  
}

#######NOTE: USE COMBINED CHINESE OUTPUT OF GOOGLE AND BAIDU AS THE INPUT OF ENGLISH SEARCH
DS_PostAPI_Combined_For_EngSearch <- sourceDataImport('Andy\'s Data.xlsx','PostAPI_Combined_For_EnSearch')
  
for (i in 1:length(keys)) {
  sn <- 2125+45*(i-1)
  en <- 2124+45*(i-0)
  DS_PreAPI <- DS_PostAPI_Combined_For_EngSearch[sn:en,]
  print(as.character(i))
  DS_PostAPI_Google <- Data_Search_Google(DS_PreAPI,keys[i],"en")
  xlsx::write.xlsx(DS_PostAPI_Google,"output_post_google_api.xlsx",sheetName = paste0("GoogleAPI_Output", as.character(i)),append = TRUE)
}


DS_PreAPI <- DS_Source_Fixed[1:40,]
DS_PostAPI_Google <- Data_Search_Google(DS_PreAPI,"AIzaSyADoa11-g4vkv6Vf0QDaVYRHheSmkuJ1tg", "en")
xlsx::write.xlsx(DS_PostAPI_Google,"output_post_google_api.xlsx",sheetName = "GoogleAPI_Output")


##############################Step 5: Baidu API search##############################
########################Baidu API function
#key1: NdgFqhKyiqPksC2pLfsGsWps15wzRsES
baidu.api <- function(query, region, ak = "NdgFqhKyiqPksC2pLfsGsWps15wzRsES"){
  root <- "http://api.map.baidu.com/place/v2/search?"
  u <- paste0(root, "q=",query,"&region=",region,"&output=json","&page_size=1","&scope=2","$tag=房地产,酒店", "&ak=", ak)
  
  PROPERTY_NAME_CN_BAPI <- ""
  ADDRESS_LC_BAPI <- ""
  DISTRICT_NAME_LC_BAPI <- ""
  CITY_LC_BAPI <- ""
  GEO_LAT_BAPI <- 0.0
  GEO_LONG_BAPI <- 0.0
  
  doc <- getURL(u)
  if(doc != "" & !is.null(doc)){
    x <- fromJSON(doc,simplify = FALSE)
    if(length(x$results) > 0){
      if(x$status==0 & !is.null(x$results[[1]]$uid)){
        result <- x$results
        result_1 <- result[[1]]
        
        if(!is.null(result_1$address) & !is.null(result_1$name)){
          PROPERTY_NAME_CN_BAPI <- result_1$name
          
          formatted_address <- result_1$address
          if(regexpr("\\s", formatted_address) > 0){
            formatted_address <- substr(formatted_address,1,regexpr("\\s", formatted_address) - 1)
          }
          formatted_address <- gsub("^中国", "", formatted_address)
          v_prov <- unique(as.character(DS_DistrictList$Province_LC))
          for(n in 1:length(v_prov)){
            formatted_address <- gsub(paste0("^" ,v_prov[n], "省"), "", formatted_address) 
          }
          v_city <- unique(as.character(DS_DistrictList$City_LC))
          for(m in 1:length(v_city)){
            if(regexpr(paste0("^" ,v_city[m], "市"),formatted_address) > 0){
              CITY_LC_BAPI <- v_city[m]
              formatted_address <- gsub(paste0("^" ,v_city[m], "市"), "", formatted_address) 
            }
          }
          if(CITY_LC_BAPI == ""){
            CITY_LC_BAPI <- region
          }
          
          v_dist <- unique(as.character(DS_DistrictList$District_CHN))
          for(k in 1:length(v_dist)){
            if(regexpr(paste0("^" ,v_dist[k], "区"),formatted_address) > 0){
              DISTRICT_NAME_LC_BAPI <- v_dist[k]
              formatted_address <- gsub(paste0("^" ,v_dist[k], "区"), "", formatted_address) 
            }
          }
          
          
          ADDRESS_LC_BAPI <- formatted_address
          GEO_LAT_BAPI <- result_1$location$lat
          GEO_LONG_BAPI <- result_1$location$lng
        }
        
      }
    }
  }
  
  return(data.frame(PROPERTY_NAME = PROPERTY_NAME_CN_BAPI
                    , ADDRESS_LC = ADDRESS_LC_BAPI 
                    , DISTRICT_NAME_LC = DISTRICT_NAME_LC_BAPI
                    , CITY_LC = CITY_LC_BAPI
                    , GEO_LAT = GEO_LAT_BAPI
                    , GEO_LONG = GEO_LONG_BAPI
                    , stringsAsFactors = FALSE))
}

ak <- "NdgFqhKyiqPksC2pLfsGsWps15wzRsES"
query <- "滨江国际广场"  #must be chinese, and need to remove "("
region <- "上海"   #must be chinese
ds_B_t <- baidu.api(query,region)
ds_B_t$PROPERTY_NAME
ds_B_t$ADDRESS_LC
ds_B_t$DISTRICT_NAME_LC
ds_B_t$CITY_LC

#######################Use Baidu API for searching
Data_Search_Baidu_CN <- function(DS_PreAPI){
  DS_PreAPI_Baidu <- DS_PreAPI
  rownum <- nrow(DS_PreAPI_Baidu)
  DS_PreAPI_Baidu$PROPERTY_NAME_CN_BAPI <- rep("", rownum)
  DS_PreAPI_Baidu$ADDRESS_LC_BAPI <- rep("", rownum)
  DS_PreAPI_Baidu$DISTRICT_NAME_LC_BAPI <- rep("", rownum)
  DS_PreAPI_Baidu$CITY_LC_BAPI <- rep("", rownum)
  DS_PreAPI_Baidu$GEO_LAT_BAPI <- rep("", rownum)
  DS_PreAPI_Baidu$GEO_LONG_BAPI <- rep("", rownum)

  for(i in 1:rownum){
    print(i)
    print(as.character(DS_PreAPI_Baidu$CLIENT_PROPERTY_CODE[i]))
    
    qele_1_1 <- ifelse(is.null(DS_PreAPI_Baidu$PROPERTY_NAME_CN[i]) | DS_PreAPI_Baidu$PROPERTY_NAME_CN[i] == "NULL", "" ,DS_PreAPI_Baidu$PROPERTY_NAME_CN[i])
    qele_1_1 <- ifelse(is.na(qele_1_1) | nchar(qele_1_1) < 3, "", qele_1_1)
    
    qele_1_2 <- ifelse(is.null(DS_PreAPI_Baidu$ADDRESS_LC[i]) | DS_PreAPI_Baidu$ADDRESS_LC[i] == "NULL", "" ,DS_PreAPI_Baidu$ADDRESS_LC[i])
    qele_1_2 <- ifelse(is.na(qele_1_2) | nchar(qele_1_2) < 3, "", qele_1_2)
    
    qele_2_1 <- ifelse(is.null(DS_PreAPI_Baidu$CITY_LC[i]) | DS_PreAPI_Baidu$CITY_LC[i] == "NULL", "", DS_PreAPI_Baidu$CITY_LC[i]) 
    qele_2_1 <- ifelse(is.na(qele_2_1) | nchar(qele_2_1) < 2, "", qele_2_1)
    
    qele_2_2 <- ifelse(is.null(as.character(DS_PreAPI_Baidu$CITY[i])) | as.character(DS_PreAPI_Baidu$CITY[i]) == "NULL", "", as.character(DS_PreAPI_Baidu$CITY[i]))
    qele_2_2 <- ifelse(is.na(qele_2_2) | nchar(qele_2_2) < 4, "", qele_2_2)
    if(qele_2_2 != ""){
      DS_unq_City <- unique(DS_DistrictList[,3:4])
      qele_2_2 <- as.character(DS_unq_City$City_LC[which(unique(DS_unq_City$City) ==  qele_2_2)]) 
    }
    
    query <- ifelse(qele_1_1 == "", qele_1_2, qele_1_1)
    region <- ifelse(qele_2_1 == "", qele_2_2, qele_2_1)
    
    
    if(query != "" & region != ""){
      DS_BAPI_Return <- baidu.api(query, region)
      print(DS_BAPI_Return$PROPERTY_NAME)
      
      if(!is.null(DS_BAPI_Return))
      {
        DS_PreAPI_Baidu$PROPERTY_NAME_CN_BAPI[i] <- DS_BAPI_Return$PROPERTY_NAME[1]
        DS_PreAPI_Baidu$ADDRESS_LC_BAPI[i] <- DS_BAPI_Return$ADDRESS_LC[1]
        DS_PreAPI_Baidu$DISTRICT_NAME_LC_BAPI[i] <- DS_BAPI_Return$DISTRICT_NAME_LC[1]
        DS_PreAPI_Baidu$CITY_LC_BAPI[i] <- DS_BAPI_Return$CITY_LC[1]
        DS_PreAPI_Baidu$GEO_LAT_BAPI[i] <- DS_BAPI_Return$GEO_LAT[1]
        DS_PreAPI_Baidu$GEO_LONG_BAPI[i] <- DS_BAPI_Return$GEO_LONG[1]
      }
      
    }
  }
  
  return(DS_PreAPI_Baidu)
}


DS_PreAPI <- DS_Source_Fixed[1720:2200,]
DS_PostAPI_Baidu <- Data_Search_Baidu_CN(DS_PreAPI)
xlsx::write.xlsx(DS_PostAPI_Baidu,"output_post_baidu_api.xlsx",sheetName = "BaiduAPI_Output")


##############################Step 6: Combine output of Baidu & Google##############################
DS_PostAPI_Baidu_BK <- sourceDataImport('Andy\'s Data.xlsx','PostAPI_Baidu')
DS_PostAPI_Google_BK <- sourceDataImport('Andy\'s Data.xlsx','PostAPI_Google')

Combine_Process_LC <- function(P_PostAPI_Baidu,P_PostAPI_Google){
  #Standardize ADDRESS_LC
  P_PostAPI_Baidu$ADDRESS_LC <-SdProcess_Address_LC(P_PostAPI_Baidu$ADDRESS_LC)
  P_PostAPI_Google$ADDRESS_LC_GAPI <- SdProcess_Address_LC(P_PostAPI_Google$ADDRESS_LC_GAPI)
  
  DS_Combined <- P_PostAPI_Google
  
  names(DS_Combined)[-(length(colnames(DS_Combined)) - 6 :length(colnames(DS_Combined)))] <- c("PROPERTY_NAME_CN_Combine",
                                                                                               "ADDRESS_LC_Combine",
                                                                                               "DISTRICT_NAME_LC_Combine",
                                                                                               "CITY_LC_Combine",
                                                                                               "GEO_LAT_Combine",
                                                                                               "GEO_LONG_Combine")
  
  r_count <- nrow(DS_Combined)
  DS_Combined$API <- rep("Google",r_count)
  v_isValid_Address_Combine <- if_valid_Address_LC(DS_Combined$ADDRESS_LC_Combine)
  v_isValid_Address_BAPI <- if_valid_Address_LC(P_PostAPI_Baidu$ADDRESS_LC_BAPI)

  for(i in 1:r_count){
    Property_Name_Combine <- ifelse(is.na(as.character(DS_Combined$PROPERTY_NAME_CN_Combine[i])),"",as.character(DS_Combined$PROPERTY_NAME_CN_Combine[i])) 
    Address_Combine <- ifelse(is.na(DS_Combined$ADDRESS_LC_Combine[i]), "", DS_Combined$ADDRESS_LC_Combine[i])
    District_Combine <-  ifelse(is.na(as.character(DS_Combined$DISTRICT_NAME_LC_Combine[i])) , "" , as.character(DS_Combined$DISTRICT_NAME_LC_Combine[i]))
    City_Combine <- ifelse(is.na(as.character(DS_Combined$CITY_LC_Combine[i]))  , "", as.character(DS_Combined$CITY_LC_Combine[i]))
    Geo_Lat_Combine <- ifelse(is.na(as.character(DS_Combined$GEO_LAT_Combine[i])) , "", as.character(DS_Combined$GEO_LAT_Combine[i]))
    Geo_Long_Combine <- ifelse(is.na(as.character(DS_Combined$GEO_LONG_Combine[i])) , "", as.character(DS_Combined$GEO_LONG_Combine[i]))
    
    
    Property_Name_Baidu <- ifelse(is.na(as.character(P_PostAPI_Baidu$PROPERTY_NAME_CN_BAPI[i])) , "", as.character(P_PostAPI_Baidu$PROPERTY_NAME_CN_BAPI[i]))  
    Address_Baidu <- ifelse(is.na(P_PostAPI_Baidu$ADDRESS_LC_BAPI[i])  , "", P_PostAPI_Baidu$ADDRESS_LC_BAPI[i])
    District_Baidu <- ifelse(is.na(as.character(P_PostAPI_Baidu$DISTRICT_NAME_LC_BAPI[i])) , "", as.character(P_PostAPI_Baidu$DISTRICT_NAME_LC_BAPI[i])) 
    City_Baidu <- ifelse(is.na(as.character(P_PostAPI_Baidu$CITY_LC_BAPI[i])) , "", as.character(P_PostAPI_Baidu$CITY_LC_BAPI[i])) 
    Geo_Lat_Baidu <-ifelse(is.na(as.character(P_PostAPI_Baidu$GEO_LAT_BAPI[i]))  , "", as.character(P_PostAPI_Baidu$GEO_LAT_BAPI[i])) 
    Geo_Long_Baidu <- ifelse(is.na(as.character(P_PostAPI_Baidu$GEO_LONG_BAPI[i])) , "", as.character(P_PostAPI_Baidu$GEO_LONG_BAPI[i])) 
    
    toBeChanged <- FALSE
    
    if(Property_Name_Combine == "" & Address_Combine == ""){
      toBeChanged <- TRUE
    }else if(Property_Name_Combine == "" | Address_Combine == ""){
      if(Property_Name_Baidu != "" & Address_Baidu != ""){
        toBeChanged <- TRUE
      }
    }else if((v_isValid_Address_Combine[i] == "Invalid" | v_isValid_Address_Combine[i] == "NULL") & v_isValid_Address_BAPI[i] == "Valid"){
      toBeChanged <- TRUE
    } 
    
    if(toBeChanged){
      DS_Combined$PROPERTY_NAME_CN_Combine[i] <- Property_Name_Baidu
      DS_Combined$ADDRESS_LC_Combine[i] <- Address_Baidu
      DS_Combined$DISTRICT_NAME_LC_Combine[i] <- District_Baidu
      DS_Combined$CITY_LC_Combine[i] <- City_Baidu
      DS_Combined$GEO_LAT_Combine[i] <- Geo_Lat_Baidu
      DS_Combined$GEO_LONG_Combine[i] <- Geo_Long_Baidu
      DS_Combined$API[i] <- "Baidu"
    }
    
    DS_PostAPI_Combined <- cbind(DS_Combined, P_PostAPI_Google[14:19], P_PostAPI_Baidu[14:19])
  }
  return(DS_PostAPI_Combined)
}

P_PostAPI_Baidu <- DS_PostAPI_Baidu_BK
P_PostAPI_Google <- DS_PostAPI_Google_BK
DS_PostAPI_Combined <- Combine_Process_LC(P_PostAPI_Baidu,P_PostAPI_Google)


xlsx::write.xlsx(DS_PostAPI_Combined,"output_post_api_combined.xlsx",sheetName = "PostAPI_Combined")


##############################Step 7: Result Analysis##############################
#function: check if the value of address is valid or not
if_valid_Address_LC <- function(v_Address_LC){
  v_Address_LC <- as.character(v_Address_LC)
  v_Address_LC[is.na(v_Address_LC) == TRUE] <- ""
  
  v_Isvalid <- rep("TBC", length(v_Address_LC))
  #num_zh <- c("一","二","三","四","五","六","七","八","九","十")
  for(i in 1:length(v_Address_LC)){
    if(v_Address_LC[i] == "" | is.null(v_Address_LC[i]) | v_Address_LC[i] == "NULL"){
      v_Isvalid[i] <- "NULL"
    } else if(is.na(v_Address_LC[i])){
      v_Isvalid[i] <- "NULL"
    } else if(regexpr("([0-9]号|[一二三四五六七八九十零]号)",v_Address_LC[i]) < 0 | regexpr("(街|路|道|巷|院|条|里|胡同)",v_Address_LC[i]) < 0){
      v_Isvalid[i] <- "Invalid"
    }else {
      v_Isvalid[i] <- "Valid"
    }
  }
  return(v_Isvalid)
}

if_valid_Property_Name_LC <- function(v_Property_LC){
  v_Property_LC <- as.character(v_Property_LC)
  v_Property_LC[is.na(v_Property_LC)] <- ""
  
  v_Isvalid <- rep("TBC", length(v_Property_LC))
  
  for(i in 1:length(v_Property_LC)){
    #print(paste0(as.character(i),"%%",v_GeoCode[i]))
    if(v_Property_LC[i] == "NULL" | is.null(v_Property_LC[i]) | v_Property_LC[i] == ""){
      v_Isvalid[i] <- "NULL"
    }else if(nchar(v_Property_LC[i]) <3){
      v_Isvalid[i] <- "Invalid"
    }else if(if_valid_Address_LC(v_Property_LC[i]) == "Valid"){
      v_Isvalid[i] <- "Invalid"
    }else{
      v_Isvalid[i] <- "Valid"
    }
  }
  return(v_Isvalid)
}

if_valid_GeoCode <- function(v_GeoCode){
  v_GeoCode <- as.character(v_GeoCode)
  v_GeoCode[is.na(v_GeoCode)] <- ""
  
  v_Isvalid <- rep("TBC", length(v_GeoCode))
  
  for(i in 1:length(v_GeoCode)){
    #print(paste0(as.character(i),"%%",v_GeoCode[i]))
    if(v_GeoCode[i] == "NULL" | is.null(v_GeoCode[i]) | v_GeoCode[i] == ""){
      v_Isvalid[i] <- "NULL"
    }else if(v_GeoCode[i] == "0.0" | v_GeoCode[i] == "0"){
      v_Isvalid[i] <- "Invalid"
    }else if(regexpr("[0-9]",v_GeoCode[i]) < 0){
      v_Isvalid[i] <- "Invalid"
    }else{
      v_Isvalid[i] <- "Valid"
    }
  }
  return(v_Isvalid)
}

ResultAnalysis_LC <- function(Dataset, 
                              source.columns = c(7,8,12), 
                              result.columns = c(14,15,18)){
  Dataset_Analysed <- Dataset
  
  #Property
  v_Remark_Source_Property <- if_valid_Property_Name_LC(as.character(Dataset_Analysed[,source.columns[1]]))
  Dataset_Analysed$Remark_Source_Property <- v_Remark_Source_Property
  
  v_Remark_Output_Property <- if_valid_Property_Name_LC(as.character(Dataset_Analysed[,result.columns[1]]))
  Dataset_Analysed$Remark_Output_Property <- v_Remark_Output_Property
  
  #Address
  v_Remark_Source_Address <- if_valid_Address_LC(as.character(Dataset_Analysed[,source.columns[2]]))
  Dataset_Analysed$Remark_Source_Address <- v_Remark_Source_Address
  
  v_Remark_Output_Address <- if_valid_Address_LC(as.character(Dataset_Analysed[,result.columns[2]]))
  Dataset_Analysed$Remark_Output_Address <- v_Remark_Output_Address
  
  #Geocode
  v_Remark_Source_Geocode<- if_valid_GeoCode(as.character(Dataset_Analysed[,source.columns[3]]))
  Dataset_Analysed$Remark_Source_Geocode <- v_Remark_Source_Geocode
  
  v_Remark_Output_Geocode <- if_valid_GeoCode(as.character(Dataset_Analysed[,result.columns[3]]))
  Dataset_Analysed$Remark_Output_Geocode <- v_Remark_Output_Geocode
  
  return(list(Dataset_Analysed = Dataset_Analysed,
              data.frame(v_Remark_Source_Property = v_Remark_Source_Property,
                         v_Remark_Output_Property = v_Remark_Output_Property,
                         v_Remark_Source_Address = v_Remark_Source_Address,
                         v_Remark_Output_Address = v_Remark_Output_Address,
                         v_Remark_Source_Geocode = v_Remark_Source_Geocode,
                         v_Remark_Output_Geocode = v_Remark_Output_Geocode,
                         stringsAsFactors = FALSE)))
}


##############################Step 7.1:API Result Analysis##############################
Dataset_API <- DS_PostAPI_Combined
list_Analysed_API <- ResultAnalysis_LC(Dataset_API)
DS_Analyed_API <- list_Analysed_API[[1]]
xlsx::write.xlsx(DS_Analyed_API,"output_result_analysis.xlsx",sheetName = "PostAPIResult_Analysis")
DS_Analyed_API_Remark <- list_Analysed_API[[2]]
View(DS_Analyed_API_Remark)

View(DS_Analyed_API_Remark)
#Statistics - PROPERTY_NAME_LC
summarise(group_by(DS_Analyed_API_Remark,v_Remark_Source_Property),
          count = n())

summarise(group_by(DS_Analyed_API_Remark,v_Remark_Output_Property),
          count = n())



#Statistics - ADDRESS_LC
summarise(group_by(DS_Analyed_API_Remark,v_Remark_Source_Address),
          count = n())
summarise(group_by(DS_Analyed_API_Remark,v_Remark_Output_Address),
          count = n())

#Statistics - GeoCode
summarise(group_by(DS_Analyed_API_Remark,v_Remark_Source_Geocode),
          count = n())
summarise(group_by(DS_Analyed_API_Remark,v_Remark_Output_Geocode),
          count = n())



##############################Step 7.2:AD Result Analysis##############################
DS_Source_AD <- sourceDataImport('IT_TestData5_China_Test_02_11_2016.xlsx','Conf EN ANDY')

ad_ncount <- nrow(df_temp_output)
CLIENT_PROPERTY_CODE <- c("",ad_ncount)
PROPERTY_NAME_CN <- c("",ad_ncount)
ADDRESS_LC <- c("",ad_ncount)
DISTRICT_NAME_LC <- c("",ad_ncount)
CITY_LC <- c("",ad_ncount)
COUNTRY_LC <- c("",ad_ncount)
GEO_LAT <- c("",ad_ncount)
GEO_LONG <- c("",ad_ncount)

PROPERTY_NAME_CN_AD <- c("",ad_ncount)
ADDRESS_LC_AD <- c("",ad_ncount)
DISTRICT_NAME_LC_AD <- c("",ad_ncount)
CITY_LC_AD <- c("",ad_ncount)
COUNTRY_LC_AD <- c("",ad_ncount)
GEO_LAT_AD <- c("",ad_ncount)
GEO_LONG_AD <- c("",ad_ncount)


for(i in 1:ad_ncount){
  which_num <- which(DS_Source_AD$SRC_SYS_ID == df_temp_output$SOURCE_PROPERTY_ID[i])
  CLIENT_PROPERTY_CODE[i] <- DS_Source_AD$SRC_SYS_ID[which_num]
  PROPERTY_NAME_CN[i] <- DS_Source_AD$PROPERTY_NAME[which_num]
  ADDRESS_LC[i] <- DS_Source_AD$ADDRESS_1[which_num]
  DISTRICT_NAME_LC[i] <- ""
  CITY_LC[i] <- DS_Source_AD$CITY[which_num]
  COUNTRY_LC[i] <- DS_Source_AD$COUNTRY[which_num]
  GEO_LAT[i] <- ""
  GEO_LONG[i] <- ""
  
  PROPERTY_NAME_CN_AD[i] <- DS_Source_AD$IDQ_BUILDING_COMPLETE1[which_num]
  ADDRESS_LC_AD[i] <- DS_Source_AD$IDQ_STREET_COMPLETE_WITH_NUMBER1[which_num]
  DISTRICT_NAME_LC_AD[i] <- ""
  CITY_LC_AD[i] <- DS_Source_AD$IDQ_CITY[which_num]
  COUNTRY_LC_AD[i] <- DS_Source_AD$IDQ_COUNTRY[which_num]
  GEO_LAT_AD[i] <- ""
  GEO_LONG_AD[i] <- ""
}

DS_PostAD <- data.frame(CLIENT_PROPERTY_CODE  = CLIENT_PROPERTY_CODE,
                        PROPERTY_NAME_CN  = PROPERTY_NAME_CN,
                        ADDRESS_LC  = ADDRESS_LC,
                        DISTRICT_NAME_LC  = DISTRICT_NAME_LC,
                        CITY_LC  = CITY_LC,
                        COUNTRY_LC  = COUNTRY_LC,
                        GEO_LAT  = GEO_LAT,
                        GEO_LONG  = GEO_LONG,
                        PROPERTY_NAME_CN_AD  = PROPERTY_NAME_CN_AD,
                        ADDRESS_LC_AD  = ADDRESS_LC_AD,
                        DISTRICT_NAME_LC_AD  = DISTRICT_NAME_LC_AD,
                        CITY_LC_AD  = CITY_LC_AD,
                        COUNTRY_LC_AD  = COUNTRY_LC_AD,
                        GEO_LAT_AD  = GEO_LAT_AD,
                        GEO_LONG_AD  = GEO_LONG_AD,
                        stringsAsFactors = FALSE)

View(DS_PostAD)

xlsx::write.xlsx(DS_PostAD,"output_PostAD.xlsx",sheetName = "PostAD")

#Start analysis
Dataset_AD <- DS_PostAD
list_Analysed_AD <- ResultAnalysis_LC(Dataset_AD,source.columns = c(2,3,7), result.columns = c(9,10,14))
DS_Analyed_AD <- list_Analysed_AD[[1]]
xlsx::write.xlsx(DS_Analyed_AD,"output_result_analysis.xlsx",sheetName = "PostADResult_Analysis")
DS_Analyed_AD_Remark <- list_Analysed_AD[[2]]
View(DS_Analyed_AD_Remark)


#Statistics - PROPERTY_NAME_LC
summarise(group_by(DS_Analyed_AD_Remark,v_Remark_Source_Property),
          count = n())
summarise(group_by(DS_Analyed_AD_Remark,v_Remark_Output_Property),
          count = n())


#Statistics - ADDRESS_LC
summarise(group_by(DS_Analyed_AD_Remark,v_Remark_Source_Address),
          count = n())
summarise(group_by(DS_Analyed_AD_Remark,v_Remark_Output_Address),
          count = n())

#Statistics - GeoCode
summarise(group_by(DS_Analyed_AD_Remark,v_Remark_Source_Geocode),
          count = n())
summarise(group_by(DS_Analyed_AD_Remark,v_Remark_Output_Geocode),
          count = n())








































































