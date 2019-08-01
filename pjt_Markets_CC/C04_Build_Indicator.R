########################################################Programm Start########################################################
setwd("C:/YuanLe/R/RWkDir/06. Markets_CC")
#ls()
#rm(list=ls())
#install.packages("lubridate")

options(stringsAsFactors = FALSE)
options(java.parameters = "-Xmx4g")
options(encoding = "utf-8")

library(readxl)            ##import data from Excel
library(plyr)
library(dplyr)             ##data manipulation, e.g. select, filter
library(stringdist)        ##string distance calculation and approximate string matching
library(RCurl)             ##use for processing Google Geocoding API and JSON file
library(RJSONIO)           ##use for processing Google Geocoding API and JSON file
library(xlsx)
library(geosphere)
library(rvest)
library(httr)
library(RSelenium)
library(R.utils)
library(lubridate)
library(RODBC)
library(stringr)
library(ggvis)
library(knitr)




########################################################Global Variables########################################################
uf_xlsxDataImport <- function(xlsx_file,spreedsheet){
  df_xlsx <- read.xlsx(xlsx_file,sheetName = spreedsheet,header=TRUE,encoding = "UTF-8")
  return(df_xlsx)
}
uf_xlsxDataExport <- function(p_df, xlsx_file, spreedsheet, append = FALSE){
  xlsx::write.xlsx(p_df,xlsx_file,sheetName = spreedsheet, append = append)
}

gv_Defualt_DBServer <- "EDW_UAT"
gv_Defualt_DBServer_Login <- "EDWAPACUser_RW"
gv_Defualt_DBServer_pwd <- "EDw@pacRead&write"
gv_table_CRU5 <- "CN_CD_CC.Company_Recruit_Url_51job"
gv_table_CI5 <- "CN_CD_CC.Companies_Infor_51Job"
gv_cities <- c("成都","重庆")
attr(gv_cities,"city_en") <- c("Chengdu",  "Chongqing")
attr(gv_cities,"alias1") <- c("蜀",  "渝")
attr(gv_cities,"alias2") <- c("川",  "渝")
attr(gv_cities,"phoneCode") <- c("028","023")

gv_china_division_list <- uf_xlsxDataImport('./ReferData/China_Cities.xlsx','DivisionList_China')

gv_BDAPI_Key <- "NdgFqhKyiqPksC2pLfsGsWps15wzRsES"
gv_geo_diff_search <- 0.005
gv_method_strSim <- "lcs"

########################################################Global Function########################################################
#p_city <- "成都"
#p_address <- "四川省成都市高新区府城大道西段399号天府新谷8栋2单元9层"
#df.BuildList <- uf_Retrieve_Build_List(p_city)
#uf_Build_Indicator(p_address,p_city,p_BuildList)
uf_Build_Indicator <- function(p_address,p_city,p_BuildList){
  op_property <- c("","","")
  attr(op_property,"name") <- c("building","lat","long")
  
  p_BuildList <- df.BuildList
  p_BuildList$Property <- iconv(p_BuildList$Property,"gb2312","UTF-8")
  count_p <- nrow(p_BuildList)
  
  v_address <- ifelse(is.na(p_address),"",p_address)  
  if(nchar(v_address) > 3){
    v_address <- uf_sdProcess_ADDR_CN_Specified(v_address)
    v_geocode <- baidu.api.geocoder(v_address,p_city,gv_BDAPI_Key)
    if(any(v_geocode == "")){
      v_geocode <- baidu.api.detailSearch(baidu.api.poiSearch(v_address,p_city,gv_BDAPI_Key),p_city,gv_BDAPI_Key)[c(5,6)]
    }
    if(all(v_geocode != "")){
      #如果地址的geocode能找到，用geocode匹配
      op_property[2] <- v_geocode[1]
      op_property[3] <- v_geocode[2]
      lat <- as.numeric(v_geocode[1])
      long <- as.numeric(v_geocode[2])
      rlt_matchs <- dplyr::filter(p_BuildList, 
                                  Latitude > lat - gv_geo_diff_search, 
                                  Latitude < lat + gv_geo_diff_search,
                                  Lontitude > long - gv_geo_diff_search, 
                                  Lontitude < long + gv_geo_diff_search)
      count_matchs <- nrow(rlt_matchs)
      v_str_dist <- rep(0,count_matchs)
      if(count_matchs > 0){
        for(k in 1:count_matchs){
          v_property <- rlt_matchs$Property[k]
          v_str_dist[k] <- stringsim(v_property,v_address,method = gv_method_strSim)
        }
        if(any(v_str_dist > 0.2)){
          num <- which(v_str_dist == max(v_str_dist))[1]
          op_property[1] <- rlt_matchs$Property[num] 
          op_property[2] <- rlt_matchs$Latitude[num]
          op_property[3] <- rlt_matchs$Lontitude[num]
        }
      }
    }
  }
  
  return(op_property)
}


uf_Retrieve_Build_List <- function(p_city){
  Query <- paste0("select ID,Property, Address, City, Latitude, Lontitude from CN_CD_CC.Properties_Info where Is_Current = 1 and City = N'", 
                  iconv(p_city, "UTF-8", "gb2312"),"'")
  df.db_return <- uf_execute_Query("DEFAULT",Query,20)
  return(df.db_return)
}

#DBConn <- "DEFAULT"
#timeouts <- 20
#Query <- "select ID,Property, Address, City, Latitude, Lontitude from CN_CD_CC.Properties_Info where Is_Current = 1 and City = N'成都'"
uf_execute_Query <- function(DBConn,Query,timeouts){
  Conn <- NA
  df.db_return <- NA
  if(DBConn == "DEFAULT"){
    Conn <- tryCatch({
      evalWithTimeout({ odbcConnect(gv_Defualt_DBServer,uid = gv_Defualt_DBServer_Login, pwd = gv_Defualt_DBServer_pwd) }, timeout = timeouts)
    },warnings = function(w){
      ;
      NA
    },error = function(e){
      ;
      NA})
  }
  if(!is.na(Conn)){
    df.db_return_p <- tryCatch({
      evalWithTimeout({ sqlQuery(Conn,Query)
      }, timeout = timeouts)
    },warnings = function(w){
      ;
      NA
    },error = function(e){
      ;
      NA})
    odbcClose(Conn)
    if(class(df.db_return_p) == "data.frame"){
      df.db_return <- df.db_return_p
    }
  }
  
  return(df.db_return)
}


#p_address_cn <- p_address
uf_sdProcess_ADDR_CN_Specified <- function(p_address_cn){
  v_address_cn <- p_address_cn
  v_address_cn <- uf_null_fix(v_address_cn)
  
  #Rule No.1 - Remove all space(' ')
  v_address_cn <- gsub("\\s{1,}", "", v_address_cn)
  
  #Rule No.2 - Remove Country, Province, City, District
  v_address_cn <- gsub("^中国", "", v_address_cn)
  
  provList <- unique(gv_china_division_list$PROVINCE_LC)
  for(prn in provList){
    pattern_prn <- paste0('^', prn, '[省|市]')
    v_address_cn <- gsub(pattern_prn, '' , v_address_cn)
  }
  
  cityList <- unique(gv_china_division_list$CITY_LC)
  for(ct in cityList){
    pattern_city <- paste0('^', ct, '(市)')
    v_address_cn <- gsub(pattern_city, '' , v_address_cn)
  }
  
  districtList <- unique(gv_china_division_list$DISTRICT_LC)
  for(ds in districtList){
    pattern_dist <- paste0('^', ds, '(区)')
    v_address_cn <- gsub(pattern_dist, '' , v_address_cn)
  }
  
  #Rule No.3 - Add ‘号’at the end if last character is digital number;
  pattern_hao <- '[0-9]$'   
  for (i in 1:length(v_address_cn)) {
    if(regexpr(pattern_hao, v_address_cn[i]) > 0){
      v_address_cn[i] <- paste0(v_address_cn[i], "号")
    }
  }
  
  #Rule No.4 - Correct street number, e.g. 17-14 to 14-17; 17-17 to 17;
  #v_address_cn <- uf_sdProcess_StreetNum(v_address_cn, "zh-CN")
  
  #Rule No.5 - Remove words after pattern '[0-9]号'
  #pattern_sufix <- "[0-9]号.*"
  #num_1 <- regexpr(pattern_sufix,v_address_cn)
  #v_address_cn <- ifelse(num_1 > 0, substr(v_address_cn,1, num_1 + 1), v_address_cn) 
  
  out_address_cn <- v_address_cn
  return(out_address_cn)
}

uf_null_fix <- function(p_input){
  v_input <- p_input
  
  in_Count <- length(v_input)
  v_input <- ifelse(is.na(v_input), "", v_input)
  v_input <- ifelse(v_input == "NULL" | v_input == "NA" | v_input == " " | v_input == "N/A" | v_input == "Null", "", v_input)
  
  out_input <- v_input
  return(out_input)
}


################Function - baidu.api.poiSearch
baidu.api.poiSearch <- function(query,region,key,tag="房地产,酒店,写字楼",scope=2,page_size=1,output="json"){
  uid = ""
  
  root <- "http://api.map.baidu.com/place/v2/search"
  u <- paste0(root, "?q=", query, "&region=",region,"&city_limit=true","&tag=", tag, "&scope=",scope,"&page_size=",page_size,"&output=",output,"&ak=", key)
  doc <- getURL(u)
  
  if(doc != "" & !is.null(doc)){
    x <- fromJSON(doc,simplify = FALSE)
    if(length(x$results) > 0){
      if(x$status==0 & !is.null(x$results[[1]]$uid)){
        results <- x$results
        result <- results[[1]]
        uid <- result$uid
      }
    }
  }else{
    print("Baidu can not find anything according to your input!")
  }
  return(uid)
}


################Function - baidu.api.detailSearch
#uid <- "7d5f3a0a60d160352cb90873"
#region <- "北京"
#key <- "NdgFqhKyiqPksC2pLfsGsWps15wzRsES"
baidu.api.detailSearch <- function(uid,region,key,output="json",scope=2){
  #####Define output
  v_output <- rep("",11)
  attr(v_output, "Elements") <- c("PROPERTY_NAME", "ADDRESS", "DISTRICT", "CITY", "GEO_LAT", "GEO_LONG", "postal_code", "phone_number","place_id","gmap_url","website")
  
  if(uid != ""){
    root <- "http://api.map.baidu.com/place/v2/detail?"
    u <- paste0(root, "uid=", uid, "&output=",output,"&scope=",scope,"&ak=", key)
    doc <- getURL(URLencode(u))
    
    if(doc != "" & !is.null(doc)){
      x <- fromJSON(doc,simplify = FALSE)
      if(x$status==0 & x$result$uid == uid){
        result <- x$result
        place_id <- result$uid
        property_name <- ifelse(is.null(result$name), "", result$name) 
        address <- ifelse(is.null(result$address), "", result$address) 
        website <- ifelse(is.null(result$detail_info$detail_url), "", result$detail_info$detail_url)  
        phone_number <- ifelse(is.null(result$telephone), "", result$telephone) 
        geo_lat <- ifelse(is.null(result$location$lat), "", result$location$lat)
        geo_long <- ifelse(is.null(result$location$lng), "", result$location$lng)
        gmap_url <- ""
        district <- ""
        postal_code <- ""
        
        v_output[which(attr(v_output,"Elements") == "PROPERTY_NAME")] <- property_name
        v_output[which(attr(v_output,"Elements") == "GEO_LAT")] <- geo_lat
        v_output[which(attr(v_output,"Elements") == "GEO_LONG")] <- geo_long
        v_output[which(attr(v_output,"Elements") == "phone_number")] <- phone_number
        v_output[which(attr(v_output,"Elements") == "place_id")] <- place_id
        v_output[which(attr(v_output,"Elements") == "gmap_url")] <- gmap_url
        v_output[which(attr(v_output,"Elements") == "website")] <- website
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- address
        v_output[which(attr(v_output,"Elements") == "DISTRICT")] <- district
        v_output[which(attr(v_output,"Elements") == "CITY")] <- region
        v_output[which(attr(v_output,"Elements") == "postal_code")] <- postal_code
        
      }
    }else{
      print("Google can not find anything according to your input!")
    }
  }
  return(v_output)
  
}


################Function - baidu.api.geocoder
baidu.api.geocoder <- function(address,city,key,output="json"){
  location = ""
  
  root <- "http://api.map.baidu.com/geocoder/v2/?"
  u <- paste0(root, "&address=", address, "&city=",city,"&output=",output,"&ak=", key)
  doc <- getURL(u)
  
  if(doc != "" & !is.null(doc)){
    x <- fromJSON(doc,simplify = FALSE)
    if(length(x$result) > 0){
      if(x$status==0){
        geo_lat <- x$result$location$lat
        geo_long <- x$result$location$lng
        
        location <- c(as.character(geo_lat),as.character(geo_long))
      }
    }
  }else{
    print("Google can not find anything according to your input!")
  }
  return(location)
}




















































