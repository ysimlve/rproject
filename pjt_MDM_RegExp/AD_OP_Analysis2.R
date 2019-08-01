########################################################1. 开始########################################################
setwd("C:/YuanLe/R/RWkDir/01. MDM_RegExp")
#ls()
#rm(list=ls())
#install.packages("Hmisc")

options(stringsAsFactors = FALSE)
options(java.parameters = "-Xmx4g")

library(readxl)
library(xlsx)
library(dplyr)
library(Kendall)
library(Hmisc)

uf_xlsxDataImport <- function(xlsx_file,spreedsheet){
  df_xlsx <- read.xlsx(xlsx_file,sheetName = spreedsheet,header=TRUE,encoding = "UTF-8")
  return(df_xlsx)
}
uf_xlsxDataExport <- function(p_df, xlsx_file, spreedsheet, append = FALSE){
  xlsx::write.xlsx(p_df,xlsx_file,sheetName = spreedsheet, append = append)
}

df.address.element <- uf_xlsxDataImport("./Dalton_Test_Output/ReferenceData.xlsx","Address_Element")

gv_geocore_fields_en <- c("SRC_SYS_ID","SRC_PROPERTY_NAME","SRC_ADDRESS_1","SRC_LOCALITY_1" ,"SRC_LOCALITY_2","SRC_POSTCODE" ,"SRC_COUNTRY")


########################################################2. 标准化函数########################################################
uf_null_fix <- function(p_input){
  v_input <- p_input
  
  in_Count <- length(v_input)
  v_input <- ifelse(is.na(v_input), "", v_input)
  v_input <- ifelse(v_input == "NULL" | v_input == "NA" | v_input == " " | v_input == "N/A" | v_input == "Null", "", v_input)
  
  out_input <- v_input
  return(out_input)
}
uf_sdProcess_Common_Eng <- function(p_input){
  v_input <- p_input
  
  #Common Rule No.1 - The first character of any separate word should be in UPPER case;
  for(lc in letters){
    pattern_UC <- paste0('(\\s|^)(',lc,')')
    v_input <- gsub(pattern_UC, paste0(' ', toupper(lc)) , v_input)
  }
  
  #Common Rule No.2 - Remove all tone sign;
  for(i in 1:length(gv_toneSign_fixed)){
    pattern_ts <- gv_toneSign[i]
    v_input <- gsub(pattern_ts,gv_toneSign_fixed[i],v_input)
  }
  
  #Common Rule No.3 -  Two or more space(‘ ’) should be cut to one;
  pattern_tp <- "\\s{2,}"
  v_input <- gsub(pattern_tp, " " , v_input)
  
  #Common Rule No.4 - Remove any space(‘ ‘) at the end or in front;
  v_input <- gsub("^\\s{1,}", "", v_input)
  v_input <- gsub("\\s{1,}$", "", v_input)
  
  
  out_input <- v_input
  return(out_input)
}
uf_sdProcess_StreetNum <- function(p_address,language = "zh-CN"){
  v_address <- p_address
  
  pattern_NoRng <- '[0-9]{1,}(-)[0-9]{1,}'
  if(language == "zh-CN"){
    pattern_NoRng <- paste0(pattern_NoRng,"号")
  }else if(language == "en"){
    pattern_NoRng <- paste0("^",pattern_NoRng,"\\s")
  }else{
    print("Please input correct language code, en or zh-CN!")
    return()
  }
  
  vec_Addr <- v_address
  match_start <- regexpr(pattern_NoRng, vec_Addr)
  match_len <- attr(match_start, "match.length")
  vec_Addr_rng <- gsub(' ', '', substr(vec_Addr, match_start, match_len + match_start - 1))
  if(language == "zh-CN"){
    vec_Addr_rng <- substr(vec_Addr, match_start, match_len + match_start - 2)
  }
  
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
  
  v_address <- vec_Addr_fixed
  
  out_address <- v_address
  return(out_address)
}
uf_sdProcess_PN <- function(p_propertyName){
  v_propertyName <- p_propertyName
  v_propertyName <- uf_null_fix(v_propertyName)
  
  #Rule No.1 - Replace any comma(,) by space(‘ ‘);
  pattern_comma <- ','
  v_propertyName <- gsub(pattern_comma, " " , v_propertyName)
  
  #Rule No.2 - Abbreviation translation;
  for(i in 1:length(gv_property_type_abbr)){
    pattern_Abbr <- paste0("(\\s|^)(",gv_property_type_abbr[i],")(\\s|$)")
    v_propertyName <- gsub(pattern_Abbr, paste0(" ", gv_property_type_full[i], ' ') , v_propertyName)
  }
  
  #Common Rules
  v_propertyName <- uf_sdProcess_Common_Eng(v_propertyName)
  
  out_propertyName <- v_propertyName
  return(out_propertyName)
}
uf_sdProcess_ADDR <- function(p_address){
  v_address <- p_address
  v_address <- uf_null_fix(v_address)
  
  #Rule No1. - Replace any comma(, and .) by space(‘ ‘);
  pattern_comma <- "[,\\.]"
  v_address <- gsub(pattern_comma, ' ' , v_address)
  
  #Rule No.2 - District, City, Province and Country should be removed;
  pattern_cntr <- '\\s[Cc](hina)$'
  v_address <- gsub(pattern_cntr, '' , v_address)
  
  provList <- unique(gv_china_division_list$PROVIENCE)
  for(prn in provList){
    pattern_prn <- paste0('\\s(', prn, ')(\\s([Pp][Rr][Oo][Vv][Ii][Nn][Cc][Ee]|[Ss][Hh][Ee][Nn][Gg]))?$')
    v_address <- gsub(pattern_prn, '' , v_address)
  }
  
  cityList <- unique(gv_china_division_list$CITY)
  for(ct in cityList){
    pattern_city <- paste0('\\s(', ct, ')(\\s([Cc][Ii][Tt][Yy]|[Ss][Hh][Ii]))?$')
    v_address <- gsub(pattern_city, '' , v_address)
  }
  
  districtList <- unique(gv_china_division_list$DISTRICT)
  for(ds in districtList){
    pattern_dist <- paste0('\\s(', ds, ')\\s?([Dd]istrict|DISTRICT|[Qq][Uu])?$')
    v_address <- gsub(pattern_dist, '' , v_address)
  }
  
  #Rule No.3 - 'NO '(case-insensitive) in front should be removed;
  v_address <- gsub("^\\s{1,}", "", v_address)
  pattern_NO <- '^N[Oo][^A-Za-z0-9]'
  v_address <- gsub(pattern_NO, "" ,  v_address)
  
  #Rule No.4 - Correct street number, e.g. 17-14 to 14-17; 17-17 to 17;
  v_address <- uf_sdProcess_StreetNum(v_address)
  
  #Rule No.5 - Abbreviation translation;
  for(i in 1:length(gv_address_type_full)){
    pattern_abbr <- paste0("(^|\\s)", gv_address_type_abbr[i], "(\\s|$|\\.?)")
    v_address <- gsub(pattern_abbr,paste0(" ", gv_address_type_full[i], " "),v_address)
  }
  
  #Rule No.- Remove charactor '号'
  v_address <- gsub("号", "", v_address)
  
  #Common Rules
  v_address <- uf_sdProcess_Common_Eng(v_address)
  
  out_address <- v_address
  return(out_address)
}
uf_sdProcess_DISTRICT <- function(p_district){
  v_district <- p_district
  v_district <- uf_null_fix(v_district)
  
  #Rule No.1 - 'District'(case-insensitive) should be removed;
  pattern_disrm <- "\\s(DISTRICT|[Dd]istrict|[Qq][Uu])"
  v_district <- gsub(pattern_disrm, "", v_district)
  
  #Rule No.2 - District Name conversion;
  for(i in 1:length(gv_district_sub)){
    pattern_disfrom <- gv_district_old[i]
    v_district <- gsub(pattern_disfrom, gv_district_sub[i], v_district, ignore.case = TRUE)
  }
  
  #Rule No.3 - The first character should be in UPPER case;
  for(lc in letters){
    pattern_UC <- paste0('(\\s|^)(',lc,')')
    v_district <- gsub(pattern_UC, paste0(' ', toupper(lc)) , v_district)
  }
  
  #Rule No.4 - Remove all tone sign;
  for(i in 1:length(gv_toneSign_fixed)){
    pattern_ts <- gv_toneSign[i]
    v_district <- gsub(pattern_ts,gv_toneSign_fixed[i],v_district)
  }
  for(i in 1:length(gv_toneSign_fixed)){
    #print(gv_toneSign[i])
  }
  
  
  #Rule No.5 - Remove all space character(‘ ‘);
  pattern_tp <- "\\s{1,}"
  v_district <- gsub(pattern_tp, "", v_district)
  
  #Rule No.6 - Approximate matching with standardized district list; 
  #v_sd_district <- unique(gv_china_division_list$DISTRICT)
  #index_district <- amatch(v_district,v_sd_district,maxDist = 1, nomatch = 0)
  #v_district <- ifelse(index_district == 0, "", v_sd_district[index_district]) 
  
  out_district <-  v_district
  return(out_district)
}
uf_sdProcess_CITY <- function(p_city){
  v_city <- p_city
  v_city <- uf_null_fix(v_city)
  
  #Rule No.1 'Shi’ or ‘City’(case-insensitive) should be removed;
  pattern_cityrm <- "\\s([Cc][Ii][Tt][Yy]|[Ss][Hh][Ii])"
  v_city <- gsub(pattern_cityrm, "", v_city)
  
  #Rule No.2 - The first character should be in UPPER case;
  for(lc in letters){
    pattern_UC <- paste0('(\\s|^)(',lc,')')
    v_city <- gsub(pattern_UC, paste0(' ', toupper(lc)) , v_city)
  }
  
  #Rule No.3 - Remove all tone sign;
  for(i in 1:length(gv_toneSign_fixed)){
    pattern_ts <- gv_toneSign[i]
    v_city <- gsub(pattern_ts,gv_toneSign_fixed[i],v_city)
  }
  
  #Rule No.4 - Remove all space character(‘ ‘);
  pattern_tp <- "\\s{1,}"
  v_city <- gsub(pattern_tp, "", v_city)
  
  out_city <- v_city
  return(out_city)
}
uf_sdProcess_PROVINCE <- function(p_province, p_city_sd){
  v_province <- p_province
  v_province <- uf_null_fix(v_province)
  
  #Rule No.1 'Sheng’ or ‘Province’(case-insensitive) should be removed;
  pattern_provincerm <- "\\s([Pp][Rr][Oo][Vv][Ii][Nn][Cc][Ee]|[Ss][Hh][Ee][Nn][Gg])"
  v_province <- gsub(pattern_provincerm, "", v_province)
  
  #Rule No.2 - The first character should be in UPPER case;
  for(lc in letters){
    pattern_UC <- paste0('(\\s|^)(',lc,')')
    v_province <- gsub(pattern_UC, paste0(' ', toupper(lc)) , v_province)
  }
  
  #Rule No.3 - Remove all tone sign;
  for(i in 1:length(gv_toneSign_fixed)){
    pattern_ts <- gv_toneSign[i]
    v_province <- gsub(pattern_ts,gv_toneSign_fixed[i],v_province)
  }
  
  #Rule No.4 - Remove all space character(‘ ‘);
  pattern_tp <- "\\s{1,}"
  v_province <- gsub(pattern_tp, "", v_province)
  
  #Rule No.5 - If PROVINCE_NAME is NULL or invalid, fix it according to CITY_NAME
  df_province_sc <- data.frame(PROVINCE_SC = v_province,CITY_SC = p_city_sd, stringsAsFactors = FALSE)
  ds_province_sd <- unique(gv_china_division_list[,c("CITY","PROVINCE")]) 
  
  ind_mch <- match(df_province_sc$CITY_SC, ds_province_sd$CITY)
  df_province_sc[ , "PROVINCE_SC"] <- ds_province_sd[ind_mch, "PROVINCE"]
  v_province <- df_province_sc[ , "PROVINCE_SC"]
  
  out_province <- ifelse(is.na(v_province), "", v_province)
  return(out_province)
}
uf_sdProcess_PN_CN <- function(p_property_name_cn){
  v_property_name_cn <- p_property_name_cn
  v_property_name_cn <- uf_null_fix(v_property_name_cn)
  
  #Rule No.1 - Remove all space(' ')
  v_property_name_cn <- gsub("\\s{1,}", "", v_property_name_cn)
  
  out_property_name_cn <- v_property_name_cn
  return(out_property_name_cn)
}
uf_sdProcess_ADDR_CN <- function(p_address_cn){
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
  v_address_cn <- uf_sdProcess_StreetNum(v_address_cn, "zh-CN")
  
  #Rule No.5 - Remove words after pattern '[0-9]号'
  pattern_sufix <- "[0-9]号.*"
  num_1 <- regexpr(pattern_sufix,v_address_cn)
  v_address_cn <- ifelse(num_1 > 0, substr(v_address_cn,1, num_1 + 1), v_address_cn) 
  
  out_address_cn <- v_address_cn
  return(out_address_cn)
}
uf_sdProcess_DISTRICT_CN <- function(p_district_cn){
  v_district_cn <- p_district_cn
  v_district_cn <- uf_null_fix(v_district_cn)
  
  #Rule No.1 - Remove all space(‘ ’)
  v_district_cn <- gsub("\\s{1,}", "",v_district_cn)
  
  #Rule No.2 - '区' at the end should be removed;
  v_district_cn <- gsub("区$", "", v_district_cn)
  
  #Rule No.3 - District Name conversion;
  for(i in 1:length(gv_district_lc_sub)){
    pattern_disfrom <- gv_district_lc_old[i]
    v_district_cn <- gsub(pattern_disfrom, gv_district_lc_sub[i], v_district_cn, ignore.case = TRUE)
  }
  
  out_district_cn <- v_district_cn 
  return(out_district_cn)
}
uf_sdProcess_CITY_CN <- function(p_city_cn){
  v_city_cn <- p_city_cn
  v_city_cn <- uf_null_fix(v_city_cn)
  
  #Rule No.1 - Remove all space(‘ ’)
  v_city_cn <- gsub("\\s{1,}", "", v_city_cn)
  
  #Rule No.2 - ‘市’at the end should be removed;
  v_city_cn <- gsub("市$", "", v_city_cn)
  
  out_city_cn <- v_city_cn 
  return(out_city_cn)
}
uf_sdProcess_PROVINCE_CN <- function(p_province_cn, p_city_cn_sd){
  v_province_cn <- p_province_cn
  v_province_cn <- uf_null_fix(v_province_cn)
  
  #Rule No.1 Remove all space(‘ ’)
  v_province_cn <- gsub("\\s{1,}", "", v_province_cn)
  
  #Rule No.2 - '省’or ‘市’at the end should be removed;
  v_province_cn <- gsub("[省|市]$", "", v_province_cn)
  
  #Rule No.3 - If PROVINCE_NAME_LC is NULL or invalid, fix it according to CITY_NAME_LC
  df_province_sc <- data.frame(PROVINCE_SC = v_province_cn,CITY_SC = p_city_cn_sd, stringsAsFactors = FALSE)
  ds_province_sd <- unique(gv_china_division_list[,c("CITY_LC","PROVINCE_LC")]) 
  
  ind_mch <- match(df_province_sc$CITY_SC, ds_province_sd$CITY_LC)
  df_province_sc[ , "PROVINCE_SC"] <- ds_province_sd[ind_mch, "PROVINCE_LC"]
  v_province_cn <- df_province_sc[ , "PROVINCE_SC"]
  
  out_province_cn <- ifelse(is.na(v_province_cn),"",v_province_cn)
  return(out_province_cn)
}
uf_sdProcess_ALL <- function(p_df){
  df_raw <- p_df
  
  d_count <- nrow(df_raw)
  
  if(any((colnames(df_raw) == gv_geocore_fields) == FALSE)){
    print("Please check the Geo Core fields of your input!")
    return()
  }else {
    PROPERTY_NAME <- uf_sdProcess_PN(df_raw$PROPERTY_NAME)
    ADDRESS_1 <- uf_sdProcess_ADDR(df_raw$ADDRESS_1)
    DISTRICT_NAME <- uf_sdProcess_DISTRICT(df_raw$DISTRICT_NAME)
    CITY_NAME <- uf_sdProcess_CITY(df_raw$CITY_NAME)
    PROVINCE_NAME <- uf_sdProcess_PROVINCE(df_raw$PROVINCE_NAME,uf_sdProcess_CITY(df_raw$CITY_NAME))
    
    PROPERTY_NAME_LC <- uf_sdProcess_PN_CN(df_raw$PROPERTY_NAME_LC)
    ADDRESS_LC <- uf_sdProcess_ADDR_CN(df_raw$ADDRESS_LC)
    DISTRICT_NAME_LC <- uf_sdProcess_DISTRICT_CN(df_raw$DISTRICT_NAME_LC)
    CITY_NAME_LC <- uf_sdProcess_CITY_CN(df_raw$CITY_NAME_LC)
    PROVINCE_NAME_LC <- uf_sdProcess_PROVINCE_CN(df_raw$PROVINCE_NAME_LC,uf_sdProcess_CITY_CN(df_raw$CITY_NAME_LC))
    
    df_fixed <- data.frame(SOURCE_PROPERTY_ID = df_raw$SOURCE_PROPERTY_ID,
                           PROPERTY_NAME = PROPERTY_NAME,
                           PROPERTY_NAME_LC = PROPERTY_NAME_LC,
                           ADDRESS_1 = ADDRESS_1,
                           ADDRESS_LC = ADDRESS_LC,
                           DISTRICT_NAME = DISTRICT_NAME,
                           DISTRICT_NAME_LC = DISTRICT_NAME_LC,
                           CITY_NAME = CITY_NAME,
                           CITY_NAME_LC = CITY_NAME_LC,
                           PROVINCE_NAME = PROVINCE_NAME,
                           PROVINCE_NAME_LC = PROVINCE_NAME_LC,
                           COUNTRY_NAME = rep("China",d_count),
                           COUNTRY_NAME_LC = rep("中国",d_count),
                           TIMEZONE = rep("UTC/GMT+08:00",d_count),
                           GEO_LAT = df_raw$GEO_LAT,
                           GEO_LONG = df_raw$GEO_LONG)
    
    return(df_fixed)
  }
  
}

########################################################2. 加载和处理数据########################################################
data_source_en <- uf_xlsxDataImport("./Dalton_Test_Output/China_Test_Source.xlsx","English")
data_source_cn <- uf_xlsxDataImport("./Dalton_Test_Output/China_Test_Source.xlsx","Chinese")
nrow(data_source_en)
nrow(data_source_cn)
names(data_source_en)
names(data_source_cn)

data_std_en_v3 <- uf_xlsxDataImport("./Dalton_Test_Output/China_Test_Standardized.xlsx","English_V3")
data_std_cn_v3 <- uf_xlsxDataImport("./Dalton_Test_Output/China_Test_Standardized.xlsx","Chinese_V3")
nrow(data_std_en_v3)
nrow(data_std_cn_v3)
names(data_std_en_v3) <- c("SRC_SYS_ID","STD_PROPERTY_NAME","STD_ADDRESS_1","STD_LOCALITY_1","STD_LOCALITY_2","STD_POSTCODE","STD_COUNTRY")
names(data_std_cn_v3) <- c("SRC_SYS_ID","STD_PROPERTY_NAME","STD_ADDRESS_1","STD_LOCALITY_1","STD_LOCALITY_2","STD_POSTCODE","STD_COUNTRY")

inner_join(data_source_en,data_std_en_v3,by = "SRC_SYS_ID")




































































































