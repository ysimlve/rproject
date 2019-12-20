setwd("C:/YuanLe/R/RWkDir/01. MDM_RegExp")
options(stringsAsFactors = FALSE); options(digits = 5); options(java.parameters = "-Xmx4g")


library(readxl); library(dplyr); library(stringdist); library(RCurl); library(RJSONIO); library(xlsx); library(geosphere)

uf_xlsxDataImport <- function(xlsx_file,spreedsheet){
  df_xlsx <- read.xlsx(xlsx_file,sheetName = spreedsheet,header=TRUE,encoding = "UTF-8")
  return(df_xlsx)
}

uf_xlsxDataExport <- function(p_df, xlsx_file, spreedsheet){
  xlsx::write.xlsx(p_df,xlsx_file,sheetName = spreedsheet)
}


pds <- uf_xlsxDataImport("./Data/ChinaProperties.xlsx","Sheet1")
n_count <- nrow(pds)

rlt <- pds
rlt$CheckResult <- rep("",n_count)
rlt$rjt_rsn <- rep("",n_count)

View(pds)
ptn_cn <- "^([^A-Za-z,!@#$*.~^%\\s]{2,})(街|路|道|巷|院|条|里|胡同)([0-9]{0,}[^A-Za-z0-9,!@#$*.~^%\\s]{1,})?([1-9][0-9]{0,3}-)?([1-9][0-9]{0,3})号([^A-Za-z,!@#$*.~^%\\s]{0,})"
ptn_en <- "^([A-Za-z1-9]{1,}\\s){0,5}([1-9][0-9]{0,3}[A-Za-z]?-)?([1-9][0-9]{0,3})([A-Za-z]?)\\s([A-Za-z1-9']{2,}\\s?){1,5}\\s(Road|Avenue|Street|Alley|Lane|Hutong|Boulevard)(\\s[A-Za-z1-9]{1,}){0,10}"

PRecord <- pds[1,]
uf_rejectCheck <- function(PRecord){
  PID <- PRecord$Source_Property_Id
  PName_EN <- PRecord$Property_Name
  PName_CN <- PRecord$Property_Name_Lc
  PAddr_EN <- PRecord$Address_Line_1
  PAddr_LC <- PRecord$Address_Line_1_Lc
  PLat <- PRecord$Geo_Lat
  PLong <- PRecord$Geo_Long
  
  rjt_rsn <- ""
  
  if(is.na(PName_EN)){
    rjt_rsn <- paste(rjt_rsn,"Property_Name_En is Null", sep = "||")
  }else if(nchar(PName_EN) < 3){
    rjt_rsn <- paste(rjt_rsn,"Length of Property_Name_En is less than 3", sep = "||")
  }
  
  if(is.na(PName_CN)){
    rjt_rsn <- paste(rjt_rsn,"Property_Name_CN is Null", sep = "||")
  }else if(nchar(PName_CN) < 3){
    rjt_rsn <- paste(rjt_rsn,"Length of Property_Name_CN is less than 3", sep = "||")
  }
  
  if(is.na(PAddr_EN)){
    rjt_rsn <- paste(rjt_rsn,"AddressLine1_EN is Null", sep = "||")
  }else if(regexpr(ptn_en,PAddr_EN) < 1){
    rjt_rsn <- paste(rjt_rsn,"AddressLine1_EN not in standard format", sep = "||")
  }
  
  if(is.na(PAddr_LC)){
    rjt_rsn <- paste(rjt_rsn,"AddressLine1_CN is Null", sep = "||")
  }else if(regexpr(ptn_cn,PAddr_LC) < 1){
    rjt_rsn <- paste(rjt_rsn,"AddressLine1_CN not in standard format", sep = "||")
  }
  
  if(is.na(PLat) | is.na(PLong)){
    rjt_rsn <- paste(rjt_rsn,"Geo code is null", sep = "||")
  }
  
  return(rjt_rsn)
}

for(i in 1:n_count){
  print(i)
  PRecord <- pds[i,]
  CheckResult <- ""
  
  rjt_rsn <- uf_rejectCheck(PRecord)
  rjt_rsn <- gsub("^\\|\\|","",rjt_rsn)
  if(rjt_rsn == ""){
    CheckResult <- "Pass"
  }else{
    CheckResult <- "Rejected"
  }
  
  rlt$CheckResult[i] <- CheckResult
  rlt$rjt_rsn[i] <- rjt_rsn
}

View(rlt)

uf_xlsxDataExport(rlt,"./Data/ChinaProperties_rlt.xlsx","Sheet1")















































































