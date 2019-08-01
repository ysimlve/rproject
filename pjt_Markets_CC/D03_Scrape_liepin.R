setwd("C:/YuanLe/R/RWkDir/06. Markets_CC")
source("C01_Scrape_51job.R")
###############################################Data Collection:get url###############################################
p_home <- gv_url_liepin_yp_home
p_jobArea <- attr(gv_cities,"liepin_jobAreaCode")[which(gv_cities == "成都")[1]]
#p_industry <- gv_industries$sub_industry_code_liepin[1]
count_industries <- nrow(gv_industries)
week_end <- gsub("-","",as.character(floor_date(Sys.Date(), "week") + 7))
not_liepin_industry_code <- c("000")

for(i in 1:count_industries){
  p_industry <- gv_industries$sub_industry_code_liepin[i]
  if(any(p_industry == not_liepin_industry_code)){
    print("No this industry")
  }else{
    p_sub_industry_en <- gv_industries$sub_industry_en[which(gv_industries$sub_industry_code_liepin == p_industry)[1]]
    print(paste0(as.character(i),"--",p_sub_industry_en))
    if(!is.na(p_industry)){
      df_companies_51urls <- uf_scrape_liepin_input(p_home,p_jobArea,p_industry)
      count_df_51urls <- nrow(df_companies_51urls)
      if(count_df_51urls > 0){
        uf_xlsxDataExport(df_companies_51urls,paste0("./Job51_Weekly_Delta_Urls/",
                                                     "Companies_LiepinUrls_",
                                                     attr(gv_cities,"city_en")[which(attr(gv_cities,"liepin_jobAreaCode") == p_jobArea)[1]],
                                                     "_Week_",week_end,
                                                     ".xlsx")
                          ,gv_industries$sub_industry_en[which(gv_industries$sub_industry_code_liepin == p_industry)[1]]
                          ,TRUE)
      }
      rm(df_companies_51urls)
    }
  }
 
}

###############################################Data Collection:get detail information of company###############################################
#count of weekly delta amount - 
#20170326 - 2836
#20170402 - 

p_jobArea <- attr(gv_cities,"liepin_jobAreaCode")[which(gv_cities == "成都")]
count_industries <- nrow(gv_industries)
week_end <- gsub("-","",as.character(floor_date(Sys.Date(), "week") + 7)) ###

#j <- 1
for(j in 1:count_industries){
  p_industry <- gv_industries$sub_industry_code_liepin[j]
  if(p_industry != "000"){
    p_sheet_import <- gv_industries$sub_industry_en[which(gv_industries$sub_industry_code_liepin == p_industry)[1]]
    print(p_sheet_import)
    df_companyUrls_import <- uf_xlsxDataImport(paste0("./Job51_Weekly_Delta_Urls/",
                                                      "Companies_LiepinUrls_",
                                                      attr(gv_cities,"city_en")[which(attr(gv_cities,"liepin_jobAreaCode") == p_jobArea)[1]],
                                                      "_Week_",week_end,
                                                      ".xlsx")
                                               ,p_sheet_import)
    
    count_total <- nrow(df_companyUrls_import)
    
    #count_total
    for(g in 1:count_total){
      v_company_name <- df_companyUrls_import[g,]$Company_Name
      if(!any(v_company_name == "")){
        print(paste0(p_sheet_import,"--",as.character(g),"/",as.character(count_total),"--",v_company_name,"--",as.character(Sys.time())))
        tryCatch({
          uf_scrape_liepin(df_companyUrls_import[g,],p_jobArea,T,F,F,F)
        },warnings = function(w){
          ;
          print(w)
        },error = function(e){
          ;
          print(e) 
        })
      }
      
    }
    
    rm("df_companyUrls_import")
  }
  
  
}


###############################################Data Collection:get more information from BDXin###############################################
######Test 12068 first
DBConn <- "DEFAULT"
timeouts <- 20
Query <-  "SELECT TOP 150 ID,company FROM CN_CD_CC.Companies_Infor_51Job WHERE data_source = '51job' AND GoBDXin IS NULL order by ID DESC"


df_IDs <- uf_execute_Query(DBConn,Query,timeouts)
count_ID <- nrow(df_IDs)

for(k in 1:count_ID){
  v_company <- iconv(df_IDs$company[k],"gb2312","UTF-8")
  v_ID <- iconv(df_IDs$ID[k],"gb2312","UTF-8")
  print(paste0(v_ID,"--",v_company,"--",as.character(Sys.time())))
  c_info <- rep("",11)
  c_info <- uf_scrape_BaiduXin(v_company)
  
  v_query <- paste0("UPDATE CN_CD_CC.Companies_Infor_51Job
                     SET address1 = N'",iconv(c_info[2], "UTF-8", "gb2312"),"',",
                        "legal_person = N'", iconv(c_info[3], "UTF-8", "gb2312"),"',",
                        "registered_No = N'", iconv(c_info[4], "UTF-8", "gb2312"),"',",
                        "registered_city = N'", iconv(c_info[5], "UTF-8", "gb2312"),"',",
                        "registered_office = N'", iconv(c_info[6], "UTF-8", "gb2312"),"',",
                        "registered_capital = N'", iconv(c_info[7], "UTF-8", "gb2312"),"',",
                        "establish_date = N'", iconv(c_info[8], "UTF-8", "gb2312"),"',",
                        "business_end_date = N'", iconv(c_info[9], "UTF-8", "gb2312"),"',",
                        "business_Sector_RA = N'", iconv(c_info[10], "UTF-8", "gb2312"),"',",
                        "op_status = N'", iconv(c_info[11], "UTF-8", "gb2312"),"',",
                        "GoBDXin = 'YES',",
                        "GoBDXin_Date = '", as.character(Sys.Date()), "'",
                      " WHERE ID = ",as.integer(v_ID))
  
    uf_execute_Query(DBConn,v_query,timeouts)
              
}




#四川天赢精采电子商务有限公司


