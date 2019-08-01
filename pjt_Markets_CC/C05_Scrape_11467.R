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

gv_url_BDXin_home <- "http://xin.baidu.com"
gv_url_11467_home <- "http://www.11467.com/"

gv_baidu_search_type <- c("联系电话","联系方式","电话","官网")
gv_baidu_search_topCount <- 7

gv_cities <- c("成都","重庆")
attr(gv_cities,"city_en") <- c("Chengdu",  "Chongqing")
attr(gv_cities,"alias1") <- c("蜀",  "渝")
attr(gv_cities,"alias2") <- c("川",  "渝")
attr(gv_cities,"phoneCode") <- c("028","023")

gv_salaryRange <- c("2000以下","2001-3000","3001-4500","4501-8000","8001-10000","10001-15000","15001-20000","20001-30000","30001-50000","50000以上","所有")
gv_companyType <- c("外资(欧美)","外资(非欧美)","合资","国企","民营公司","外企代表处","政府机关","事业单位","非营利机构","上市公司","创业公司","所有")
gv_companySize <- c("少于50人","50-150人","150-500人","500-1000人","1000-5000人","5000-10000人","10000人以上","所有")
gv_industries <- uf_xlsxDataImport("./ReferData/Job_Industry_Category.xlsx","Industry")

gv_not_official <- c("zhaopin","51job","lagou","ganji","baidu","dajie","hao123","atobo","jobui","yl1001","qixin","pe168","caihao","5858","cyzone","xizhi","lvse","cn716","chinahr",
                     "metalnews","ciidoo","12580.tv","yellowurl","qy.58","kanzhun","1024sj","wealink","hc360","b2b168","9928.tv","hunt007","huix","cnlinfo")

gv_method_strSim <- "lcs"

gv_ptns_phone_number <- c(paste0("[^0-9]","000","(\\)|.)?","\\s?(\\s|-)?\\s?[5-9][0-9]{3}-?[0-9]{3,4}"),
                          "[^0-9]400(-|\\s)?[0-9]{3,8}(-|\\s)?[0-9]{3,8}",
                          "[^0-9]1(3|6|7|8|5)[0-9]{9}")
#1. e.g. 028-12345678; (028)12345678; 028 12345678
#2. e.g. 400-1234-1234
#3. e.g. 13702938495

gv_BDXin_js <- "BDXin.js"
gv_BDXin_html <- "./html/BDXin.html"

gv_deep_search_OW_list <- c("联系我们")
gv_giveup_companies <- c("四川天赢精采电子商务有限公司")


########################################################Global Function########################################################
################Function - uf_scrape_11467
#max: 294715?
#p_home <- gv_url_11467_home
#p_city_en <- "Chengdu"
#coID <- 1
#uf_scrape_11467(p_home,coID,p_city_en)

uf_scrape_11467 <- function(p_home,coID,p_city_en){
  #输入变量
  if(any(p_city_en == attr(gv_cities,"city_en")) & p_home == "http://www.11467.com/" & is.numeric(coID)){
    v_url_home <- paste0(p_home,tolower(p_city_en),"/co/",as.character(coID),".htm") 
  }
  
  #输出变量
  v_output <- "DONE"
  
  #信息变量
  v_city <- gv_cities[which(attr(gv_cities,"city_en") == p_city_en)[1]]
  v_recruitPage_51job <- v_url_home
  
  v_company <- v_industry <- v_sub_industry <- v_job_issue_latest <- v_address <- v_address1 <- v_building <- 
    v_contact <- v_official_contact <- v_offical_contacts_qualified <-
    v_coType <- v_coSize <- v_is_local <- v_headquarter <- v_legal_person <- v_registered_No <- v_registered_city <- 
    v_registered_office <- v_registered_capital <- v_establish_date <- v_business_end_date <-
    v_business_Sector_RA <- v_op_status <- v_official_website <- v_web_record_code <- v_coDesc <- v_jobCount_all <- 
    v_jobCount_cd <- v_salaryRange_all <- v_salaryRange_cd <- ""
  
  
  v_scape_date <- as.character(Sys.Date())
  v_data_source <- "11467"
  v_Effective_Date <- v_scape_date
  v_Expiry_Date <- "9999-12-31"
  v_Is_Current <- 1
  v_job_issue_latest <- "1900-01-01"
  
  html_compnay <- uf_read_html(v_recruitPage_51job,10)
  
  if(!all(is.na(html_compnay))){
    navs <- html_compnay %>% html_nodes("body") %>% html_nodes("div") %>% html_nodes("div.navleft") %>% html_nodes("a")
    #公司名称/公司子行业
    if(length(navs) > 3){
      count_nav <- length(navs)
      v_company <- navs[count_nav] %>% html_text()
      v_sub_industry <- navs[count_nav-1] %>% html_text()
      v_sub_industry <- gsub("黄页$","",gsub("公司$","",gsub(paste0("^",v_city) ,"",gsub("\\s","",v_sub_industry))))
    }
    if(nchar(v_company) > 3){
      #公司简介
      box_desc <- html_compnay %>% html_nodes("body") %>% html_nodes("div") %>% html_nodes("div") %>% html_nodes("div.box.t5")
      count_box_desc <- length(box_desc)
      if(count_box_desc > 0){
        for(i in 1:count_box_desc){
          if(box_desc[i] %>% html_attr("id") == "aboutus"){
            v_coDesc <- box_desc[i] %>% html_nodes("div.boxcontent.text") %>% html_text()
            v_coDesc <- gsub("(\\\n|\\\t|\\s)","",v_coDesc)
            v_coDesc <- ifelse(nchar(v_coDesc) > 2000,paste0(substr(v_coDesc,1,1997),"..."), v_coDesc)
            break()
          }
        }
      }
      
      #联系方式
      box_contact <- html_compnay %>% html_nodes("body") %>% html_nodes("div") %>% html_nodes("div") %>% html_nodes("div.box")
      count_box_contact <- length(box_contact)
      if(count_box_contact > 1){
        for(j in 1:count_box_contact){
          if(!is.na(box_contact[j] %>% html_attr("id"))){
            if(box_contact[j] %>% html_attr("id") == "contact"){
              contact_titles <- box_contact[j] %>% html_nodes("div.boxcontent") %>% html_nodes("dt") %>% html_text()
              contact_infos <- box_contact[j] %>% html_nodes("div.boxcontent") %>% html_nodes("dd") %>% html_text()
              count_contact_titles <- length(contact_titles)
              if(count_contact_titles > 0){
                for(k in 1:count_contact_titles){
                  contact_title <- contact_titles[k]
                  contact_info <- contact_infos[k]
                  if(regexpr("地址",contact_title) > 0){
                    v_address <- contact_info
                  }else if(regexpr("(手机|电话)",contact_title) > 0){
                    v_tmp_contact <- contact_info
                    for(n in 1:length(gv_ptns_phone_number)){
                      if(regexpr(gsub("000",attr(gv_cities,"phoneCode")[which(gv_cities == v_city)[1]],gv_ptns_phone_number[n]),v_tmp_contact) > 0){
                        v_contact <- paste0(v_contact," | ",v_tmp_contact)
                        break()
                      }
                    }
                    v_contact <- gsub("^\\s\\|\\s","",v_contact)
                  }
                  
                }
              }
              
              
              break()
            }
          }
        }
      }
      
      
      
    }
  }
  
  #insert new record(update existing record) into database
  if(1==1){
    v_entity_all <- c(v_company,
                      v_address,
                      v_address1,
                      v_city,
                      v_building,
                      v_contact,
                      v_official_contact,
                      v_offical_contacts_qualified,
                      v_coType,
                      v_coSize,
                      v_industry,
                      v_sub_industry,
                      v_is_local,
                      v_headquarter,
                      v_legal_person,
                      v_registered_No,
                      v_registered_city,
                      v_registered_office,
                      v_registered_capital,
                      v_establish_date,
                      v_business_end_date,
                      v_business_Sector_RA,
                      v_op_status,
                      v_official_website,
                      v_web_record_code,
                      v_coDesc,
                      v_jobCount_all,
                      v_jobCount_cd,
                      v_salaryRange_all,
                      v_salaryRange_cd,
                      v_job_issue_latest,
                      v_scape_date,
                      v_data_source,
                      v_recruitPage_51job,
                      v_Effective_Date,
                      v_Expiry_Date,
                      v_Is_Current,
                      c(0,0,0))
  }    
  
  if(v_company != ""){
    uf_entity_insert_update("DEFAULT",v_entity_all,gv_table_CI5,"INSERT",20)
  }  
  
  return(v_output)
}



################Function - uf_read_html
#p_web <- "http://www.baidu.com"        #p_web is a link of website
#p_web <- "http://www.unionbigdata.com"
uf_read_html <- function(p_web,timeouts){
  html_web <- NA
  
  if(all(class(p_web) != c("xml_document","xml_node"))){
    html_web <- tryCatch({
      evalWithTimeout({ read_html(p_web,encoding = "utf-8") }, timeout = timeouts)
    },warnings = function(w){
      ;
      NA
    },error = function(e){
      ;
      tryCatch({
        evalWithTimeout({ read_html(p_web,encoding = "gbk") }, timeout = timeouts)
      },warnings = function(w){
        ;
        NA
      },error = function(e){
        ;
        tryCatch({
          evalWithTimeout({ read_html(p_web,encoding = "gb2312") }, timeout = timeouts)
        },warnings = function(w){
          ;
          NA
        },error = function(e){
          ;
          NA
        })
      })
    })
    
  }
  return(html_web)
}



################Function - uf_entity_insert_update
#DBConn <- "DEFAULT"
#Entity <- v_entity_all
#Table <- "CN_CD_CC.Companies_Infor_51Job"
#Ops <- "INSERT"
#timeouts <- 20
uf_entity_insert_update <- function(DBConn,Entity,Table,Ops,timeouts){
  Conn <- NA
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
    sql_query <- ""
    if(Ops == "INSERT"){
      sql_query <- paste0("INSERT INTO ",Table,
                          "(company, address, address1, city, building, contact, official_contact, offical_contacts_qualified, coType, coSize, industry, 
                          sub_industry, is_local, headquarter, legal_person, registered_No, registered_city, registered_office, registered_capital, establish_date, 
                          business_end_date, business_Sector_RA, op_status, official_website, 
                          web_record_code, coDesc, jobCount_all, jobCount_cd, salaryRange_all, salaryRange_cd, job_issue_latest, scape_date, data_source, 
                          recruitPage_51job, Effective_Date, Expiry_Date, Is_Current) VALUES(") 
      sql_query <- paste0(sql_query,
                          "N'",iconv(Entity[1], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[2], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[3], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[4], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[5], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[6], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[7], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[8], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[9], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[10], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[11], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[12], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[13], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[14], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[15], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[16], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[17], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[18], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[19], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[20], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[21], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[22], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[23], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[24], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[25], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[26], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[27], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[28], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[29], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[30], "UTF-8", "gb2312"),"',",
                          "'",as.Date(Entity[31]),"',",
                          "'",as.Date(Entity[32]),"',",
                          "N'",iconv(Entity[33], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[34], "UTF-8", "gb2312"),"',",
                          "'",as.Date(Entity[35]),"',",
                          "'",as.Date(Entity[36]),"',",
                          "'",as.integer(Entity[37]),"')")
      
    }
    if(Ops == "UPDATE"){
      sql_query <- paste0("UPDATE ",Table," SET ",
                          "Expiry_Date = '", as.Date(Entity[35]),"',",
                          "Is_Current = 0 ",
                          "WHERE ID = ", as.integer(Entity[38]))
    }
    df.db_return <- tryCatch({
      evalWithTimeout({ sqlQuery(Conn,sql_query)
      }, timeout = timeouts);
      TRUE
    },warnings = function(w){
      ;
      FALSE
    },error = function(e){
      ;
      FALSE})
    odbcClose(Conn)
  }
  
  
}


################Function - uf_execute_Query
#DBConn <- "DEFAULT"
#timeouts <- 20
#Query <- v_query
#uf_execute_Query(DBConn,Query,timeouts)
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
  }
  if(class(df.db_return_p) == "data.frame"){
    df.db_return <- df.db_return_p
  }
  return(df.db_return)
}






