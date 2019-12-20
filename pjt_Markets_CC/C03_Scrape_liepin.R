########################################################Programm Start########################################################
setwd("C:/YuanLe/R/RWkDir/06. Markets_CC")
#ls()
#rm(list=ls())
#install.packages("R.utils")

options(stringsAsFactors = FALSE)
options(java.parameters = "-Xmx4g")
options(warn = -1)
options(encoding = "utf-8")
options(timeout = 60)


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

gv_baidu_search_type <- c("联系电话","联系方式","电话","官网")
gv_baidu_search_topCount <- 7

gv_cities <- c("成都","重庆")
attr(gv_cities,"city_en") <- c("Chengdu",  "Chongqing")
attr(gv_cities,"alias1") <- c("蜀",  "渝")
attr(gv_cities,"alias2") <- c("川",  "渝")
attr(gv_cities,"phoneCode") <- c("028","023")

gv_industries <- uf_xlsxDataImport("./ReferData/Job_Industry_Category.xlsx","Industry")


gv_ptns_phone_number <- c(paste0("[^0-9]","000","(\\)|.)?","\\s?(\\s|-)?\\s?[5-9][0-9]{3}-?[0-9]{3,4}"),
                          "[^0-9]400(-|\\s)?[0-9]{3,8}(-|\\s)?[0-9]{3,8}",
                          "[^0-9]1(3|6|7|8|5)[0-9]{9}")
#1. e.g. 028-12345678; (028)12345678; 028 12345678
#2. e.g. 400-1234-1234
#3. e.g. 13702938495

gv_method_strSim <- "lcs"

gv_url_liepin_yp_home <- "https://www.liepin.com/company/"
attr(gv_cities,"liepin_jobAreaCode") <- c("280020","040")

v_liepin_updateFreq <- "360"


########################################################Global Function########################################################
################Function - uf_scrape_liepin_input
uf_scrape_liepin_input <- function(p_home,p_jobArea,p_industry){
  if(1==1){
    v_search_url_home <- paste0(p_home,p_jobArea,"-",p_industry)
    v_session <- html_session(v_search_url_home)
  }
  
  #获取搜索得到的所有公司对应的页面
  df_companyUrls <- uf_scrape_liepin_companyUrl(v_session,p_industry,p_jobArea)
  
  return(df_companyUrls)
  
}

uf_scrape_liepin_companyUrl <- function(p_session,p_industry,p_jobArea){
  df_companyUrl <- data.frame(Company_Name = character(0), Url_liepin = character(0),Industry = character(0), Sub_Industry = character(0), City = character(0))
  v_page_num <- p_session %>% html_nodes("div.pager-box") %>% html_nodes("span.addition") %>% html_text()
  v_page_num <- ifelse(length(v_page_num) == 0, 1, v_page_num)
  count_f <- regexpr("共[1-9]{1,5}页",v_page_num)
  if(count_f > 0){
    v_page_num <- substr(v_page_num, count_f + 1, attr(count_f,"match.length") - 1)
  }
  
  sub_ind <- gv_industries$sub_industry[which(gv_industries$sub_industry_code_liepin == p_industry)[1]]
  ind <- gv_industries$industry[which(gv_industries$sub_industry_code_liepin == p_industry)[1]]
  city <- gv_cities[which(attr(gv_cities,"liepin_jobAreaCode") == p_jobArea)]
  
  for(i in 1:v_page_num){
    #print(i)
    v_nodes <- p_session %>% html_nodes("div.company-list.clearfix") %>% html_nodes("div.list-item") %>% html_nodes("div.item-top.clearfix") %>% 
      html_nodes("div.company-info") %>% html_nodes("p.company-name") %>% html_nodes("a") 
    Company_Name <- v_nodes %>% html_attr("title")
    Url_51job <- v_nodes %>% html_attr("href")
    Sub_Industry <- rep(sub_ind,length(Company_Name))
    Industry <- rep(ind,length(Company_Name))
    Recruit_City <- rep(city,length(Company_Name))
    
    df_companyUrl <- rbind(df_companyUrl, cbind(Company_Name, Url_51job, Industry, Sub_Industry, Recruit_City))
    if(i != v_page_num){
      As <- p_session %>% html_nodes("div.pager-box") %>% html_nodes("div.pagerbar") %>% html_nodes("a")
      count_As <- length(As)
      for(k in 1:count_As){
        if(regexpr("下一页",As[k] %>% html_text()) > 0){
          p_session <- p_session %>% jump_to(As[k] %>% html_attr("href"))
        }
      }
     
    }
  }
  df_companyUrl <- subset(df_companyUrl, !duplicated(Company_Name))
  
  df_companyUrl <- filter(df_companyUrl,regexpr("https://www.liepin.com/company/",df_companyUrl$Url_51job) >= 1)
  
  
  return(df_companyUrl)
}


################Function - uf_scrape_liepin
#p_companyUrls <- df_companyUrls_import[956,]
#p_jobArea <- "280020"
#initial_load <- FALSE
#p_companyUrls$Company_Name
#df_test_1 <- uf_scrape_51job(p_companyUrls,p_jobArea,initial_load,FALSE)
#df_test_1$official_website
#df_test_1$official_contact
#df_test_1$contact
uf_scrape_liepin <- function(p_companyUrls,p_jobArea,initial_load = F,GoFindOfficalWesite = F, GoFindContact = F, GoBDXin = F){
  #输入变量
  df_companyUrls <- p_companyUrls
  #输出变量
  v_output <- "DONE"
  #循环次数
  v_count_companies <- nrow(df_companyUrls)
  #循环搜索所有公司的页面
  for(i in 1:v_count_companies){
    #从df_companyUrls取出信息
    if(1==1){
      v_company <- df_companyUrls$Company_Name[i]
      v_industry <- df_companyUrls$Industry[i]
      v_sub_industry <- df_companyUrls$Sub_Industry[i]
      v_recruitPage_51job <- df_companyUrls$Url_51job[i]
      v_city <- df_companyUrls$Recruit_City[i]
      v_job_issue_latest <- "1900-01-01"
      
      v_address <- v_address1 <- v_building <- v_contact <- v_official_contact <- v_offical_contacts_qualified <-
        v_coType <- v_coSize <- v_is_local <- v_headquarter <- v_legal_person <- v_registered_No <- v_registered_city <- 
        v_registered_office <- v_registered_capital <- v_establish_date <- v_business_end_date <-
        v_business_Sector_RA <- v_op_status <- v_official_website <- v_web_record_code <- v_coDesc <- v_jobCount_all <- 
        v_jobCount_cd <- v_salaryRange_all <- v_salaryRange_cd <- ""
      
      v_scape_date <- as.character(Sys.Date())
      v_data_source <- "liepin"
      v_Effective_Date <- v_scape_date
      v_Expiry_Date <- "9999-12-31"
      v_Is_Current <- 1
    }
    
    ##if the company already existed in database?
    ##defferent control for initial and delta
    check_rlt <- c(0,0,0)
    if(!initial_load){
      v_entity <- c(v_company,v_industry,v_sub_industry,v_recruitPage_51job,v_city,v_job_issue_latest,v_data_source)
      check_rlt <- uf_entity_existInDB_check("DEFAULT",v_entity,gv_table_CI5,20) 
      attr(check_rlt,"state") <- c("exist","update_required","IDENTITY")
    }
    
    if(check_rlt[1] == 1 & check_rlt[2] == 0){ #no action required
      print("no action required")
    }else if(check_rlt[1] == 0 | (check_rlt[1] == 1 & check_rlt[2] == 1)){#need retrieve new information of the company
      if(regexpr("https://www.liepin.com/company/",v_recruitPage_51job) >= 1){  #如果公司页面的URL满足这样的格式，继续
        html_compnay <- uf_read_html(v_recruitPage_51job,10)
        
        #在公司的招聘页面获取各种信息
        if(!all(is.na(html_compnay))){
          #从51Job获取基本信息
          if("basic_infor" == "basic_infor"){
            nodes_basic <- html_compnay %>% html_nodes("div.base-info.clearfix") %>% html_nodes("li")
            count_basic <- length(nodes_basic)
            if(count_basic > 0){
              for(b in 1:count_basic){
                span_text <- nodes_basic[b] %>% html_nodes("span") %>% html_text()
                if(regexpr("规模",span_text) > 0){
                  v_coSize_tmp <- nodes_basic[b] %>% html_text()
                  v_coSize <- gsub("\\s","",substr(v_coSize_tmp,4,1000)) 
                }else if(regexpr("地址",span_text) > 0){
                  v_address <- nodes_basic[b] %>% html_attr("title")
                }
              }
            }
            
            txt_desc <- html_compnay %>% html_nodes("div.company-introduction.clearfix") %>% html_nodes("p") %>% html_text()
            if(length(txt_desc) > 0){
              v_coDesc <- gsub("(\\\r|\\\n|\\\t|\\s)","",txt_desc)
              v_coDesc <- ifelse(nchar(v_coDesc) > 2000,paste0(substr(v_coDesc,1,1997),"..."), v_coDesc)
            }
          }
          
          #公司的官网地址
          v_link <- ""
          if(GoFindOfficalWesite){
            v_link <- html_compnay %>% html_nodes("div.tBorderTop_box") %>% html_nodes("div.bmsg.tmsg.inbox") %>% html_nodes("p") %>% html_nodes("a") %>% html_text()
            v_link <- ifelse(length(v_link) > 0, v_link, "")
            if(v_link == ""){ #如果官网仍然没找到，直接到备案网站去查找
              v_link <- uf_find_official_beianbeian(v_company)
            }
            if(v_link == ""){ #if official website is still not found, try to use baidu search to get.
              v_link <- uf_scrape_baidu_search_company(v_company,p_jobArea,"官网")
            }
            
            if(regexpr("^http://",v_link) < 0 & v_link != ""){
              v_link <- paste0("http://",v_link)
            }
            v_official_website <- v_link
          }
          
          #获取公司联系方式
          if(GoFindContact){
            v_contact <- ""
            v_official_contact <- "N/A"
            v_offical_contacts_qualified <- "N/A"
            
            #如果公司官网链接可用，从官网获取公司联系方式
            if(v_contact == "" & v_link != ""){ 
              v_contact <- uf_scrape_officialWebsite_contact(v_link,p_jobArea,gv_ptns_phone_number)
              #如果官网首页找你到联系电话，取到子页面查找(联系我们)
              if(v_contact == ""){
                v_contact <- uf_scrape_contact_OW_Deep(v_link,p_jobArea,gv_ptns_phone_number)
              }
              if(v_contact != ""){
                v_official_contact <- "YES"
                v_offical_contacts_qualified <- "NO"
                
                ptn_phone_number_1 <- gsub("000",attr(gv_cities,"phoneCode")[which(attr(gv_cities,"51job_code") == p_jobArea)[1]],
                                           gv_ptns_phone_number[1]) 
                ptn_phone_number_2 <- gv_ptns_phone_number[3]
                list_offical_contact <- strsplit(v_contact," \\| ")[[1]]
                count_offical_contact <- length(list_offical_contact)
                for(c in 1:count_offical_contact){
                  if(regexpr(ptn_phone_number_1,paste0("a",list_offical_contact[c])) > 0 | regexpr(ptn_phone_number_2,paste0("a",list_offical_contact[c])) > 0){
                    v_offical_contacts_qualified <- "YES"
                    break()
                  }
                }
              }
            }
            #if contact still null, try to use Baidu Search to retrieve contact
            if(v_contact == ""){
              v_contact <- uf_scrape_baidu_search_company(v_company,p_jobArea,"联系电话")
              if(v_contact != ""){
                v_official_contact <- "NO"
              }
            }
            #删除重复的电话号码
            if(v_contact != ""){
              phoneCode <- attr(gv_cities,"phoneCode")[which(attr(gv_cities,"51job_code") == p_jobArea)[1]]
              v_t <- gsub("\\s{1,10}$","",gsub("^\\s{1,10}","",v_contact))
              v_t <- gsub(paste0(phoneCode,"\\s{1,2}"),paste0(phoneCode,"-"),v_t)
              v_t_c <- unlist(strsplit(v_t, split=" \\| "))
              v_t_c <- unique(v_t_c)
              l_t_c <- ifelse(length(v_t_c) >= 4,4,length(v_t_c))
              v_t_c <- v_t_c[1:l_t_c]
              v_contact <- paste(v_t_c,collapse = " | ")
              v_contact <- gsub(paste0(phoneCode,"(\\)|\\）)"),paste0(phoneCode,"-"),v_contact)
              v_contact <- gsub("^\\s?\\|\\s?","",v_contact)
            }
          }
          
          if(GoBDXin){
            #print("GoBDXin")
            #判断公司是否是本地公司
            #if(v_link != ""){
            #  if(any(substr(attr(v_link,"code"),1,1) == c(attr(gv_51job_jobAreaCode,"alias1")[which(gv_51job_jobAreaCode == p_jobArea)[1]],
            #                                              attr(gv_51job_jobAreaCode,"alias2")[which(gv_51job_jobAreaCode == p_jobArea)[1]]))){
            #    attr_is_local[i] <- "YES"
            #  }
            #}
            
            #从企查查获取更多的企业信息
            #v_qcc_rlt <- uf_scrape_qcc_basic(v_company)
            #if(stringsim(v_qcc_rlt[1],v_company,method = gv_method_strSim)>=0.9){
            #  attr_contact_qcc[i] <- v_qcc_rlt[which(attr(v_qcc_rlt,"name") == "contact")]
            #  attr_address1[i] <- v_qcc_rlt[which(attr(v_qcc_rlt,"name") == "address")]
            #  attr_legal_person[i] <- v_qcc_rlt[which(attr(v_qcc_rlt,"name") == "legal_person")]
            #  attr_registered_capital[i] <- v_qcc_rlt[which(attr(v_qcc_rlt,"name") == "registered_capital")]
            #  attr_establish_date[i] <- v_qcc_rlt[which(attr(v_qcc_rlt,"name") == "establish_date")]
            #  attr_op_status[i] <- v_qcc_rlt[which(attr(v_qcc_rlt,"name") == "status")]
            #  
            #  if(attr_contact_qcc[i] != "" & attr_contact_qcc[i] != attr_contact[i]){
            #    attr_contact[i] <- paste(attr_contact[i],attr_contact_qcc[i],sep = " | ")
            #  }
            #}
            #从百度企业信用平台获取更多的企业信息
            v_info_BDXin <- uf_scrape_BaiduXin(v_company)
            v_address1 <- v_info_BDXin[2]
            v_legal_person <- v_info_BDXin[3]
            v_registered_No <- v_info_BDXin[4]
            v_registered_city <- v_info_BDXin[5]
            v_registered_office <- v_info_BDXin[6]
            v_registered_capital <- v_info_BDXin[7]
            v_establish_date <- v_info_BDXin[8]
            v_business_end_date <- v_info_BDXin[9]
            v_business_Sector_RA <- v_info_BDXin[10]
            v_op_status <- v_info_BDXin[11]
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
                            check_rlt[3])
        }
        if(v_company != ""){
          uf_entity_insert_update("DEFAULT",v_entity_all,gv_table_CI5,"INSERT",20)
          if(check_rlt[2] == 1){
            uf_entity_insert_update("DEFAULT",v_entity_all,gv_table_CI5,"UPDATE",20)
          }
        }
      }
    }
  }
  
  return(v_output)
}




uf_entity_existInDB_check <- function(DBConn = "DEFAULT",Entity ,Table, timeouts = 20){
  Conn <- NA
  df.db_return <- NA
  exist <- 0
  update_Require <- 0
  ID <- 0
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
    df.db_return <- tryCatch({
      evalWithTimeout({ sqlQuery(Conn,paste0("SELECT TOP 1 ID,job_issue_latest FROM ",Table
                                             ," WHERE company = N'",iconv(Entity[1], "UTF-8", "gb2312")
                                             ,"' AND industry = N'",iconv(Entity[2], "UTF-8", "gb2312")
                                             ,"' AND sub_industry = N'",iconv(Entity[3], "UTF-8", "gb2312")
                                             ,"' AND city = N'",iconv(Entity[5], "UTF-8", "gb2312")
                                             ,"' AND data_source = N'",iconv(Entity[7], "UTF-8", "gb2312")
                                             #,"' AND DATEADD(DAY,",v_51job_updateFreq,",job_issue_latest) < '", Entity[6]
                                             ,"' AND Is_Current = 1"
                                             ," ORDER BY ID DESC"))
      }, timeout = timeouts)
    },warnings = function(w){
      ;
      NA
    },error = function(e){
      ;
      NA})
    odbcClose(Conn)
  }
  if(class(df.db_return) == "data.frame"){
    if(nrow(df.db_return) >= 1){
      exist <- 1
      diff_days <- as.integer(difftime(as.Date(Entity[6]),as.Date(df.db_return$job_issue_latest),units = "days")) 
      if(diff_days > v_51job_updateFreq){
        update_Require <- 1
        ID <- df.db_return$ID
      }else{
        update_Require <- 0
        ID <- 0
      }
    }else{
      exist <- 0
    }
  }
  return(c(exist,update_Require,ID))
}


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






