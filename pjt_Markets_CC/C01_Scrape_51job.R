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

gv_url_qcc_homr <- "http://www.qichacha.com/"
gv_url_BDXin_home <- "http://xin.baidu.com"
gv_BDXin_js <- "BDXin.js"
gv_BDXin_html <- "./html/BDXin.html"

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
gv_deep_search_OW_list <- c("联系我们")

gv_giveup_companies <- c("四川天赢精采电子商务有限公司")



v_51job_jobSearch_home <- "http://search.51job.com/jobsearch/search_result.php?fromJs=1&"
v_51job_issuedateCode <-                       c("0",       "1",     "2",     "3",     "-1")
attr(v_51job_issuedateCode,"issuedateDesc") <- c("24小时内","近三天","近一周","近一月","所有")
attr(gv_cities,"51job_code") <- c("090200","060000")
attr(gv_salaryRange,"51job_code") <- c("01",      "02",       "03",       "04",       "05",        "06",         "07",         "08",         "09",         "10",       "-1")
attr(gv_companyType,"51job_code") <- c("01",        "02",          "03",  "04",  "05",      "06",        "07",      "08",      "09",        "10",      "11",      "-1")
attr(gv_companySize,"51job_code") <- c("01",      "02",      "03",       "04",        "05",         "06",          "07",         "-1")
v_51job_updateFreq <- 360

########################################################Global Function########################################################
uf_scrape_51job_input <- function(p_home,p_jobArea,p_issuedate,p_providesalary,p_cotype,p_companysize,p_industry){
  if(1==1){
    v_search_url_home <- paste0(p_home,"jobarea=",p_jobArea)
    if(!any(p_issuedate == "-1")){
      v_search_url_home <- paste0(v_search_url_home,"&issuedate=",paste(p_issuedate,collapse = "%2C"))
    }
    if(!any(p_providesalary == "-1")){
      v_search_url_home <- paste0(v_search_url_home,"&providesalary=",paste(p_providesalary,collapse = "%2C"))
    }
    if(!any(p_cotype == "-1")){
      v_search_url_home <- paste0(v_search_url_home,"&cotype=",paste(p_cotype,collapse = "%2C"))
    }
    if(!any(p_companysize == "-1")){
      v_search_url_home <- paste0(v_search_url_home,"&companysize=",paste(p_companysize,collapse = "%2C"))
    }
    if(!any(p_industry == "-1")){
      v_search_url_home <- paste0(v_search_url_home,"&industrytype=",paste(p_industry,collapse = "%2C"))
    }
    v_search_url_home <- paste0(v_search_url_home,"&district=000000&funtype=0000&keywordtype=1&curr_page=1&lang=c&stype=1&postchannel=0000&workyear=99&degreefrom=99&jobterm=99&lonlat=0%2C0&radius=-1&ord_field=0&list_type=0&dibiaoid=0&confirmdate=9")
    
    v_session <- html_session(v_search_url_home)
  }
  
  #获取搜索得到的所有公司对应的页面
  df_companyUrls <- uf_scrape_51job_companyUrl(v_session,p_industry,p_jobArea)
  
  return(df_companyUrls)
  
}
uf_scrape_51job_companyUrl <- function(p_session,p_industry,p_jobArea){
  df_companyUrl <- data.frame(Company_Name = character(0), Url_51job = character(0),Industry = character(0)
                              ,Sub_Industry = character(0), Recruit_City = character(0),job_issue_latest = character(0), scape_date = character(0))
  v_page_num <- p_session %>% html_nodes("span.td") %>% magrittr::extract2(1) %>% html_text()
  v_page_num_len <- regexpr("[1-9][0-9]?[0-9]?[0-9]?",gsub("\\s","",v_page_num)) 
  v_page_num <- substr(v_page_num,v_page_num_len,v_page_num_len + attr(v_page_num_len,"match.length") - 1)
  
  
  sub_ind <- gv_industries$sub_industry[which(gv_industries$sub_industry_code_51job == p_industry)[1]]
  ind <- gv_industries$industry[which(gv_industries$sub_industry_code_51job == p_industry)[1]]
  city <- gv_cities[which(attr(gv_cities,"51job_code") == p_jobArea)[1]]
  for(i in 1:v_page_num){
    #print(i)
    v_nodes <- p_session %>% html_nodes("div.el") %>% html_nodes("span.t2") %>% html_nodes("a") 
    Company_Name <- v_nodes %>% html_attr("title")
    Url_51job <- v_nodes %>% html_attr("href")
    Sub_Industry <- rep(sub_ind,length(Company_Name))
    Industry <- rep(ind,length(Company_Name))
    Recruit_City <- rep(city,length(Company_Name))
    job_issue_latest <- (p_session %>% html_nodes("div.el") %>% html_nodes("span.t5") %>% html_text())[-1]
    scape_date <- rep(as.character(Sys.Date()),length(Company_Name))
    
    df_companyUrl <- rbind(df_companyUrl, cbind(Company_Name, Url_51job, Industry, Sub_Industry, Recruit_City, job_issue_latest, scape_date))
    if(i != v_page_num){
      p_session <- p_session %>% jump_to(p_session %>% html_nodes("li.bk") %>% magrittr::extract2(2)  %>% html_nodes("a") %>% html_attr("href"))
    }
  }
  df_companyUrl <- subset(df_companyUrl, !duplicated(Company_Name))
  
  df_companyUrl <- filter(df_companyUrl,regexpr("http://jobs.51job.com/all/co",df_companyUrl$Url_51job) >= 1)
  
  
  return(df_companyUrl)
}


################Function - uf_scrape_51job
#p_companyUrls <- df_companyUrls_import[g,]
#p_jobArea <- "090200"
#initial_load <- FALSE
#p_companyUrls$Company_Name
#df_test_1 <- uf_scrape_51job(p_companyUrls,p_jobArea,initial_load,FALSE)
#df_test_1$official_website
#df_test_1$official_contact
#df_test_1$contact
uf_scrape_51job <- function(p_companyUrls,p_jobArea,initial_load = F,GoFindOfficalWesite = F, GoFindContact = F, GoBDXin = F){
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
      v_job_issue_latest <- ifelse(regexpr("^[0-1][0-9]-[0-3]?[0-9]",df_companyUrls$job_issue_latest[i]) > 0 , 
                                   paste0(format(Sys.Date(), "%Y"),"-",df_companyUrls$job_issue_latest[i]),
                                   as.character(Sys.Date()))
      
      v_address <- v_address1 <- v_building <- v_contact <- v_official_contact <- v_offical_contacts_qualified <-
        v_coType <- v_coSize <- v_is_local <- v_headquarter <- v_legal_person <- v_registered_No <- v_registered_city <- 
        v_registered_office <- v_registered_capital <- v_establish_date <- v_business_end_date <-
        v_business_Sector_RA <- v_op_status <- v_official_website <- v_web_record_code <- v_coDesc <- v_jobCount_all <- 
        v_jobCount_cd <- v_salaryRange_all <- v_salaryRange_cd <- ""
      
      v_scape_date <- as.character(Sys.Date())
      v_data_source <- "51job"
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
      if(regexpr("http://jobs.51job.com/all/co",v_recruitPage_51job) >= 1){  #如果公司页面的URL满足这样的格式，继续
        html_compnay <- uf_read_html(v_recruitPage_51job,10)
        
        #在公司的招聘页面获取各种信息
        if(!all(is.na(html_compnay))){
          #从51Job获取基本信息
          if("basic_infor" == "basic_infor"){
            v_coattrs <- html_compnay %>% html_nodes("p.ltype") %>% html_text()
            if(length(v_coattrs) > 0){v_coattrs <- unlist(strsplit(gsub("(\r|\n|\t|\\s)","",v_coattrs),split = "\\|"))}else{v_coattrs <- c("","","")}
            v_coType <- v_coattrs[1]
            v_coSize <- v_coattrs[2]
            v_coDesc <- substr(html_compnay %>% html_nodes("div.tCompany_full") %>% html_nodes("div.tBorderTop_box.bt") %>% html_nodes("div.tmsg.inbox") %>% html_nodes("div.con_msg") %>% html_nodes("div.in") %>% html_nodes("p")  %>% html_text(),1,500)
            v_coDesc <- ifelse(length(v_coDesc) > 0, v_coDesc, "")
            #公司地址：
            v_address <- gsub("(\r|\n|\t|\\s)","",html_compnay %>% html_nodes("div.tBorderTop_box") %>% html_nodes("div.bmsg.inbox") %>% html_nodes("p.fp") %>% html_text()) 
            v_address <- ifelse(length(v_address) > 0, substr(gsub("公司地址","",v_address),2,10000), "")    
            v_address <- ifelse(regexpr("\\(邮编",v_address) > 0, substr(v_address,1,regexpr("\\(邮编",v_address)-1),v_address)
            #公司在招人数及薪资范围：
            v_all_joblist <- uf_scrape_51job_openJob(html_compnay)
            v_chengdu_joblist <- c("unknown","unknown")
            html_compnay_local <- uf_read_html(gsub("all",attr(gv_cities,"city_en")[which(attr(gv_cities,"51job_code") == p_jobArea)[1]],v_recruitPage_51job),10)
            if(!all(is.na(html_compnay_local))){v_chengdu_joblist <- uf_scrape_51job_openJob(html_compnay_local)}
            v_chengdu_joblist <- uf_scrape_51job_openJob(html_compnay_local)
            v_jobCount_all <- v_all_joblist[1]
            v_jobCount_cd <- v_chengdu_joblist[1]
            v_salaryRange_all <- v_all_joblist[2]
            v_salaryRange_cd <- v_chengdu_joblist[2]
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

################Function - uf_scrape_baidu_search_company
#p_company <- "成都迪阳贸易有限公司"
#p_Area <- "090200"
#p_searchType <- "官网"
#uf_scrape_baidu_search_company_athome(p_company,p_Area,p_searchType)
uf_scrape_baidu_search_company <- function(p_company,p_Area,p_searchType){
  op_official_website <- ""
  op_contact_phone <- ""
  attr(op_official_website,"code") <- ""
  
  if(!any(gv_baidu_search_type == p_searchType)){
    print("Please input correct search type!")
    return()
  }
  
  v_search_input <- paste(p_company,p_searchType)
  url_baidu_home <- "http://www.baidu.com/"
  session_baidu <- tryCatch({
    html_session(url_baidu_home)
  },warnings = function(w){
    ;
    NA
  },error = function(e){
    ;
    NA 
  })
  
  if(all(is.na(session_baidu))){
    return("")
  }else{
    baidu_form <- html_form(session_baidu)[[1]]
    baidu_form <- set_values(baidu_form,wd = v_search_input)
    session_search <- submit_form(session_baidu, baidu_form)
    searched_hrefs <- session_search %>% html_nodes("a.c-showurl") %>% html_attr("href")
    count_hrefs <- length(searched_hrefs)
    count_hrefs <- ifelse(count_hrefs > gv_baidu_search_topCount, gv_baidu_search_topCount, count_hrefs)
      
    if(any(gv_baidu_search_type[1:3] == p_searchType)){ #搜索联系电话
      for(h in 1:count_hrefs){
        contact_phone = ""
        #print(paste0("开始搜索联系电话",as.character(h)))
        searched_href <- searched_hrefs[h]
        session_search <- session_search  %>% jump_to(searched_href)
        
        next_url <- session_search %>% html_node("script") %>% html_text()
        next_url <- substr(gsub("^window.location.replace","",next_url),3,nchar(gsub("^window.location.replace","",next_url)) - 2)
        
        contact_phone <- uf_scrape_officialWebsite_contact(next_url,p_Area,gv_ptns_phone_number[c(1,3)])
        
        
        op_contact_phone <- paste(op_contact_phone,contact_phone,sep = " | ")
        op_contact_phone <- gsub("\\s\\|\\s$","",gsub("^\\s\\|\\s","",op_contact_phone))  
        op_contact_phone <- gsub("\\s{1,10}$","",gsub("^\\s{1,10}","",op_contact_phone)) 
      }
      
      return(op_contact_phone)
    }else if(any(gv_baidu_search_type[4] == p_searchType)){ #搜索官网
      for(h in 1:count_hrefs){
        #print(paste0("开始搜索官网",as.character(h)))
        searched_href <- searched_hrefs[h]
        session_search <- session_search  %>% jump_to(searched_href)
        next_url <- session_search %>% html_node("script") %>% html_text()
        next_url <- substr(gsub("^window.location.replace","",next_url),3,nchar(gsub("^window.location.replace","",next_url)) - 2)
        
        no_off <- gv_not_official
        is_off <- TRUE
        for(n in 1:length(no_off)){
          if(regexpr(no_off[n],next_url) > 0){
            is_off <- FALSE
            break
          }
        }
        if(is_off){
          html_guanwang <- uf_read_html(next_url,10)
          text_url <- ""
          if(!all(is.na(html_guanwang))){
            text_url <- html_guanwang %>% html_text()
          }
          
          if(text_url != ""){
            ptn_off <- ".ICP\\s?\\s?备(\\s{1,}|:|：)?[0-9]{8}号"
            reg_count <- regexpr(ptn_off,text_url)
            if(reg_count > 0){  #this is a offical website
              op_official_website <- next_url
              attr(op_official_website,"code") <- substr(text_url,reg_count,reg_count + attr(reg_count,"match.length") - 1)
              #验证备案号
              if(!uf_verify_official_website(p_company,next_url,attr(op_official_website,"code"))){
                op_official_website <- ""
              }
              break
            }
          }
        }
      }
      return(op_official_website)
    }
    
  }
  
}
uf_scrape_baidu_search_company_athome <- function(p_company,p_Area,p_searchType){
  op_official_website <- ""
  op_contact_phone <- ""
  attr(op_official_website,"code") <- ""
  
  if(!any(gv_baidu_search_type == p_searchType)){
    print("Please input correct search type!")
    return()
  }
  
  v_search_input <- paste(p_company,p_searchType)
  url_baidu_home <- "http://www.baidu.com/"
  session_baidu <- tryCatch({
    html_session(url_baidu_home)
  },warnings = function(w){
    ;
    NA
  },error = function(e){
    ;
    NA 
  })
  
  if(all(is.na(session_baidu))){
    return("")
  }else{
    baidu_form <- html_form(session_baidu)[[1]]
    baidu_form <- set_values(baidu_form,wd = v_search_input)
    #print("baidu1")
    #session_search <- submit_form(session_baidu, baidu_form)
    session_search <- tryCatch({
      evalWithTimeout({ submit_form(session_baidu, baidu_form) }, timeout = 10)
    },warnings = function(w){
      ;
      NA
    },error = function(e){
      ;
      NA
    })
    if(!all(is.na(session_search))){
      #print("baidu2")
      searched_hrefs <- session_search %>% html_nodes("a.c-showurl") %>% html_attr("href")
      count_hrefs <- length(searched_hrefs)
      count_hrefs <- ifelse(count_hrefs > gv_baidu_search_topCount - 1, gv_baidu_search_topCount - 1, count_hrefs)
      
      if(any(gv_baidu_search_type[1:3] == p_searchType)){ #搜索联系电话
        #print("开始搜索联系电话....")
        for(h in 1:count_hrefs){
          contact_phone = ""
          #print(paste0("开始搜索联系电话",as.character(h)))
          searched_href <- searched_hrefs[h]
          #print("athome1")
          session_search <- tryCatch({
            evalWithTimeout({ session_search  %>% jump_to(searched_href) }, timeout = 10)
          },warnings = function(w){
            ;
            NA
          },error = function(e){
            ;
            NA
          })
          if(!all(is.na(session_search))){
            next_url <- (session_history(session_search)[-1])[[1]]
            contact_phone <- uf_scrape_officialWebsite_contact(next_url,p_Area,gv_ptns_phone_number[c(1,3)])
            
            
            op_contact_phone <- paste(op_contact_phone,contact_phone,sep = " | ")
            op_contact_phone <- gsub("\\s\\|\\s$","",gsub("^\\s\\|\\s","",op_contact_phone))  
            op_contact_phone <- gsub("\\s{1,10}$","",gsub("^\\s{1,10}","",op_contact_phone)) 
          }
          
        }
        
        
        return(op_contact_phone)
      }else if(any(gv_baidu_search_type[4] == p_searchType)){ #搜索官网
        #print("开始搜索官网....")
        for(h in 1:count_hrefs){
          #print(paste0("开始搜索官网",as.character(h)))
          searched_href <- searched_hrefs[h]
          #print("athome2")
          session_search <- tryCatch({
            evalWithTimeout({ session_search  %>% jump_to(searched_href) }, timeout = 10)
          },warnings = function(w){
            ;
            NA
          },error = function(e){
            ;
            NA
          })
          #session_search <- session_search  %>% jump_to(searched_href)
          if(!all(is.na(session_search))){
            next_url <- (session_history(session_search)[-1])[[1]]
            
            no_off <- gv_not_official
            is_off <- TRUE
            for(n in 1:length(no_off)){
              if(regexpr(no_off[n],next_url) > 0){
                is_off <- FALSE
                break
              }
            }
            if(is_off){
              html_guanwang <- uf_read_html(next_url,10)
              text_url <- ""
              if(!is.na(html_guanwang)){
                text_url <- html_guanwang %>% html_text()
              }
              
              if(text_url != ""){
                ptn_off <- ".ICP\\s?\\s?备(\\s{1,}|:|：)?[0-9]{8}号"
                reg_count <- regexpr(ptn_off,text_url)
                if(reg_count > 0){  #this is a offical website
                  op_official_website <- next_url
                  attr(op_official_website,"code") <- substr(text_url,reg_count,reg_count + attr(reg_count,"match.length") - 1)
                  #验证备案号
                  if(!uf_verify_official_website(p_company,next_url,attr(op_official_website,"code"))){
                    op_official_website <- ""
                  }
                  break
                }
              }
            }
          }
          
        }
        return(op_official_website)
      }
      
    }else{
      return("")
    }
    
  }
  
}


################Function - uf_scrape_officialWebsite_contact
#p_company_link <- "http://www.cddypos.cn"
#p_jobArea <- "090200"
#p_ptns_phone_number <- gv_ptns_phone_number
uf_scrape_officialWebsite_contact <- function(p_company_link,p_jobArea,p_ptns_phone_number){
  op_contact_phone <- ""
  phone_area_code <- attr(gv_cities,"phoneCode")[which(attr(gv_cities,"51job_code") == p_jobArea)[1]]
  
  s_ptns_phone_number <- p_ptns_phone_number
  op_contact_phone <- uf_scrape_contactOnWebPage(p_company_link,phone_area_code,s_ptns_phone_number)
  
  op_contact_phone <- gsub("\\s\\|\\s$","",gsub("^\\s\\|\\s","",op_contact_phone))  
  
  return(op_contact_phone)
  
}


################Function - uf_scrape_contactOnWebPage
#p_web <- "http://www.sharegroup.cn" #
#phone_area_code <- "028"
#p_ptns_phone_number <- gv_ptns_phone_number
uf_scrape_contactOnWebPage <- function(p_web,phone_area_code,p_ptns_phone_number){
  s_ptns_phone_number <- p_ptns_phone_number
  contact_phone <- ""
  op_contact_phone <- ""
  html_web <- uf_read_html(p_web,10)
  
  if(!all(is.na(html_web))){
    text_all <- html_web %>% html_node("body") %>% html_text()
    if(!is.na(text_all)){
      for(s in 1:length(s_ptns_phone_number)){
        s_ptns_phone <- s_ptns_phone_number[s]
        if(s_ptns_phone == gv_ptns_phone_number[1]){
          s_ptns_phone <- gsub("000",phone_area_code,s_ptns_phone)
        }
        count_phone <- (gregexpr(s_ptns_phone, text_all))[[1]]
        counts <- length(count_phone)
        
        if(any(count_phone >0)){
          contact_phone <- ""
          for(n in 1:counts){
            ctc_phone <- substr(text_all,count_phone[n],attr(count_phone,"match.length")[n] + count_phone[n] - 1)
            ctc_phone <- substr(ctc_phone,2,nchar(ctc_phone))
            contact_phone <- paste0(contact_phone," | ",ctc_phone)
            contact_phone <- gsub("^\\s\\|\\s","",contact_phone)
          }
          
        }
        
        if(contact_phone != ""){
          op_contact_phone <- paste0(op_contact_phone," | ",contact_phone)
        }
      }
    }
    
  }
  op_contact_phone <- gsub("^\\s?\\|\\s?","",op_contact_phone)
  return(op_contact_phone)
}

#p_web <- "http://www.daqsoft.com"  #"http://www.chinanetpro.net/", http://www.roymark.com.cn
#p_jobArea <- "090200"
#p_ptns_phone_number <- gv_ptns_phone_number
#gv_deep_search_OW_list <- c("联系我们")
#uf_scrape_contact_OW_Deep(p_web,p_jobArea,p_ptns_phone_number)
uf_scrape_contact_OW_Deep <- function(p_web,p_jobArea,p_ptns_phone_number){
  op_contact_phone <- ""
  phone_area_code <- attr(gv_cities,"phoneCode")[which(attr(gv_cities,"51job_code") == p_jobArea)[1]]
  
  html_web <- uf_read_html(p_web,10)
  if(!all(is.na(html_web))){
    next_link <- ""
    #情况1：下级链接在<a>中
    href_nodes <- html_web %>% html_nodes("a")
    href_nodes_text <- href_nodes %>% html_text()
    len_href_nodes_text <- length(href_nodes_text)
    if(len_href_nodes_text > 0){
      for(i in 1:len_href_nodes_text){
        #print(i)
        if(href_nodes_text[i] == gv_deep_search_OW_list[1]){
          next_link <- href_nodes[i] %>% html_attr("href")
          if(regexpr("javascript",ifelse(is.na(next_link),"javascript",next_link)) < 0){
            next_link <- paste0(gsub("\\/$","",p_web),ifelse(regexpr("^\\/",next_link) < 0, paste0("/",next_link), next_link))
            break()
          }
        }
      }
    }
    
    if(next_link != ""){
      op_contact_phone <- uf_scrape_contactOnWebPage(next_link,phone_area_code,p_ptns_phone_number)
      op_contact_phone <- gsub("\\s\\|\\s$","",gsub("^\\s\\|\\s","",op_contact_phone))  
    }
    
  }
  
  return(op_contact_phone)
}

################Function - uf_scrape_51job_openJob
#p_html_compnay <- html_compnay
uf_scrape_51job_openJob <- function(p_html_compnay){
  op_result <- c("Unkown","Unkown")
  
  if(!all(is.na(p_html_compnay))){
    op_result_inputs <- p_html_compnay %>% html_nodes("ul")  %>% html_nodes("input")
    if(length(op_result_inputs) > 0){
      op_result_num <- p_html_compnay %>% html_nodes("ul")  %>% html_nodes("input") %>% magrittr::extract2(1) %>% html_attr("value")
      op_result_num <- ifelse(length(op_result_num) > 0, op_result_num, "Unknown")
      
      op_result_salaries <- p_html_compnay %>% html_nodes("div.el") %>% html_nodes("span.t4") %>% html_text()
      count_op_result_salaries <- length(op_result_salaries)
      low_ss <- rep("0",count_op_result_salaries)
      low_hs <- rep("0",count_op_result_salaries)
      for(s in 1:count_op_result_salaries){
        op_result_salary <- op_result_salaries[s]
        if(regexpr("^[1-9][0-9]{3,5}-[1-9][0-9]{3,5}/月$",op_result_salary) > 0){
          pst_1 <- regexpr("-",op_result_salary)
          pst_2 <- regexpr("/月",op_result_salary)
          low_s <- substr(op_result_salary,1,pst_1 - 1)
          low_h <- substr(op_result_salary,pst_1 + 1,pst_2 - 1)
          
        }else if(regexpr("^[1-9]\\.?[0-9]?\\.?[0-9]?\\.?[0-9]{0,3}-[1-9]\\.?[0-9]?\\.?[0-9]?\\.?[0-9]{0,3}万/年$",op_result_salary) > 0){
          pst_1 <- regexpr("-",op_result_salary)
          pst_2 <- regexpr("万/年",op_result_salary)
          low_s <- (as.numeric(substr(op_result_salary,1,pst_1 - 1)) * 10000) / 12 
          low_h <- (as.numeric(substr(op_result_salary,pst_1 + 1,pst_2 - 1)) * 10000) / 12  
        }else if(regexpr("^[1-9]\\.?[0-9]?\\.?[0-9]?\\.?[0-9]{0,3}-[1-9]\\.?[0-9]?\\.?[0-9]?\\.?[0-9]{0,3}千/月$",op_result_salary) > 0){
          pst_1 <- regexpr("-",op_result_salary)
          pst_2 <- regexpr("千/月",op_result_salary)
          low_s <- as.numeric(substr(op_result_salary,1,pst_1 - 1)) * 1000
          low_h <- as.numeric(substr(op_result_salary,pst_1 + 1,pst_2 - 1)) * 1000
        }else if(regexpr("^[0-9]\\.?[0-9]?\\.?[0-9]?\\.?[0-9]{0,3}-[0-9]\\.?[0-9]?\\.?[0-9]?\\.?[0-9]{0,3}万/月$",op_result_salary) > 0){
          pst_1 <- regexpr("-",op_result_salary)
          pst_2 <- regexpr("万/月",op_result_salary)
          low_s <- as.numeric(substr(op_result_salary,1,pst_1 - 1)) * 10000
          low_h <- as.numeric(substr(op_result_salary,pst_1 + 1,pst_2 - 1)) * 10000
        }else{
          low_s = 0
          low_h = 0
        }
        low_ss[s] <- low_s
        low_hs[s] <- low_h
      }
      low_ss <- low_ss[low_ss != "0"]
      lowest_s <- min(as.numeric(low_ss))
      highest_s <- max(as.numeric(low_hs))
      
      op_result[1] <- op_result_num
      op_result[2] <- paste0(as.character(lowest_s),"-",highest_s,"/月")
    }
  }
  
  return(op_result)
}



################Function - uf_scrape_qcc_basic
#p_company <- "成都宣池汽车贸易有限公司"
#uf_scrape_qcc_basic(p_company)
uf_scrape_qcc_basic <- function(p_company){
  v_op <- c("","","","","","","")
  attr(v_op,"name") <- c("company","contact","address","legal_person","registered_capital","establish_date","status")
  
  v_url_search_rlt <- paste0(gv_url_qcc_homr,"search?key=",p_company) 
  html_rlt <- uf_read_html(v_url_search_rlt,10)
    
  if(!all(is.na(html_rlt))){
    nodes_tbodys <- html_rlt %>% html_nodes("table.m_srchList") %>% html_nodes("tbody")
    if(length(nodes_tbodys)>0){
      nodes_tbody <- html_rlt %>% html_nodes("table.m_srchList") %>% html_nodes("tbody") %>% magrittr::extract2(1)
      if(!all(is.na(nodes_tbody))){
        v_company_name <- nodes_tbody %>% html_nodes("td") %>% html_nodes("a.ma_h1") %>% magrittr::extract2(1) %>% html_text()
        nodes_tds <- nodes_tbody %>% html_nodes("tr") %>% magrittr::extract2(1) %>% html_nodes("td") 
        infor_base <- nodes_tds %>% magrittr::extract2(2)  %>% html_text()
        infor_base <- gsub("\\s{0,}\\\n\\s{0,}$","",gsub("^\\s{1,}","",gsub("\\\t","",infor_base))) 
        infor_bases <- unlist(strsplit(infor_base, split="\\\n"))
        v_legal_person <- ifelse(nchar(infor_bases[2])>5,substr(infor_bases[2],6,1000),"")
        v_contact <- ifelse(nchar(infor_bases[3])>5, substr(infor_bases[3],6,1000), "") 
        v_address <- ifelse(nchar(infor_bases[4])>3,substr(infor_bases[4],4,1000),"")
        v_registered_capital <-gsub("\\s{1,}$","",gsub("^\\s{1,}","",nodes_tds %>% magrittr::extract2(3)  %>% html_text()))
        v_establish_date <- gsub("\\s{1,}$","",gsub("^\\s{1,}","",nodes_tds %>% magrittr::extract2(4)  %>% html_text()))
        v_status <- gsub("\\s{1,}$","",gsub("^\\s{1,}","",nodes_tds %>% magrittr::extract2(5)  %>% html_text()))
        
        v_op[1:7] <- c(v_company_name,v_contact,v_address,v_legal_person,v_registered_capital,v_establish_date,v_status)
      }
    }
  }
  return(v_op)
  
}


################Function - uf_scrapeJSSite
#url <- "http://xin.baidu.com/detail/compinfo?pid=qpVv3GU6iErsOeMmZ4gxh1xQxpzGhAtc8wVy"
#p_html <- gv_BDXin_html
#p_js <- gv_BDXin_js
#uf_scrapeJSSite(url,p_html,p_js)
uf_scrapeJSSite <- function(url,p_html,p_js){
  if(file.exists(p_html)){
    file.remove(p_html)
  }
  lines <- readLines(paste0("./jsFiles/",p_js))
  lines[1] <- paste0("var url ='", url ,"';")
  lines[2] <- paste0("var company_html = '",p_html,"';")
  writeLines(lines, paste0("./jsFiles/",p_js))
  
  p_system <- paste0("phantomjs ","./jsFiles/",p_js)
  
  ## Download website
  rlt <- FALSE
  rlt <- tryCatch({
    system(p_system);
    TRUE
  },warnings = function(w){
    ;
    FALSE
  },error = function(e){
    ;
    FALSE}) 
  
  return(rlt)
}

################Function - uf_scrape_BaiduXin
#p_company <- "北京花海阁婚礼策划有限公司"
#uf_scrape_BaiduXin(p_company)
uf_scrape_BaiduXin <- function(p_company){
  v_op <- rep("",11)
  attr(v_op,"name") <- c("company","address1","legal_person","registered_No","registered_city","registered_office","registered_capital","establish_date","business_end_date","business_Sector_RA","op_status")
  
  v_url_search_rlt <- paste0(gv_url_BDXin_home,"/s?q=",p_company) 
  html_rlt <- uf_read_html(v_url_search_rlt,10)
  
  if(!all(is.na(html_rlt))){
    hrefs  <- html_rlt %>% html_nodes("div.zx-ent-info") %>% html_nodes("div.zx-ent-items") %>% html_nodes("h3.zx-ent-title") %>% 
      html_nodes("a.zx-list-item-url") %>% html_attr("href")
    if(length(hrefs) > 0){
      relative.path <- hrefs[1]
      if(length(relative.path) > 0 & regexpr("detail\\/compinfo\\?pid=",relative.path) > 1){
        obs.path <- paste0(gv_url_BDXin_home,relative.path)
        if(uf_scrapeJSSite(obs.path,gv_BDXin_html,gv_BDXin_js)){
          html_Detail <- read_html(gv_BDXin_html)
          company_title <- html_Detail %>% html_nodes("div.zx-detail-company") %>% html_nodes("div.zx-detail-company-info") %>% html_nodes("h2.zx-detail-company-title") %>% html_text()
          v_op[1] <-ifelse(length(company_title) > 0, company_title, "Not Found")  
          if(stringsim(v_op[1],p_company,method = gv_method_strSim) >= 0.8){
            info_text <- read_html(gv_BDXin_html) %>% html_text()
            
            address1_reg_count <- regexpr("所在地址..{0,50}\\s{0,3}经营范围",info_text)
            if(address1_reg_count > 0){
              address1_reg <- substr(info_text,address1_reg_count,address1_reg_count + attr(address1_reg_count,"match.length") - 1)
              address1_reg <- gsub("\\s","",address1_reg)
              address1 <- substr(address1_reg,5 + 1,nchar(address1_reg) - 4)
              v_op[2] <- address1
            }
            if(v_op[2] == ""){
              v_op[2] <- "NotFound"
            }
            
            legal_person_reg_count <- regexpr("法定代表人.(.{0,5}|[a-zA-Z]{1,20}\\s{0,2}[a-zA-Z]{0,20}\\s{0,2}[a-zA-Z]{0,20})\\s{0,3}成立日期",info_text)
            if(legal_person_reg_count > 0){
              legal_person_reg <- substr(info_text,legal_person_reg_count,legal_person_reg_count + attr(legal_person_reg_count,"match.length") - 1)
              if(nchar(legal_person_reg) != nchar(legal_person_reg,type = "bytes")){
                legal_person <- substr(legal_person_reg,6 + 1,nchar(legal_person_reg) - 4)
              }else{
                legal_person_reg <- gsub("\\s","",legal_person_reg)
                legal_person <- substr(legal_person_reg,6 + 1,nchar(legal_person_reg) - 4)
              }
              v_op[3] <- legal_person
            }
            
            registered_No_reg_count <- regexpr("注册号.[0-9]{11,17}\\s{0,3}税务登记证号",info_text)
            if(registered_No_reg_count > 0){
              registered_No_reg <- substr(info_text,registered_No_reg_count,registered_No_reg_count + attr(registered_No_reg_count,"match.length") - 1)
              registered_No_reg <- gsub("\\s","",registered_No_reg)
              registered_No <- substr(registered_No_reg,4 + 1,nchar(registered_No_reg) - 6)
              v_op[4] <- registered_No
            }
            
            registered_city_reg_count <- regexpr("行政区划..{0,20}\\s{0,3}登记机关",info_text)
            if(registered_city_reg_count > 0){
              registered_city_reg <- substr(info_text,registered_city_reg_count,registered_city_reg_count + attr(registered_city_reg_count,"match.length") - 1)
              registered_city_reg <- gsub("\\s","",registered_city_reg)
              registered_city <- substr(registered_city_reg,5 + 1,nchar(registered_city_reg) - 4)
              v_op[5] <- registered_city
            }
            
            registered_office_reg_count <- regexpr("登记机关..{0,30}\\s{0,3}电话号码",info_text)
            if(registered_office_reg_count > 0){
              registered_office_reg <- substr(info_text,registered_office_reg_count,registered_office_reg_count + attr(registered_office_reg_count,"match.length") - 1)
              registered_office_reg <- gsub("\\s","",registered_office_reg)
              registered_office <- substr(registered_office_reg,5 + 1,nchar(registered_office_reg) - 4)
              v_op[6] <- registered_office
            }
            
            registered_capital_reg_count <- regexpr("注册资本.[0-9]{1,7}.?[0-9]{0,4}.?[0-9]{0,4}.{1,6}\\s{0,3}该企业资金实力",info_text)
            if(registered_capital_reg_count > 0){
              registered_capital_reg <- substr(info_text,registered_capital_reg_count,registered_capital_reg_count + attr(registered_capital_reg_count,"match.length") - 1)
              registered_capital_reg <- gsub("\\s","",registered_capital_reg)
              registered_capital <- substr(registered_capital_reg,5 + 1,nchar(registered_capital_reg) - 7)
              v_op[7] <- registered_capital
            }
            
            establish_date_reg_count <- regexpr("成立日期.[0-9]{2,4}(-)[0-1][0-9](-)[0-3][0-9]\\s{0,3}审核",info_text)
            if(establish_date_reg_count > 0){
              establish_date_reg <- substr(info_text,establish_date_reg_count,establish_date_reg_count + attr(establish_date_reg_count,"match.length") - 1)
              establish_date_reg <- gsub("\\s","",establish_date_reg)
              establish_date <- substr(establish_date_reg,5 + 1,nchar(establish_date_reg) - 2)
              v_op[8] <- establish_date
            }
            
            business_end_date_reg_count <- regexpr("至\\s({0,3}[0-9]{2,4}(-)[0-1][0-9](-)[0-3][0-9]|.{0,9})注册资本",info_text)
            if(business_end_date_reg_count > 0){
              business_end_date_reg <- substr(info_text,business_end_date_reg_count,business_end_date_reg_count + attr(business_end_date_reg_count,"match.length") - 1)
              business_end_date_reg <- gsub("\\s","",business_end_date_reg)
              business_end_date <- substr(business_end_date_reg,1 + 1,nchar(business_end_date_reg) - 4)
              v_op[9] <- business_end_date
            }
            
            business_Sector_RA_reg_count <- regexpr("经营范围..{0,255}\\s{0,3}股东",info_text)
            if(business_Sector_RA_reg_count > 0){
              business_Sector_RA_reg <- substr(info_text,business_Sector_RA_reg_count,business_Sector_RA_reg_count + attr(business_Sector_RA_reg_count,"match.length") - 1)
              business_Sector_RA_reg <- gsub("\\s","",business_Sector_RA_reg)
              business_Sector_RA <- substr(business_Sector_RA_reg,5 + 1,nchar(business_Sector_RA_reg) - 2)
              v_op[10] <- business_Sector_RA
            }
            
            op_status_reg_count <- regexpr("经营状态..{0,5}\\s{0,3}营业期限",info_text)
            if(op_status_reg_count > 0){
              op_status_reg <- substr(info_text,op_status_reg_count,op_status_reg_count + attr(op_status_reg_count,"match.length") - 1)
              op_status_reg <- gsub("\\s","",op_status_reg)
              op_status <- substr(op_status_reg,5 + 1,nchar(op_status_reg) - 4)
              v_op[11] <- op_status
            }
          }else if(stringsim(v_op[1],p_company,method = gv_method_strSim) >= 0.2){
            v_op[2] <- "NotFound"
          }
        }
      }
    }
  }
  return(v_op)
  
}


################Function - uf_verify_official_website
#p_company <- "成都悠途信息技术有限公司"
#p_url <- "http://www.uto360.com/"
#p_record_code <- "蜀ICP备15023964号"
#uf_verify_official_website(p_company, p_url, p_record_code)
uf_verify_official_website <- function(p_company, p_url, p_record_code){
  rlt <- FALSE
  search_url <- paste0("http://www.beianbeian.com/s?keytype=1&q=",p_record_code)

  html_record_code <- uf_read_html(search_url,10)
    
  if(!all(is.na(html_record_code))){
    is_strong <- html_record_code %>% html_nodes("table.seo") %>% html_nodes("tr") %>% html_nodes("td") %>% html_nodes("strong")
    if(length(is_strong) == 0){
      vrfy_company <- html_record_code %>% html_nodes("table.seo") %>% html_nodes("tr") %>% magrittr::extract2(2) %>% html_nodes("td") %>% magrittr::extract2(2) %>% html_nodes("div") %>% html_text()
      if(length(vrfy_company) == 0){
        vrfy_company <- html_record_code %>% html_nodes("table.seo") %>% html_nodes("tr") %>% magrittr::extract2(2) %>% html_nodes("td") %>% magrittr::extract2(2) %>% html_text()
      }
      if(length(vrfy_company) != 0){
        if(stringsim(vrfy_company,p_company,method = gv_method_strSim) >= 0.5){
          rlt <- TRUE
        }
      }
    }
  }
  return(rlt)
}

################Function - uf_verify_official_website
#p_company <- "成都微科技有限公司"
#p_url <- "http://www.uto360.com/"
#p_record_code <- "蜀ICP备15023964号"
#uf_find_official_beianbeian(p_company)
uf_find_official_beianbeian <- function(p_company){
  rlt <- ""
  search_url <- paste0("http://www.beianbeian.com/s?keytype=2&q=",p_company)
  
  html_rlt <- uf_read_html(search_url,10)
  if(!all(is.na(html_rlt))){
    divs <- html_rlt %>% html_nodes("table") %>% html_nodes("div") 
    attrs <- divs %>% html_attr("id")
    url_num <- which(attrs == "home_url")
    if(length(url_num) > 0){
      o_url <- divs[url_num[1]] %>% html_text()
      o_url <- gsub("\\s{0,}$","",gsub("^\\s{0,}","",gsub("(\\\r|\\\n)","",o_url)))
      rlt <- o_url
    }
  }
  return(rlt)
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



################Function - uf_entity_existInDB_check
#DBConn <- "DEFAULT"
#Entity <- v_entity
#timeouts <- 20
#Table <- gv_table_CI5
#uf_entity_existInDB_check(DBConn,Entity,timeouts)
uf_entity_existInDB_check <- function(DBConn,Entity,Table,timeouts){
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

  
################Function - uf_entity_insert_update
#DBConn <- "DEFAULT"
#Entity <- v_entity_all
#Table <- "CN_CD_CC.Companies_Infor_51Job"
#Ops <- "INSERT"
#timeouts <- "INSERT"
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
















































































































































































































































































































































