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




########################################################Global Variables########################################################
uf_xlsxDataImport <- function(xlsx_file,spreedsheet){
  df_xlsx <- read.xlsx(xlsx_file,sheetName = spreedsheet,header=TRUE,encoding = "UTF-8")
  return(df_xlsx)
}
uf_xlsxDataExport <- function(p_df, xlsx_file, spreedsheet, append = FALSE){
  xlsx::write.xlsx(p_df,xlsx_file,sheetName = spreedsheet, append = append)
}

gv_baidu_search_type <- c("联系电话","联系方式","电话","官网")

gv_url_51job_jobSearch_home <- "http://search.51job.com/jobsearch/search_result.php?fromJs=1&"
gv_url_qcc_homr <- "http://www.qichacha.com/"

gv_51job_jobAreaCode <-              c("090200","060000")
attr(gv_51job_jobAreaCode,"city") <- c("成都",  "重庆")
attr(gv_51job_jobAreaCode,"city_en") <- c("Chengdu",  "Chongqing")
attr(gv_51job_jobAreaCode,"alias1") <- c("蜀",  "渝")
attr(gv_51job_jobAreaCode,"alias2") <- c("川",  "渝")
attr(gv_51job_jobAreaCode,"phoneCode") <- c("028","023")


gv_51job_issuedateCode <-                       c("0",       "1",     "2",     "3",     "-1")
attr(gv_51job_issuedateCode,"issuedateDesc") <- c("24小时内","近三天","近一周","近一月","所有")

gv_51job_providesalaryCode <-                           c("01",      "02",       "03",       "04",       "05",        "06",         "07",         "08",         "09",         "10",       "-1")
attr(gv_51job_providesalaryCode,"providesalaryDesc") <- c("2000以下","2001-3000","3001-4500","4501-8000","8001-10000","10001-15000","15001-20000","20001-30000","30001-50000","50000以上","所有")

gv_51job_cotypeCode <-                    c("01",        "02",          "03",  "04",  "05",      "06",        "07",      "08",      "09",        "10",      "11",      "-1")
attr(gv_51job_cotypeCode,"cotypeDesc") <- c("外资(欧美)","外资(非欧美)","合资","国企","民营公司","外企代表处","政府机关","事业单位","非营利机构","上市公司","创业公司","所有")

gv_51job_companysizeCode <-                         c("01",      "02",      "03",       "04",        "05",         "06",          "07",         "-1")
attr(gv_51job_companysizeCode,"companysizeDesc") <- c("少于50人","50-150人","150-500人","500-1000人","1000-5000人","5000-10000人","10000人以上","所有")

gv_51job_industries <- uf_xlsxDataImport("Job_Industry_Category_51job.xlsx","Industry")

gv_not_official <- c("zhaopin","51job","lagou","ganji","baidu","dajie","hao123","atobo","jobui","yl1001","qixin","pe168","caihao","5858","cyzone","xizhi","lvse","cn716","chinahr",
                     "metalnews","ciidoo","12580.tv","yellowurl","qy.58","kanzhun","1024sj","wealink","hc360","b2b168","9928.tv","hunt007","huix","cnlinfo")

gv_yellow_pages <- c("http://www.yellowurl.cn/","http://qy.58.com/","http://www.ciidoo.com/","http://www.11467.com/")
attr(gv_yellow_pages,"identity") <- c("yellowurl","qy.58","ciidoo","11467")

gv_baidu_search_topCount <- 7

gv_ptns_phone_number <- c(paste0("[^0-9]","000","(\\)|.)?","\\s?(\\s|-)?\\s?[5-9][0-9]{3}-?[0-9]{3,4}"),
                          "[^0-9]400(-|\\s)?[0-9]{3,8}(-|\\s)?[0-9]{3,8}",
                          "[^0-9]1(3|6|7|8|5)[0-9]{9}")
#1. e.g. 028-12345678; (028)12345678; 028 12345678
#2. e.g. 400-1234-1234
#3. e.g. 13702938495

gv_method_strSim <- "lcs"




########################################################Global Function########################################################
#p_home <- gv_url_51job_jobSearch_home
#p_jobArea <- gv_51job_jobAreaCode[1]
#p_industry <- gv_51job_industries$sub_industry_code[1]
#p_issuedate <- gv_51job_issuedateCode[5]
#p_providesalary <- gv_51job_providesalaryCode[11]
#p_cotype <- gv_51job_cotypeCode[12]
#p_companysize <- gv_51job_companysizeCode[8]
#df_companies_51urls <- uf_scrape_51job_input(p_home,p_jobArea,p_issuedate,p_providesalary,p_cotype,p_companysize,p_industry)
#uf_xlsxDataExport(df_companies_51urls,paste0("companies_51urls_",attr(gv_51job_jobAreaCode,"city_en")[which(gv_51job_jobAreaCode == p_jobArea)[1]],".xlsx"),gv_51job_industries$sub_industry_en[which(gv_51job_industries$sub_industry_code == p_industry)[1]])

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


################Function - uf_scrape_51job
#p_companyUrls <- df_companyUrls_import[629,]
#p_jobArea <- "090200"
#p_companyUrls$Company_Name
#df_test_1 <- uf_scrape_51job(p_companyUrls,p_jobArea)
#df_test_1$official_website
#df_test_1$official_contact
#df_test_1$contact
uf_scrape_51job <- function(p_companyUrls,p_jobArea){
  df_companyUrls <- p_companyUrls
  
  #定义输出变量
  if(1==1){
    v_count_companies <- nrow(df_companyUrls)
    df_output <- data.frame(company = "", address = "", address1 = "", city = "", building = "", contact = "", official_contact = "", coType = "", coSize = "", industry = "", sub_industry = "", 
                            is_local = "", headquarter = "", legal_person = "", registered_capital = "", establish_date = "", business_Sector_RA = "",op_status = "", 
                            official_website = "", web_record_code = "", coDesc = "", jobCount_all = "",
                            jobCount_cd = "", salaryRange_all = "", salaryRange_cd = "", job_issue_latest = "", scape_date = "", data_source = "", recruitPage_51job = "")[0]
    temp_v_attr <- rep("",v_count_companies)
    attr_building <- temp_v_attr
    attr_sub_industry <- temp_v_attr
    attr_is_local <- temp_v_attr
    attr_headquarter <- temp_v_attr
    attr_business_Sector_RA <- temp_v_attr
    attr_company <- temp_v_attr
    attr_address <- temp_v_attr
    attr_address1 <- temp_v_attr
    attr_cotype <- temp_v_attr
    attr_size <- temp_v_attr
    attr_industry <- temp_v_attr
    attr_legal_person <- temp_v_attr
    attr_registered_capital <- temp_v_attr
    attr_establish_date <- temp_v_attr
    attr_op_status <- temp_v_attr
    attr_contact_qcc <- temp_v_attr
    attr_desc <- temp_v_attr
    attr_jobCount_all <- temp_v_attr
    attr_jobCount_cd <- temp_v_attr
    attr_salaryRange_all <- temp_v_attr
    attr_salaryRange_cd <- temp_v_attr
    attr_link <- temp_v_attr
    attr_web_record_code <- temp_v_attr
    attr_job_issue_latest <- temp_v_attr
    attr_scape_date <- temp_v_attr
    attr_data_source <- rep("51job",v_count_companies)
    attr_51jobRecruit_page <- temp_v_attr
    attr_contact <- temp_v_attr
    attr_official_contact <- temp_v_attr
  }
  
  #循环搜索所有公司的页面
  for(i in 1:v_count_companies){
    #公司名称：
    v_company <- df_companyUrls$Company_Name[i]
    attr_company[i] <- v_company
    
    #print(paste0("搜索开始-",v_company))
    #公司对应的51job的职位展示页面
    v_company_url <- df_companyUrls$Url_51job[i]
    attr_51jobRecruit_page[i] <- v_company_url
    
    if(regexpr("http://jobs.51job.com/all/co",v_company_url) >= 1){  #如果公司页面的URL满足这样的格式，继续
      #获得公司页面的html信息
      #print("read1")
      html_compnay <- uf_read_html(v_company_url,10)
      
      #在公司的招聘页面获取各种信息
      if(!all(is.na(html_compnay))){
        #公司类型/公司规模/公司行业：e.g. 民营公司
        v_coattrs <- html_compnay %>% html_nodes("p.ltype") %>% html_text()
        if(length(v_coattrs) > 0){v_coattrs <- unlist(strsplit(gsub("(\r|\n|\t|\\s)","",v_coattrs),split = "\\|"))}else{v_coattrs <- c("","","")}
        #公司描述：
        #v_compDesc <- substr(html_compnay %>% html_nodes("div.tCompany_full") %>% html_nodes("div.tBorderTop_box.bt") %>% html_nodes("div.tmsg.inbox") %>% html_nodes("div.con_msg") %>% html_nodes("div.in") %>% html_nodes("p")  %>% html_text(),1,500)
        #v_compDesc <- ifelse(length(v_compDesc) > 0, v_compDesc, "")
        #公司地址：
        v_address <- gsub("(\r|\n|\t|\\s)","",html_compnay %>% html_nodes("div.tBorderTop_box") %>% html_nodes("div.bmsg.inbox") %>% html_nodes("p.fp") %>% html_text()) 
        v_address <- ifelse(length(v_address) > 0, substr(gsub("公司地址","",v_address),2,10000), "")    
        v_address <- ifelse(regexpr("\\(邮编",v_address) > 0, substr(v_address,1,regexpr("\\(邮编",v_address)-1),v_address)
        #公司在招人数及薪资范围：
        v_all_joblist <- uf_scrape_51job_openJob(v_company_url)
        v_chengdu_joblist <- uf_scrape_51job_openJob(gsub("all","chengdu",v_company_url))
        v_jobCount_all <- v_all_joblist[1]
        v_jobCount_cd <- v_chengdu_joblist[1]
        v_salaryRange_all <- v_all_joblist[2]
        v_salaryRange_cd <- v_chengdu_joblist[2]
        #公司招聘职位最后发布/更新日期
        nodes_t5 <- html_compnay %>% html_nodes("div.dw_table") %>% html_nodes("div.el") %>% html_nodes("span.t5")
        v_job_issue_latest <- ifelse(length(nodes_t5) > 0, nodes_t5 %>% magrittr::extract2(1) %>% html_text(), "Unknown") 
        v_job_issue_latest <- ifelse(length(v_job_issue_latest) > 0 & v_job_issue_latest != "Unknown", paste0(format(Sys.Date(), "%Y"),"-",v_job_issue_latest),"Unkown")
        #公司的官网地址
        v_link <- html_compnay %>% html_nodes("div.tBorderTop_box") %>% html_nodes("div.bmsg.tmsg.inbox") %>% html_nodes("p") %>% html_nodes("a") %>% html_text()
        v_link <- ifelse(length(v_link) > 0, v_link, "")
        if(v_link == ""){ #如果官网仍然没找到，直接到备案网站去查找
          #print("read2")
          v_link <- uf_find_official_beianbeian(v_company)
        }
        
        if(v_link == ""){ #if official website is not available on 51job, try to use baidu search to get.
          #print("read3")
          v_link <- uf_scrape_baidu_search_company_athome(v_company,p_jobArea,"官网")
        }
        
        if(regexpr("^http://",v_link) < 0 & v_link != ""){
          v_link <- paste0("http://",v_link)
        }
        
        attr_cotype[i] <- v_coattrs[1]
        attr_size[i] <- v_coattrs[2]
        attr_sub_industry[i] <- df_companyUrls$Sub_Industry[i]
        attr_industry[i] <- df_companyUrls$Industry[i]
        #attr_desc[i] <- v_compDesc
        attr_address[i] <- v_address
        attr_jobCount_all[i] <- v_jobCount_all
        attr_jobCount_cd[i] <- v_jobCount_cd
        attr_salaryRange_all[i] <- v_salaryRange_all
        attr_salaryRange_cd[i] <- v_salaryRange_cd
        attr_link[i] <- v_link
        attr_job_issue_latest[i] <- v_job_issue_latest
        attr_scape_date[i] <- as.character(Sys.Date()) 
        
        #判断公司是否是本地公司
        #if(v_link != ""){
        #  if(any(substr(attr(v_link,"code"),1,1) == c(attr(gv_51job_jobAreaCode,"alias1")[which(gv_51job_jobAreaCode == p_jobArea)[1]],
        #                                              attr(gv_51job_jobAreaCode,"alias2")[which(gv_51job_jobAreaCode == p_jobArea)[1]]))){
        #    attr_is_local[i] <- "YES"
        #  }
        #}
        
        #如果公司官网链接可用，从官网获取公司联系方式
        if(attr_contact[i] == "" & attr_link[i] != ""){ 
          #print("read4")
          attr_contact[i] <- uf_scrape_officialWebsite_contact(attr_link[i],p_jobArea,gv_ptns_phone_number)
          if(attr_contact[i] != ""){
            attr_official_contact[i] = "YES"
          }
        }
        
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
        
        #if contact still null, try to use Baidu Search to retrieve contact
        if(attr_contact[i] == ""){
          #print("read5")
          attr_contact[i] <- uf_scrape_baidu_search_company_athome(attr_company[i],p_jobArea,"联系电话")
          if(attr_contact[i] != ""){
            attr_official_contact[i] = "NO"
          }
        }
        
        #删除重复的电话号码
        if(attr_contact[i] != ""){
          v_t <-gsub("\\s{1,10}$","",gsub("^\\s{1,10}","",attr_contact[i]))
          v_t_c <- unlist(strsplit(v_t, split=" \\| "))
          v_t_c <- unique(v_t_c)
          l_t_c <- ifelse(length(v_t_c) >= 4,4,length(v_t_c))
          v_t_c <- v_t_c[1:l_t_c]
          attr_contact[i] <- paste(v_t_c,collapse = " | ")
          phoneCode <- attr(gv_51job_jobAreaCode,"phoneCode")[which(gv_51job_jobAreaCode == p_jobArea)]
          attr_contact[i] <- gsub(paste0(phoneCode,"(\\)|\\）)"),paste0(phoneCode,"-"),attr_contact[i])
          attr_contact[i] <- gsub("^\\s?\\|\\s?","",attr_contact[i])
        }else{
          attr_official_contact[i] = "N/A"
        }
      }
      
    }
    
    if(1==1){
      df_output <- rbind(df_output,cbind(company = attr_company[i], 
                                         address = attr_address[i], 
                                         address1 = attr_address1[i],
                                         city = attr(gv_51job_jobAreaCode,"city")[which(gv_51job_jobAreaCode==p_jobArea)[1]],
                                         building = attr_building[i],
                                         contact = attr_contact[i], 
                                         official_contact = attr_official_contact[i],
                                         coType = attr_cotype[i], 
                                         coSize = attr_size[i], 
                                         industry = attr_industry[i], 
                                         sub_industry = attr_sub_industry[i],
                                         is_local = attr_is_local[i],
                                         headquarter = attr_headquarter[i],
                                         legal_person = attr_legal_person[i],
                                         registered_capital = attr_registered_capital[i],
                                         establish_date = attr_establish_date[i],
                                         business_Sector_RA = attr_business_Sector_RA[i],
                                         op_status = attr_op_status[i],
                                         official_website = attr_link[i], 
                                         web_record_code = attr_web_record_code[i],
                                         coDesc = attr_desc[i], 
                                         jobCount_all = attr_jobCount_all[i], 
                                         jobCount_cd = attr_jobCount_cd[i], 
                                         salaryRange_all = attr_salaryRange_all[i], 
                                         salaryRange_cd = attr_salaryRange_cd[i],
                                         job_issue_latest = attr_job_issue_latest[i], 
                                         scape_date = attr_scape_date[i], 
                                         data_source = attr_data_source[i], 
                                         recruitPage_51job = attr_51jobRecruit_page[i]))
    }
  }
  
  return(df_output)
}

################Function - uf_scrape_baidu_search_company
#p_company <- "成都数领科技有限公司"
#p_Area <- "090200"
#p_searchType <- "联系电话"
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
          if(!is.na(html_guanwang)){
            text_url <- html_guanwang %>% html_text()
          }
          #text_url <- tryCatch({
          #  read_html(session_search  %>% jump_to(next_url),encoding = "utf-8") %>% html_text()
          #},warnings = function(w){
          #  ;
          #  ""
          #},error = function(e){
          #  ;
          #  tryCatch({
          #    read_html(session_search  %>% jump_to(next_url),encoding = "gbk") %>% html_text()
          #  },warnings = function(w){
          #    ;
          #    ""
          #  },error = function(e){
          #    print("gkb problem");
          #    ""}) 
          #})
          
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
    if(!is.na(session_search)){
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
#p_company_link <- "http://www.sharegroup.cn"
#p_jobArea <- "090200"
#p_ptns_phone_number <- gv_ptns_phone_number
uf_scrape_officialWebsite_contact <- function(p_company_link,p_jobArea,p_ptns_phone_number){
  op_contact_phone <- ""
  phone_area_code <- attr(gv_51job_jobAreaCode,"phoneCode")[which(gv_51job_jobAreaCode == p_jobArea)]
  
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


################Function - uf_scrape_51job_companyUrl
#p_session <- v_session
uf_scrape_51job_companyUrl <- function(p_session,p_industry,p_jobArea){
  df_companyUrl <- data.frame(Company_Name = "", Url_51job = "",Industry = "", Sub_Industry = "", Recruit_City = "")[0]
  v_page_num <- p_session %>% html_nodes("span.td") %>% magrittr::extract2(1) %>% html_text()
  v_page_num_len <- regexpr("[1-9][0-9]?[0-9]?[0-9]?",gsub("\\s","",v_page_num)) 
  v_page_num <- substr(v_page_num,v_page_num_len,v_page_num_len + attr(v_page_num_len,"match.length") - 1)
  
  sub_ind <- gv_51job_industries$sub_industry[which(gv_51job_industries$sub_industry_code == p_industry)[1]]
  ind <- gv_51job_industries$industry[which(gv_51job_industries$sub_industry_code == p_industry)[1]]
  city <- attr(gv_51job_jobAreaCode,"city")[which(gv_51job_jobAreaCode == p_jobArea)[1]]
  for(i in 1:v_page_num){
    #print(i)
    v_nodes <- p_session %>% html_nodes("span.t2") %>% html_nodes("a") 
    Company_Name <- v_nodes %>% html_attr("title")
    Url_51job <- v_nodes %>% html_attr("href")
    Sub_Industry <- rep(sub_ind,length(Company_Name))
    Industry <- rep(ind,length(Company_Name))
    Recruit_City <- rep(city,length(Company_Name))
    
    df_companyUrl <- rbind(df_companyUrl, cbind(Company_Name, Url_51job, Industry, Sub_Industry, Recruit_City))
    if(i != v_page_num){
      p_session <- p_session %>% jump_to(p_session %>% html_nodes("li.bk") %>% magrittr::extract2(2)  %>% html_nodes("a") %>% html_attr("href"))
    }
  }
  df_companyUrl <- subset(df_companyUrl, !duplicated(Company_Name))
  
  df_companyUrl <- filter(df_companyUrl,regexpr("http://jobs.51job.com/all/co",df_companyUrl$Url_51job) >= 1)

  
  return(df_companyUrl)
}


################Function - uf_scrape_51job_openJob
uf_scrape_51job_openJob <- function(p_company_url){
  op_result <- c("Unkown","Unkown")
  v_html_openJob <- uf_read_html(p_company_url,10)
  
  if(!all(is.na(v_html_openJob))){
    op_result_inputs <- v_html_openJob %>% html_nodes("ul")  %>% html_nodes("input")
    if(length(op_result_inputs) > 0){
      op_result_num <- v_html_openJob %>% html_nodes("ul")  %>% html_nodes("input") %>% magrittr::extract2(1) %>% html_attr("value")
      op_result_num <- ifelse(length(op_result_num) > 0, op_result_num, "Unknown")
      
      op_result_salaries <- v_html_openJob %>% html_nodes("div.el") %>% html_nodes("span.t4") %>% html_text()
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


























































































































































































































































































































































