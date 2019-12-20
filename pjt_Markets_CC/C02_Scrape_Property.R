########################################################Programm Start########################################################
setwd("C:/YuanLe/R/RWkDir/06. Markets_CC")
#ls()
#rm(list=ls())

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

gv_028office_link <- "http://info.028office.com/building/office/"
attr(gv_028office_link,"name") <- "成都写字楼信息网"
gv_028office_PP_prefix <- "http://info.028office.com"

gv_anjuke_link <- "http://cd.xzl.anjuke.com/loupan/"
attr(gv_anjuke_link,"name") <- "安居客"
gv_Anjuke_PP_prefix <- "http://cd.xzl.anjuke.com/loupan/"

gv_city <- c("成都")
attr(gv_city,"En") <- c("Chengdu")

gv_Defualt_DBServer <- "EDW_UAT"
gv_Defualt_DBServer_Login <- "EDWAPACUser_RW"
gv_Defualt_DBServer_pwd <- "EDw@pacRead&write"
gv_table_PI <- "CN_CD_CC.Properties_Info"
gv_BDAPI_Key <- "NdgFqhKyiqPksC2pLfsGsWps15wzRsES"

v_Anjuke_updateFreq <- 9999
gv_method_strSim <- "lcs"

########################################################Google Keys########################################################
keys <- c("AIzaSyA8Dmo33Ao1BwqTYthLhzmBRsv0Y9EtcBY"
          ,"AIzaSyDWDVo9Sy-Yk2eqjF5nS88hOPuiZpTOmD0"
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
          ,"AIzaSyBFgq1V4WBORn_Guw2989IFc4biqahMIiY"
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
          ,"AIzaSyDPdMOZNWGkCpSVcU9AKsPB7Rqrw5sxNRQ"
          ,"AIzaSyCdeheCQP7K4abGbnXduKjtxocQEfhX3s4"
          ,"AIzaSyBr9KLDJbE7YiAqcwrrkTsenrZJusD9S44"
          ,"AIzaSyArYyiHgQJDIZYWPaWMAGCeTn4MTMnNeME"
          ,"AIzaSyCHGUo37HTZ-mORyi78cPgCp-MeDXBH6OM"
          ,"AIzaSyD6AJi4tjV0xK1tPW7hO-__00rJ6WfqXrE"
          ,"AIzaSyDkinkKUqsQ29o4nkxduHp1BKOnx5wPzFk"
          ,"AIzaSyD6-YuO_vMYns9U3JVBCY3zobtKxFbL-WY"
          ,"AIzaSyDHWrZLRpzUe1RDGXyVAtJRzDKPNcUYRi0"
          ,"AIzaSyCIbOPps5BBU1rjsBRM2SMW86jyO5WpKog"
          ,"AIzaSyDe4TVXfbYIaoIKsqNg1cGiefzJllpCK_8"
          ,"AIzaSyA11qa9JWv0ueIXeblI828Q7FCOuYGXSPI" #ChrisHuang2727
          ,"AIzaSyBNxVNb4ktkWXysMFdl_LBbUevPKWuhXVs"
          ,"AIzaSyC5asx7Rvcb_tTdOC0XWacpw5zyrbAN-jQ"
          ,"AIzaSyA-H38lXu3-C2ReM9utCjyOj4MQjbsu_S0"
          ,"AIzaSyDd-9IJUuQB1j98XObuJFhjhatwDjSmFIY"
          ,"AIzaSyDy4TfKU02TRNkBMyzXPjISU6k_D-62nKI"
          ,"AIzaSyBmBxm8PAoC9oRFPdzzo6wfjAoenRJ-e6s"
          ,"AIzaSyCD73DzYXJINTjfCJfge6GA41IqQY8zgns"
          ,"AIzaSyDFaK-RDT8ridXsx9SCTxpCR8PYI3b0MU0"
          ,"AIzaSyCg3iI4JkZjzwaXfGz9Z2NJnqVRfSg0Wes"
          ,"AIzaSyB-g9PLdZspsxMANIZr0OBZ7ODOrX1vqps"
          ,"AIzaSyCX85Krz6_TAQvFuEfOc5VL5x2D1djMP_A"
          ,"AIzaSyAXeKw4HY6dRRgCrqgzyDD7mYvhfcszCo8" #NeilJin2727
          ,"AIzaSyBc5wAhMhQwUj_lcGyYCjIVJ9PEV0LwDIk"
          ,"AIzaSyCtkmGVIcXsNk9JJxt3FFw5xTatUSkhLK4"
          ,"AIzaSyDbfWT2kLDinwxWBzxSSEGog-9mrnzZff4"
          ,"AIzaSyABj2gtLQrSLt6RWFBfO-ek4NVWSj2NHRQ"
          ,"AIzaSyDy_xdhoI9bBNs99dWAjj1IOEnpfpVImQk"
          ,"AIzaSyDwzw_H_fQidBOqwNIRM3st5iw4lZyICyo"
          ,"AIzaSyDFJyzURabjpTGq5b3pM6K56QYDwfNqPdo"
          ,"AIzaSyAyRxHnurdyEGTlvhdwlJA8AgbxZRtWULg"
          ,"AIzaSyCCGC1mpUImCYY37FXnSeP-a9GUsjE708w"
          ,"AIzaSyASD-dMT5knHfO6cNZRFID3NvGoUt7JkkU"
          ,"AIzaSyBqbX6egM-SuVrB3e5lb84MXZhtGtTGq-I"
          ,"AIzaSyBseP9kM-mYSpz7T5Anmb3h2uxtPpc9UtE" #TerryTang2727
          ,"AIzaSyAsUAwSONushGaAfekiPZ6lP8QZmnX5Hf0"
          ,"AIzaSyA1fPBr3uLSMMgIwVsxCkb4UKK0kWI0Dm4"
          ,"AIzaSyCVRRzkFs-tG4k17AQ5A1wCQR83UA8cciE"
          ,"AIzaSyBthiLYOYPvsYhx96SBl2BUUHKxfZKmfew"
          ,"AIzaSyAFGjl-c7q5tYpXTSQnst_v_YXlWckGXyI"
          ,"AIzaSyCFUYV9WgquJccwh05eWo9KsAug7xj_iTE"
          ,"AIzaSyAoKirzJQ8u57e5L0zNRVVwbIREeoxP5f4"
          ,"AIzaSyDwka2VcE0OTfwtaY3p_jXXyQJM2FdY43c"
          ,"AIzaSyAXdQgqYejW-KXPv4uaIobu5dS6NJEOgdE"
          ,"AIzaSyAozR75Hz4Irr-l7zLYZu6u9PK5tEmil6I"
          ,"AIzaSyAGimiDR3pewa85FEt6y6-Icz1kMH5H7JE"
          ,"AIzaSyDsOMvp7aBMKgFIH8jIJXxa1ZyeEVzQKu8" #TomXie2727
          ,"AIzaSyA6x2RCsG0hgYmzhwqQPaPMM2Dsse62JNs"
          ,"AIzaSyCLI-SM52Z5-AOCwesweq6GxFtVD8tfmtE"
          ,"AIzaSyDrygN6ZzamQ8qXuiQqYhiMOkso8XhW_xc"
          ,"AIzaSyBqPNftbSBsxz5_wNzNf7aQ3vPBmYOqUpM"
          ,"AIzaSyC24SxN5KiENZ_zHhd-BCljPMnoapjccV8"
          ,"AIzaSyBCl2NnZCq2KqPUaz5qa9cqScx6h9BBoe8"
          ,"AIzaSyDUfVIrp_UBIh9SFfTWcT2KpB10pxGvD8Y"
          ,"AIzaSyDRlOHkp2GWBXeNqBceZ9PieJ_9b3sqaEc"
          ,"AIzaSyDIfNTTtGRUBdtCILyXEI9rUJE-S_pSqyo"
          ,"AIzaSyBNtReKGZqAYgH-VTs1Mwvyr2xE9FUR64g"
          ,"AIzaSyDBlyO0cZF2--Vd38RSB13OOVTkmPAE0ik"
          ,"AIzaSyArOluo4CLn2kZ7APdNXdrFcN52tHKQP94" #JielingLiang2727
          ,"AIzaSyAMj3OQSflF2SmOd23LHQpsZrodfC6QD7Y"
          ,"AIzaSyCJxiFVD5jMHxrVxlvHmoqcJ8kQB6VsX3Y"
          ,"AIzaSyBVZNxrwIEZ9yQDMqUsQiS_VOGXBVa52q4"
          ,"AIzaSyAgikjUeGX-zc_i2rIFagabb7oElphOuQU"
          ,"AIzaSyAe9gd93BFnA5w8lpW1mbw0gmeWAZcytqI"
          ,"AIzaSyBPfy1ksMOFxxWQzhKmu9i2RfHcWrN4okE"
          ,"AIzaSyDgDHtKzeusG679F_Qqld11GvIsyGX6PcA"
          ,"AIzaSyCFzPfikEqkm8ga7d97jokoHa_T5IvHIp8"
          ,"AIzaSyDYCXNuoriMi71prW_IvjLmJQOS3YZwktc"
          ,"AIzaSyCoZfZjtfw9pSeY23gM1A1IZ-tEH8TtTXo")




########################################################Global Function########################################################
################Function - uf_scrape_office028_input
#p_home <- gv_028office_link
#p_city <- "成都"
uf_scrape_office028_input <- function(p_home,p_city){
  df_rlt <- data.frame(Property = character(0),Address = character(0), District = character(0), City = character(0),Off028_Link = character(0))
  v_home <- p_home
  v_html <- uf_read_html(v_home,20)
  if(!all(is.na(v_html))){
    v_pages_text <- v_html %>% html_nodes("div.page") %>% html_nodes("span") %>% magrittr::extract2(1) %>% html_text()
    if(!is.na(v_pages_text)){
      if(regexpr(".[0-9]{1,4}.",v_pages_text) > 0){
        v_pages <- substr(v_pages_text,2,nchar(v_pages_text) - 1)
        if(as.integer(v_pages) >= 1){
          t_count_nodes <- rep(0,as.integer(v_pages))
          for(i in 1:as.integer(v_pages)){
            v_web <- paste0(p_home,"page",ifelse(i == 1, "", as.character(i)),".html")
            v_html_p <- uf_read_html(v_web,20)
            
            if(!all(is.na(v_html_p))){
              v_xml_nodes <- v_html_p %>% html_nodes("div.cont") %>% html_nodes("div.list") %>% html_nodes("div.c") %>% 
                html_nodes("ul") %>% html_nodes("li")
              if(length(v_xml_nodes) > 0){
                v_count_nodes <- length(v_xml_nodes)
                t_count_nodes[i] <- v_count_nodes
                for(j in 1:v_count_nodes){
                  v_Property <- v_Off028_Link <- v_District <- v_Address <- ""
                  v_xml_node <- v_xml_nodes[j]
                  seq_ele <- j + (i - 1) * ifelse(i == 1, 0, t_count_nodes[(i - 1)])
                  v_Property <- v_xml_node %>% html_nodes("div.a") %>% html_nodes("a") %>% html_nodes("h3") %>% html_text()
                  v_Property <- ifelse(length(v_Property) > 0, v_Property, "")
                  if(v_Property != ""){
                    v_Off028_Link <- v_xml_node %>% html_nodes("div.p") %>% html_nodes("a") %>% html_attr("href")
                    v_Off028_Link <- ifelse(length(v_Off028_Link) > 0, v_Off028_Link, "")
                    v_Off028_Link <- ifelse(v_Off028_Link == "", "", paste0(gv_028office_PP_prefix,v_Off028_Link))
                    v_Off028_Link <- gsub("com\\/\\/","com\\/",v_Off028_Link)
                    
                    v_basic_info <- v_xml_node %>% html_nodes("div.a") %>% html_text()
                    v_basic_info <- ifelse(length(v_basic_info) > 0, v_basic_info, "")
                    if(v_basic_info == ""){
                      v_District_num <- regexpr(paste0("^",v_Property,".{2,3}","写字楼"),v_basic_info)
                      if(v_District_num > 0){
                        v_District <- substr(v_basic_info,nchar(v_Property) + 1,attr(v_District_num,"match.length") - 3)
                      }
                      v_Address_num <- regexpr("写字楼\\s.{1,100}\\s([0-9]{4}|建成时间)",v_basic_info)
                      if(v_Address_num > 0){
                        v_Address <- substr(v_basic_info,v_Address_num + 4,attr(v_Address_num,"match.length") + v_Address_num - 6)
                      }
                    }
                  }
                  
                  df_rlt[seq_ele,]$Property <- v_Property
                  df_rlt[seq_ele,]$Address <- v_Address
                  df_rlt[seq_ele,]$District <- v_District
                  df_rlt[seq_ele,]$City <- p_city
                  df_rlt[seq_ele,]$Off028_Link <- v_Off028_Link
                 }
              }
            }
          }
        }
      }
    }
    
  }
  
  return(df_rlt)
}

################Function - uf_scrape_anjuke_input
#p_home <- gv_anjuke_link
#p_city <- "成都"
uf_scrape_anjuke_input <- function(p_home,p_city){
  df_rlt <- data.frame(Property = character(0),Address = character(0), District = character(0), City = character(0),Off028_Link = character(0))
  v_home <- p_home
  v_html <- uf_read_html(v_home,20)
  if(!all(is.na(v_html))){
    v_pages_text <- v_html %>% html_nodes("body") %>% html_nodes("div.bd_lis_millde") %>% html_nodes("div.bdlis_mid_bana") %>% 
      html_nodes("p.ban_r_cen") %>% html_nodes("span") %>% magrittr::extract2(2) %>% html_text() 
    if(!is.na(v_pages_text)){
      if(regexpr("\\/[0-9]{1,4}",v_pages_text) > 0){
        v_pages <- gsub("\\s","",substr(v_pages_text,regexpr("\\/[0-9]{1,4}",v_pages_text) + 1,100))
        if(as.integer(v_pages) >= 1){
          t_count_nodes <- rep(0,as.integer(v_pages))
          for(i in 1:as.integer(v_pages)){
            v_web <- paste0(p_home,"p",as.character(i),"/")
            v_html_p <- uf_read_html(v_web,20)
            
            if(!all(is.na(v_html_p))){
              v_xml_nodes <- v_html_p %>% html_nodes("body") %>% html_nodes("ul.bdlis_mid_cent") %>% html_nodes("li")
              if(length(v_xml_nodes) > 0){
                v_count_nodes <- length(v_xml_nodes)
                t_count_nodes[i] <- v_count_nodes
                for(j in 1:v_count_nodes){
                  v_Property <- v_Off028_Link <- v_District <- v_Address <- ""
                  v_xml_node <- v_xml_nodes[j]
                  seq_ele <- j + (i - 1) * ifelse(i == 1, 0, t_count_nodes[(i - 1)])
                  v_Property <- v_xml_node %>% html_nodes("div.bdl_mic_nrjj") %>% html_nodes("h4")  %>% html_text()
                  v_Property <- ifelse(length(v_Property) > 0, v_Property, "")
                  v_Property <- gsub("(\\\n|\\\t|\\s)","",v_Property)
                  if(v_Property != ""){
                    v_Off028_Link <- v_xml_node %>% html_nodes("div.bdl_mic_nrjj")  %>% html_nodes("a") %>% html_attr("href")
                    v_Off028_Link <- ifelse(length(v_Off028_Link) > 0, v_Off028_Link, "")
                    #v_Off028_Link <- ifelse(v_Off028_Link == "", "", paste0(gv_028office_PP_prefix,v_Off028_Link))
                    v_Off028_Link <- gsub("com\\/\\/","com\\/",v_Off028_Link)
                    
                  }
                  
                  df_rlt[seq_ele,]$Property <- v_Property
                  df_rlt[seq_ele,]$Address <- v_Address
                  df_rlt[seq_ele,]$District <- v_District
                  df_rlt[seq_ele,]$City <- p_city
                  df_rlt[seq_ele,]$Off028_Link <- v_Off028_Link
                }
              }
            }
          }
        }
      }
    }
    
  }
  
  return(df_rlt)
}

################Function - uf_scrape_office028
#p_property <- df.Property_Basic
#initial_load <- TRUE
#p_city <- "成都"
uf_scrape_office028 <- function(p_property,p_city,initial_load){
  #输入变量
  df_property <- p_property
  #输出变量
  v_output <- "DONE"
  #循环次数
  v_count_property <- nrow(p_property)
  #循环搜索所有公司的页面
  for(i in 1:v_count_property){
    #从df_property取出信息
    if("start" == "start"){
      v_Property <- df_property$Property[i]
      print(paste0(as.character(i),"--",v_Property,"--",as.character(Sys.time())))
      v_City <- df_property$City[i]
      v_Off028_Link <- df_property$Off028_Link[i]
      
      v_Address <- v_District <- v_City_Direction <- v_Business_District <- v_Ring_District <- v_Latitude <- v_Lontitude <- v_PT_Type <- v_PT_Grade <-
        v_Salse_Type <- v_Deliver_Criterion <- v_Year_Built <- v_Area_Range <- v_Refer_Price_Rent <- v_PT_Intro <- v_Current_Customers <- v_Floors <- 
        v_Gross_Area <- v_Std_Floor_Area <- v_Clear_Height <- v_Cover_Area <- v_Management_Fee <- v_Parking_Lot <- v_Parking_Fee_By_Month <- 
        v_Management_Company <- v_Developer <- v_Lift <- v_AC_System <- v_COM_System <- v_ENG_System <- v_POS_System <- v_Security_System <-
        v_Reg_Date <- v_Visit_Count <- ""
      
      v_scape_date <- as.character(Sys.Date())
      v_data_source <- "office028"
      v_Effective_Date <- v_scape_date
      v_Expiry_Date <- "9999-12-31"
      v_Is_Current <- 1
      
      check_rlt <- c(0,0,0)
      if(!initial_load){
        v_entity <- c(v_Property,v_City,v_scape_date)
        check_rlt <- uf_entity_existInDB_check("DEFAULT",v_entity,gv_table_PI,20) 
        attr(check_rlt,"state") <- c("exist","update_required","IDENTITY")
      }
    }
    
    if(check_rlt[1] == 1 & check_rlt[2] == 0){ #no action required
      print("no action required")
    }else if(check_rlt[1] == 0 | (check_rlt[1] == 1 & check_rlt[2] == 1)){#need retrieve new information of the property
      if(regexpr(gv_028office_PP_prefix,v_Off028_Link) >= 1){  #如果property页面的URL满足这样的格式，继续
        html_property <- uf_read_html(v_Off028_Link,10)
    
        if(!all(is.na(html_property))){
          div_exist <- html_property %>% html_nodes("body") %>% html_nodes("div.blank10")
          if(length(div_exist) == 0){
            #从028office获取基本信息
            if("basic_infor" == "basic_infor"){
              info_table <- html_property %>% html_nodes("div.maininfo") %>% html_nodes("table") 
              if(length(info_table) > 0){
                info_trs <- info_table %>% html_nodes("tr")
                info_tr1 <- info_trs %>% magrittr::extract2(1)
                v_PT_Type <- info_tr1 %>% html_nodes("td") %>% magrittr::extract2(2) %>% html_text()
                v_PT_Grade <- info_tr1 %>% html_nodes("td") %>% magrittr::extract2(4) %>% html_text()
                
                info_tr2 <- info_trs %>% magrittr::extract2(2)
                v_District <- info_tr2 %>% html_nodes("td") %>% magrittr::extract2(2) %>% html_text()
                v_City_Direction <- info_tr2 %>% html_nodes("td") %>% magrittr::extract2(4) %>% html_text()
                
                info_tr3 <- info_trs %>% magrittr::extract2(3)
                v_Business_District <- info_tr3 %>% html_nodes("td") %>% magrittr::extract2(2) %>% html_text()
                v_Ring_District <- info_tr3 %>% html_nodes("td") %>% magrittr::extract2(4) %>% html_text()
                
                info_tr4 <- info_trs %>% magrittr::extract2(4)
                v_Salse_Type <- info_tr4 %>% html_nodes("td") %>% magrittr::extract2(2) %>% html_text()
                v_Area_Range <- info_tr4 %>% html_nodes("td") %>% magrittr::extract2(4) %>% html_text()
                
                info_tr5 <- info_trs %>% magrittr::extract2(5)
                v_Deliver_Criterion <- info_tr5 %>% html_nodes("td") %>% magrittr::extract2(2) %>% html_text()
                v_Year_Built <- info_tr5 %>% html_nodes("td") %>% magrittr::extract2(4) %>% html_text() 
                
                info_tr6 <- info_trs %>% magrittr::extract2(6)
                v_Refer_Price_Rent <- info_tr6 %>% html_nodes("td") %>% magrittr::extract2(2) %>% html_text()
                
              }
              
              info_contact <- html_property %>% html_nodes("div.contact") %>% html_nodes("div")
              info_contact_attr <- info_contact %>% html_attr("id")
              address_num <- which(info_contact_attr == "mainaddress")[1]
              if(!is.na(address_num)){
                v_Address <- info_contact %>% magrittr::extract2(address_num) %>% html_nodes("ul") %>% html_nodes("li") %>% magrittr::extract2(1) %>% html_text()
                v_Address <- gsub("物业地址.","",v_Address)
              }
              
              info_uls <- html_property %>% html_nodes("body") %>% html_nodes("div.tip") %>% html_nodes("div.content") %>% html_nodes("ul")
              v_PT_Intro <- info_uls %>% magrittr::extract2(1) %>% html_text()
              v_PT_Intro <- gsub("(\\\t|\\\r|\\\n)","",v_PT_Intro)
              v_PT_Intro <- ifelse(nchar(v_PT_Intro) > 2000, paste0(substr(v_PT_Intro,1,2000),"..."),v_PT_Intro)
              
              v_Current_Customers <- info_uls %>% magrittr::extract2(2) %>% html_text()
              v_Current_Customers <-  gsub("(\\\t|\\\r|\\\n)","",v_Current_Customers)
              v_Current_Customers <- ifelse(nchar(v_Current_Customers) > 2000, paste0(substr(v_Current_Customers,1,2000),"..."),v_Current_Customers)
              
              info_table2 <- html_property %>% html_nodes("body") %>% html_nodes("div.content") %>% html_nodes("table")
              if(length(info_table2) > 0){
                info_t2_trs <- info_table2 %>% html_nodes("tr")
                
                info_t2_tr2 <- info_t2_trs %>% magrittr::extract2(2)
                v_Floors <- info_t2_tr2 %>% html_nodes("td") %>% magrittr::extract2(2) %>% html_text()
                v_Gross_Area <- info_t2_tr2 %>% html_nodes("td") %>% magrittr::extract2(4) %>% html_text()
                v_Std_Floor_Area <- info_t2_tr2 %>% html_nodes("td") %>% magrittr::extract2(6) %>% html_text()
                
                info_t2_tr3 <- info_t2_trs %>% magrittr::extract2(3)
                v_Clear_Height <- info_t2_tr3 %>% html_nodes("td") %>% magrittr::extract2(2) %>% html_text()
                v_Cover_Area <- info_t2_tr3 %>% html_nodes("td") %>% magrittr::extract2(4) %>% html_text()
                
                info_t2_tr4 <- info_t2_trs %>% magrittr::extract2(4)
                v_Management_Fee <- info_t2_tr4 %>% html_nodes("td") %>% magrittr::extract2(2) %>% html_text()
                v_Parking_Lot <- info_t2_tr4 %>% html_nodes("td") %>% magrittr::extract2(4) %>% html_text()
                v_Parking_Fee_By_Month <- info_t2_tr4 %>% html_nodes("td") %>% magrittr::extract2(6) %>% html_text()
                
                info_t2_tr5 <- info_t2_trs %>% magrittr::extract2(5)
                v_Management_Company <- info_t2_tr5 %>% html_nodes("td") %>% magrittr::extract2(2) %>% html_text()
                
                info_t2_tr6 <- info_t2_trs %>% magrittr::extract2(6)
                v_Developer <- info_t2_tr6 %>% html_nodes("td") %>% magrittr::extract2(2) %>% html_text()
                
                info_t2_tr7 <- info_t2_trs %>% magrittr::extract2(7)
                v_Lift <- info_t2_tr7 %>% html_nodes("td") %>% magrittr::extract2(2) %>% html_text()
                
                info_t2_tr8 <- info_t2_trs %>% magrittr::extract2(8)
                v_AC_System <- info_t2_tr8 %>% html_nodes("td") %>% magrittr::extract2(2) %>% html_text()
                
                info_t2_tr9 <- info_t2_trs %>% magrittr::extract2(9)
                v_COM_System <- info_t2_tr9 %>% html_nodes("td") %>% magrittr::extract2(2) %>% html_text()
                
                info_t2_tr10 <- info_t2_trs %>% magrittr::extract2(10)
                v_ENG_System <- info_t2_tr10 %>% html_nodes("td") %>% magrittr::extract2(2) %>% html_text()
                
                info_t2_tr11 <- info_t2_trs %>% magrittr::extract2(11)
                v_POS_System <- info_t2_tr11 %>% html_nodes("td") %>% magrittr::extract2(2) %>% html_text()
                
                info_t2_tr12 <- info_t2_trs %>% magrittr::extract2(12)
                v_Security_System <- info_t2_tr12 %>% html_nodes("td") %>% magrittr::extract2(2) %>% html_text()
              }
              
              v_divs <- html_property %>% html_nodes("body") %>% html_nodes("div")
              v_divs_ids <- v_divs %>% html_attr("id")
              v_divs_id_num <- which(v_divs_ids == "statistics")[1]
              v_text_Reg_Date <- v_divs %>% magrittr::extract2(v_divs_id_num) %>% html_nodes("ul") %>% html_nodes("li") %>% magrittr::extract2(1) %>% html_text()
              v_Reg_Date <- gsub("\\s{1,2}","",gsub("登记时间.","",v_text_Reg_Date))
            }
            
            if("GeoCode" == "GeoCode"){
              v_geocode <- baidu.api.geocoder(v_Property,v_City,gv_BDAPI_Key)
              if(!any(v_geocode == "")){
                v_Latitude <- v_geocode[1]
                v_Lontitude <- v_geocode[2]
              }
              
            }
            
          }
          
        }
        
        #insert new record(update existing record) into database
        if(1==1){
          v_entity_all <- c(v_Property,
                            v_Address,
                            v_District,
                            v_City,
                            v_City_Direction,
                            v_Business_District,
                            v_Ring_District,
                            v_Latitude,
                            v_Lontitude,
                            v_PT_Type,
                            v_PT_Grade,
                            v_Salse_Type,
                            v_Deliver_Criterion,
                            v_Year_Built,
                            v_Area_Range,
                            v_Refer_Price_Rent,
                            v_PT_Intro,
                            v_Current_Customers,
                            v_Floors,
                            v_Gross_Area,
                            v_Std_Floor_Area,
                            v_Clear_Height,
                            v_Cover_Area,
                            v_Management_Fee,
                            v_Parking_Lot,
                            v_Parking_Fee_By_Month,
                            v_Management_Company,
                            v_Developer,
                            v_Lift,
                            v_AC_System,
                            v_COM_System,
                            v_ENG_System,
                            v_POS_System,
                            v_Security_System,
                            v_Reg_Date,
                            v_Visit_Count,
                            v_Off028_Link,
                            v_scape_date,
                            v_data_source,
                            v_Effective_Date,
                            v_Expiry_Date,
                            v_Is_Current,
                            check_rlt[3])
        }
        if(v_Property != ""){
          uf_entity_insert_update("DEFAULT",v_entity_all,gv_table_PI,"INSERT",20)
          if(check_rlt[2] == 1){
            uf_entity_insert_update("DEFAULT",v_entity_all,gv_table_PI,"UPDATE",20)
          }
        }
      }
    }
  }
  
  return(v_output)
}

################Function - uf_scrape_Anjuke
#p_property <- df.Property_Basic_Anjuke
#initial_load <- FALSE
#p_city <- "成都"
uf_scrape_Anjuke <- function(p_property,p_city,initial_load){
  #输入变量
  df_property <- p_property
  #输出变量
  v_output <- "DONE"
  #循环次数
  v_count_property <- nrow(p_property)
  #循环搜索所有公司的页面
  for(i in 1:v_count_property){
    #从df_property取出信息
    if("start" == "start"){
      v_Property <- df_property$Property[i]
      print(paste0(as.character(i),"--",v_Property,"--",as.character(Sys.time())))
      v_City <- df_property$City[i]
      v_Off028_Link <- df_property$Off028_Link[i]
      
      v_Address <- v_District <- v_City_Direction <- v_Business_District <- v_Ring_District <- v_Latitude <- v_Lontitude <- v_PT_Type <- v_PT_Grade <-
        v_Salse_Type <- v_Deliver_Criterion <- v_Year_Built <- v_Area_Range <- v_Refer_Price_Rent <- v_PT_Intro <- v_Current_Customers <- v_Floors <- 
        v_Gross_Area <- v_Std_Floor_Area <- v_Clear_Height <- v_Cover_Area <- v_Management_Fee <- v_Parking_Lot <- v_Parking_Fee_By_Month <- 
        v_Management_Company <- v_Developer <- v_Lift <- v_AC_System <- v_COM_System <- v_ENG_System <- v_POS_System <- v_Security_System <-
        v_Reg_Date <- v_Visit_Count <- ""
      
      v_scape_date <- as.character(Sys.Date())
      v_data_source <- "Anjuke"
      v_Effective_Date <- v_scape_date
      v_Expiry_Date <- "9999-12-31"
      v_Is_Current <- 1
      
      check_rlt <- c(0,0,0)
      if(!initial_load){
        v_entity <- c(v_Property,v_City,v_scape_date)
        check_rlt <- uf_entity_existInDB_check("DEFAULT",v_entity,gv_table_PI,20) 
        attr(check_rlt,"state") <- c("exist","update_required","IDENTITY")
      }
    }
    
    if(check_rlt[1] == 1 & check_rlt[2] == 0){ #no action required
      print("no action required")
    }else if(check_rlt[1] == 0 | (check_rlt[1] == 1 & check_rlt[2] == 1)){#need retrieve new information of the property
      if(regexpr(gv_Anjuke_PP_prefix,v_Off028_Link) >= 1){  #如果property页面的URL满足这样的格式，继续
        html_property <- uf_read_html(v_Off028_Link,10)
        if(!all(is.na(html_property))){
          div_top_right <- html_property %>% html_nodes("body") %>% html_nodes("div.l-top-right")
          if(length(div_top_right) > 0){
            if("div_top_right" == "div_top_right"){
              v_Refer_Price_Rent <- div_top_right %>% html_nodes("div.l-price") %>% html_text()
              v_Refer_Price_Rent <- gsub("(\\\r|\\\n|\\s)","",v_Refer_Price_Rent)
              
              div_top_right_info <- div_top_right %>% html_nodes("ul.l-info") %>% html_nodes("li")
              v_kjmj <- div_top_right_info %>% magrittr::extract2(2) %>% html_nodes("div.l-info-item") %>% html_nodes("span.l-info-val") %>% html_text()
              v_kjmj <- ifelse(length(v_kjmj) >0,v_kjmj,"")
              v_Area_Range <- v_kjmj
              
              div_top_right_info_1 <- div_top_right_info %>% magrittr::extract2(3) %>% html_nodes("div.l-info-item") %>% html_nodes("span.l-info-val")
              v_wylb <- div_top_right_info_1 %>% magrittr::extract2(1) %>% html_text()
              v_wylb <- gsub("(\\\r|\\\n|\\s)","",v_wylb)
              v_wylb <- ifelse(length(v_wylb) >0,v_wylb,"")
              v_PT_Type <- v_wylb
              
              v_wyf <- div_top_right_info_1 %>% magrittr::extract2(2) %>% html_text()
              v_wyf <- gsub("(\\\r|\\\n|\\s)","",v_wyf)
              v_wyf <- ifelse(length(v_wyf) >0,v_wyf,"")
              v_Management_Fee <- v_wyf
              
              div_top_right_info_2 <- div_top_right_info %>% magrittr::extract2(4) %>% html_nodes("div.l-info-item") %>% html_nodes("span.l-info-val")
              v_jgsj <- div_top_right_info_2 %>% magrittr::extract2(1) %>% html_text()
              v_jgsj <- gsub("(\\\r|\\\n|\\s)","",v_jgsj)
              v_jgsj <- ifelse(length(v_jgsj) >0,v_jgsj,"")
              v_Year_Built <- v_jgsj
              
              v_zlc <- div_top_right_info_2 %>% magrittr::extract2(2) %>% html_text()
              v_zlc <- gsub("(\\\r|\\\n|\\s)","",v_zlc)
              v_zlc <- ifelse(length(v_zlc) >0,v_zlc,"")
              v_Floors <- v_zlc
            }
          }
        }
        
        html_property_canshu <- uf_read_html(gsub("\\/loupan\\/","\\/loupan\\/canshu\\/",v_Off028_Link),10)
        if(!all(is.na(html_property_canshu))){
          div_canshu <- html_property_canshu %>% html_nodes("body") %>% html_nodes("div.pag_cent_l")
          if(length(div_canshu) > 0){
            div_ps <- div_canshu %>% html_nodes("div.cent_l_wlzc") %>% html_nodes("p")
            p1_text <- div_ps %>% magrittr::extract2(1) %>% html_text()
            if(regexpr("地\\s{0,1}址.{1,3}\\[.{0,30}\\].{0,30}",p1_text) > 0){
              num_d_start <- regexpr("\\[",p1_text)
              num_d_end <- regexpr("\\]",p1_text)
              v_qus <- substr(p1_text,num_d_start + 1, num_d_end - 1)
              if(nchar(v_qus) >= 2){
                num_1_space <- regexpr("\\s",v_qus)
                if(num_1_space > 0){
                  v_qu <- substr(v_qus,1,num_1_space-1)
                  v_bq <- gsub("\\s","", substr(v_qus,num_1_space + 1,100))
                  v_District <- v_qu
                  v_Business_District <- v_bq
                }
              }
              v_dizhi <- gsub("\\s","",substr(p1_text,num_d_end + 1, 200)) 
              v_Address <- v_dizhi
            }
            
            p2_text <- div_ps %>% magrittr::extract2(2) 
            v_lb <- p2_text %>% html_nodes("span.typle") %>% html_text()
            v_lb <- substr(v_lb,5,100)
            v_PT_Type <- v_lb
            
            v_level <- p2_text %>% html_nodes("span.level") %>% html_text()
            v_level <- substr(v_level,5,100)
            v_PT_Grade <- v_level
            
            p3_text <- div_ps %>% magrittr::extract2(3) %>% html_nodes("span") %>% html_text() 
            p3_text <- ifelse(nchar(p3_text) > 2000, paste0(substr(p3_text,1,1997),"..."),p3_text)
            v_PT_Intro <- p3_text
            
            p4_text <- div_ps %>% magrittr::extract2(4)
            v_wggs <- p4_text %>% html_nodes("span.typleb") %>% html_text()
            v_wggs <- substr(v_wggs,8,100)
            v_Management_Company <- v_wggs
            v_kts <- p4_text %>% html_nodes("span.levelb") %>% html_text()
            v_kts <- ifelse(nchar(v_kts) <= 4,"客梯数：未知",v_kts)
            
            
            p5_text <- div_ps %>% magrittr::extract2(5)
            v_wyglf <- p5_text %>% html_nodes("span.typle") %>% html_text()
            v_wyglf <- substr(v_wyglf,7,100)
            v_Management_Fee <- v_wyglf
            v_ktpp <- p5_text %>% html_nodes("span.level") %>% html_text()
            
            p6_text <- div_ps %>% magrittr::extract2(6)
            v_ktlx <- p6_text %>% html_nodes("span.typleb") %>% html_text()
            v_dtywfq <- p6_text %>% html_nodes("span.levelb") %>% html_text()
            
            p7_text <- div_ps %>% magrittr::extract2(7)
            v_ktkfsj <- p7_text %>% html_nodes("span.typle") %>% html_text()
            v_hts <- p7_text %>% html_nodes("span.level") %>% html_text()
            
            v_Lift <- paste(v_ktpp,v_kts,v_hts,v_dtywfq,sep = " | ")
            v_AC_System <- paste(v_ktlx,v_ktkfsj,sep = " | ")
            
            p8_text <- div_ps %>% magrittr::extract2(8)
            v_cwzj <- p8_text %>% html_nodes("span.typleb") %>% html_text()
            v_dscws <- p8_text %>% html_nodes("span.levelb") %>% html_text()
            
            p9_text <- div_ps %>% magrittr::extract2(9)
            v_txxt <- p9_text %>% html_nodes("span.typle") %>% html_text()
            v_dxcws <- p9_text %>% html_nodes("span.level") %>% html_text()
            
            v_Parking_Fee_By_Month <- substr(v_cwzj,7,100)
            v_Parking_Fee_By_Month <- ifelse(v_Parking_Fee_By_Month == "","暂无资料",v_Parking_Fee_By_Month)
            v_COM_System <- v_txxt
            
            v_Parking_Lot <- paste(v_dscws,v_dxcws,sep = " | ")
            
            div_tables <- div_canshu %>% html_nodes("table")
            v_Current_Customers <- div_tables %>% magrittr::extract2(1) %>% html_nodes("tr") %>% html_nodes("td") %>% magrittr::extract2(2) %>% html_text()
            v_Security_System <- div_tables %>% magrittr::extract2(2) %>% html_nodes("tr") %>% html_nodes("td") %>% magrittr::extract2(2) %>% html_text()
            
            p11_text <- div_ps %>% magrittr::extract2(11)
            v_kfs <- p11_text %>% html_nodes("span.typleb") %>% html_text()
            v_kfs <- substr(v_kfs,5,100)
            v_Developer <- v_kfs
            v_zlc <- p11_text %>% html_nodes("span.levelb") %>% html_text()
            v_zlc <- substr(v_zlc,5,100)
            v_Floors <- v_zlc
            
            p12_text <- div_ps %>% magrittr::extract2(12)
            v_jgny <- substr(p12_text %>% html_nodes("span.typle") %>% html_text(),6,100)
            v_Year_Built <- v_jgny
            v_bzcg <- substr(p12_text %>% html_nodes("span.level") %>% html_text(),6,100)
            v_Clear_Height <- v_bzcg
            
            p13_text <- div_ps %>% magrittr::extract2(13)
            v_zjzmj <- substr(p13_text %>% html_nodes("span.typleb") %>% html_text(),7,100)
            v_Gross_Area <- v_zjzmj
            
            p14_text <- div_ps %>% magrittr::extract2(14)
            v_bzcmj <- substr(p14_text %>% html_nodes("span.typle") %>% html_text(),7,100)
            v_Std_Floor_Area <- v_bzcmj
            
            p15_text <- div_ps %>% magrittr::extract2(15)
            v_kjmj <- substr(p15_text %>% html_nodes("span.typleb") %>% html_text(),6,100)
            v_Area_Range <- v_kjmj
          }
        }
        
        
        if("GeoCode" == "GeoCode"){
          v_geocode <- baidu.api.geocoder(v_Property,v_City,gv_BDAPI_Key)
          if(any(v_geocode == "")){
            v_geocode <- baidu.api.detailSearch(baidu.api.poiSearch(v_Property,v_City,gv_BDAPI_Key),v_City,gv_BDAPI_Key)[c(5,6)]
          }
          if(all(v_geocode != "")){
            v_Latitude <- v_geocode[1]
            v_Lontitude <- v_geocode[2]
          }
        }
        
        #insert new record(update existing record) into database
        if(1==1){
          v_entity_all <- c(v_Property,
                            v_Address,
                            v_District,
                            v_City,
                            v_City_Direction,
                            v_Business_District,
                            v_Ring_District,
                            v_Latitude,
                            v_Lontitude,
                            v_PT_Type,
                            v_PT_Grade,
                            v_Salse_Type,
                            v_Deliver_Criterion,
                            v_Year_Built,
                            v_Area_Range,
                            v_Refer_Price_Rent,
                            v_PT_Intro,
                            v_Current_Customers,
                            v_Floors,
                            v_Gross_Area,
                            v_Std_Floor_Area,
                            v_Clear_Height,
                            v_Cover_Area,
                            v_Management_Fee,
                            v_Parking_Lot,
                            v_Parking_Fee_By_Month,
                            v_Management_Company,
                            v_Developer,
                            v_Lift,
                            v_AC_System,
                            v_COM_System,
                            v_ENG_System,
                            v_POS_System,
                            v_Security_System,
                            v_Reg_Date,
                            v_Visit_Count,
                            v_Off028_Link,
                            v_scape_date,
                            v_data_source,
                            v_Effective_Date,
                            v_Expiry_Date,
                            v_Is_Current,
                            check_rlt[3])
        }
        if(v_Property != ""){
          uf_entity_insert_update("DEFAULT",v_entity_all,gv_table_PI,"INSERT",20)
          #if(check_rlt[2] == 1){
          #  uf_entity_insert_update("DEFAULT",v_entity_all,gv_table_PI,"UPDATE",20)
          #}
        }
      }
    }
  }
  
  return(v_output)
}

################Function - uf_read_html
#p_web <- "http://info.028office.com/building/office/"        #p_web is a link of website
#timeouts <- 20
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
#Table <- gv_table_PI
#uf_entity_existInDB_check(DBConn,Entity,Table,timeouts)
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
      evalWithTimeout({ sqlQuery(Conn,paste0("SELECT TOP 1 ID,scape_date FROM ",Table
                                             ," WHERE Property = N'",iconv(Entity[1], "UTF-8", "gb2312")
                                             ,"' AND City = N'",iconv(Entity[2], "UTF-8", "gb2312")
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
      diff_days <- as.integer(difftime(as.Date(Entity[3]),as.Date(df.db_return$scape_date),units = "days")) 
      if(diff_days > v_Anjuke_updateFreq){
        update_Require <- 1
        ID <- df.db_return$ID
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
#Table <- gv_table_PI
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
                          "(Property, Address, District, City, City_Direction, Business_District, Ring_District, Latitude, Lontitude, 
                          PT_Type, PT_Grade, Salse_Type, Deliver_Criterion, Year_Built, Area_Range, Refer_Price_Rent, PT_Intro, 
                          Current_Customers, Floors, Gross_Area, Std_Floor_Area, Clear_Height, Cover_Area, Management_Fee, Parking_Lot, 
                          Parking_Fee_By_Month, Management_Company, Developer, Lift, AC_System, COM_System, ENG_System, POS_System, 
                          Security_System, Reg_Date, Visit_Count, Off028_Link, scape_date, data_source, Effective_Date, Expiry_Date, 
                          Is_Current) VALUES(") 
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
                          "N'",iconv(Entity[31], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[32], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[33], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[34], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[35], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[36], "UTF-8", "gb2312"),"',",
                          "N'",iconv(Entity[37], "UTF-8", "gb2312"),"',",
                          "'",as.Date(Entity[38]),"',",
                          "N'",iconv(Entity[39], "UTF-8", "gb2312"),"',",
                          "'",as.Date(Entity[40]),"',",
                          "'",as.Date(Entity[41]),"',",
                          "'",as.integer(Entity[42]),"')")
    }
    if(Ops == "UPDATE"){
      sql_query <- paste0("UPDATE ",Table," SET ",
                          "Expiry_Date = '", as.Date(Entity[38]),"',",
                          "Is_Current = 0 ",
                          "WHERE ID = ", as.integer(Entity[43]))
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


################Function - baidu.api.nearbySearch
baidu.api.nearbySearch <- function(location,city,key,type="写字楼$房地产$酒店",scope=2,radius=500,output="json",in_pty_name){
  #####Define output
  v_output <- rep("",11)
  attr(v_output, "Elements") <- c("PROPERTY_NAME", "ADDRESS", "DISTRICT", "CITY", "GEO_LAT", "GEO_LONG", "postal_code", "phone_number","place_id","gmap_url","website")
  
  root <- "http://api.map.baidu.com/place/v2/search?"
  u <- paste0(root, "query=", type, "&scope=",scope,"&location=",paste0(location[1],",",location[2]),"&radius=",radius,"&output=",output,"&ak=", key)
  doc <- getURL(URLencode(u))
  
  if(doc != "" & !is.null(doc)){
    x <- fromJSON(doc,simplify = FALSE)
    if(x$status==0 & length(x$results) > 0){
      results <- x$results
      n_rlt <- length(results)
      result <- results[[1]]
      
      
      rlt_pty_name <- rep("",n_rlt)
      str_dist_pty_name <- rep(1,n_rlt)
      dst_diff <- rep(1,n_rlt)
      for(i in 1:n_rlt){
        rlt_pty_name[i] <- results[[i]]$name
        #str_dist_pty_name[i] <- stringdist(rlt_pty_name[i],in_pty_name)/max(nchar(rlt_pty_name[i]),nchar(in_pty_name))
        str_dist_pty_name[i] <- stringsim(rlt_pty_name[i],in_pty_name,method = gv_method_strSim)
        
        r_lat <- results[[i]]$location$lat
        r_lat <- ifelse(is.null(r_lat), 180, r_lat)
        r_long <- results[[i]]$location$lng
        r_long <- ifelse(is.null(r_long), 180, r_long)
        
        point_dist <- distm(c(r_long, r_lat), c(as.numeric(location[2]), as.numeric(location[1])), fun = distHaversine)
        
        dst_diff[i] <- point_dist
      }
      
      rlt_num <- 1
      if(any(str_dist_pty_name >= gv_accept_sim - 0.25)){
        rlt_num <- which(str_dist_pty_name == max(str_dist_pty_name))[1]
      }else{
        rlt_num <- which(dst_diff == min(dst_diff))[1]
      }
      
      result <- results[[rlt_num]]
      
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
      v_output[which(attr(v_output,"Elements") == "CITY")] <- city
      v_output[which(attr(v_output,"Elements") == "postal_code")] <- postal_code
      
    }
  }else{
    print("Google can not find anything according to your input!")
  }
  return(v_output)
}


google.api.textSearch <- function(query, key, language){
  placeid = ""
  
  query_1 <- paste0(query[1],",",query[2],",",query[3],",",query[4])
  root <- "https://maps.googleapis.com/maps/api/place/textsearch/json"
  u <- paste0(root, "?query=", query_1, "&language=", language, "&key=", key)
  doc <- getURL(URLencode(u))
  x <- fromJSON(doc,simplify = FALSE)
  
  if(x$status == "OK"){
    #select the best matched result
    result <- x$results[[1]]   
    rsl_placeid <- result$place_id
    placeid <- rsl_placeid
  }else{
    print("Google can not find anything according to your input!")
  }
  return(placeid)
}


google.api.detailSearch <- function(placeid, key, language, in_pty_name){
  #####Define output
  v_output <- rep("",12)
  attr(v_output, "Elements") <- c("PROPERTY_NAME", "ADDRESS", "DISTRICT", "CITY", "GEO_LAT", "GEO_LONG", "postal_code", "phone_number","place_id","gmap_url","website","remark")
  
  root <- "https://maps.googleapis.com/maps/api/place/details/json?placeid="
  u <- paste0(root,placeid,"&language=", language, "&key=", key)
  doc <- getURL(URLencode(u))
  x <- fromJSON(doc,simplify = FALSE)
  
  if(x$status == "OK"){
    result <- x$result
    rsl_types <- result$types
    
    if(any(rsl_types == "establishment")){
      #print("The found place is an establishment!")
      v_output[which(attr(v_output,"Elements") == "PROPERTY_NAME")] <- result$name
      v_output[which(attr(v_output,"Elements") == "GEO_LAT")] <- result$geometry$location$lat  
      v_output[which(attr(v_output,"Elements") == "GEO_LONG")] <- result$geometry$location$lng  
      v_output[which(attr(v_output,"Elements") == "phone_number")] <- ifelse(is.null(result$formatted_phone_number), "", result$formatted_phone_number)
      v_output[which(attr(v_output,"Elements") == "place_id")] <- ifelse(is.null(result$place_id), "", result$place_id)
      v_output[which(attr(v_output,"Elements") == "gmap_url")] <- ifelse(is.null(result$url), "", result$url)
      v_output[which(attr(v_output,"Elements") == "website")] <- ifelse(is.null(result$website), "", result$website)
      
      street_number <- ""
      route <- ""
      district <- ""
      city <- ""
      postal_code <- ""
      address_components <- result$address_components
      for(i in 1:length(address_components)){
        if(any(address_components[[i]]$types == "street_number")){
          street_number <-  address_components[[i]]$long_name
        }else if(any(address_components[[i]]$types == "route")){
          route <- address_components[[i]]$long_name
        }else if(any(address_components[[i]]$types == "sublocality")){
          district <- address_components[[i]]$long_name
          district <- gsub("(区$|\\sQu)","",district)
        }else if(any(address_components[[i]]$types == "locality")){
          city <- address_components[[i]]$long_name
          city <- gsub("(市$|\\s{0,1}[Ss]hi)","",city)
        }else if(any(address_components[[i]]$types == "postal_code")){
          postal_code <- address_components[[i]]$long_name
        }
      }
      if(language == "zh-CN"){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- paste0(route,street_number)
      }else if(language == "en"){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- paste0(street_number," ", route)
      }
      
      v_output[which(attr(v_output,"Elements") == "DISTRICT")] <- district
      v_output[which(attr(v_output,"Elements") == "CITY")] <- city
      v_output[which(attr(v_output,"Elements") == "postal_code")] <- postal_code
      
      if(gsub("\\s{1,}","",v_output[which(attr(v_output,"Elements") == "ADDRESS")]) == ""){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- result$vicinity
      }
      v_output[12] <- "establishment"
      
    }else if(any(rsl_types == "street_address")){
      #print("The found place is a street_address")
      
      v_output[which(attr(v_output,"Elements") == "PROPERTY_NAME")] <- ""
      v_output[which(attr(v_output,"Elements") == "GEO_LAT")] <- result$geometry$location$lat  
      v_output[which(attr(v_output,"Elements") == "GEO_LONG")] <- result$geometry$location$lng  
      v_output[which(attr(v_output,"Elements") == "phone_number")] <- ifelse(is.null(result$formatted_phone_number), "", result$formatted_phone_number)
      v_output[which(attr(v_output,"Elements") == "place_id")] <- ifelse(is.null(result$place_id), "", result$place_id)
      v_output[which(attr(v_output,"Elements") == "gmap_url")] <- ifelse(is.null(result$url), "", result$url)
      v_output[which(attr(v_output,"Elements") == "website")] <- ifelse(is.null(result$website), "", result$website)
      
      street_number <- ""
      route <- ""
      district <- ""
      city <- ""
      postal_code <- ""
      address_components <- result$address_components
      for(i in 1:length(address_components)){
        if(any(address_components[[i]]$types == "street_number")){
          street_number <-  address_components[[i]]$long_name
        }else if(any(address_components[[i]]$types == "route")){
          route <- address_components[[i]]$long_name
        }else if(any(address_components[[i]]$types == "sublocality")){
          district <- address_components[[i]]$long_name
          district <- gsub("(区$|\\sQu)","",district)
        }else if(any(address_components[[i]]$types == "locality")){
          city <- address_components[[i]]$long_name
          city <- gsub("(市$|\\s{0,1}[Ss]hi)","",city)
        }else if(any(address_components[[i]]$types == "postal_code")){
          postal_code <- address_components[[i]]$long_name
        }
      }
      if(language == "zh-CN"){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- paste0(route,ifelse(substr(street_number,nchar(street_number),nchar(street_number)) == "号",street_number,paste0(street_number,"号")))
      }else if(language == "en"){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- paste0(street_number," ", route)
      }
      v_output[which(attr(v_output,"Elements") == "DISTRICT")] <- district
      v_output[which(attr(v_output,"Elements") == "CITY")] <- city
      v_output[which(attr(v_output,"Elements") == "postal_code")] <- postal_code
      
      latlng = c(v_output[which(attr(v_output,"Elements") == "GEO_LAT")],
                 v_output[which(attr(v_output,"Elements") == "GEO_LONG")])
      property_name <- google.api.reverse.geocode(latlng,language = language, key = key)
      if(gsub("\\s{1,}","",property_name) == ""){  #if property_name is null, try nearbysearch
        property_name = google.api.nearbysearch(latlng,language = language, key = key,placetype = "street_address",in_pty_name = in_pty_name)
        v_output[which(attr(v_output,"Elements") == "PROPERTY_NAME")] <- property_name
        v_output[12] <- "street_address.nearbysearch"
      }else{
        v_output[which(attr(v_output,"Elements") == "PROPERTY_NAME")] <- property_name
        v_output[12] <- "street_address.reverse.geocode"
      }
    }else if(any(rsl_types == "route")){
      #print("The found place is a route")
      
      v_output[which(attr(v_output,"Elements") == "GEO_LAT")] <- result$geometry$location$lat  
      v_output[which(attr(v_output,"Elements") == "GEO_LONG")] <- result$geometry$location$lng  
      v_output[which(attr(v_output,"Elements") == "phone_number")] <- ifelse(is.null(result$formatted_phone_number), "", result$formatted_phone_number)
      v_output[which(attr(v_output,"Elements") == "place_id")] <- ifelse(is.null(result$place_id), "", result$place_id)
      v_output[which(attr(v_output,"Elements") == "gmap_url")] <- ifelse(is.null(result$url), "", result$url)
      v_output[which(attr(v_output,"Elements") == "website")] <- ifelse(is.null(result$website), "", result$website)
      
      street_number <- ""
      route <- ""
      district <- ""
      city <- ""
      postal_code <- ""
      address_components <- result$address_components
      for(i in 1:length(address_components)){
        if(any(address_components[[i]]$types == "street_number")){
          street_number <-  address_components[[i]]$long_name
        }else if(any(address_components[[i]]$types == "route")){
          route <- address_components[[i]]$long_name
        }else if(any(address_components[[i]]$types == "sublocality")){
          district <- address_components[[i]]$long_name
          district <- gsub("(区$|\\sQu)","",district)
        }else if(any(address_components[[i]]$types == "locality")){
          city <- address_components[[i]]$long_name
          city <- gsub("(市$|\\s{0,1}[Ss]hi)","",city)
        }else if(any(address_components[[i]]$types == "postal_code")){
          postal_code <- address_components[[i]]$long_name
        }
      }
      if(language == "zh-CN"){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- paste0(route,street_number)
      }else if(language == "en"){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- paste0(street_number," ", route)
      }
      v_output[which(attr(v_output,"Elements") == "DISTRICT")] <- district
      v_output[which(attr(v_output,"Elements") == "CITY")] <- city
      v_output[which(attr(v_output,"Elements") == "postal_code")] <- postal_code
      
      latlng = c(v_output[which(attr(v_output,"Elements") == "GEO_LAT")],
                 v_output[which(attr(v_output,"Elements") == "GEO_LONG")])
      
      property_name = google.api.nearbysearch(latlng,language = language, key = key,placetype = "route",in_pty_name = in_pty_name)
      v_output[which(attr(v_output,"Elements") == "PROPERTY_NAME")] <- property_name
      v_output[12] <- "street_address.nearbysearch"
    }else if(any(rsl_types == "premise")){
      #print("The found place is a premise!")
      v_output[which(attr(v_output,"Elements") == "PROPERTY_NAME")] <- result$name
      v_output[which(attr(v_output,"Elements") == "GEO_LAT")] <- result$geometry$location$lat  
      v_output[which(attr(v_output,"Elements") == "GEO_LONG")] <- result$geometry$location$lng  
      v_output[which(attr(v_output,"Elements") == "phone_number")] <- ifelse(is.null(result$formatted_phone_number), "", result$formatted_phone_number)
      v_output[which(attr(v_output,"Elements") == "place_id")] <- ifelse(is.null(result$place_id), "", result$place_id)
      v_output[which(attr(v_output,"Elements") == "gmap_url")] <- ifelse(is.null(result$url), "", result$url)
      v_output[which(attr(v_output,"Elements") == "website")] <- ifelse(is.null(result$website), "", result$website)
      
      street_number <- ""
      route <- ""
      district <- ""
      city <- ""
      postal_code <- ""
      address_components <- result$address_components
      for(i in 1:length(address_components)){
        if(any(address_components[[i]]$types == "street_number")){
          street_number <-  address_components[[i]]$long_name
        }else if(any(address_components[[i]]$types == "route")){
          route <- address_components[[i]]$long_name
        }else if(any(address_components[[i]]$types == "sublocality")){
          district <- address_components[[i]]$long_name
          district <- gsub("(区$|\\sQu)","",district)
        }else if(any(address_components[[i]]$types == "locality")){
          city <- address_components[[i]]$long_name
          city <- gsub("(市$|\\s{0,1}[Ss]hi)","",city)
        }else if(any(address_components[[i]]$types == "postal_code")){
          postal_code <- address_components[[i]]$long_name
        }
      }
      if(language == "zh-CN"){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- paste0(route,street_number)
      }else if(language == "en"){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- paste0(street_number," ", route)
      }
      v_output[which(attr(v_output,"Elements") == "DISTRICT")] <- district
      v_output[which(attr(v_output,"Elements") == "CITY")] <- city
      v_output[which(attr(v_output,"Elements") == "postal_code")] <- postal_code
      
      if(gsub("\\s{1,}","",v_output[which(attr(v_output,"Elements") == "ADDRESS")]) == ""){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- ""
      }
      v_output[12] <- "premise"
      
      #if(v_output[which(attr(v_output,"Elements") == "ADDRESS")] == ""){
      #  latlng = c(v_output[which(attr(v_output,"Elements") == "GEO_LAT")],
      #             v_output[which(attr(v_output,"Elements") == "GEO_LONG")])
      #  v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- google.api.nearbysearch(latlng, language = language, key = key,placetype = "premise", in_pty_name = in_pty_name)
      #}
      
    }
  }else{
    print("Placeid you inputed is incorrect!")
  }
  return(v_output)
}


#DBConn <- "DEFAULT"
#Query <- "select * from [CN_CD_CC].[Properties_Info]  where latitude = ''"
#timeouts <- 20
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






































































