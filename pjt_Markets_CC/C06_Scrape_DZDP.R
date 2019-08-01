########################################################Programm Start########################################################
setwd("C:/YuanLe/R/RWkDir/06. Markets_CC")

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

gv_url_dzdp <- "http://www.dianping.com"
gv_cities <- c("成都","重庆")
attr(gv_cities,"city_en") <- c("Chengdu",  "Chongqing")
gv_broad_heading <- c("food(美食)")


########################################################Global Function########################################################
p_city <- attr(gv_cities,"city_en")[1]
p_broad_heading <- gv_broad_heading[1]
#df.chengduList <- uf_scrape_ref_data(p_city,p_broad_heading)
#uf_xlsxDataExport(df.result,"./OPFiles/chengduList.xlsx","Chengdu")

uf_scrape_ref_data <- function(p_city,p_broad_heading){
  df.result <- data.frame(level0_desc = character(0), level0_url = character(0),   #大众点评首页
                          level1_desc = character(0), level1_url = character(0),   #城市
                          level2_desc = character(0), level2_url = character(0),   #大类
                          level3_desc = character(0), level3_url = character(0),   #搜索主页
                          level4_desc = character(0), level4_url = character(0),   #行政区
                          level5_desc = character(0), level5_url = character(0),   #行政区-子集
                          level6_desc = character(0), level6_url = character(0),   #分类
                          level7_desc = character(0), level7_url = character(0),   #分类-子分类
                          count_pages = character(0))                              #页数
  level0_desc <- "首页"
  level0_url <- gv_url_dzdp
  level1_desc <- paste0(gv_cities[which(attr(gv_cities,"city_en") == p_city)[1]],"频道")
  level1_url <- paste0(gv_url_dzdp,"/",tolower(p_city))
  
  html_level1 <- uf_read_html(level1_url,20)
  
  if(!all(is.na(html_level1))){
    level2_nodes <- html_level1 %>% html_nodes("body") %>% html_nodes("div.aside.aside-left") %>% html_nodes("ul.category-nav.J-category-nav") %>% 
                      html_nodes("li.primary-category.J-primary-category") %>% html_nodes("a.name")
    
    level2_urls_suffix <- level2_nodes %>% html_attr("href")
    level2_desc_suffix <- paste0(level2_nodes %>% html_attr("date-name"),"(",level2_nodes %>% html_nodes("span") %>% html_text(), ")") 
    
    count_level2 <- length(level2_urls_suffix)
    
    for(i in 1:count_level2){     #开始循环大类
      if(p_broad_heading == level2_desc_suffix[i]){
        level2_url <- paste0(gv_url_dzdp,level2_urls_suffix[i])
        level2_desc <- paste0("大类-",level2_desc_suffix[i])
        
        html_level2 <- uf_read_html(level2_url,20)
        if(!all(is.na(html_level2))){
          seach_home_urls <- html_level2 %>% html_nodes("ul.nav_col") %>% html_nodes("li.nc_li") %>% 
            html_nodes("div.nc_item.list_business") %>% html_nodes("a") %>% html_attr("href")
          level3_url <- ""
          level3_desc <- ""
          for(j in 1:length(seach_home_urls)){
            pos_search <- regexpr("(/search/category/)[0-9]{1,3}/[0-9]{1,5}/",seach_home_urls[j])
            if(pos_search > 0){
              level3_url <- paste0(level0_url,substr(seach_home_urls[j],pos_search,attr(pos_search,"match.length")))  
              level3_desc <- paste0("搜索主页","-",level2_desc)
              break()
            }
          } #找到搜索主页
          
          html_level3 <- uf_read_html(level3_url,20)
          if(!all(is.na(html_level3))){
            nodes_J_nav <- html_level3 %>% html_nodes("body") %>% html_nodes("div.nav-category.nav-tabs") %>% html_nodes("div.nc-items")
            J_nav_attr <- nodes_J_nav %>% html_attr("id")
            nodes_J_nav_regions <- nodes_J_nav[which(J_nav_attr == "region-nav")[1]] %>% html_nodes("a") 
            count_nodes_J_nav_regions <- length(nodes_J_nav_regions)
            for(q in 1:count_nodes_J_nav_regions){ #开始循环行政区  count_nodes_J_nav_regions  #########TEST##########
              level4_url_sufix <- nodes_J_nav_regions[q] %>% html_attr("href")
              level4_url <- paste0(level0_url,level4_url_sufix) 
              level4_desc_sufix <- nodes_J_nav_regions[q] %>% html_nodes("span") %>% html_text()
              level4_desc <- paste0("行政区-",level4_desc_sufix)
              
              html_level4 <- uf_read_html(level4_url,20)
              if(!all(is.na(html_level4))){
                nodes_sub_region <- html_level4 %>% html_nodes("body") %>% html_nodes("div.nav-category.nav-tabs") %>% html_nodes("div.nc-items.nc-sub") %>%
                                      html_nodes("a")
                count_nodes_sub_region <- length(nodes_sub_region)
                
                for(w in 1:count_nodes_sub_region){#开始循环行政区-子集   count_nodes_sub_region   #########TEST##########
                  if(nodes_sub_region[w] %>% html_attr("href") != level4_url_sufix){
                    level5_url_sufix <- nodes_sub_region[w] %>% html_attr("href")
                    level5_url <- paste0(level0_url,level5_url_sufix) 
                    level5_desc_sufix <- nodes_sub_region[w] %>% html_nodes("span") %>% html_text()
                    level5_desc <- paste0("行政区-Sub-",level5_desc_sufix)
                    
                    html_level5 <- uf_read_html(level5_url,20) 
                    if(!all(is.na(html_level5))){
                      nodes_categorys_nav <- html_level5 %>% html_nodes("body") %>% html_nodes("div.nav-category") %>% html_nodes("div.nc-items")
                      nodes_category <- nodes_categorys_nav[which((nodes_categorys_nav %>% html_attr("id")) == "classfy")[1]] %>% html_nodes("a")
                      count_nodes_category <- length(nodes_category)
                      for(r in 1:count_nodes_category){ #开始循环分类 count_nodes_category  #########TEST##########
                        level6_url_sufix <- nodes_category[r] %>% html_attr("href")
                        level6_url <- paste0(level0_url,level6_url_sufix) 
                        level6_desc_sufix <- nodes_category[r] %>% html_nodes("span") %>% html_text()
                        level6_desc <- paste0("分类-",level6_desc_sufix)
                        
                        html_level6 <- uf_read_html(level6_url,20)
                        if(!all(is.na(html_level6))){
                          nodes_sub_category <- html_level6 %>% html_nodes("body") %>% html_nodes("div.nav-category") %>% html_nodes("div.nc-items.nc-sub")
                          node_sub_category <- nodes_sub_category[which(nodes_sub_category %>% html_attr("id") == "classfy-sub")[1]] %>% html_nodes("a")
                          count_nodes_sub_category <- length(node_sub_category)
                          for(t in 1:count_nodes_sub_category){ #开始循环子分类  count_nodes_sub_category    #########TEST##########
                            level7_url_sufix <- node_sub_category[t] %>% html_attr("href")
                            if(level7_url_sufix != level6_url_sufix){
                              level7_url_sufix <- node_sub_category[t] %>% html_attr("href")
                              level7_url <- paste0(level0_url,level7_url_sufix) 
                              level7_desc_sufix <- node_sub_category[t] %>% html_nodes("span") %>% html_text()
                              level7_desc <- paste0("子分类-",level7_desc_sufix)
                              
                              print(level7_url)
                              
                              #取page number,将search link插入数据表中
                              if(level7_url != ""){
                                count_pages <- uf_retrieve_pagenum(level7_url)
                                df.result <- rbind(df.result,cbind(level0_desc,level0_url,level1_desc,level1_url,level2_desc,level2_url,level3_desc,level3_url,
                                                                   level4_desc,level4_url,level5_desc,level5_url,level6_desc,level6_url,level7_desc,level7_url,
                                                                   count_pages))
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  return(df.result)
  
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

uf_retrieve_pagenum <- function(p_url){
  count_pages <- "1"
  html_url <- uf_read_html(p_url,20)
  if(!all(is.na(html_url))){
    nodes_pagelink <- html_url %>% html_nodes("body") %>% html_nodes("div.page") %>% html_nodes("a.PageLink")
    if(length(nodes_pagelink) > 0){
      count_pages_temp <- nodes_pagelink[length(nodes_pagelink)] %>% html_attr("title")
      if(regexpr("[0-9]{1,5}",count_pages_temp) > 0){
        count_pages <- count_pages_temp
      }
    }
  }
  return(count_pages)
}





















































































































































