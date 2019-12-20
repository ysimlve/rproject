########################################################START########################################################
setwd("C:/YuanLe/R/RWkDir/06. Markets_CC")
#ls()
#rm(list=ls())
#install.packages("lubridate")
options(warn=-1)
options(stringsAsFactors = FALSE)
options(java.parameters = "-Xmx4g")
options(encoding = "utf-8")
Sys.setlocale(category = "LC_CTYPE", locale = "Chinese") 

#install.packages("geosphere",repos='http://cran.us.r-project.org', lib = .libPaths())


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
library(tidyverse)
library(xgboost); library(data.table); library(vcd); library(Matrix); library(Ckmeans.1d.dp);library(modelr)



########################################################Global Vars&Funs########################################################
uf_NumberList_Summary <- function(numx){
  rlt <- rep(0,8)
  names(rlt) <- c("mean","sd","Min","Q1","Median","Q3","Max","IQR")
  if(is.numeric(numx)){
    #数据集的中心趋势度量 - 平均值
    rlt[1] <- mean(numx)
    #数据集的离散程度 - 标准差
    rlt[2] <- sd(numx)
    #五数概括 - Minimum, Q1, Median, Q3, Maximum
    rlt[3:7] <- fivenum(numx)
    #四分位数极差（IQR）= Q3 – Q1
    rlt[8] <- rlt[6] - rlt[4]
  }
  return(rlt)
}
uf_xlsxDataImport <- function(xlsx_file,spreedsheet){
  df_xlsx <- read.xlsx(xlsx_file,sheetName = spreedsheet,header=TRUE,encoding = "UTF-8")
  return(df_xlsx)
}
uf_xlsxDataExport <- function(p_df, xlsx_file, spreedsheet, append = FALSE){
  xlsx::write.xlsx(p_df,xlsx_file,sheetName = spreedsheet, append = append)
}

uf_read_html <- function(p_web,timeouts = 10){
  html_web <- NA
  
  if(all(class(p_web) != c("xml_document","xml_node"))){
    html_web <- tryCatch({
      withTimeout({ read_html(p_web,encoding = "utf-8") }, timeout = timeouts)
    },warnings = function(w){
      ;
      NA
    },error = function(e){
      ;
      tryCatch({
        withTimeout({ read_html(p_web,encoding = "gbk") }, timeout = timeouts)
      },warnings = function(w){
        ;
        NA
      },error = function(e){
        ;
        tryCatch({
          withTimeout({ read_html(p_web,encoding = "gb2312") }, timeout = timeouts)
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

gv_cities <- c("Chengdu","Guangzhou")
attr(gv_cities,"abbr") <- c("cd",  "gz")
attr(gv_cities,"LC") <- c("成都",  "广州")

gv_Homelink <- "ke.com"

city <- "Guangzhou"


baidu.api.geocoder <- function(address,city="Guangzhou",key="NdgFqhKyiqPksC2pLfsGsWps15wzRsES",output="json"){
  location = ""
  city <- attr(gv_cities,"LC")[which(gv_cities == city)]
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


########################################################Process of data scrape########################################################
uf_scrape <- function(city, searchType = "ershoufang"){
  if(1==1){
    houses <- data.frame(url = "",
                         city = "",
                         district = "",
                         sub_district = "",
                         xiaoqu = "",
                         lat ="",
                         long = "",
                         total_price = "",
                         unit_price = "",
                         huxin = "",
                         huxinjiegou = "", ##
                         louceng = "",
                         chaoxiang = "",
                         zhuangxiuqingkuang = "", ##
                         mianji = "",
                         taineimianji = "",     ##
                         Nianfen = "",
                         is_dianti = "",
                         tihubili = "",
                         chanquannianxian = "",
                         guapaishijian = "",
                         jiaoyiquanshu = "",
                         fangwuyongtu = "",
                         jianzhujiegou = "",
                         shangcijiaoyi = "",
                         fangwunianxian = "",
                         chanquansuoshu = "",
                         diyaxinxi = "",
                         fangbenbeijian = "",
                         jianzhuleixing = ""                   
    )
    houses <- houses[-1,]
  }
  
  url_L0 <- paste0("https://",attr(gv_cities,"abbr")[which(gv_cities == city)],".ke.com/") 
  url_L1 <- paste0(url_L0,searchType)   #https://gz.ke.com/ershoufang/ 
  html_L1 <- uf_read_html(url_L1)
  
  if(!all(is.na(html_L1))){
    nodes_districts <- html_L1 %>% html_nodes("div.position") %>% html_nodes("a.CLICKDATA")
    if(length(nodes_districts) > 0){
      suffix_links <- nodes_districts %>% html_attr("href")
      districts <- nodes_districts %>% html_text()
      len_dist <- length(districts)
      for(i in 1:len_dist){ #循环区    #ANDY - 1:len_dist
        url_L2 <- paste0(substr(url_L0,1,nchar(url_L0)-1) ,suffix_links[i]) 
        html_L2 <- uf_read_html(url_L2)         #https://gz.ke.com/ershoufang/tianhe/
        if(!all(is.na(html_L2))){
          for(j in 1:101){ #循环page   #ANDY - 1:100
            url_L3 <- paste0(url_L2,"pg",j,"/")     #https://gz.ke.com/ershoufang/tianhe/pg1/
            html_L3 <- uf_read_html(url_L3)     
            if(!all(is.na(html_L3))){
              if(html_L3 %>% html_node("head") %>% html_node("link") %>% html_attr("href") != "https://m.ke.com/gz/ershoufang/"){
                imgs_link <- html_L3 %>% html_nodes("div.content") %>% html_nodes("a.img.VIEWDATA.CLICKDATA.maidian-detail")
                len_imgs <- length(imgs_link)
                if(len_imgs>0){
                  for(k in 1:len_imgs){  #循环page中的每个item 
                    url_L4 <- imgs_link[k] %>% html_attr("href")  #https://gz.ke.com/ershoufang/108200002827.html; 房屋信息的详细页面
                    print(paste0(districts[i],"-page",j,"-item",k,"-",url_L4))
                    html_L4 <- uf_read_html(url_L4)
                    if(!all(is.na(html_L4))){
                      #--开始读取该房屋的信息
                      
                      node_price <- tryCatch({
                        html_L4 %>% html_nodes("div.content") %>% html_nodes("div.price")
                      },warnings = function(w){
                        ;
                        NA
                      },error = function(e){
                        ;
                        NA
                      })
                      #node_price <- html_L4 %>% html_nodes("div.content") %>% html_nodes("div.price")
                      total_price <- "";unit_price<-""
                      if(!all(is.na(node_price))){
                        #--总价
                        total_price <- paste0(node_price %>% html_node("span.total") %>% html_text(),node_price %>% html_node("span.unit") %>% html_text())
                        #--单价
                        unit_price <- node_price %>% html_node("span.unitPriceValue") %>% html_text()
                        if(node_price %>% html_node("span.unitPriceValue") %>% html_node("i") %>% html_text() == "元/平米") unit_price <- gsub("元/平米","",unit_price)
                      }
                      
                      node_houseInfo <- tryCatch({
                        html_L4 %>% html_nodes("div.content") %>% html_nodes("div.houseInfo") 
                      },warnings = function(w){
                        ;
                        NA
                      },error = function(e){
                        ;
                        NA
                      })
                      #node_houseInfo <- html_L4 %>% html_nodes("div.content") %>% html_nodes("div.houseInfo") 
                      huxin <- "";louceng<-"";chaoxiang<-"";mianji<-"";Nianfen<-"";
                      if(!all(is.na(node_houseInfo))){
                        #--户型
                        huxin <- node_houseInfo %>% html_nodes("div.room") %>% html_node("div.mainInfo") %>% html_text()
                        #--楼层
                        louceng <- node_houseInfo %>% html_nodes("div.room") %>% html_node("div.subInfo") %>% html_text()
                        #--朝向
                        chaoxiang <- node_houseInfo %>% html_nodes("div.type") %>% html_node("div.mainInfo") %>% html_text()
                        #--装修情况
                        #zhuangxiu <- node_houseInfo %>% html_nodes("div.type") %>% html_node("div.subInfo") %>% html_text()
                        #面积
                        mianji <- node_houseInfo %>% html_nodes("div.area") %>% html_node("div.mainInfo") %>% html_text()
                        if(regexpr("[㎡|平方米|平米]",mianji)>0) mianji <- gsub("[㎡|平方米|平米]","",mianji)
                        #--年份
                        Nianfen <- node_houseInfo %>% html_nodes("div.area") %>% html_node("div.subInfo") %>% html_text()
                        if(regexpr("/",Nianfen)>0) Nianfen <- substr(Nianfen,1,regexpr("/",Nianfen)-1)
                        if(regexpr("年建$",Nianfen)>0) Nianfen <- substr(Nianfen,1,regexpr("年建$",Nianfen)-1)
                      }
                      
                      node_aroundInfo <- tryCatch({
                        html_L4 %>% html_nodes("div.content") %>% html_nodes("div.aroundInfo")  
                      },warnings = function(w){
                        ;
                        NA
                      },error = function(e){
                        ;
                        NA
                      })
                      #node_aroundInfo <- html_L4 %>% html_nodes("div.content") %>% html_nodes("div.aroundInfo") 
                      xiaoqu<-"";quyu<-"";
                      if(!all(is.na(node_aroundInfo))){
                        #--所在小区名字
                        xiaoqu <- node_aroundInfo%>%html_nodes("div.communityName") %>% html_node("a") %>% html_text()
                        #--区域(行政区下一级)
                        quyu <- node_aroundInfo%>%html_nodes("div.areaName") %>%  html_nodes("a") %>% html_text()
                        quyu <- paste(quyu,collapse = "")
                      }
                      
                      infos <- tryCatch({
                        html_L4 %>% html_nodes("div.m-content") %>% html_nodes("div.introContent") %>% html_nodes("div.content") %>% html_nodes("ul") %>% html_nodes("li") %>% html_text() 
                      },warnings = function(w){
                        ;
                        NA
                      },error = function(e){
                        ;
                        NA
                      })
                      #infos <- html_L4 %>% html_nodes("div.m-content") %>% html_nodes("div.introContent") %>% html_nodes("div.content") %>% html_nodes("ul") %>% html_nodes("li") %>% html_text()
                      huxin<-"";is_dianti<-"";tihubili<-"";chanquannianxian<-"";guapaishijian<-"";jiaoyiquanshu<-"";fangwuyongtu<-"";jianzhujiegou<-"";taineimianji<-"";
                      shangcijiaoyi<-"";fangwunianxian<-"";chanquansuoshu<-"";diyaxinxi<-"";fangbenbeijian<-"";huxinjiegou<-"";zhuangxiuqingkuang<-"";jianzhuleixing<-"";
                      if(!all(is.na(infos))){
                        #--户型
                        huxin <- gsub("房屋户型","",gsub("\\s","",gsub("\n","",infos[1])))
                        #--有无电梯
                        is_dianti <- gsub("配备电梯","",gsub("\\s","",gsub("\n","",infos[11])))
                        #--梯户比例
                        tihubili <- ""
                        if(is_dianti == "有") tihubili <- gsub("梯户比例","",infos[10]) ####gsub("产权年限","",gsub("\\s","",gsub("\n","",infos[12])))
                        #--产权年限
                        chanquannianxian <- gsub("产权年限","",gsub("\\s","",gsub("\n","",infos[12])))
                        #--挂牌时间
                        guapaishijian <- gsub("挂牌时间","",gsub("\\s","",gsub("\n","",infos[13])))
                        #--交易权属
                        jiaoyiquanshu <- gsub("交易权属","",gsub("\\s","",gsub("\n","",infos[14])))
                        #--房屋用途
                        fangwuyongtu <- gsub("房屋用途","",gsub("\\s","",gsub("\n","",infos[16])))
                        #--建筑结构
                        jianzhujiegou <- gsub("建筑结构","",gsub("\\s","",gsub("\n","",infos[8])))
                        #--套内面积
                        taineimianji <- gsub("套内面积","",gsub("\\s","",gsub("\n","",infos[5])))
                        if(regexpr("[㎡|平方米|平米]",taineimianji)) taineimianji <- gsub("[㎡|平方米|平米]","",taineimianji)
                        #--上次交易
                        shangcijiaoyi <- gsub("上次交易","",gsub("\\s","",gsub("\n","",infos[15])))
                        #--房屋年限
                        fangwunianxian <- gsub("房屋年限","",gsub("\\s","",gsub("\n","",infos[17])))
                        #--产权所属
                        chanquansuoshu <- gsub("产权所属","",gsub("\\s","",gsub("\n","",infos[18])))
                        #--抵押信息
                        diyaxinxi <- gsub("抵押信息","",gsub("\\s","",gsub("\n","",infos[19])))
                        #--房本备件
                        fangbenbeijian <- gsub("房本备件","",gsub("\\s","",gsub("\n","",infos[20])))
                        #--户型结构
                        huxinjiegou <- gsub("户型结构","",gsub("\\s","",gsub("\n","",infos[4])))
                        #--装修情况
                        zhuangxiuqingkuang <- gsub("装修情况","",gsub("\\s","",gsub("\n","",infos[9])))
                        #--建筑类型
                        jianzhuleixing <- gsub("建筑类型","",gsub("\\s","",gsub("\n","",infos[6])))
                      }
                      
                      
                      #--加载房屋信息
                      houses <- rbind(houses, data.frame(url = url_L4,
                                                         city = city,
                                                         district = districts[i],
                                                         sub_district = quyu,
                                                         xiaoqu = xiaoqu,
                                                         lat ="",
                                                         long = "",
                                                         total_price = total_price,
                                                         unit_price = unit_price,
                                                         huxin = huxin,
                                                         huxinjiegou = huxinjiegou, ##
                                                         louceng = louceng,
                                                         chaoxiang = chaoxiang,
                                                         
                                                         zhuangxiuqingkuang = zhuangxiuqingkuang, ##
                                                         mianji = mianji,
                                                         taineimianji = taineimianji,     ##
                                                         Nianfen = Nianfen,
                                                         is_dianti = is_dianti,
                                                         tihubili = tihubili,
                                                         chanquannianxian = chanquannianxian,
                                                         guapaishijian = guapaishijian,
                                                         jiaoyiquanshu = jiaoyiquanshu,
                                                         fangwuyongtu = fangwuyongtu,
                                                         jianzhujiegou = jianzhujiegou,
                                                         shangcijiaoyi = shangcijiaoyi,
                                                         fangwunianxian = fangwunianxian,
                                                         chanquansuoshu = chanquansuoshu,
                                                         diyaxinxi = diyaxinxi,
                                                         fangbenbeijian = fangbenbeijian,
                                                         jianzhuleixing =   jianzhuleixing                 
                      ))
                      
                      
                      
                      
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
  
  #--调用Baidu geocoder API获取经纬度
  
  return(houses)
}


searchType <- "ershoufang"
city <- "Guangzhou"
if(1==1){
  houses <- data.frame(url = "",
                       city = "",
                       district = "",
                       sub_district = "",
                       xiaoqu = "",
                       lat ="",
                       long = "",
                       total_price = "",
                       unit_price = "",
                       huxin = "",
                       huxinjiegou = "", ##
                       louceng = "",
                       chaoxiang = "",
                       zhuangxiuqingkuang = "", ##
                       mianji = "",
                       taineimianji = "",     ##
                       Nianfen = "",
                       is_dianti = "",
                       tihubili = "",
                       chanquannianxian = "",
                       guapaishijian = "",
                       jiaoyiquanshu = "",
                       fangwuyongtu = "",
                       jianzhujiegou = "",
                       shangcijiaoyi = "",
                       fangwunianxian = "",
                       chanquansuoshu = "",
                       diyaxinxi = "",
                       fangbenbeijian = "",
                       jianzhuleixing = ""                   
  )
  houses <- houses[-1,]
}

#View(houses)

url_L0 <- paste0("https://",attr(gv_cities,"abbr")[which(gv_cities == city)],".ke.com/") 
url_L1 <- paste0(url_L0,searchType)   #https://gz.ke.com/ershoufang/ 
html_L1 <- uf_read_html(url_L1)

#--拿贝壳的数据
if(!all(is.na(html_L1))){
  nodes_districts <- html_L1 %>% html_nodes("div.position") %>% html_nodes("a.CLICKDATA")
  if(length(nodes_districts) > 0){
    suffix_links <- nodes_districts %>% html_attr("href")
    districts <- nodes_districts %>% html_text()
    len_dist <- length(districts)
    for(i in 7:len_dist){ #循环区    #ANDY - 1:len_dist
      url_L2 <- paste0(substr(url_L0,1,nchar(url_L0)-1) ,suffix_links[i]) 
      html_L2 <- uf_read_html(url_L2)         #https://gz.ke.com/ershoufang/tianhe/
      if(!all(is.na(html_L2))){
        for(j in 1:101){ #循环page   #ANDY - 1:101
          url_L3 <- paste0(url_L2,"pg",j,"/")     #https://gz.ke.com/ershoufang/tianhe/pg1/
          html_L3 <- uf_read_html(url_L3)     
          if(!all(is.na(html_L3))){
            if(html_L3 %>% html_node("head") %>% html_node("link") %>% html_attr("href") != "https://m.ke.com/gz/ershoufang/"){
              imgs_link <- html_L3 %>% html_nodes("div.content") %>% html_nodes("a.img.VIEWDATA.CLICKDATA.maidian-detail")
              len_imgs <- length(imgs_link)
              if(len_imgs>0){
                for(k in 1:len_imgs){  #循环page中的每个item 
                  url_L4 <- imgs_link[k] %>% html_attr("href")  #https://gz.ke.com/ershoufang/108200002827.html; 房屋信息的详细页面
                  print(paste0(districts[i],"-page",j,"-item",k,"-",url_L4))
                  html_L4 <- uf_read_html(url_L4)
                  if(!all(is.na(html_L4))){
                    #--开始读取该房屋的信息
                    
                    node_price <- tryCatch({
                      html_L4 %>% html_nodes("div.content") %>% html_nodes("div.price")
                    },warnings = function(w){
                      ;
                      NA
                    },error = function(e){
                      ;
                      NA
                    })
                    #node_price <- html_L4 %>% html_nodes("div.content") %>% html_nodes("div.price")
                    total_price <- "";unit_price<-""
                    if(!all(is.na(node_price))){
                      #--总价
                      total_price <- paste0(node_price %>% html_node("span.total") %>% html_text(),node_price %>% html_node("span.unit") %>% html_text())
                      #--单价
                      unit_price <- node_price %>% html_node("span.unitPriceValue") %>% html_text()
                      if(node_price %>% html_node("span.unitPriceValue") %>% html_node("i") %>% html_text() == "元/平米") unit_price <- gsub("元/平米","",unit_price)
                    }
                    
                    node_houseInfo <- tryCatch({
                      html_L4 %>% html_nodes("div.content") %>% html_nodes("div.houseInfo") 
                    },warnings = function(w){
                      ;
                      NA
                    },error = function(e){
                      ;
                      NA
                    })
                    #node_houseInfo <- html_L4 %>% html_nodes("div.content") %>% html_nodes("div.houseInfo") 
                    huxin <- "";louceng<-"";chaoxiang<-"";mianji<-"";Nianfen<-"";
                    if(!all(is.na(node_houseInfo))){
                      #--户型
                      huxin <- node_houseInfo %>% html_nodes("div.room") %>% html_node("div.mainInfo") %>% html_text()
                      #--楼层
                      louceng <- node_houseInfo %>% html_nodes("div.room") %>% html_node("div.subInfo") %>% html_text()
                      #--朝向
                      chaoxiang <- node_houseInfo %>% html_nodes("div.type") %>% html_node("div.mainInfo") %>% html_text()
                      #--装修情况
                      #zhuangxiu <- node_houseInfo %>% html_nodes("div.type") %>% html_node("div.subInfo") %>% html_text()
                      #面积
                      mianji <- node_houseInfo %>% html_nodes("div.area") %>% html_node("div.mainInfo") %>% html_text()
                      if(regexpr("[㎡|平方米|平米]",mianji)>0) mianji <- gsub("[㎡|平方米|平米]","",mianji)
                      #--年份
                      Nianfen <- node_houseInfo %>% html_nodes("div.area") %>% html_node("div.subInfo") %>% html_text()
                      if(regexpr("/",Nianfen)>0) Nianfen <- substr(Nianfen,1,regexpr("/",Nianfen)-1)
                      if(regexpr("年建$",Nianfen)>0) Nianfen <- substr(Nianfen,1,regexpr("年建$",Nianfen)-1)
                    }
                    
                    node_aroundInfo <- tryCatch({
                      html_L4 %>% html_nodes("div.content") %>% html_nodes("div.aroundInfo")  
                    },warnings = function(w){
                      ;
                      NA
                    },error = function(e){
                      ;
                      NA
                    })
                    #node_aroundInfo <- html_L4 %>% html_nodes("div.content") %>% html_nodes("div.aroundInfo") 
                    xiaoqu<-"";quyu<-"";
                    if(!all(is.na(node_aroundInfo))){
                      #--所在小区名字
                      xiaoqu <- node_aroundInfo%>%html_nodes("div.communityName") %>% html_node("a") %>% html_text()
                      #--区域(行政区下一级)
                      quyu <- node_aroundInfo%>%html_nodes("div.areaName") %>%  html_nodes("a") %>% html_text()
                      quyu <- paste(quyu,collapse = "")
                    }
                    
                    infos <- tryCatch({
                      html_L4 %>% html_nodes("div.m-content") %>% html_nodes("div.introContent") %>% html_nodes("div.content") %>% html_nodes("ul") %>% html_nodes("li") %>% html_text() 
                    },warnings = function(w){
                      ;
                      NA
                    },error = function(e){
                      ;
                      NA
                    })
                    #infos <- html_L4 %>% html_nodes("div.m-content") %>% html_nodes("div.introContent") %>% html_nodes("div.content") %>% html_nodes("ul") %>% html_nodes("li") %>% html_text()
                    huxin<-"";is_dianti<-"";tihubili<-"";chanquannianxian<-"";guapaishijian<-"";jiaoyiquanshu<-"";fangwuyongtu<-"";jianzhujiegou<-"";taineimianji<-"";
                    shangcijiaoyi<-"";fangwunianxian<-"";chanquansuoshu<-"";diyaxinxi<-"";fangbenbeijian<-"";huxinjiegou<-"";zhuangxiuqingkuang<-"";jianzhuleixing<-"";
                    if(!all(is.na(infos))){
                      
                      infos <- gsub("\\s","",gsub("\n","",infos))
                      
                      info_item <- "房屋户型"
                      which_item <- which(regexpr(info_item,infos) > 0)[1]
                      if(!is.na(which_item)) huxin <- gsub(info_item,"",infos[which_item])
                      
                      info_item <- "配备电梯"
                      which_item <- which(regexpr(info_item,infos) > 0)[1]
                      if(!is.na(which_item)) is_dianti <- gsub(info_item,"",infos[which_item])
                      
                      info_item <- "梯户比例"
                      which_item <- which(regexpr(info_item,infos) > 0)[1]
                      if(!is.na(which_item)) tihubili <- gsub(info_item,"",infos[which_item])
                      
                      info_item <- "产权年限"
                      which_item <- which(regexpr(info_item,infos) > 0)[1]
                      if(!is.na(which_item)) chanquannianxian <- gsub(info_item,"",infos[which_item])
                      
                      info_item <- "挂牌时间"
                      which_item <- which(regexpr(info_item,infos) > 0)[1]
                      if(!is.na(which_item)) guapaishijian <- gsub(info_item,"",infos[which_item])
                      
                      info_item <- "交易权属"
                      which_item <- which(regexpr(info_item,infos) > 0)[1]
                      if(!is.na(which_item)) jiaoyiquanshu <- gsub(info_item,"",infos[which_item])
                      
                      info_item <- "房屋用途"
                      which_item <- which(regexpr(info_item,infos) > 0)[1]
                      if(!is.na(which_item)) fangwuyongtu <- gsub(info_item,"",infos[which_item])
                      
                      info_item <- "建筑结构"
                      which_item <- which(regexpr(info_item,infos) > 0)[1]
                      if(!is.na(which_item)) jianzhujiegou <- gsub(info_item,"",infos[which_item])
                      
                      info_item <- "套内面积"
                      which_item <- which(regexpr(info_item,infos) > 0)[1]
                      if(!is.na(which_item)){
                        taineimianji <- gsub(info_item,"",infos[which_item])
                        if(regexpr("[㎡|平方米|平米]",taineimianji)) taineimianji <- gsub("[㎡|平方米|平米]","",taineimianji)
                      }
                      
                      
                      info_item <- "上次交易"
                      which_item <- which(regexpr(info_item,infos) > 0)[1]
                      if(!is.na(which_item)) shangcijiaoyi <- gsub(info_item,"",infos[which_item])
                      
                      info_item <- "房屋年限"
                      which_item <- which(regexpr(info_item,infos) > 0)[1]
                      if(!is.na(which_item)) fangwunianxian <- gsub(info_item,"",infos[which_item])
                      
                      info_item <- "产权所属"
                      which_item <- which(regexpr(info_item,infos) > 0)[1]
                      if(!is.na(which_item)) chanquansuoshu <- gsub(info_item,"",infos[which_item])
                      
                      info_item <- "抵押信息"
                      which_item <- which(regexpr(info_item,infos) > 0)[1]
                      if(!is.na(which_item)) diyaxinxi <- gsub(info_item,"",infos[which_item])
                      
                      info_item <- "房本备件"
                      which_item <- which(regexpr(info_item,infos) > 0)[1]
                      if(!is.na(which_item)) fangbenbeijian <- gsub(info_item,"",infos[which_item])
                      
                      info_item <- "户型结构"
                      which_item <- which(regexpr(info_item,infos) > 0)[1]
                      if(!is.na(which_item)) huxinjiegou <- gsub(info_item,"",infos[which_item])
                      
                      info_item <- "装修情况"
                      which_item <- which(regexpr(info_item,infos) > 0)[1]
                      if(!is.na(which_item)) zhuangxiuqingkuang <- gsub(info_item,"",infos[which_item])
                      
                      info_item <- "建筑类型"
                      which_item <- which(regexpr(info_item,infos) > 0)[1]
                      if(!is.na(which_item)) jianzhuleixing <- gsub(info_item,"",infos[which_item])
                    }
                    
                    
                    #--加载房屋信息
                    houses <- rbind(houses, data.frame(url = url_L4,
                                                       city = city,
                                                       district = districts[i],
                                                       sub_district = quyu,
                                                       xiaoqu = xiaoqu,
                                                       lat ="",
                                                       long = "",
                                                       total_price = total_price,
                                                       unit_price = unit_price,
                                                       huxin = huxin,
                                                       huxinjiegou = huxinjiegou, ##
                                                       louceng = louceng,
                                                       chaoxiang = chaoxiang,
                                                       
                                                       zhuangxiuqingkuang = zhuangxiuqingkuang, ##
                                                       mianji = mianji,
                                                       taineimianji = taineimianji,     ##
                                                       Nianfen = Nianfen,
                                                       is_dianti = is_dianti,
                                                       tihubili = tihubili,
                                                       chanquannianxian = chanquannianxian,
                                                       guapaishijian = guapaishijian,
                                                       jiaoyiquanshu = jiaoyiquanshu,
                                                       fangwuyongtu = fangwuyongtu,
                                                       jianzhujiegou = jianzhujiegou,
                                                       shangcijiaoyi = shangcijiaoyi,
                                                       fangwunianxian = fangwunianxian,
                                                       chanquansuoshu = chanquansuoshu,
                                                       diyaxinxi = diyaxinxi,
                                                       fangbenbeijian = fangbenbeijian,
                                                       jianzhuleixing =   jianzhuleixing                 
                    ))
                    
                    
                    
                    
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

ct_houses <- nrow(houses)
#--拿Geocode
for(j in 13706:ct_houses){
  print(j)
  address <- houses$xiaoqu[j]
  if(houses$lat[j] == "" & houses$long[j] == ""){
    latlong <- baidu.api.geocoder(address,city="广州")
    if(latlong != ""){
      houses[houses$xiaoqu == address,c("lat")] <- latlong[1]
      houses[houses$xiaoqu == address,c("long")] <- latlong[2]
    }
  }
}


#View(houses)
uf_xlsxDataExport(houses.gz,"houses_gz_1.xlsx",spreedsheet = "houses")

########################################################Process of data scrape2########################################################
homepage <- "ke.com"
cities <- c("Chengdu","Guangzhou"); attr(cities,"abbr") <- c("cd",  "gz"); attr(cities,"LC") <- c("成都",  "广州")

#--input parameter
city <- "Guangzhou"; rep <- paste0("./DS_KE/city_",attr(cities,"abbr")[which(cities == city)][1])


##########--level: city->xiaoqu->district
#--data - district - 01_xiaoqu_districts.csv
path.rep.districts <- paste0(rep,"/01_xiaoqu_districts.csv")
rep.districts <- read_csv(path.rep.districts)
if(nrow(rep.districts) == 0 & length(names(rep.districts)) == 0){
  rep.districts <- data.frame(city = "", distrct = "", district_LC = "", link = "", lastUpdateDate = Sys.time())[0,]
}
#--process
process.district.run <- FALSE
if(process.district.run){
  link_city_xiaoqu <- paste0("https://",attr(cities,"abbr")[which(cities == city)][1],".",homepage,"/xiaoqu/")
  html_city_xiaoqu <- uf_read_html(link_city_xiaoqu)
  if(all(!is.na(html_city_xiaoqu))){ #-- https://gz.ke.com/xiaoqu/
    
    nodeset.district <- tryCatch({html_city_xiaoqu %>% html_nodes("div.m-filter") %>% html_nodes("div.position") %>% html_nodes("a.CLICKDATA")},
                                 warnings = function(w){
                                   ;
                                   NA
                                 },
                                 error = function(e){
                                   ;
                                   NA
                                 })
    if(all(!is.na(nodeset.district)) & length(nodeset.district) > 0){
      districts_LC <- nodeset.district %>% html_text()
      tp_href <- nodeset.district %>% html_attr("href")
      districts <- ifelse(regexpr("/xiaoqu/",tp_href)>0, substr(tp_href,9,nchar(tp_href)-1)) 
      link_districts <- paste0(link_city_xiaoqu, districts, "/")
      ct_districts <- length(link_districts)
      
      for(i in 1:ct_districts){
        if(!any(link_districts[i] == rep.districts$link)){
          rep.districts <- rbind(rep.districts, data.frame(city,distrct=districts[i],district_LC=districts_LC[i],link=link_districts[i],lastUpdateDate=Sys.time()))
        }
      }
      
      
      
    }
    
    
  }
  if(file.copy(path.rep.districts,paste0(substr(path.rep.districts,1,nchar(path.rep.districts)-4),"_bk_",as.integer(Sys.Date()),".csv"),overwrite = T)){
    write_csv(rep.districts,path.rep.districts)
  }
  
}

##########--level: city->xiaoqu->district->xiaoqu
#--data - district.xiaoqus - 02_district_xiaoqus.csv
path.rep.d.xiaoqus <- paste0(rep,"/02_district_xiaoqus.csv")
rep.d.xiaoqus <- read_csv(path.rep.d.xiaoqus)
if(nrow(rep.d.xiaoqus) == 0 & length(names(rep.d.xiaoqus)) == 0){
  rep.d.xiaoqus <- data.frame(city = "", distrct = "", xiaoqu = "", link = "", lastUpdateDate = Sys.time())[0,]
}
#--process
process.d.xiaoqus.run <- FALSE
if(process.d.xiaoqus.run){
  ct_districts <- nrow(rep.districts)
  for(i in 1:ct_districts){ #loop district
    rd <- rep.districts[i,]
    print(paste0("district-",rd$distrct))
    link_district_xiaoqus <- rd$link
    for(k in 1:100){  #loop pages
      print(paste0("district-",rd$distrct,"-page-",k))
      link_district_xiaoqu <- paste0(link_district_xiaoqus,"pg",k,"/") 
      html_district_xiaoqu <- uf_read_html(link_district_xiaoqu)
      
      if(all(!is.na(html_district_xiaoqu))){
        nodeset.district.xiaoqu <- tryCatch({html_district_xiaoqu %>% html_nodes("li.clear.xiaoquListItem.CLICKDATA")},
                                            warnings = function(w){
                                              ;
                                              NA
                                            },
                                            error = function(e){
                                              ;
                                              NA
                                            })
        if(all(!is.na(nodeset.district.xiaoqu)) & length(nodeset.district.xiaoqu) > 0){
          ct_district_xiaoqus <- length(nodeset.district.xiaoqu)
          for(j in 1:ct_district_xiaoqus){
            print(paste0("district-",rd$distrct,"-page-",k,"-xiaoqu-",j))
            node.xiaoqu <- tryCatch({nodeset.district.xiaoqu[j] %>% html_node("div.info") %>% html_node("a")},
                                    warnings = function(w){
                                      ;
                                      NA
                                    },
                                    error = function(e){
                                      ;
                                      NA
                                    })
            if(all(!is.na(node.xiaoqu)) & length(node.xiaoqu) > 0){
              xiaoqu.name <- node.xiaoqu %>% html_text()
              xiaoqu.link <- node.xiaoqu %>% html_attr("href")
              if(!any(rep.d.xiaoqus$link == xiaoqu.link)){
                rep.d.xiaoqus <- rbind(rep.d.xiaoqus, data.frame(city = rd$city, 
                                                                 distrct = rd$distrct, 
                                                                 xiaoqu = xiaoqu.name, 
                                                                 link = xiaoqu.link, 
                                                                 lastUpdateDate = Sys.time()))
              }
            }
          }
        }
      }
    }
  }
  if(file.copy(path.rep.d.xiaoqus,paste0(substr(path.rep.d.xiaoqus,1,nchar(path.rep.d.xiaoqus)-4),"_bk_",as.integer(Sys.Date()),".csv"),overwrite = T)){
    write_csv(rep.d.xiaoqus,path.rep.d.xiaoqus)
  }
  
}

##########--level: city->xiaoqu->district->xiaoqu->GeneralInfor
#--data - xiaoqu.GeneralInfor - 03_xiaoqu_GeneralInfor.csv
path.rep.xq.GI <- paste0(rep,"/03_xiaoqu_GeneralInfor.csv")
rep.xq.GI <- read_csv(path.rep.xq.GI)
if(nrow(rep.xq.GI) == 0 & length(names(rep.xq.GI)) == 0){
  rep.xq.GI <- data.frame(city = "", distrct = "", xiaoqu = "", link = "", 
                          xiaoquUnitPrice = "",jianZhuNianFen = "", jianZhuNeiXing = "", 
                          wuYeFeiYong = "", wuYeGongSi = "", kaiFaShang = "", 
                          louDongZongShu = "", fangWuZongShu = "", linkToErshoufang = "",
                          linkToDeals = "", linkToRents = "", linkToNearBy = "", subDistrct = "",
                          lastUpdateDate = Sys.time())[0,]
}
#--process
process.xq.GI.run <- FALSE
if(process.xq.GI.run){
  ct_d_xiaoqus <- nrow(rep.d.xiaoqus)
  for(i in 1:ct_d_xiaoqus){
    rd <- rep.d.xiaoqus[i,]
    print(paste0("district-",rd$distrct,"-xiaoqu-",rd$xiaoqu, "-",i))
    link_xiaoqu <- rd$link
    html_xiaoqu <- uf_read_html(link_xiaoqu)
    if(all(!is.na(html_xiaoqu))){
      xiaoquUnitPrice <- "";jianZhuNianFen <- ""; jianZhuNeiXing <- ""; 
      wuYeFeiYong <- ""; wuYeGongSi <- ""; kaiFaShang <- ""; 
      louDongZongShu <- ""; fangWuZongShu <- ""; linkToErshoufang <- "";
      linkToDeals <- ""; linkToRents <- ""; linkToNearBy <- ""; subDistrct <- ""
      if(all(!is.na(html_xiaoqu))){
        
        #--小区基本情况信息
        node.xiaoquDescribe <- tryCatch({html_xiaoqu %>% html_nodes("div.xiaoquDescribe.fr")},
                                        warnings = function(w){
                                          ;
                                          NA
                                        },
                                        error = function(e){
                                          ;
                                          NA
                                        })
        if(all(!is.na(node.xiaoquDescribe)) & length(node.xiaoquDescribe) > 0){
          #--均价
          xiaoquUnitPrice <- node.xiaoquDescribe %>% html_node("span.xiaoquUnitPrice") %>% html_text()
          
          #--基本信息
          xiaoquInfoItem <- node.xiaoquDescribe %>% html_nodes("div.xiaoquInfoItem")
          
          ds.xiaoquInfoItem <- data.frame(Item = "", Infor = "")[0]
          ct_xiaoquInfoItem <- length(xiaoquInfoItem)
          if(ct_xiaoquInfoItem>0){
            for(m in 1:ct_xiaoquInfoItem){
              it <- xiaoquInfoItem[m] %>% html_nodes("span") %>% html_text()
              ds.xiaoquInfoItem <- rbind(ds.xiaoquInfoItem, data.frame(Item = it[1] , 
                                                                       Infor = it[2]))
            }
            jianZhuNianFen <- ds.xiaoquInfoItem[which(ds.xiaoquInfoItem$Item == "建筑年代"),"Infor"]
            jianZhuNeiXing <- ds.xiaoquInfoItem[which(ds.xiaoquInfoItem$Item == "建筑类型"),"Infor"]
            wuYeFeiYong <- ds.xiaoquInfoItem[which(ds.xiaoquInfoItem$Item == "物业费用"),"Infor"]
            wuYeGongSi <- ds.xiaoquInfoItem[which(ds.xiaoquInfoItem$Item == "物业公司"),"Infor"]
            kaiFaShang <- ds.xiaoquInfoItem[which(ds.xiaoquInfoItem$Item == "开发商"),"Infor"]
            louDongZongShu <- ds.xiaoquInfoItem[which(ds.xiaoquInfoItem$Item == "楼栋总数"),"Infor"]
            fangWuZongShu <- ds.xiaoquInfoItem[which(ds.xiaoquInfoItem$Item == "房屋总数"),"Infor"]
          }
        }
        
        #--在售二手房链接
        node.linkToErshoufang <- tryCatch({html_xiaoqu %>% html_nodes("div.goodSellHeader.clear")},
                                          warnings = function(w){
                                            ;
                                            NA
                                          },
                                          error = function(e){
                                            ;
                                            NA
                                          })
        if(all(!is.na(node.linkToErshoufang)) & length(node.linkToErshoufang) > 0){
          linkToErshoufang <- node.linkToErshoufang %>% html_node("a") %>% html_attr("href")
        }
        
        #--成交记录  
        node.deals <-tryCatch({html_xiaoqu %>% html_nodes("a.btn-large.CLICKDATA")},
                              warnings = function(w){
                                ;
                                NA
                              },
                              error = function(e){
                                ;
                                NA
                              })
        if(all(!is.na(node.deals)) & length(node.deals) > 0){
          linkToDeals <- node.deals %>% html_attr("href")
        }
        #--小区全部出租房
        node.rents<-tryCatch({html_xiaoqu %>% html_nodes("div.rentListHeader.clear")},
                             warnings = function(w){
                               ;
                               NA
                             },
                             error = function(e){
                               ;
                               NA
                             })
        if(all(!is.na(node.rents)) & length(node.rents) > 0){
          linkToRents <- node.rents %>% html_node("a") %>% html_attr("href")
        }
        #--同商圈小区
        node.nearBy<-tryCatch({html_xiaoqu %>% html_nodes("div.nearby") %>% html_node("div.nearbyHeader.clear") %>% html_node("a")},
                              warnings = function(w){
                                ;
                                NA
                              },
                              error = function(e){
                                ;
                                NA
                              })
        if(all(!is.na(node.nearBy)) & length(node.nearBy) > 0){
          linkToNearBy <- node.nearBy %>% html_attr("href")
          subDistrct <- gsub("/$","",gsub("https://gz.ke.com/xiaoqu/","",linkToNearBy))
        }
      }
      rep.xq.GI <- rbind(rep.xq.GI, data.frame(city = rd$city, distrct = rd$distrct, xiaoqu = rd$xiaoqu, link = rd$link, 
                                               xiaoquUnitPrice = xiaoquUnitPrice,jianZhuNianFen = jianZhuNianFen, 
                                               jianZhuNeiXing = jianZhuNeiXing, 
                                               wuYeFeiYong = wuYeFeiYong, wuYeGongSi = wuYeGongSi, kaiFaShang = kaiFaShang, 
                                               louDongZongShu = louDongZongShu, fangWuZongShu = fangWuZongShu, 
                                               linkToErshoufang = linkToErshoufang,
                                               linkToDeals = linkToDeals, linkToRents = linkToRents, linkToNearBy = linkToNearBy, 
                                               subDistrct = subDistrct,
                                               lastUpdateDate = Sys.time()))
    }
    
  }
  if(file.copy(path.rep.xq.GI,paste0(substr(path.rep.xq.GI,1,nchar(path.rep.xq.GI)-4),"_bk_",as.integer(Sys.Date()),".csv"),overwrite = T)){
    write_csv(rep.xq.GI,path.rep.xq.GI)
  }
}

##########--level: xiaoqu->ErShouFang
#--data - xiaoqu.ErShouFang - 04_xiaoqu_ErShouFang.csv
path.rep.xq.ESF <- paste0(rep,"/04_xiaoqu_ErShouFang.csv")
rep.xq.ESF <- read_csv(path.rep.xq.ESF)
if(nrow(rep.xq.ESF) == 0 & length(names(rep.xq.ESF)) == 0){
  rep.xq.ESF <- data.frame(city = "", distrct = "", subDistrct = "", xiaoqu = "", link_xiaoqu = "", 
                           link_house = "",
                           
                           total_price = "",unit_price="",
                           louceng="",chaoxiang="",mianji="",Nianfen="",
                           huxin="",is_dianti="",tihubili="",chanquannianxian="",guapaishijian="",jiaoyiquanshu="",fangwuyongtu="",jianzhujiegou="",taineimianji="",
                           shangcijiaoyi="",fangwunianxian="",chanquansuoshu="",diyaxinxi="",fangbenbeijian="",huxinjiegou="",zhuangxiuqingkuang="",jianzhuleixing="",
                           
                          lastUpdateDate = Sys.time())[0,]
}
#View(rep.xq.ESF); nrow(rep.xq.ESF)
#--process
process.xq.ESF.run <- TRUE
if(process.xq.ESF.run){
  ct_xq_GI <- nrow(rep.xq.GI)
  for(i in 1:ct_xq_GI){ #--循环小区###########################################################################
    rd <- rep.xq.GI[i,]
    print(paste0("district-",rd$distrct,"-xiaoqu-",rd$xiaoqu, "-",i))
    link_ESFs <- rd$linkToErshoufang
    if(!is.na(link_ESFs) & nchar(link_ESFs) > 0){
      html_ESFs <- uf_read_html(link_ESFs)
      if(all(!is.na(html_ESFs))){
        node.total <- tryCatch({html_ESFs %>% html_nodes("div.resultDes.clear")},
                               warnings = function(w){
                                 ;
                                 NA
                               },
                               error = function(e){
                                 ;
                                 NA
                               })
        if(all(!is.na(node.total)) & length(node.total) > 0){
          total_xiaoqus <- gsub("\\s","",node.total %>% html_node("h2.total.fl") %>% html_node("span") %>% html_text())
          if(!is.na(total_xiaoqus) & nchar(total_xiaoqus)>0){
            total_xiaoqus <- as.numeric(total_xiaoqus)
            num_of_single_page <- 30
            num_pages <- as.integer(total_xiaoqus/num_of_single_page) + 1
            for(j in 1:num_pages){#--循环页面###########################################################################
              print(paste0("district-",rd$distrct,"-xiaoqu-",rd$xiaoqu, "-",i,"-page-",j))
              
              link_gi <- gsub("city",attr(cities,"abbr")[which(cities == city)[1]] ,"https://city.ke.com/ershoufang/")
              link_ESF <- paste0(link_gi,"pg",j,gsub(link_gi,"",link_ESFs)) 
              html_ESF <- uf_read_html(link_ESF)
              if(all(!is.na(html_ESF))){
                
                node.houseList <- tryCatch({html_ESF %>% html_nodes("ul.sellListContent") %>% html_nodes("a.CLICKDATA.maidian-detail")},
                                       warnings = function(w){
                                         ;
                                         NA
                                       },
                                       error = function(e){
                                         ;
                                         NA
                                       })
                if(all(!is.na(node.houseList)) & length(node.houseList) > 0){
                  houseList <- unique(node.houseList %>% html_attr("href"))
                  ct_houseList <- length(houseList)
                  for(n in 1:ct_houseList){ #--循环每一个房子###########################################################################
                    print(paste0("district-",rd$distrct,"-xiaoqu-",rd$xiaoqu, "-",i,"-page-",j,"-house-",n))
                    link_house <- houseList[n]
                    html_house <- uf_read_html(link_house)
                    
                    total_price <- "";unit_price<-"";louceng<-"";chaoxiang<-"";mianji<-"";Nianfen<-"";
                    huxin<-"";is_dianti<-"";tihubili<-"";chanquannianxian<-"";guapaishijian<-"";jiaoyiquanshu<-"";fangwuyongtu<-"";jianzhujiegou<-"";taineimianji<-"";
                    shangcijiaoyi<-"";fangwunianxian<-"";chanquansuoshu<-"";diyaxinxi<-"";fangbenbeijian<-"";huxinjiegou<-"";zhuangxiuqingkuang<-"";jianzhuleixing<-"";
                  
                    if(all(!is.na(html_house))){
                      html_L4 <- html_house
                      if(!all(is.na(html_L4))){
                        #--开始读取该房屋的信息
                        
                        node_price <- tryCatch({
                          html_L4 %>% html_nodes("div.content") %>% html_nodes("div.price")
                        },warnings = function(w){
                          ;
                          NA
                        },error = function(e){
                          ;
                          NA
                        })
                        if(!all(is.na(node_price))){
                          #--总价
                          total_price <- tryCatch({
                            paste0(node_price %>% html_node("span.total") %>% html_text(),node_price %>% html_node("span.unit") %>% html_text())
                          },warnings = function(w){
                            ;
                            NA
                          },error = function(e){
                            ;
                            NA
                          })
                          #total_price <- paste0(node_price %>% html_node("span.total") %>% html_text(),node_price %>% html_node("span.unit") %>% html_text())
                          #--单价
                          unit_price <- tryCatch({
                            node_price %>% html_node("span.unitPriceValue") %>% html_text()
                          },warnings = function(w){
                            ;
                            NA
                          },error = function(e){
                            ;
                            NA
                          })
                          #unit_price <- node_price %>% html_node("span.unitPriceValue") %>% html_text()
                          #if(node_price %>% html_node("span.unitPriceValue") %>% html_node("i") %>% html_text() == "元/平米") unit_price <- gsub("元/平米","",unit_price)
                        }
                        
                        node_houseInfo <- tryCatch({
                          html_L4 %>% html_nodes("div.content") %>% html_nodes("div.houseInfo") 
                        },warnings = function(w){
                          ;
                          NA
                        },error = function(e){
                          ;
                          NA
                        }) 
                        if(!all(is.na(node_houseInfo))){
                          #--楼层
                          louceng <- tryCatch({
                            node_houseInfo %>% html_nodes("div.room") %>% html_node("div.subInfo") %>% html_text()
                          },warnings = function(w){
                            ;
                            NA
                          },error = function(e){
                            ;
                            NA
                          }) 
                          #louceng <- node_houseInfo %>% html_nodes("div.room") %>% html_node("div.subInfo") %>% html_text()
                          #--朝向
                          chaoxiang <- tryCatch({
                            node_houseInfo %>% html_nodes("div.type") %>% html_node("div.mainInfo") %>% html_text()
                          },warnings = function(w){
                            ;
                            NA
                          },error = function(e){
                            ;
                            NA
                          }) 
                          #chaoxiang <- node_houseInfo %>% html_nodes("div.type") %>% html_node("div.mainInfo") %>% html_text()
                          #--装修情况
                          #zhuangxiu <- node_houseInfo %>% html_nodes("div.type") %>% html_node("div.subInfo") %>% html_text()
                          #面积
                          mianji <- tryCatch({
                            node_houseInfo %>% html_nodes("div.area") %>% html_node("div.mainInfo") %>% html_text()
                          },warnings = function(w){
                            ;
                            NA
                          },error = function(e){
                            ;
                            NA
                          }) 
                          #mianji <- node_houseInfo %>% html_nodes("div.area") %>% html_node("div.mainInfo") %>% html_text()
                          #if(regexpr("[㎡|平方米|平米]",mianji)>0) mianji <- gsub("[㎡|平方米|平米]","",mianji)
                          #--年份
                          Nianfen <- tryCatch({
                            node_houseInfo %>% html_nodes("div.area") %>% html_node("div.subInfo") %>% html_text()
                          },warnings = function(w){
                            ;
                            NA
                          },error = function(e){
                            ;
                            NA
                          }) 
                          #Nianfen <- node_houseInfo %>% html_nodes("div.area") %>% html_node("div.subInfo") %>% html_text()
                          #if(regexpr("/",Nianfen)>0) Nianfen <- substr(Nianfen,1,regexpr("/",Nianfen)-1)
                          #if(regexpr("年建$",Nianfen)>0) Nianfen <- substr(Nianfen,1,regexpr("年建$",Nianfen)-1)
                        }
                        
                        infos <- tryCatch({
                          html_L4 %>% html_nodes("div.m-content") %>% html_nodes("div.introContent") %>% html_nodes("div.content") %>% html_nodes("ul") %>% html_nodes("li") %>% html_text() 
                        },warnings = function(w){
                          ;
                          NA
                        },error = function(e){
                          ;
                          NA
                        })
                        if(!all(is.na(infos))){
                          
                          infos <- gsub("\\s","",gsub("\n","",infos))
                          
                          info_item <- "房屋户型"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) huxin <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "配备电梯"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) is_dianti <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "梯户比例"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) tihubili <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "产权年限"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) chanquannianxian <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "挂牌时间"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) guapaishijian <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "交易权属"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) jiaoyiquanshu <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "房屋用途"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) fangwuyongtu <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "建筑结构"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) jianzhujiegou <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "套内面积"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)){
                            taineimianji <- gsub(info_item,"",infos[which_item])
                            if(regexpr("[㎡|平方米|平米]",taineimianji)) taineimianji <- gsub("[㎡|平方米|平米]","",taineimianji)
                          }
                          
                          
                          info_item <- "上次交易"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) shangcijiaoyi <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "房屋年限"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) fangwunianxian <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "产权所属"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) chanquansuoshu <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "抵押信息"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) diyaxinxi <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "房本备件"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) fangbenbeijian <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "户型结构"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) huxinjiegou <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "装修情况"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) zhuangxiuqingkuang <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "建筑类型"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) jianzhuleixing <- gsub(info_item,"",infos[which_item])
                        }
                        
                      }
                    }
                    
                    #--加载房屋信息
                    if(1==1){
                      rep.xq.ESF <- rbind(rep.xq.ESF, data.frame(city = rd$city, distrct = rd$distrct, subDistrct = rd$subDistrct, 
                                                                 xiaoqu = rd$xiaoqu, link_xiaoqu = rd$link, 
                                                                 link_house = link_house,
                                                                 
                                                                 total_price = total_price[1],unit_price=unit_price[1],
                                                                 louceng=louceng[1],chaoxiang=chaoxiang[1],mianji=mianji[1],Nianfen=Nianfen[1],
                                                                 huxin=huxin[1],is_dianti=is_dianti[1],tihubili=tihubili[1],
                                                                 chanquannianxian=chanquannianxian[1],guapaishijian=guapaishijian[1],
                                                                 jiaoyiquanshu=jiaoyiquanshu[1],fangwuyongtu=fangwuyongtu[1],jianzhujiegou=jianzhujiegou[1],
                                                                 taineimianji=taineimianji[1],shangcijiaoyi=shangcijiaoyi[1],fangwunianxian=fangwunianxian[1],
                                                                 chanquansuoshu=chanquansuoshu[1],diyaxinxi=diyaxinxi[1],fangbenbeijian=fangbenbeijian[1],
                                                                 huxinjiegou=huxinjiegou[1],zhuangxiuqingkuang=zhuangxiuqingkuang[1],jianzhuleixing=jianzhuleixing[1],
                                                                 
                                                                 lastUpdateDate = Sys.time()              
                      ))
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
  if(file.copy(path.rep.xq.ESF,paste0(substr(path.rep.xq.ESF,1,nchar(path.rep.xq.ESF)-4),"_bk_",as.integer(Sys.Date()),".csv"),overwrite = T)){
    write_csv(rep.xq.ESF,path.rep.xq.ESF)
  }
}

##########--level: xiaoqu->Deals
#--data - xiaoqu.Deals - 05_xiaoqu_Deals.csv
path.rep.xq.DLs <- paste0(rep,"/05_xiaoqu_Deals.csv")
rep.xq.DLs <- read_csv(path.rep.xq.DLs)
if(nrow(rep.xq.DLs) == 0 & length(names(rep.xq.DLs)) == 0){
  rep.xq.DLs <- data.frame(city = "", distrct = "", subDistrct = "", xiaoqu = "", link_xiaoqu = "", 
                           link_house = "",
                           
                           deal_total_price =  "",deal_unit_price= "",guapai_total_price =  "", chengjiaozhouqi =  "",tianjiacishu= "",daikancishu= "",guangzhu= "",liulangcishu= "",
                           huxin=  "",is_dianti=  "",tihubili=  "",chanquannianxian=  "",jianzhujiegou=  "",taineimianji=  "",huxinjiegou=  "",zhuangxiuqingkuang=  "",
                           jianzhuleixing=  "",louceng=  "",mianji=  "",chaoxiang=  "",Nianfen=  "",
                           guapaishijian= "",jiaoyiquanshu= "",fangwuyongtu= "",fangwunianxian= "",chanquansuoshu= "",lianjiabianhao= "",
                           
                           lastUpdateDate = Sys.time())[0,]
}
#View(rep.xq.DLs)
#--process
process.xq.DLs.run <- TRUE
if(process.xq.DLs.run){
  ct_xq_GI <- nrow(rep.xq.GI)
  for(i in 1:ct_xq_GI){ #--循环小区###########################################################################
    rd <- rep.xq.GI[i,]
    print(paste0("district-",rd$distrct,"-xiaoqu-",rd$xiaoqu, "-",i))
    link_DLs <- rd$linkToDeals
    if(!is.na(link_DLs) & nchar(link_DLs) > 0){
      html_DLs <- uf_read_html(link_DLs)
      if(all(!is.na(html_DLs))){
        node.total <- tryCatch({html_DLs %>% html_nodes("div.resultDes.clear")},
                               warnings = function(w){
                                 ;
                                 NA
                               },
                               error = function(e){
                                 ;
                                 NA
                               })
        if(all(!is.na(node.total)) & length(node.total) > 0){
          total_xiaoqus <- gsub("\\s","",node.total %>% html_node("div.total.fl") %>% html_node("span") %>% html_text())
          if(!is.na(total_xiaoqus) & nchar(total_xiaoqus)>0){
            total_xiaoqus <- as.numeric(total_xiaoqus)
            num_of_single_page <- 30
            num_pages <- as.integer(total_xiaoqus/num_of_single_page) + 1
            for(j in 1:num_pages){#--循环页面###########################################################################
              print(paste0("district-",rd$distrct,"-xiaoqu-",rd$xiaoqu, "-",i,"-page-",j))
              link_gi <- gsub("city",attr(cities,"abbr")[which(cities == city)[1]] ,"https://city.ke.com/chengjiao/")
              link_DL <- paste0(link_gi,"pg",j,gsub(link_gi,"",link_DLs)) 
              html_DL <- uf_read_html(link_DL)
              if(all(!is.na(html_DL))){
                
                node.houseList <- tryCatch({html_DL %>% html_nodes("ul.listContent") %>% html_nodes("a")},
                                           warnings = function(w){
                                             ;
                                             NA
                                           },
                                           error = function(e){
                                             ;
                                             NA
                                           })
                if(all(!is.na(node.houseList)) & length(node.houseList) > 0){
                  houseList <- unique(node.houseList %>% html_attr("href"))
                  ct_houseList <- length(houseList)
                  for(n in 1:ct_houseList){ #--循环每一个房子###########################################################################
                    print(paste0("district-",rd$distrct,"-xiaoqu-",rd$xiaoqu, "-",i,"-page-",j,"-house-",n))
                    link_house <- houseList[n]
                    html_house <- uf_read_html(link_house)
                    
                    deal_total_price <- "";deal_unit_price<-"";guapai_total_price <- ""; chengjiaozhouqi <- "";tianjiacishu<-"";daikancishu<-"";guangzhu<-"";liulangcishu<-"";
                    huxin<- "";is_dianti<- "";tihubili<- "";chanquannianxian<- "";jianzhujiegou<- "";taineimianji<- "";huxinjiegou<- "";zhuangxiuqingkuang<- "";
                    jianzhuleixing<- "";louceng<- "";mianji<- "";chaoxiang<- "";Nianfen<- ""
                    guapaishijian<-"";jiaoyiquanshu<-"";fangwuyongtu<-"";fangwunianxian<-"";chanquansuoshu<-"";lianjiabianhao<-""
                    
                    
                    if(all(!is.na(html_house))){
                      html_L4 <- html_house
                      if(!all(is.na(html_L4))){
                        #--开始读取该房屋的信息
                        
                        #--成交价
                        node_price <- tryCatch({
                          html_L4 %>% html_nodes("div.info.fr") %>% html_nodes("div.price")
                        },warnings = function(w){
                          ;
                          NA
                        },error = function(e){
                          ;
                          NA
                        })
                        if(!all(is.na(node_price))){
                          #--成交总价
                          deal_total_price <- tryCatch({
                            node_price %>% html_node("span.dealTotalPrice") %>% html_text()
                          },warnings = function(w){
                            ;
                            NA
                          },error = function(e){
                            ;
                            NA
                          })
                          
                          #--成交单价
                          deal_unit_price <- tryCatch({
                            node_price %>% html_node("b") %>% html_text()
                          },warnings = function(w){
                            ;
                            NA
                          },error = function(e){
                            ;
                            NA
                          })
                          
                        }
                        
                        #--挂牌价及相关信息
                        node_guapai <- tryCatch({
                          html_L4 %>% html_nodes("div.info.fr") %>% html_nodes("span") %>% html_text()
                        },warnings = function(w){
                          ;
                          NA
                        },error = function(e){
                          ;
                          NA
                        })
                        if(!all(is.na(node_guapai))){
                          guapai_total_price <- gsub("挂牌价格","",node_guapai[which(regexpr("挂牌价格",node_guapai)>0)[1]])
                          chengjiaozhouqi <- gsub("成交周期","",node_guapai[which(regexpr("成交周期",node_guapai)>0)[1]])
                          tianjiacishu <- gsub("调价","",node_guapai[which(regexpr("调价",node_guapai)>0)[1]])
                          daikancishu <- gsub("带看","",node_guapai[which(regexpr("带看",node_guapai)>0)[1]])
                          guangzhu <- gsub("关注","",node_guapai[which(regexpr("关注",node_guapai)>0)[1]])
                          liulangcishu <- gsub("浏览","",node_guapai[which(regexpr("浏览",node_guapai)>0)[1]])
                        }
                        
                        #--房屋基本信息
                        infos <- tryCatch({
                          html_L4 %>% html_nodes("div.introContent") %>% html_nodes("div.base") %>% html_nodes("div.content") %>% html_nodes("ul") %>% html_nodes("li") %>% html_text() 
                        },warnings = function(w){
                          ;
                          NA
                        },error = function(e){
                          ;
                          NA
                        })
                        if(!all(is.na(infos))){
                          
                          infos <- gsub("\\s","",gsub("\n","",infos))
                          
                          info_item <- "房屋户型"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) huxin <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "配备电梯"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) is_dianti <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "梯户比例"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) tihubili <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "产权年限"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) chanquannianxian <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "建筑结构"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) jianzhujiegou <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "套内面积"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)){
                            taineimianji <- gsub(info_item,"",infos[which_item])
                            if(regexpr("[㎡|平方米|平米]",taineimianji)) taineimianji <- gsub("[㎡|平方米|平米]","",taineimianji)
                          }
                          
                          info_item <- "户型结构"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) huxinjiegou <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "装修情况"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) zhuangxiuqingkuang <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "建筑类型"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) jianzhuleixing <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "所在楼层"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) louceng <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "建筑面积"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) mianji <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "房屋朝向"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) chaoxiang <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "建成年代"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) Nianfen <- gsub(info_item,"",infos[which_item])
                          
                        }
                        
                        #--房屋更多信息(交易属性)
                        infos_more <- tryCatch({
                          html_L4 %>% html_nodes("div.introContent") %>% html_nodes("div.transaction") %>% html_nodes("ul") %>% html_nodes("li") %>% html_text() 
                        },warnings = function(w){
                          ;
                          NA
                        },error = function(e){
                          ;
                          NA
                        })
                        if(!all(is.na(infos_more))){
                          infos <- gsub("\\s","",gsub("\n","",infos_more))
                          
                          info_item <- "挂牌时间"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) guapaishijian <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "交易权属"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) jiaoyiquanshu <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "房屋用途"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) fangwuyongtu <- gsub(info_item,"",infos[which_item])
                          
                          #info_item <- "上次交易"
                          #which_item <- which(regexpr(info_item,infos) > 0)[1]
                          #if(!is.na(which_item)) shangcijiaoyi <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "房屋年限"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) fangwunianxian <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "房权所属"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) chanquansuoshu <- gsub(info_item,"",infos[which_item])
                          
                          #info_item <- "抵押信息"
                          #which_item <- which(regexpr(info_item,infos) > 0)[1]
                          #if(!is.na(which_item)) diyaxinxi <- gsub(info_item,"",infos[which_item])
                          
                          #info_item <- "房本备件"
                          #which_item <- which(regexpr(info_item,infos) > 0)[1]
                          #if(!is.na(which_item)) fangbenbeijian <- gsub(info_item,"",infos[which_item])
                          
                          info_item <- "链家编号"
                          which_item <- which(regexpr(info_item,infos) > 0)[1]
                          if(!is.na(which_item)) lianjiabianhao <- gsub(info_item,"",infos[which_item])
                        }
                        
                        
                      }
                    }
                    
                    #--加载房屋信息
                    if(1==1){
                      rep.xq.DLs <- rbind(rep.xq.DLs, data.frame(city = rd$city, distrct = rd$distrct, subDistrct = rd$subDistrct, 
                                                                 xiaoqu = rd$xiaoqu, link_xiaoqu = rd$link, 
                                                                 link_house = link_house,
                                                                 
                                                                 deal_total_price = deal_total_price[1],deal_unit_price= deal_unit_price[1],guapai_total_price = guapai_total_price[1], 
                                                                 chengjiaozhouqi = chengjiaozhouqi[1],tianjiacishu= tianjiacishu[1],daikancishu= daikancishu[1],guangzhu= guangzhu[1],
                                                                 liulangcishu= liulangcishu[1],huxin= huxin[1],is_dianti= is_dianti[1],tihubili= tihubili[1],chanquannianxian= chanquannianxian[1],
                                                                 jianzhujiegou= jianzhujiegou[1],taineimianji= taineimianji[1],huxinjiegou= huxinjiegou[1],zhuangxiuqingkuang= zhuangxiuqingkuang[1],
                                                                 jianzhuleixing= jianzhuleixing[1],louceng= louceng[1],mianji= mianji[1],chaoxiang= chaoxiang[1],Nianfen=Nianfen[1],
                                                                 guapaishijian= guapaishijian[1],jiaoyiquanshu= jiaoyiquanshu[1],fangwuyongtu= fangwuyongtu[1],fangwunianxian= fangwunianxian[1],
                                                                 chanquansuoshu= chanquansuoshu[1],lianjiabianhao=lianjiabianhao[1],
                                                                 
                                                                 lastUpdateDate = Sys.time()              
                      ))
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
  if(file.copy(path.rep.xq.DLs,paste0(substr(path.rep.xq.DLs,1,nchar(path.rep.xq.DLs)-4),"_bk_",as.integer(Sys.Date()),".csv"),overwrite = T)){
    write_csv(rep.xq.DLs,path.rep.xq.DLs)
  }
}

##########--level: xiaoqu->Rents
path.rep.xq.RTs <- paste0(rep,"/06_xiaoqu_Rents.csv")
rep.xq.RTs <- read_csv(path.rep.xq.RTs)
if(nrow(rep.xq.RTs) == 0 & length(names(rep.xq.RTs)) == 0){
  rep.xq.RTs <- data.frame(city = "", distrct = "", subDistrct = "", xiaoqu = "", link_xiaoqu = "", 
                           link_house = "",
                           
                           liulangcishu="",shangjiashijian="",fangyuanbianhao="",zujin="",fangyuanTags="",rentType="",huxin="",mianji="",
                           chaoxiang="",ruzhu="",zuqi="",kanfang="",louceng="",dianti="",chewei="",yongshui="",yongdian="",
                           ranqi="",cailuan="",peizhi="",
                           
                           lastUpdateDate = Sys.time())[0,]
}
#View(rep.xq.RTs)
#--process
process.xq.RTs.run <- TRUE
if(process.xq.RTs.run){
  ct_xq_GI <- nrow(rep.xq.GI)
  for(i in 1:ct_xq_GI){ #--循环小区###########################################################################
    rd <- rep.xq.GI[i,]
    print(paste0("district-",rd$distrct,"-xiaoqu-",rd$xiaoqu, "-",i))
    link_RTs <- gsub("http","https",rd$linkToRents)
    if(!is.na(link_RTs) & nchar(link_RTs) > 0){
      html_RTs <- uf_read_html(link_RTs)
      if(all(!is.na(html_RTs))){
        node.total <- tryCatch({html_RTs %>% html_nodes("span.content__title--hl")},
                               warnings = function(w){
                                 ;
                                 NA
                               },
                               error = function(e){
                                 ;
                                 NA
                               })
        if(all(!is.na(node.total)) & length(node.total) > 0){
          total_xiaoqus <- gsub("\\s","",node.total %>% html_text())
          if(!is.na(total_xiaoqus) & nchar(total_xiaoqus)>0){
            total_xiaoqus <- as.numeric(total_xiaoqus)
            num_of_single_page <- 30
            num_pages <- as.integer(total_xiaoqus/num_of_single_page) + 1
            for(j in 1:num_pages){#--循环页面###########################################################################
              print(paste0("district-",rd$distrct,"-xiaoqu-",rd$xiaoqu, "-",i,"-page-",j))
              link_gi <- gsub("city",attr(cities,"abbr")[which(cities == city)[1]] ,"https://city.zu.ke.com/zufang/")
              link_RT <- paste0(link_gi,"pg",j,gsub(link_gi,"",link_RTs)) 
              html_RT <- uf_read_html(link_RT)
              if(all(!is.na(html_RT))){
                
                node.houseList <- tryCatch({html_RT %>% html_nodes("div.content__list") %>% html_nodes("div.content__list--item") %>% html_nodes("a.content__list--item--aside")},
                                           warnings = function(w){
                                             ;
                                             NA
                                           },
                                           error = function(e){
                                             ;
                                             NA
                                           })
                if(all(!is.na(node.houseList)) & length(node.houseList) > 0){
                  houseList <- unique(node.houseList %>% html_attr("href"))
                  ct_houseList <- length(houseList)
                  for(n in 1:ct_houseList){ #--循环每一个房子###########################################################################
                    print(paste0("district-",rd$distrct,"-xiaoqu-",rd$xiaoqu, "-",i,"-page-",j,"-house-",n))
                    link_house <- paste0(gsub("/zufang/","",link_gi) ,houseList[n])
                    html_house <- uf_read_html(link_house)
                    
                    liulangcishu<-"";shangjiashijian<-"";fangyuanbianhao<-"";zujin<-"";fangyuanTags<-"";rentType<-"";huxin<-"";mianji<-"";
                    chaoxiang<-"";ruzhu<-"";zuqi<-"";kanfang<-"";louceng<-"";dianti<-"";chewei<-"";yongshui<-"";yongdian<-"";
                    ranqi<-"";cailuan<-"";peizhi<-""
                    
                    
                    if(all(!is.na(html_house))){
                      html_L4 <- html_house
                      if(!all(is.na(html_L4))){
                        #--开始读取该房屋的信息
                        
                        node_subtitle <- tryCatch({
                          html_L4 %>% html_nodes("div.content__subtitle")
                        },warnings = function(w){
                          ;
                          NA
                        },error = function(e){
                          ;
                          NA
                        })
                        if(!all(is.na(node_subtitle))){
                          #--浏览次数
                          liulangcishu <- tryCatch({
                            node_subtitle %>% html_node("i.hide") %>% html_text()
                          },warnings = function(w){
                            ;
                            NA
                          },error = function(e){
                            ;
                            NA
                          })
                          
                          #--房源上架时间
                          shangjiashijian <- tryCatch({
                            substr(node_subtitle %>% html_text(),regexpr("[1-2][0-9]{3}-[0-1][1-9]-[0-3][0-9]",node_subtitle %>% html_text()),regexpr("[1-2][0-9]{3}-[0-1][1-9]-[0-3][0-9]",node_subtitle %>% html_text())+9)  
                          },warnings = function(w){
                            ;
                            NA
                          },error = function(e){
                            ;
                            NA
                          })
                          
                          #--房源编号
                          fangyuanbianhao <- tryCatch({
                            gsub("房源编号","",node_subtitle %>% html_node("i.house_code") %>% html_text()) 
                          },warnings = function(w){
                            ;
                            NA
                          },error = function(e){
                            ;
                            NA
                          })
                          
                        }
                        
                        node_right <- tryCatch({
                          html_L4 %>% html_nodes("div.content__aside.fr")
                        },warnings = function(w){
                          ;
                          NA
                        },error = function(e){
                          ;
                          NA
                        })
                        if(!all(is.na(node_right))){
                          #--租金及支付方式
                          zujin <- tryCatch({
                            node_right %>% html_node("p.content__aside--title") %>% html_text()
                          },warnings = function(w){
                            ;
                            NA
                          },error = function(e){
                            ;
                            NA
                          })
                          
                          #--房源标签列表
                          fangyuanTags <- ""
                          node_fangyuanTags <- tryCatch({
                            node_right %>% html_nodes("p.content__aside--tags") %>% html_nodes("i")
                          },warnings = function(w){
                            ;
                            NA
                          },error = function(e){
                            ;
                            NA
                          })
                          if(!all(is.na(node_fangyuanTags))){
                            len_tages <- length(node_fangyuanTags)
                            if(len_tages > 0){
                              for(t in 1:len_tages){
                                fangyuanTags <- paste0(fangyuanTags,"|",node_fangyuanTags[t] %>% html_text())
                              }
                            }
                          }
                          
                          #--信息1
                          node_info1 <- tryCatch({
                            node_right %>% html_node("ul.content__aside__list") %>% html_node("p.content__article__table") %>% html_nodes("span")
                          },warnings = function(w){
                            ;
                            NA
                          },error = function(e){
                            ;
                            NA
                          })
                          if(!all(is.na(node_info1))){
                            #--租赁方式
                            rentType <- tryCatch({
                              node_info1[1] %>% html_text()
                            },warnings = function(w){
                              ;
                              NA
                            },error = function(e){
                              ;
                              NA
                            })
                            #--房源户型
                            huxin <- tryCatch({
                              node_info1[2] %>% html_text()
                            },warnings = function(w){
                              ;
                              NA
                            },error = function(e){
                              ;
                              NA
                            })
                            #--面积
                            mianji <- tryCatch({
                              node_info1[3] %>% html_text()
                            },warnings = function(w){
                              ;
                              NA
                            },error = function(e){
                              ;
                              NA
                            })
                            #--朝向
                            chaoxiang <- tryCatch({
                              node_info1[4] %>% html_text()
                            },warnings = function(w){
                              ;
                              NA
                            },error = function(e){
                              ;
                              NA
                            })
                          }
                        }
                        
                        node_basInfo <- tryCatch({
                          html_L4 %>% html_nodes("div.content__article__info") %>% html_nodes("ul") %>% html_nodes("li.fl.oneline") %>% html_text()
                        },warnings = function(w){
                          ;
                          NA
                        },error = function(e){
                          ;
                          NA
                        })
                        if(!all(is.na(node_basInfo))){
                          len_bis <- length(node_basInfo)
                          if(len_bis > 0){
                            ruzhu <- gsub("入住","",node_basInfo[which(regexpr("入住",node_basInfo)>0)[1]])
                            zuqi <- gsub("租期","",node_basInfo[which(regexpr("租期",node_basInfo)>0)[1]])
                            kanfang <- gsub("看房","",node_basInfo[which(regexpr("看房",node_basInfo)>0)[1]])
                            louceng <- gsub("楼层","",node_basInfo[which(regexpr("楼层",node_basInfo)>0)[1]])
                            dianti <- gsub("电梯","",node_basInfo[which(regexpr("电梯",node_basInfo)>0)[1]])
                            chewei <- gsub("车位","",node_basInfo[which(regexpr("车位",node_basInfo)>0)[1]])
                            yongshui <- gsub("用水","",node_basInfo[which(regexpr("用水",node_basInfo)>0)[1]])
                            yongdian <- gsub("用电","",node_basInfo[which(regexpr("用电",node_basInfo)>0)[1]])
                            ranqi <- gsub("燃气","",node_basInfo[which(regexpr("燃气",node_basInfo)>0)[1]])
                            cailuan <- gsub("采暖","",node_basInfo[which(regexpr("采暖",node_basInfo)>0)[1]])
                          }
                        }
                        
                        node_peizhi <- tryCatch({
                          html_L4 %>% html_nodes("ul.content__article__info2") %>% html_nodes("li.fl.oneline")
                        },warnings = function(w){
                          ;
                          NA
                        },error = function(e){
                          ;
                          NA
                        })
                        peizhi <- ""
                        if(!all(is.na(node_peizhi))){
                          len_peizi <- length(node_peizhi)
                          if(len_peizi>0){
                            for(q in 1:len_peizi){
                              class_pz <- node_peizhi[q] %>% html_attr("class")
                              if(class_pz != "fl oneline"){
                                if(regexpr("_no$",gsub(" ","",class_pz))<0){
                                  peizhi <- paste0(peizhi,"|",node_peizhi[q] %>% html_text())
                                }
                              }
                            }
                          }
                        }
                        
                      }
                    }
                    
                    #--加载房屋信息
                    if(1==1){
                      rep.xq.RTs <- rbind(rep.xq.RTs, data.frame(city = rd$city, distrct = rd$distrct, subDistrct = rd$subDistrct, 
                                                                 xiaoqu = rd$xiaoqu, link_xiaoqu = rd$link, 
                                                                 link_house = link_house,
                                                                 
                                                                 liulangcishu=liulangcishu[1],shangjiashijian=shangjiashijian[1],fangyuanbianhao=fangyuanbianhao[1],zujin=zujin[1],
                                                                 fangyuanTags=fangyuanTags[1],rentType=rentType[1],huxin=huxin[1],mianji=mianji[1],
                                                                 chaoxiang=chaoxiang[1],ruzhu=ruzhu[1],zuqi=zuqi[1],kanfang=kanfang[1],louceng=louceng[1],
                                                                 dianti=dianti[1],chewei=chewei[1],yongshui=yongshui[1],yongdian=yongdian[1],
                                                                 ranqi=ranqi[1],cailuan=cailuan[1],peizhi=peizhi[1],
                                                                 
                                                                 lastUpdateDate = Sys.time()              
                      ))
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
  if(file.copy(path.rep.xq.RTs,paste0(substr(path.rep.xq.RTs,1,nchar(path.rep.xq.RTs)-4),"_bk_",as.integer(Sys.Date()),".csv"),overwrite = T)){
    write_csv(rep.xq.RTs,path.rep.xq.RTs)
  }
}


########################################################Feature Engineering########################################################
#--广州经纬度范围: 纬22°26 ′～ 23°56 ′、东经112°57 ′～ 114°03 '
houses.gz <- uf_xlsxDataImport("houses_gz.xlsx","houses_Org")
houses.gz.bk <- houses.gz
View(houses.gz); dim(houses.gz); 
str(houses.gz); 
ct_houses <- nrow(houses.gz)

##################-------------------district
district <- unique(houses.gz$district)
attr(district,"Eng") <- c("Tianhe","Yuexiu","Liwan","Haizhu","Fanyu","Baiyun","Huangpu","Conghua","Zengcheng","Huadu","Lansha")
for(i in 1:length(district)) {houses.gz[houses.gz$district==district[i],"district"] <- attr(district,"Eng")[i]}

##################-------------------unit price
houses.gz$unit_price <- as.numeric(houses.gz$unit_price) 
uf_NumberList_Summary(houses.gz$unit_price)
ggplot(data = houses.gz,aes(x = unit_price)) + geom_density()

##################-------------------huxin
#--derive to '# of bedroom', '# of livingroom', '# of kitchen', '# of washroom'
any(is.na(houses.gz$huxin))  #--FALSE
unique(houses.gz$huxin);     #patern: xx室yy厅zz厨tt卫

houses.gz$bedroom <- 0
houses.gz$livingroom <- 0
houses.gz$kitchen <- 0
houses.gz$washroom <- 0

for(i in 1:ct_houses){
  print(i)
  huxin <- gsub(" ","",houses.gz$huxin[i])  
  if(huxin==""){
    houses.gz$bedroom[i] <- 1
  }else if(regexpr("[0-9]{1,2}室[0-9]{1,2}厅[0-9]{1,2}厨[0-9]{1,2}卫",huxin) > 0){
    p1 <- regexpr("室",huxin)
    p2 <- regexpr("厅",huxin)
    p3 <- regexpr("厨",huxin)
    p4 <- regexpr("卫",huxin)
    
    houses.gz$bedroom[i] <- as.integer(substr(huxin,1,p1-1)) 
    houses.gz$livingroom[i] <- as.integer(substr(huxin,p1+1,p2-1))
    houses.gz$kitchen[i] <- as.integer(substr(huxin,p2+1,p3-1))
    houses.gz$washroom[i] <- as.integer(substr(huxin,p3+1,p4-1))
  }else{
    houses.gz$bedroom[i] <- NA
    houses.gz$livingroom[i] <- NA
    houses.gz$kitchen[i] <- NA
    houses.gz$washroom[i] <- NA
  }
  
}
any(is.na(houses.gz$bedroom))       #--FALSE

##################-------------------huxinjiegou
unique(houses.gz$huxinjiegou)
table(houses.gz.bk$huxinjiegou); houses.gz[houses.gz$huxinjiegou == "暂无数据","huxinjiegou"] <- ""
for (i in 1:ct_houses) {
  if(houses.gz[i,"huxinjiegou"]==""){
    jiegous <- houses.gz[houses.gz$xiaoqu == houses.gz[i,"xiaoqu"],"huxinjiegou"]
    if(length(jiegous)==1 | all(jiegous=="")){
      houses.gz[i,"huxinjiegou"]<-"平层"
    }else{
      houses.gz[i,"huxinjiegou"]<-jiegous[jiegous!=""][1]
    }
  }
}
houses.gz$huxinjiegou[houses.gz$huxinjiegou=="Villa"] <- "Double"

fangwuyongtu <- unique(houses.gz$huxinjiegou)
attr(huxinjiegou,"Eng") <- c("Flat","Multi","Double")
for(i in 1:length(huxinjiegou)) {houses.gz[houses.gz$huxinjiegou==huxinjiegou[i],"huxinjiegou"] <- attr(huxinjiegou,"Eng")[i]}


##################-------------------louceng
#--derive 'total # of floor', 'floorPosition'
#--patern1: [低|中|高]楼层/共[xxx]层
#--patern2: 地下室
#--patern3: [独栋|联排|双拼]/共[xxx]层
unique(houses.gz$louceng); any(houses.gz$louceng=="")

houses.gz$totalFloor <- NA
houses.gz$floorPosition <- NA
patern1 <- "[低|中|高]楼层/共[0-9]{1,3}层"
patern2 <- "地下室"
patern3 <- "[独栋|联排|双拼]/共[0-9]{1,3}层"
for(i in 1:ct_houses){
  print(i)
  louceng <- gsub(" ","",houses.gz[i,"louceng"])  
  
  reg1 <- regexpr(patern1,louceng)
  reg2 <- regexpr(patern2,louceng)
  reg3 <- regexpr(patern3,louceng)
  
  if(any(c(reg1,reg2,reg3))){
    if(reg1>0){
      houses.gz$floorPosition[i] <- substr(louceng,1,1)
      houses.gz$totalFloor[i] <- as.numeric(substr(louceng,regexpr("[0-9]",louceng), nchar(louceng)-1))
    }else if(reg2 > 0){
      houses.gz$floorPosition[i] <- "低"
      houses.gz$totalFloor[i] <- 0
    }else if(reg3 > 0){
      houses.gz$floorPosition[i] <- "中"
      houses.gz$totalFloor[i] <- as.numeric(substr(louceng,regexpr("[0-9]",louceng), nchar(louceng)-1))
      houses.gz$huxinjiegou[i] <- "Villa"
    }
  }
  
}

any(is.na(houses.gz$totalFloor))      #--FALSE
any(is.na(houses.gz$floorPosition))   #--FALSE

unique(houses.gz$floorPosition)
floorPosition <- unique(houses.gz$floorPosition)
attr(floorPosition,"Eng") <- c("Mid","Low","High")
for(i in 1:length(floorPosition)) {houses.gz[houses.gz$floorPosition==floorPosition[i],"floorPosition"] <- attr(floorPosition,"Eng")[i]}


##################-------------------chaoxiang
unique(houses.gz$chaoxiang)
houses.gz$chaoxiang[houses.gz$chaoxiang=="暂无数据"] <- "南"

for (i in 1:ct_houses) {
  if(regexpr(" ",houses.gz$chaoxiang[i])>0){houses.gz$chaoxiang[i] <- substr(houses.gz$chaoxiang[i],1,regexpr(" ",houses.gz$chaoxiang[i])-1)}
}

table(houses.gz$chaoxiang)

chaoxiang <- unique(houses.gz$chaoxiang)
attr(chaoxiang,"Eng") <- c("W","ES","E","N","S","WN","EN","WS")
for(i in 1:length(chaoxiang)) {houses.gz[houses.gz$chaoxiang==chaoxiang[i],"chaoxiang"] <- attr(chaoxiang,"Eng")[i]}

##################-------------------zhuangxiuqingkuang
unique(houses.gz.bk$zhuangxiuqingkuang); table(houses.gz.bk$zhuangxiuqingkuang)
unique(houses.gz$zhuangxiuqingkuang); table(houses.gz$zhuangxiuqingkuang)
houses.gz$zhuangxiuqingkuang[houses.gz$zhuangxiuqingkuang=="Unknown"] <- NA

zhuangxiuqingkuang <- unique(houses.gz$zhuangxiuqingkuang)
attr(zhuangxiuqingkuang,"Eng") <- c("Jing","Jian","Unknown","Mao")
for(i in 1:length(zhuangxiuqingkuang)) {houses.gz[houses.gz$zhuangxiuqingkuang==zhuangxiuqingkuang[i],"zhuangxiuqingkuang"] <- attr(zhuangxiuqingkuang,"Eng")[i]}

##################-------------------mianji/taineimianji
#--derive gongtanbili
houses.gz$gongtanbili <- NA

houses.gz$mianji <- as.numeric(houses.gz$mianji) 
uf_NumberList_Summary(houses.gz$gongtanbili)
ggplot(data = houses.gz,aes(x = mianji)) + geom_density()

houses.gz$taineimianji <- as.numeric(houses.gz$taineimianji) 
uf_NumberList_Summary(houses.gz$taineimianji)
temp1 <- houses.gz %>% filter(!is.na(taineimianji)) %>% select(taineimianji,mianji)
temp2 <- temp1$taineimianji / temp1$mianji
temp2 <- temp2[temp2<=1]
temp3 <- 1 - mean(temp2)

houses.gz$gongtanbili <- ifelse(is.na(houses.gz$taineimianji),temp3,1 - houses.gz$taineimianji / houses.gz$mianji)
range(houses.gz$gongtanbili)
houses.gz$gongtanbili <- ifelse(houses.gz$gongtanbili>1 | houses.gz$gongtanbili<0,temp3,houses.gz$gongtanbili)
ggplot(data = houses.gz,aes(x = gongtanbili)) + geom_density()


##################-------------------Nianfen
unique(houses.gz.bk$Nianfen); table(houses.gz.bk$Nianfen)
houses.gz$Nianfen[houses.gz$Nianfen=="未知"]<-NA
houses.gz$Nianfen <- as.numeric(houses.gz$Nianfen)

for(i in 1:ct_houses){
  if(is.na(houses.gz$Nianfen[i])){
    Nianfen <- houses.gz[houses.gz$xiaoqu == houses.gz$xiaoqu[i],"Nianfen"]
    if(!all(is.na(Nianfen))){
      houses.gz$Nianfen[i] <- Nianfen[!is.na(Nianfen)][1]
    }
  }
}



##################-------------------is_dianti/tihubili
#-derive "diantishu", "hushu"
unique(houses.gz$is_dianti);table(houses.gz$is_dianti)
houses.gz$is_dianti[houses.gz$is_dianti=="暂无数据"|houses.gz$is_dianti==""] <- NA 

unique(houses.gz$tihubili)
houses.gz$tihubili[houses.gz$tihubili == ""] <- NA
houses.gz$is_dianti <- ifelse(!is.na(houses.gz$tihubili),"Yes","No")


#--patern: .{1,2}梯.{1,3}户
uf_trans_chineseNum <- function(t_shu){
  if(nchar(t_shu)==1){
    t_shu <- attr(ch_shu,"Eng")[which(t_shu==ch_shu)]
  }else if(nchar(t_shu)==2){
    t_shu <- 10 + attr(ch_shu,"Eng")[which(substr(t_shu,2,2)==ch_shu)]
  }else if(nchar(t_shu)==3){
    t_shu <- 10*attr(ch_shu,"Eng")[which(substr(t_shu,1,1)==ch_shu)] + attr(ch_shu,"Eng")[which(substr(t_shu,3,3)==ch_shu)]
  }else if(nchar(t_shu)==4){
    t_shu <- 100*attr(ch_shu,"Eng")[which(substr(t_shu,1,1)==ch_shu)] + attr(ch_shu,"Eng")[which(substr(t_shu,4,4)==ch_shu)]
  }else if(nchar(t_shu)==5){
    t_shu <- 100*attr(ch_shu,"Eng")[which(substr(t_shu,1,1)==ch_shu)] + 10*attr(ch_shu,"Eng")[which(substr(t_shu,3,3)==ch_shu)] + attr(ch_shu,"Eng")[which(substr(t_shu,5,5)==ch_shu)]
  }else{
    t_shu <- NA
  }
  return(t_shu)
}
ch_shu <- c("一","二","两","三","四","五","六","七","八","九","十")
attr(ch_shu,"Eng") <- c(1,2,2,3,4,5,6,7,8,9,10)

houses.gz$diantishu <- NA
houses.gz$hushu <- NA
for (i in 1:ct_houses) {
  print(i)
  tihubili <- houses.gz$tihubili[i]
  if(!is.na(tihubili)){
    if(regexpr(".{1,2}梯.{1,3}户",tihubili)){
      houses.gz$diantishu[i] <- uf_trans_chineseNum(substr(tihubili,1,regexpr("梯",tihubili)-1))
      houses.gz$hushu[i] <- uf_trans_chineseNum(substr(tihubili,regexpr("梯",tihubili)+1,nchar(tihubili)-1))
    }
  }
}

View(houses.gz %>% select(is_dianti,tihubili,diantishu,hushu))

houses.gz$hushu[is.na(houses.gz$hushu)] <- 1
table(houses.gz$hushu)

##################-------------------chanquannianxian
unique(houses.gz.bk$chanquannianxian);table(houses.gz.bk$chanquannianxian)
houses.gz$chanquannianxian[houses.gz$chanquannianxian==""] <- "70年"

chanquannianxian <- unique(houses.gz$chanquannianxian)
attr(chanquannianxian,"Eng") <- c("70","50","40")
for(i in 1:length(chanquannianxian)) {houses.gz[houses.gz$chanquannianxian==chanquannianxian[i],"chanquannianxian"] <- attr(chanquannianxian,"Eng")[i]}



##################-------------------jiaoyiquanshu
unique(houses.gz$jiaoyiquanshu);table(houses.gz$jiaoyiquanshu)
unique(houses.gz.bk$jiaoyiquanshu);table(houses.gz.bk$jiaoyiquanshu)


jiaoyiquanshu <- unique(houses.gz$jiaoyiquanshu)
attr(jiaoyiquanshu,"Eng") <- c("SPF","SC","FGF","AZF")
for(i in 1:length(jiaoyiquanshu)) {houses.gz[houses.gz$jiaoyiquanshu==jiaoyiquanshu[i],"jiaoyiquanshu"] <- attr(jiaoyiquanshu,"Eng")[i]}

##################-------------------fangwuyongtu
unique(houses.gz$fangwuyongtu);table(houses.gz$fangwuyongtu)
unique(houses.gz.bk$fangwuyongtu);table(houses.gz.bk$fangwuyongtu)

fangwuyongtu <- unique(houses.gz$fangwuyongtu)
attr(fangwuyongtu,"Eng") <- c("PTZZ","SZLY","CK","BS")
for(i in 1:length(fangwuyongtu)) {houses.gz[houses.gz$fangwuyongtu==fangwuyongtu[i],"fangwuyongtu"] <- attr(fangwuyongtu,"Eng")[i]}

##################-------------------jianzhujiegou
unique(houses.gz$jianzhujiegou);table(houses.gz$jianzhujiegou)
unique(houses.gz.bk$jianzhujiegou);table(houses.gz.bk$jianzhujiegou)
houses.gz$jianzhujiegou[houses.gz$jianzhujiegou=="未知结构"] <- "钢混结构"

jianzhujiegou <- unique(houses.gz$jianzhujiegou)
attr(jianzhujiegou,"Eng") <- c("GH","ZH","ZM")
for(i in 1:length(jianzhujiegou)) {houses.gz[houses.gz$jianzhujiegou==jianzhujiegou[i],"jianzhujiegou"] <- attr(jianzhujiegou,"Eng")[i]}


##################-------------------fangwunianxian
unique(houses.gz$fangwunianxian);table(houses.gz$fangwunianxian)
unique(houses.gz.bk$fangwunianxian);table(houses.gz.bk$fangwunianxian)
houses.gz$fangwunianxian[houses.gz$fangwunianxian=="Unknown"] <- NA
fangwunianxian <- unique(houses.gz$fangwunianxian)
attr(fangwunianxian,"Eng") <- c("Less2","Greater5","Unknown","Greater2")
for(i in 1:length(fangwunianxian)) {houses.gz[houses.gz$fangwunianxian==fangwunianxian[i],"fangwunianxian"] <- attr(fangwunianxian,"Eng")[i]}

for (i in 1:ct_houses) {
  print(i)
  fangwunianxian <- houses.gz$fangwunianxian[i]
  if(fangwunianxian == "Unknown"){
    if(!is.na(houses.gz$Nianfen[i])){
      YearDiff <- as.numeric(substr(houses.gz$guapaishijian[i],1,4)) - as.numeric(houses.gz$Nianfen[i])
      houses.gz$fangwunianxian[i] <- ifelse(YearDiff >= 5, 
                                            "Greater5", 
                                            ifelse(YearDiff <= 2, 
                                                   "Less2",
                                                   ifelse(YearDiff < 5 & YearDiff > 2,
                                                          "Greater2",
                                                          "Unknown")))
    }
  }
}


##################-------------------chanquansuoshu
unique(houses.gz$chanquansuoshu);table(houses.gz$chanquansuoshu)
unique(houses.gz.bk$chanquansuoshu);table(houses.gz.bk$chanquansuoshu)
houses.gz$chanquansuoshu[houses.gz$chanquansuoshu=="Unknown"]<-NA
chanquansuoshu <- unique(houses.gz$chanquansuoshu)
attr(chanquansuoshu,"Eng") <- c("FGY","GY","Unknown")
for(i in 1:length(chanquansuoshu)) {houses.gz[houses.gz$chanquansuoshu==chanquansuoshu[i],"chanquansuoshu"] <- attr(chanquansuoshu,"Eng")[i]}


##################-------------------diyaxinxi
for (i in 1:ct_houses) {houses.gz$diyaxinxi[i] <- ifelse(houses.gz$diyaxinxi[i]=="暂无数据",houses.gz$diyaxinxi[i],substr(houses.gz$diyaxinxi[i],1,3))}
unique(houses.gz$diyaxinxi);table(houses.gz$diyaxinxi)
unique(houses.gz.bk$diyaxinxi);table(houses.gz.bk$diyaxinxi)

diyaxinxi <- unique(houses.gz$diyaxinxi)
attr(diyaxinxi,"Eng") <- c("YDY","WDY","Unknown")
for(i in 1:length(diyaxinxi)) {houses.gz[houses.gz$diyaxinxi==diyaxinxi[i],"diyaxinxi"] <- attr(diyaxinxi,"Eng")[i]}


##################-------------------fangbenbeijian
for (i in 1:ct_houses) {houses.gz$fangbenbeijian[i]<-ifelse(houses.gz$fangbenbeijian[i]=="已上传房本照片","YSC","WSC")}
unique(houses.gz$fangbenbeijian);table(houses.gz$fangbenbeijian)

##################-------------------jianzhuleixing
unique(houses.gz$jianzhuleixing);table(houses.gz$jianzhuleixing)
unique(houses.gz.bk$jianzhuleixing);table(houses.gz.bk$jianzhuleixing)
houses.gz$jianzhuleixing[houses.gz$jianzhuleixing=="暂无数据" | houses.gz$jianzhuleixing==""] <- "塔楼"

jianzhuleixing <- unique(houses.gz$jianzhuleixing)
attr(jianzhuleixing,"Eng") <- c("TL","BL","BTJH","PF")
for(i in 1:length(jianzhuleixing)) {houses.gz[houses.gz$jianzhuleixing==jianzhuleixing[i],"jianzhuleixing"] <- attr(jianzhuleixing,"Eng")[i]}

##################-------------------lat/long
#纬22°26 ′～ 23°56 ′、东经112°57 ′～ 114°03 '
houses.gz$lat <- ifelse(houses.gz$lat=="",0,as.numeric(houses.gz$lat))
houses.gz$long <- ifelse(houses.gz$long=="",0,as.numeric(houses.gz$long))
houses.gz$lat <- ifelse(houses.gz$lat>22 & houses.gz$lat<24, houses.gz$lat, 0)
houses.gz$long <- ifelse(houses.gz$long>112 & houses.gz$long<115, houses.gz$long, 0)


#--derive
#--小区的平均价格
#--小区x公里范围的平均价格
houses.gz$xqjj <- NA
houses.gz$xq_x_jj <- NA

range_x <- 3
for (i in 1:ct_houses) {
  print(i)
  h <- houses.gz[i,]
  
  if(is.na(houses.gz$xqjj[i])){
    xqjj <- round(mean(houses.gz[houses.gz$xiaoqu==h$xiaoqu,"unit_price"]))
    houses.gz[houses.gz$xiaoqu==h$xiaoqu,"xqjj"] <- xqjj
    
    lat <- houses.gz$lat[i]
    long <- houses.gz$long[i]
    if(lat==0 | long==0){houses.gz[houses.gz$xiaoqu==h$xiaoqu,"xq_x_jj"]<-xqjj}else{
      temp_geo <- distinct(houses.gz[houses.gz$xiaoqu!=h$xiaoqu & houses.gz$lat != 0 & houses.gz$long != 0, c("xiaoqu","lat","long")])
      temp_geo$distince <- NA
      ct_temp_geo <- nrow(temp_geo)
      for(j in 1:ct_temp_geo){
        temp_geo$distince[j] <- distm(c(long,lat), c(temp_geo$long[j],temp_geo$lat[j]), fun = distHaversine)
      }
      xq_x_jj <- round(mean(houses.gz[houses.gz$xiaoqu %in% temp_geo[temp_geo$distince <= range_x*1000,"xiaoqu"],"unit_price"] ))
      houses.gz[houses.gz$xiaoqu==h$xiaoqu,"xq_x_jj"] <- xq_x_jj
    }
  }
}

uf_NumberList_Summary(houses.gz$xqjj)
uf_NumberList_Summary(houses.gz$xq_x_jj)


houses.gz.FEed.all <- houses.gz
houses.gz.FEed.selected <- houses.gz %>% select(url,city,district,bedroom,livingroom,kitchen,washroom,huxinjiegou,totalFloor,floorPosition,
                                                chaoxiang,zhuangxiuqingkuang,mianji,gongtanbili,Nianfen,diantishu,
                                                hushu,chanquannianxian,jiaoyiquanshu,fangwuyongtu,jianzhujiegou,fangwunianxian,
                                                chanquansuoshu,diyaxinxi,fangbenbeijian,jianzhuleixing,xqjj,xq_x_jj,unit_price)

View(houses.gz.FEed.selected)

uf_xlsxDataExport(houses.gz.FEed.all,"houses_gz_FEed.xlsx",spreedsheet = "houses_FEed_All")
uf_xlsxDataExport(houses.gz.FEed.selected,"houses_gz_FEed.xlsx",spreedsheet = "houses_FEed_Selected")


########################################################FEed Data########################################################
#houses.gz <- uf_xlsxDataImport("houses_gz.xlsx","houses_FEed_Selected")
#write_csv(houses.gz,"houses_gz.csv")
houses.gz <- read_csv("houses_gz.csv")
View(houses.gz); dim(houses.gz)
houses.gz$url <- c(1:nrow(houses.gz)); 
houses.gz <- houses.gz %>% select(c(-1,-2))
houses.gz <- cbind(unit_price = houses.gz$unit_price, houses.gz[,-length(colnames(houses.gz))])
#houses.gz <- houses.gz %>% select(-jiaoyiquanshu,-jianzhujiegou)
dim(houses.gz); 
str(houses.gz); 
ct_houses <- nrow(houses.gz)

##############--label: unit_price
uf_NumberList_Summary(houses.gz$unit_price)
ggplot(data = houses.gz, aes(x = unit_price)) + geom_density() + scale_x_continuous(limits = c(0,80000)) 
ggplot(data = houses.gz, aes(x = unit_price)) + geom_histogram(binwidth = 10000) + scale_x_continuous(limits = c(0,80000)) 
ggplot(data = houses.gz,aes(x = 1, y = unit_price)) + geom_boxplot() + scale_y_continuous(limits = c(0,80000)) 

#--Note: if remove data which with unit_price > 80000, the distribution of unit_price will be more like to normal
#houses.gz %>% filter(unit_price < 80000)

##############--label: district
d.distrct <- aggregate(unit_price~district, data = houses.gz,FUN = function(x){c(mean(x),round(length(x),0))})
d.distrct <- data.frame(district = d.distrct$district, avg_price = d.distrct$unit_price[,1], ct = d.distrct$unit_price[,2])
ggplot(data = d.distrct,aes(x = reorder(district,avg_price), y = avg_price)) + geom_bar(stat = "identity") 

#--note: don't need to remove any data in specific dictrict


##############--bedroom, livingroom, kitchen, washroom, mianji
table(houses.gz$bedroom)
table(houses.gz$livingroom)
table(houses.gz$kitchen)
table(houses.gz$washroom)





########################################################Data Split########################################################
options(na.action="na.pass")
#--split into train/val/test
set.seed(1111)
RndSeq <- sample(1:ct_houses,ct_houses,replace = F)
trainRate <- 0.7
TrainSeq <- RndSeq[1:round(ct_houses * trainRate)]
TestSeq <- RndSeq[-c(1:round(ct_houses * trainRate))]

train <- houses.gz[TrainSeq,]
test <- houses.gz[TestSeq,]


########################################################XGBoost########################################################
spM_train <- sparse.model.matrix(unit_price~.-1, data = train); nrow(spM_train); spM_train@Dim; spM_train@Dimnames[[2]]
spM_test  <- sparse.model.matrix(unit_price~.-1, data = test);  nrow(spM_test); spM_test@Dim; spM_test@Dimnames[[2]]
dtrain <- xgb.DMatrix(data = spM_train, label = train$unit_price); 
dtest  <- xgb.DMatrix(data = spM_test,  label = test$unit_price);  

#--设置初始超参值
param0.xgb <- list(objective = "reg:linear",
                   eval_metric = "rmse",
                   
                   eta = 0.05,
                   
                   gamma = 0,
                   max_depth = 6,
                   min_child_weight = 5,
                   
                   subsample = 0.8,
                   colsample_bytree = 0.8,
                   colsample_bylevel = 1,
                   max_delta_step = 0,
                   
                   lambda = 1,
                   alpha = 1,
                   
                   booster = "gbtree",
                   nthread = 2, 
                   seed = 1234)


fit.xgb.init <- xgb.train(param0.xgb, 
                          dtrain, 
                          missing = NA,  
                          nrounds = 2000,
                          watchlist = list(train = dtrain, val = dtest), 
                          print_every_n = 1, 
                          early_stopping_rounds = 50)

fit.xgb.init
fit.xgb.init$best_iteration; fit.xgb.init$best_ntreelimit; fit.xgb.init$best_score; fit.xgb.init$niter; 

iter.error <-  data.frame(iter = rep(1:fit.xgb.init$niter,time=2),
                          error_set = rep(c("train","val"),each=fit.xgb.init$niter),
                          error_p0 = c(fit.xgb.init$evaluation_log$train_rmse,fit.xgb.init$evaluation_log$val_rmse))
ggplot(data = iter.error[iter.error$iter>100,], aes(x = iter, y = error_p0, colour = error_set)) + 
  geom_line(size = 1) 

impt.xgb.init <- xgb.importance(feature_names = fit.xgb.init$feature_names, model = fit.xgb.init)
xgb.ggplot.importance(importance_matrix = impt.xgb.init,top_n = min(30,fit.xgb.init$nfeatures))
xgb.ggplot.deepness(model = fit.xgb.init)

#--交叉验证
fit.xgb.cv <- xgb.cv(param0.xgb,
                     dtrain, 
                     missing = NA,  
                     nrounds = 2000,
                     nfold = 10,
                     prediction = TRUE,   #return the prediction using the final model 
                     showsd = TRUE,       #standard deviation of loss across folds
                     #stratified = TRUE,   #sample is unbalanced; use stratified sampling(分层抽样)
                     verbose = TRUE,
                     print_every_n = 5, 
                     early_stopping_rounds = 100
)
fit.xgb.cv
fit.xgb.cv$best_iteration; fit.xgb.cv$best_ntreelimit; fit.xgb.cv$best_score; fit.xgb.cv$niter; 
cv.error <-  data.frame(iter = rep(1:fit.xgb.cv$niter,time=3),
                        error_set = rep(c("train","val","diff"),each=fit.xgb.cv$niter),
                        error_p0 = c(fit.xgb.cv$evaluation_log$train_rmse_mean,fit.xgb.cv$evaluation_log$test_rmse_mean,abs(fit.xgb.cv$evaluation_log$train_rmse_mean-fit.xgb.cv$evaluation_log$test_rmse_mean)),
                        error_sd = c(rep(0,fit.xgb.cv$niter),fit.xgb.cv$evaluation_log$test_rmse_std,rep(0,fit.xgb.cv$niter))
)

inter_bais_variance <- which.min(abs(cv.error[cv.error$error_set == "train","error_p0"] - cv.error[cv.error$error_set == "diff","error_p0"]))
ggplot(data = cv.error, aes(x = iter, y = error_p0, colour = error_set)) + 
  geom_line(size = 1) + 
  #geom_errorbar(aes(ymin=error_p0-error_sd, ymax=error_p0+error_sd), colour="light green", width=.1) +
  geom_ribbon(data = cv.error[cv.error$error_set == "val",],aes(ymin=error_p0-error_sd,ymax=error_p0+error_sd), fill="light green", alpha="0.5") + 
  geom_vline(xintercept = inter_bais_variance, colour = "black", linetype = 2) +
  geom_hline(yintercept = c(cv.error[cv.error$error_set == "train",]$error_p0[inter_bais_variance],
                            cv.error[cv.error$error_set == "val",]$error_p0[inter_bais_variance]),
             colour = "black", linetype = 2) +
  geom_text(x = inter_bais_variance, y = 0, label = inter_bais_variance, size = 4, vjust = 2, hjust = -.2) + 
  geom_text(x=0,y=cv.error[cv.error$error_set == "train",]$error_p0[inter_bais_variance], label = cv.error[cv.error$error_set == "train",]$error_p0[inter_bais_variance], size = 4, vjust = 0, hjust = 0)+
  geom_text(x=0,y=cv.error[cv.error$error_set == "val",]$error_p0[inter_bais_variance], label = cv.error[cv.error$error_set == "val",]$error_p0[inter_bais_variance], size = 4, vjust = 0, hjust = 0)+
  theme_bw()


#--残差探索
pred <- predict(fit.xgb.init, dtest, ntreelimit = 500)
act <- test$unit_price
resid <- pred-act
ds.resid <- data.frame(pred=pred,act=act,resid=resid)
ggplot(data = ds.resid, aes(x = act,y=pred))+geom_point()
ggplot(data = ds.resid, aes(x = act,y=resid))+geom_point() + geom_smooth(method = "lm", se = T, colour = alpha("red",0.5), na.rm = T, size = 1)
ggplot(data = ds.resid, aes(x = resid))+geom_density()



########################################################OLS########################################################
































































































