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
for(j in 1:ct_houses){
  address <- houses$xiaoqu[j]
  if(houses$lat[j] == "" & houses$long[j] == ""){
    latlong <- baidu.api.geocoder(address,city="成都")
    if(latlong != ""){
      houses[houses$xiaoqu == address,c("lat")] <- latlong[1]
      houses[houses$xiaoqu == address,c("long")] <- latlong[2]
    }
  }
}


#View(houses)
uf_xlsxDataExport(houses,"houses_gz.xlsx",spreedsheet = "houses")

























