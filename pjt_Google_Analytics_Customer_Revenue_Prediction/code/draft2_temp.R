getwd()
options(warn=-1)
library(tidyverse) 
library(stringr)
library(jsonlite)
library(lubridate)
library(Matrix)
set.seed(0)
uf_NumberList_Summary <- function(numx){
  rlt <- rep(0,8)
  names(rlt) <- c("mean","sd","Min","Q1","Median","Q3","Max","IQR")
  if(is.numeric(numx)){
    numx <- numx[!is.na(numx)]
    #数据集的中心趋势度量 - 平均???
    rlt[1] <- mean(numx)
    #数据集的离散程度 - 标准???
    rlt[2] <- sd(numx)
    #五数概括 - Minimum, Q1, Median, Q3, Maximum
    rlt[3:7] <- fivenum(numx)
    #四分位数极差（IQR???= Q3 ??? Q1
    rlt[8] <- rlt[6] - rlt[4]
  }
  return(rlt)
}
############################################################All data load############################################################
all <- read_csv("./data/all.csv")

dim(all)    #1708337  36
str(all)
names(all)
ct_all <- nrow(all)
head(all)

uf_NumberList_Summary(all$tt_transactionRevenue)
typeof(all$tt_transactionRevenue)
all_bk <- all
#all <- all_bk
############################################################FE&EDA############################################################
unkown_value <- c("not available in demo dataset", "(not provided)","(not set)", "<NA>", "unknown.unknown",  "(none)")
uf_any_na <- function(x){
  if(!all(class(x) == c("tbl_df","tbl","data.frame"))){
    x <- as.tibble(x)
  }
  x$is_bad <- ifelse((x[[1]] %in% unkown_value) | is.na(x[[1]]), "Y", "N") 
  
  return(x %>% group_by(is_bad) %>% summarise(n = n()))
}


###################################variable - tt_transactionRevenue
all$tt_transactionRevenue2 <- round(log(all$tt_transactionRevenue + 1),1)
ggplot(data = all%>%filter(flag==1&tt_transactionRevenue2>0), aes(x=tt_transactionRevenue2)) + geom_density()
all %>% filter(flag == 1) %>% select(tt_transactionRevenue2) %>% mutate(is_Tran = ifelse(tt_transactionRevenue2>0,1,0)) %>%
  group_by(is_Tran) %>% summarise(n = n())


###################################variable - channelGrouping 
##nothing
uf_any_na(all$channelGrouping) 
table(all$channelGrouping)
View(all %>% filter(flag == 1) %>% group_by(channelGrouping) %>% 
       summarise(avg = mean(tt_transactionRevenue2), n = n()) %>% arrange(avg))


###################################variable - date, visitNumber, visitStartTime
range(all$visitNumber)
View(all %>% select(visitNumber) %>% group_by(visitNumber) %>% summarise(n=n()))  #非常重要

all$visitStartTime <- as.POSIXct(all$visitStartTime, tz="UTC", origin='1970-01-01')
typeof(all$visitStartTime)

all$date <- ymd(as.Date(all$visitStartTime))

all <- 
  all %>% 
  mutate(dateInt = as.integer(date),
         year = year(date),
         month = month(date),
         day = day(date),
         hour = hour(visitStartTime),
         weekday = weekdays(date))


#interal between this time and last time
all$last_time <- all$visitStartTime
all_tp1 <- all %>% select(fullVisitorId,visitNumber,visitStartTime)
v_l <- all_tp1$visitNumber
#all$last_time[1:10] - all$visitStartTime[1:10]
startT <- Sys.time()
for(i in 1:ct_all){
  print(i)
  if(v_l[i] != 1){
    crt <- all_tp1[i,]
    crt_hist <- all_tp1 %>% filter(fullVisitorId == crt$fullVisitorId)
    
    if(crt$visitNumber > min(crt_hist$visitNumber)){
      diff_list <- crt$visitNumber - crt_hist$visitNumber 
      diff_list[diff_list<1] <- NA
      all[i,"last_time"] <- crt_hist[which.min(diff_list),"visitStartTime"]
    }
  }
  
}
endT <- Sys.time()
endT - startT

all$time_last <- round((all$visitStartTime - all$last_time) / 3600, 0)
range(all$time_last)
range(all$time_last[all$visitNumber == 1])
all$time_last <- ifelse(all$time_last < 0, 0, all$time_last)


#visitNum duplication
dup_viNum <-  all %>% select(fullVisitorId,visitNumber) %>% group_by(fullVisitorId,visitNumber) %>% summarise(n=n())
nrow(dup_viNum)
nrow(dup_viNum %>% filter(n > 1)) #5092
dup_users <- all %>% filter(fullVisitorId %in% dup_viNum$fullVisitorId[dup_viNum$n>1]) %>% arrange(fullVisitorId,visitNumber)
nrow(dup_users)



###################################variable - fullVisitorId,sessionId,visitId
View(all %>% filter(fullVisitorId == "8675363628183181565") %>% arrange(visitNumber) %>% select(fullVisitorId, visitNumber,date, hour))


###################################variable - dvc_*
# dvc_browser
uf_any_na(all$dvc_browser)   #check if there any unknown/na/null value
all$dvc_browser[(all$dvc_browser %in% unkown_value) | is.na(all$dvc_browser)] <- NA    #if yes, fill up with NA

View(by_dvc_browser <- 
  all %>% select(dvc_browser,tt_transactionRevenue2) %>% group_by(dvc_browser) %>% 
  summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))

all$dvc_browser[(all$dvc_browser %in% (by_dvc_browser %>% filter(avg <= 0, !is.na(dvc_browser)))$dvc_browser) |  #deal vith avg = 0
                  (all$dvc_browser %in% by_dvc_browser$dvc_browser[is.nan(by_dvc_browser$avg)])] <- "avg_0"

all$dvc_browser[is.na(all$dvc_browser)] <- "avg_0"  #deal with NA value


# dvc_operatingSystem
uf_any_na(all$dvc_operatingSystem)   #check if there any unknown/na/null value
all$dvc_operatingSystem[(all$dvc_operatingSystem %in% unkown_value) | is.na(all$dvc_operatingSystem)] <- NA    #if yes, fill up with NA

View(by_dvc_operatingSystem <- 
       all %>% select(dvc_operatingSystem,tt_transactionRevenue2) %>% group_by(dvc_operatingSystem) %>% 
       summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))

all$dvc_operatingSystem[(all$dvc_operatingSystem %in% (by_dvc_operatingSystem %>% filter(avg <= 0, !is.na(dvc_operatingSystem)))$dvc_operatingSystem) |  #deal vith avg = 0
                          (all$dvc_operatingSystem %in% by_dvc_operatingSystem$dvc_operatingSystem[is.nan(by_dvc_operatingSystem$avg)])] <- "avg_0"

all$dvc_operatingSystem[is.na(all$dvc_operatingSystem)] <- "avg_0"  #deal with NA value

# dvc_isMobile
uf_any_na(all$dvc_isMobile)   #check if there any unknown/na/null value

View(by_dvc_isMobile <- 
       all %>% select(dvc_isMobile,tt_transactionRevenue2) %>% group_by(dvc_isMobile) %>% 
       summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))

all$dvc_isMobile <- as.integer(all$dvc_isMobile)


# dvc_deviceCategory
uf_any_na(all$dvc_deviceCategory)   #check if there any unknown/na/null value

View(by_dvc_deviceCategory <- 
       all %>% select(dvc_deviceCategory,tt_transactionRevenue2) %>% group_by(dvc_deviceCategory) %>% 
       summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))


###################################geo_*
names(all)
# geo_continent
uf_any_na(all$geo_continent)   #check if there any unknown/na/null value
all$geo_continent[(all$geo_continent %in% unkown_value) | is.na(all$geo_continent)] <- NA    #if yes, fill up with NA

View(by_geo_continent <- 
       all %>% select(geo_continent,tt_transactionRevenue2) %>% group_by(geo_continent) %>% 
       summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))


# geo_subContinent
uf_any_na(all$geo_subContinent)   #check if there any unknown/na/null value
all$geo_subContinent[(all$geo_subContinent %in% unkown_value) | is.na(all$geo_subContinent)] <- NA    #if yes, fill up with NA

View(by_geo_subContinent <- 
       all %>% select(geo_subContinent,tt_transactionRevenue2) %>% group_by(geo_subContinent) %>% 
       summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))

all$geo_subContinent[(all$geo_subContinent %in% (by_geo_subContinent %>% filter(avg <= 0, !is.na(geo_subContinent)))$geo_subContinent) |  #deal vith avg = 0
                       (all$geo_subContinent %in% by_geo_subContinent$geo_subContinent[is.nan(by_geo_subContinent$avg)])] <- "avg_0"



# geo_country
uf_any_na(all$geo_country)   #check if there any unknown/na/null value
all$geo_country[(all$geo_country %in% unkown_value) | is.na(all$geo_country)] <- NA    #if yes, fill up with NA

View(by_geo_country <- 
       all %>% select(geo_country,tt_transactionRevenue2) %>% group_by(geo_country) %>% 
       summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))

all$geo_country[(all$geo_country %in% (by_geo_country %>% filter(avg <= 0, !is.na(geo_country)))$geo_country) |  #deal vith avg = 0
                  (all$geo_country %in% by_geo_country$geo_country[is.nan(by_geo_country$avg)])] <- "avg_0"


# geo_region
uf_any_na(all$geo_region)   #check if there any unknown/na/null value
all$geo_region[(all$geo_region %in% unkown_value) | is.na(all$geo_region)] <- NA    #if yes, fill up with NA

View(by_geo_region <- 
       all %>% select(geo_region,tt_transactionRevenue2) %>% group_by(geo_region) %>% 
       summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))

all$geo_region[(all$geo_region %in% (by_geo_region %>% filter(avg <= 0, !is.na(geo_region)))$geo_region) |  #deal vith avg = 0
                 (all$geo_region %in% by_geo_region$geo_region[is.nan(by_geo_region$avg)])] <- "avg_0"


# geo_metro
uf_any_na(all$geo_metro)   #check if there any unknown/na/null value
all$geo_metro[(all$geo_metro %in% unkown_value) | is.na(all$geo_metro)] <- NA    #if yes, fill up with NA

View(by_geo_metro <- 
       all %>% select(geo_metro,tt_transactionRevenue2) %>% group_by(geo_metro) %>% 
       summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))

all$geo_metro[(all$geo_metro %in% (by_geo_metro %>% filter(avg <= 0, !is.na(geo_metro)))$geo_metro) |  #deal vith avg = 0
                (all$geo_metro %in% by_geo_metro$geo_metro[is.nan(by_geo_metro$avg)])] <- "avg_0"


# geo_city
uf_any_na(all$geo_city)   #check if there any unknown/na/null value
all$geo_city[(all$geo_city %in% unkown_value) | is.na(all$geo_city)] <- NA    #if yes, fill up with NA

View(by_geo_city <- 
       all %>% select(geo_city,tt_transactionRevenue2) %>% group_by(geo_city) %>% 
       summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))

all$geo_city[(all$geo_city %in% (by_geo_city %>% filter(avg <= 0, !is.na(geo_city)))$geo_city) |  #deal vith avg = 0
               (all$geo_city %in% by_geo_city$geo_city[is.nan(by_geo_city$avg)])] <- "avg_0"


# geo_networkDomain
uf_any_na(all$geo_networkDomain)   #check if there any unknown/na/null value
all$geo_networkDomain[(all$geo_networkDomain %in% unkown_value) | is.na(all$geo_networkDomain)] <- NA    #if yes, fill up with NA

View(by_geo_networkDomain <- 
       all %>% select(geo_networkDomain,tt_transactionRevenue2) %>% group_by(geo_networkDomain) %>% 
       summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))

all$geo_networkDomain[(all$geo_networkDomain %in% (by_geo_networkDomain %>% filter(avg <= 0, !is.na(geo_networkDomain)))$geo_networkDomain) |  #deal vith avg = 0
                        (all$geo_networkDomain %in% by_geo_networkDomain$geo_networkDomain[is.nan(by_geo_networkDomain$avg)])] <- "avg_0"


###################################tfcS_*
names(all)

# tfcS_campaign
uf_any_na(all$tfcS_campaign)   #check if there any unknown/na/null value
all$tfcS_campaign[(all$tfcS_campaign %in% unkown_value) | is.na(all$tfcS_campaign)] <- NA    #if yes, fill up with NA

View(by_tfcS_campaign <- 
       all %>% select(tfcS_campaign,tt_transactionRevenue2) %>% group_by(tfcS_campaign) %>% 
       summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))

all$tfcS_campaign[(all$tfcS_campaign %in% (by_tfcS_campaign %>% filter(avg <= 0, !is.na(tfcS_campaign)))$tfcS_campaign) |  #deal vith avg = 0
                    (all$tfcS_campaign %in% by_tfcS_campaign$tfcS_campaign[is.nan(by_tfcS_campaign$avg)])] <- "avg_0"

all$tfcS_campaign[is.na(all$tfcS_campaign)] <- "avg_0"  #deal with NA value

all$tfcS_is_campaign <- ifelse(is.na(all$tfcS_campaign), 0, 1)
View(by_tfcS_is_campaign <- 
       all %>% select(tfcS_is_campaign,tt_transactionRevenue2) %>% group_by(tfcS_is_campaign) %>% 
       summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))

# tfcS_source
uf_any_na(all$tfcS_source)   #check if there any unknown/na/null value
all$tfcS_source[(all$tfcS_source %in% unkown_value) | is.na(all$tfcS_source)] <- NA    #if yes, fill up with NA

View(by_tfcS_source <- 
       all %>% select(tfcS_source,tt_transactionRevenue2) %>% group_by(tfcS_source) %>% 
       summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))

all$tfcS_source[(all$tfcS_source %in% (by_tfcS_source %>% filter(avg <= 0, !is.na(tfcS_source)))$tfcS_source) |  #deal vith avg = 0
                  (all$tfcS_source %in% by_tfcS_source$tfcS_source[is.nan(by_tfcS_source$avg)])] <- "avg_0"

all$tfcS_source[is.na(all$tfcS_source)] <- "avg_0"  #deal with NA value

# tfcS_medium
uf_any_na(all$tfcS_medium)   #check if there any unknown/na/null value
all$tfcS_medium[(all$tfcS_medium %in% unkown_value) | is.na(all$tfcS_medium)] <- NA    #if yes, fill up with NA

View(by_tfcS_medium <- 
       all %>% select(tfcS_medium,tt_transactionRevenue2) %>% group_by(tfcS_medium) %>% 
       summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))

all$tfcS_medium[(all$tfcS_medium %in% (by_tfcS_medium %>% filter(avg <= 0, !is.na(tfcS_medium)))$tfcS_medium) |  #deal vith avg = 0
                  (all$tfcS_medium %in% by_tfcS_medium$tfcS_medium[is.nan(by_tfcS_medium$avg)])] <- "avg_0"


# tfcS_keyword
uf_any_na(all$tfcS_keyword)   #check if there any unknown/na/null value
all$tfcS_keyword[(all$tfcS_keyword %in% unkown_value) | is.na(all$tfcS_keyword)] <- NA    #if yes, fill up with NA

View(by_tfcS_keyword <- 
       all %>% select(tfcS_keyword,tt_transactionRevenue2) %>% group_by(tfcS_keyword) %>% 
       summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))

all$tfcS_keyword[(all$tfcS_keyword %in% (by_tfcS_keyword %>% filter(avg <= 0, !is.na(tfcS_keyword)))$tfcS_keyword) |  #deal vith avg = 0
                   (all$tfcS_keyword %in% by_tfcS_keyword$tfcS_keyword[is.nan(by_tfcS_keyword$avg)])] <- "avg_0"


# tfcS_isTrueDirect
uf_any_na(all$tfcS_isTrueDirect)   #check if there any unknown/na/null value
all$tfcS_isTrueDirect[(all$tfcS_isTrueDirect %in% unkown_value) | is.na(all$tfcS_isTrueDirect)] <- NA    #if yes, fill up with NA

View(by_tfcS_isTrueDirect <- 
       all %>% select(tfcS_isTrueDirect,tt_transactionRevenue2) %>% group_by(tfcS_isTrueDirect) %>% 
       summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))

all$tfcS_isTrueDirect <- ifelse(is.na(all$tfcS_isTrueDirect),0,1)

# tfcS_referralPath
uf_any_na(all$tfcS_referralPath)   #check if there any unknown/na/null value
all$tfcS_referralPath[(all$tfcS_referralPath %in% unkown_value) | is.na(all$tfcS_referralPath)] <- NA    #if yes, fill up with NA

View(by_tfcS_referralPath <- 
       all %>% select(tfcS_referralPath,tt_transactionRevenue2) %>% group_by(tfcS_referralPath) %>% 
       summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))

all$tfcS_referralPath[(all$tfcS_referralPath %in% (by_tfcS_referralPath %>% filter(avg <= 0, !is.na(tfcS_referralPath)))$tfcS_referralPath) |  #deal vith avg = 0
                        (all$tfcS_referralPath %in% by_tfcS_referralPath$tfcS_referralPath[is.nan(by_tfcS_referralPath$avg)])] <- "avg_0"


# tfcS_adContent
uf_any_na(all$tfcS_adContent)   #check if there any unknown/na/null value
all$tfcS_adContent[(all$tfcS_adContent %in% unkown_value) | is.na(all$tfcS_adContent)] <- NA    #if yes, fill up with NA

View(by_tfcS_adContent <- 
       all %>% select(tfcS_adContent,tt_transactionRevenue2) %>% group_by(tfcS_adContent) %>% 
       summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))

all$tfcS_adContent[(all$tfcS_adContent %in% (by_tfcS_adContent %>% filter(avg <= 0, !is.na(tfcS_adContent)))$tfcS_adContent) |  #deal vith avg = 0
                     (all$tfcS_adContent %in% by_tfcS_adContent$tfcS_adContent[is.nan(by_tfcS_adContent$avg)])] <- "avg_0"


# tfcS_adwordsClickInfo.page
uf_any_na(all$tfcS_adwordsClickInfo.page)   #check if there any unknown/na/null value
all$tfcS_adwordsClickInfo.page[(all$tfcS_adwordsClickInfo.page %in% unkown_value) | is.na(all$tfcS_adwordsClickInfo.page)] <- NA    #if yes, fill up with NA

View(by_tfcS_adwordsClickInfo.page <- 
       all %>% select(tfcS_adwordsClickInfo.page,tt_transactionRevenue2) %>% group_by(tfcS_adwordsClickInfo.page) %>% 
       summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))

all$tfcS_adwordsClickInfo.page[(all$tfcS_adwordsClickInfo.page %in% (by_tfcS_adwordsClickInfo.page %>% filter(avg <= 0, !is.na(tfcS_adwordsClickInfo.page)))$tfcS_adwordsClickInfo.page) |  #deal vith avg = 0
                                 (all$tfcS_adwordsClickInfo.page %in% by_tfcS_adwordsClickInfo.page$tfcS_adwordsClickInfo.page[is.nan(by_tfcS_adwordsClickInfo.page$avg)])] <- "avg_0"

all$tfcS_adwordsClickInfo.page <- ifelse(all$tfcS_adwordsClickInfo.page == 1, 1, 0)
all$tfcS_adwordsClickInfo.page <- ifelse(is.na(all$tfcS_adwordsClickInfo.page), 0, 1)

# tfcS_adwordsClickInfo.slot
uf_any_na(all$tfcS_adwordsClickInfo.slot)   #check if there any unknown/na/null value
all$tfcS_adwordsClickInfo.slot[(all$tfcS_adwordsClickInfo.slot %in% unkown_value) | is.na(all$tfcS_adwordsClickInfo.slot)] <- NA    #if yes, fill up with NA

View(by_tfcS_adwordsClickInfo.slot <- 
       all %>% select(tfcS_adwordsClickInfo.slot,tt_transactionRevenue2) %>% group_by(tfcS_adwordsClickInfo.slot) %>% 
       summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))

all$tfcS_adwordsClickInfo.slot[(all$tfcS_adwordsClickInfo.slot %in% (by_tfcS_adwordsClickInfo.slot %>% filter(avg <= 0, !is.na(tfcS_adwordsClickInfo.slot)))$tfcS_adwordsClickInfo.slot) |  #deal vith avg = 0
                                 (all$tfcS_adwordsClickInfo.slot %in% by_tfcS_adwordsClickInfo.slot$tfcS_adwordsClickInfo.slot[is.nan(by_tfcS_adwordsClickInfo.slot$avg)])] <- "avg_0"



# tfcS_adwordsClickInfo.gclId
uf_any_na(all$tfcS_adwordsClickInfo.gclId)   #check if there any unknown/na/null value
all$tfcS_adwordsClickInfo.gclId[(all$tfcS_adwordsClickInfo.gclId %in% unkown_value) | is.na(all$tfcS_adwordsClickInfo.gclId)] <- NA    #if yes, fill up with NA

View(by_tfcS_adwordsClickInfo.gclId <- 
       all %>% select(tfcS_adwordsClickInfo.gclId,tt_transactionRevenue2) %>% group_by(tfcS_adwordsClickInfo.gclId) %>% 
       summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))

all <- all %>% select(-tfcS_adwordsClickInfo.gclId)

# tfcS_adwordsClickInfo.adNetworkType
uf_any_na(all$tfcS_adwordsClickInfo.adNetworkType)   #check if there any unknown/na/null value
all$tfcS_adwordsClickInfo.adNetworkType[(all$tfcS_adwordsClickInfo.adNetworkType %in% unkown_value) | is.na(all$tfcS_adwordsClickInfo.adNetworkType)] <- NA    #if yes, fill up with NA

View(by_tfcS_adwordsClickInfo.adNetworkType <- 
       all %>% select(tfcS_adwordsClickInfo.adNetworkType,tt_transactionRevenue2) %>% group_by(tfcS_adwordsClickInfo.adNetworkType) %>% 
       summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))

all$tfcS_adwordsClickInfo.adNetworkType[(all$tfcS_adwordsClickInfo.adNetworkType %in% (by_tfcS_adwordsClickInfo.adNetworkType %>% filter(avg <= 0, !is.na(tfcS_adwordsClickInfo.adNetworkType)))$tfcS_adwordsClickInfo.adNetworkType) |  #deal vith avg = 0
                                          (all$tfcS_adwordsClickInfo.adNetworkType %in% by_tfcS_adwordsClickInfo.adNetworkType$tfcS_adwordsClickInfo.adNetworkType[is.nan(by_tfcS_adwordsClickInfo.adNetworkType$avg)])] <- "avg_0"


# tfcS_adwordsClickInfo.isVideoAd
uf_any_na(all$tfcS_adwordsClickInfo.isVideoAd)   #check if there any unknown/na/null value
all$tfcS_adwordsClickInfo.isVideoAd[(all$tfcS_adwordsClickInfo.isVideoAd %in% unkown_value) | is.na(all$tfcS_adwordsClickInfo.isVideoAd)] <- NA    #if yes, fill up with NA

View(by_tfcS_adwordsClickInfo.isVideoAd <- 
       all %>% select(tfcS_adwordsClickInfo.isVideoAd,tt_transactionRevenue2) %>% group_by(tfcS_adwordsClickInfo.isVideoAd) %>% 
       summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))

all$tfcS_adwordsClickInfo.isVideoAd <- ifelse(is.na(all$tfcS_adwordsClickInfo.isVideoAd),1,0)

###################################tt_*
names(all)

# tt_hits
uf_any_na(all$tt_hits)   #check if there any unknown/na/null value
range(all$tt_hits)

# tt_pageviews
uf_any_na(all$tt_pageviews)   #check if there any unknown/na/null value
range(all$tt_pageviews[!is.na(all$tt_pageviews)])
all$tt_pageviews[is.na(all$tt_pageviews)] <- 0


# tt_bounces
uf_any_na(all$tt_bounces)   #check if there any unknown/na/null value
all$tt_bounces[(all$tt_bounces %in% unkown_value) | is.na(all$tt_bounces)] <- NA    #if yes, fill up with NA

View(by_tt_bounces <- 
       all %>% select(tt_bounces,tt_transactionRevenue2) %>% group_by(tt_bounces) %>% 
       summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))

all$tt_bounces[is.na(all$tt_bounces)] <- 0

# tt_newVisits
uf_any_na(all$tt_newVisits)   #check if there any unknown/na/null value
all$tt_newVisits[(all$tt_newVisits %in% unkown_value) | is.na(all$tt_newVisits)] <- NA    #if yes, fill up with NA

View(by_tt_newVisits <- 
       all %>% select(tt_newVisits,tt_transactionRevenue2) %>% group_by(tt_newVisits) %>% 
       summarise(n=n(), avg = mean(tt_transactionRevenue2, na.rm = T)) %>% arrange(desc(avg)))

all$tt_newVisits[is.na(all$tt_newVisits)] <- 0

##########################
names(all)
all <- all %>% 
  select(35,36,34,3,5,6,7,37:43,1,8:19,44,20:33)

write_csv(all,"./data/all2.csv")





