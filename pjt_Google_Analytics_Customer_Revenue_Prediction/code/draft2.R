############################################################Packages############################################################
getwd()
options(warn=-1)

library(tidyverse) 
library(stringr)
library(jsonlite)
library(lubridate)
library(Matrix)
library(xgboost)
library(keras)

set.seed(0)

############################################################对all2 去重处理(只跑一次)############################################################
#2. 有些用户有这样的duplicate情况: (visitNumber,fullVisitorId)
#(fullVisitorId,visitStartTime)则没有duplicate
user_dup_vnID <- all2 %>% select(visitNumber,fullVisitorId) %>% group_by(fullVisitorId,visitNumber) %>% summarise(n = n())
user_dup_vnStartTime <- all2 %>% select(visitStartTime,fullVisitorId) %>% group_by(fullVisitorId,visitStartTime) %>% summarise(n = n()) %>% filter(n > 1)
nrow(user_dup_vnID %>% filter(n > 1)) / nrow(user_dup_vnID)  # 5092rows - 0.0055 

#解决方案: 去重
user_dup_vnID$IDvn <- str_c(user_dup_vnID$fullVisitorId,"_",user_dup_vnID$visitNumber)
records_dup <- all2 %>% filter(str_c(fullVisitorId,"_",visitNumber) %in% user_dup_vnID$IDvn[user_dup_vnID$n > 1])

sum(c(NA,NA,NA), na.rm = T)
if(1==1){
  records_dup <- 
    records_dup %>% group_by(fullVisitorId,visitNumber) %>% 
    summarise(flag = as.integer(max(flag)),
              tt_transactionRevenue2 = sum(tt_transactionRevenue2, na.rm = T),
              tt_transactionRevenue = sum(tt_transactionRevenue, na.rm = T),
              visitId = as.integer(max(visitId)),
              visitStartTime = max(visitStartTime),
              dateInt = as.integer(max(dateInt)),
              year = as.integer(max(year)),
              month = as.integer(max(month)),
              day = as.integer(max(day)),
              hour = as.integer(max(hour)),
              weekday = max(weekday),
              time_last = as.integer(max(time_last)),
              channelGrouping = max(channelGrouping),
              dvc_browser = max(dvc_browser),
              dvc_operatingSystem = max(dvc_operatingSystem),
              dvc_isMobile = as.integer(max(dvc_isMobile)),
              dvc_deviceCategory = max(dvc_deviceCategory),
              geo_continent = max(geo_continent),
              geo_subContinent = max(geo_subContinent),
              geo_country = max(geo_country),
              geo_region = max(geo_region),
              geo_metro = max(geo_metro),
              geo_city = max(geo_city),
              tfcS_campaign = max(tfcS_campaign),
              tfcS_is_campaign = as.integer(max(tfcS_is_campaign)),
              tfcS_source = max(tfcS_source),
              tfcS_medium = max(tfcS_medium),
              tfcS_keyword = max(tfcS_keyword),
              tfcS_isTrueDirect = as.integer(max(tfcS_isTrueDirect)),
              tfcS_referralPath = max(tfcS_referralPath),
              tfcS_adContent = max(tfcS_adContent),
              tfcS_adwordsClickInfo.page = as.integer(max(tfcS_adwordsClickInfo.page)),
              tfcS_adwordsClickInfo.slot = max(tfcS_adwordsClickInfo.slot),
              tfcS_adwordsClickInfo.adNetworkType = max(tfcS_adwordsClickInfo.adNetworkType),
              tfcS_adwordsClickInfo.isVideoAd = as.integer(max(tfcS_adwordsClickInfo.isVideoAd)),
              tt_hits = sum(tt_hits),
              tt_pageviews = sum(tt_pageviews),
              tt_bounces = sum(tt_bounces),
              tt_newVisits = as.integer(max(tt_newVisits))
    ) %>%
    select(3:5,1,6,2,7:41)
}
all(names(records_dup) == names(all2))
records_dup$tt_transactionRevenue2[records_dup$flag == 0] <- NA
records_dup$tt_transactionRevenue[records_dup$flag == 0] <- NA
all2 <- all2 %>% filter( !(str_c(fullVisitorId,"_",visitNumber) %in% user_dup_vnID$IDvn[user_dup_vnID$n > 1]) )
all2 <- union_all(all2, records_dup) 

write_csv(all2, "./data/all3.csv")

############################################################All2 data load############################################################
all2 <- read_csv("./data/all3.csv"); 
all2 <- all2 %>% select(-tt_transactionRevenue2)

names(all2)
dim(all2)
glimpse(all2)

#查看每一列的空值比例, 以及character列的unique数
ct_all2 <- nrow(all2)
ct_all2_1 <- nrow(all2 %>% filter(flag == 1, tt_transactionRevenue2 > 0))
name_attrs <- names(all2)
len_attrs <- length(name_attrs)
dtypes <- sapply(all2,typeof)
na_len <- round(sapply(all2, function(x){sum(as.integer(is.na(x)))}) / ct_all2, 5)
na_len_1 <-  round(sapply(all2 %>% filter(flag == 1, tt_transactionRevenue2 > 0),function(x){sum(as.integer(is.na(x)))}) /  ct_all2_1, 5)
unique_len <- sapply(all2, function(x){ifelse(typeof(x)=="character", length(unique(x)), NA)})
ds_each_attr <- tibble(var_name = name_attrs, 
                       DType = dtypes, 
                       NA_prop = na_len, 
                       NA_prop_1 = na_len_1,
                       unq_ct = unique_len)
ds_each_attr$DType[ds_each_attr$var_name %in% c("tfcS_is_campaign",
                                                "tfcS_isTrueDirect","tfcS_adwordsClickInfo.page",
                                                "tfcS_adwordsClickInfo.isVideoAd","tt_newVisits")] <- "bool"
#View(ds_each_attr)

#需要特殊处理的数据
#1. 有些用户的记录即存在于train set中也存在于test set 中
user_in_both <- dplyr::intersect(all2 %>% filter(flag == 1) %>% distinct(fullVisitorId),
                                 all2 %>% filter(flag == 0) %>% distinct(fullVisitorId))
#nrow(user_in_both)  #7679

sp_both <- all2 %>% filter(fullVisitorId %in% user_in_both$fullVisitorId)
#nrow(sp_both)       #42862
#View(sp_both %>% filter(fullVisitorId == "3677897653314763609") %>% arrange(visitNumber))
# 有没有这样的情况: user在train set里面的max(visitNumber)大于在test set里面的min(visitNumber)??
user_both_train <- sp_both %>% filter(flag == 1) %>% select(fullVisitorId, visitNumber) %>% group_by(fullVisitorId) %>% summarise(max_vn = max(visitNumber))
user_both_test <- sp_both %>% filter(flag == 0) %>% select(fullVisitorId, visitNumber) %>% group_by(fullVisitorId) %>% summarise(min_vn = min(visitNumber))
nrow(user_both_train) == nrow(user_both_test)
user_min_max <- user_both_train %>% inner_join(user_both_test, by = "fullVisitorId")
bad1 <- user_min_max %>% filter(max_vn > min_vn)

#解决方案: 从train中排除bad1中的user数据, 创建好模型后，预测的时候给定一个预测值，再与已经产生的transaction比较,去较大的一个值
#在做padding的时候要注意，train和test可以交叉pad






############################################################1. Focus on First Time - XGBoost############################################################
############################################################1.1. Data Preparation############################################################
user_vn <- 
  all2 %>% select(visitNumber, fullVisitorId) %>% 
    group_by(fullVisitorId) %>% 
    summarise(n = n(), min_vn = min(visitNumber), max_vn = max(visitNumber))

user_vn$only_v1 <- ifelse(user_vn$n == 1 & user_vn$min_vn == 1 & user_vn$max_vn == 1, "Y","N")

#table(user_vn$only_v1)

all2_v1 <- all2 %>% filter(fullVisitorId %in% user_vn$fullVisitorId[user_vn$only_v1 == "Y"])
#nrow(all2_v1)
#glimpse(all2)

rm(list = c("all2","user_vn")); gc()

# 添加两个变量
all2_v1$tt_transactionRevenue2 <- ifelse(all2_v1$flag == 1, log(all2_v1$tt_transactionRevenue+1), NA)
#ggplot(data = all2_v1 %>% filter(tt_transactionRevenue2 > 0)) + geom_density(aes(x = tt_transactionRevenue2))

all2_v1$is_Tran <- ifelse(all2_v1$flag == 1 & all2_v1$tt_transactionRevenue > 0, 1, 0)
#table(all2_v1$is_Tran); prop.table(table(all2_v1$is_Tran))

# 将all2_v1 分为Dev和Exam 数据集
Dev <- all2_v1 %>% filter(flag == 1)
Exam <- all2_v1 %>% filter(flag == 0)

rm(list = c("all2_v1")); gc()

# 去掉不需要的变量
#names(Dev)
Dev <- Dev %>% select(-c("flag","tt_transactionRevenue","fullVisitorId","visitId","visitNumber","visitStartTime","dateInt","dvc_isMobile","is_Tran"))
Exam <- Exam %>% select(-c("flag","tt_transactionRevenue","fullVisitorId","visitId","visitNumber","visitStartTime","dateInt","dvc_isMobile","is_Tran"))

# 转换为XGBoost 需要的Spase Matrix数据结构
options("na.action"); options(na.action="na.pass"); 
spM_Dev_x <- sparse.model.matrix(tt_transactionRevenue2~.-1, data = Dev, na.action = "na.pass")
spM_Exam_x <-  sparse.model.matrix(tt_transactionRevenue2~.-1, data = Exam, na.action = "na.pass")
options(na.action="na.omit"); options("na.action");
#spM_Dev_x@Dim;    #spM_Dev_x@Dim[[1]] == nrow(Dev)
#spM_Exam_x@Dim;   #spM_Exam_x@Dim[[1]] == nrow(Exam)

var_list_Dev <- spM_Dev_x@Dimnames[[2]]                        #528
var_list_Exam <-  spM_Exam_x@Dimnames[[2]]                     #480
its_DevExam <- dplyr::intersect(var_list_Dev, var_list_Exam)   #478
spM_Dev_x <- spM_Dev_x[,-which(!(var_list_Dev %in% its_DevExam))]
spM_Exam_x <- spM_Exam_x[,-which(!(var_list_Exam %in% its_DevExam))]
all(spM_Dev_x@Dimnames[[2]] == spM_Exam_x@Dimnames[[2]])       #检验结果为TRUE

label_Dev <- Dev$tt_transactionRevenue2
label_Exam <- Exam$tt_transactionRevenue2

rm(list = c("Dev","Exam")); gc()

# 将Dev数据集分割成train, val, test
set.seed(1000)
#nrow(spM_Dev_x);length(label_Dev)         #611698

total_seq <- c(1:length(label_Dev))
seq_lb <- ifelse(label_Dev > 0, 1, 0); table(seq_lb)
lb0_seq <- total_seq[seq_lb == 0]
lb1_seq <- total_seq[seq_lb == 1]
len_lb0_seq <- length(lb0_seq)
len_lb1_seq <- length(lb1_seq)

splt_rate <- c(0.65,0.2,0.15)
attr(splt_rate,"setType") <- c("train","val","test")
attr(splt_rate,"type_len_0") <- c(round(len_lb0_seq * splt_rate) - 1)
attr(splt_rate,"type_len_1") <- c(round(len_lb1_seq * splt_rate) - 1)

lb0_seq_suffle <- lb0_seq[sample(1:len_lb0_seq,size = len_lb0_seq, replace = F)]
lb1_seq_suffle <- lb1_seq[sample(1:len_lb1_seq,size = len_lb1_seq, replace = F)]

train_seq <- c(lb0_seq_suffle[1:attr(splt_rate,"type_len_0")[1]], lb1_seq_suffle[1:attr(splt_rate,"type_len_1")[1]])
val_seq <- c(lb0_seq_suffle[(attr(splt_rate,"type_len_0")[1]+1):(sum(attr(splt_rate,"type_len_0")[1:2]))],lb1_seq_suffle[(attr(splt_rate,"type_len_1")[1]+1):(sum(attr(splt_rate,"type_len_1")[1:2]))])
test_seq <- c(lb0_seq_suffle[(sum(attr(splt_rate,"type_len_0")[1:2])+1):(sum(attr(splt_rate,"type_len_0")[1:3]))],lb1_seq_suffle[(sum(attr(splt_rate,"type_len_1")[1:2])+1):(sum(attr(splt_rate,"type_len_1")[1:3]))])

spM_train_x <- spM_Dev_x[train_seq,]
spM_val_x <- spM_Dev_x[val_seq,]
spM_test_x <- spM_Dev_x[test_seq,]
label_train <- label_Dev[train_seq]
label_val <- label_Dev[val_seq]
label_test <- label_Dev[test_seq]

dtrain <- xgb.DMatrix(data = spM_train_x, label = label_train, missing = NA)
dval <- xgb.DMatrix(data = spM_val_x, label = label_val, missing = NA)
dtest <- xgb.DMatrix(data = spM_test_x, label = label_test, missing = NA)
dExam <- xgb.DMatrix(data = spM_Exam_x, label = label_Exam, missing = NA)
getinfo(dtrain,"label");getinfo(dtrain,"nrow");getinfo(dtrain,"weight"); getinfo(dtrain,"base_margin")

rm(list = ls()[!((ls() %in% c("dtrain","dval","dtest","dExam")))]); gc()


############################################################1.2. XGBoost Model############################################################
#dtrain; dval; dtest; dExam

#' 初始化参数
param0.xgb <- list(
  seed = 0,
  silent = 1,
  nthread = 2,
  booster = "gbtree",
  objective = "reg:linear",
  eval_metric = "rmse",
  
  eta = 0.01,
  
  gamma = 0,
  max_depth = 5,
  min_child_weight = 10,
  max_delta_step = 0,
  
  subsample = 0.8,
  colsample_bytree = 0.7,
  colsample_bylevel = 0.6,
  
  lambda = 10,
  alpha = 0
  
  #scale_pos_weight = 
)

#' 初始模型
fit.xgb0 <- xgb.train(
  params = param0.xgb,
  data = dtrain,
  watchlist = list(train = dtrain, val = dval),
  print_every_n = 1,
  early_stopping_rounds = 10,
  nrounds = 100
)

#’ 初始模型结果分析
fit.xgb0

iter.error <-  data.frame(iter = rep(1:fit.xgb0$niter,time=2),
                          error_set = rep(c("train","val"),each=fit.xgb0$niter),
                          error_p0 = c(fit.xgb0$evaluation_log$train_rmse,fit.xgb0$evaluation_log$val_rmse))
ggplot(data = iter.error, aes(x = iter, y = error_p0, colour = error_set)) + 
  geom_line(size = 1) 
impt.xgb0 <- xgb.importance(feature_names = fit.xgb0$feature_names, model = fit.xgb0)
xgb.ggplot.importance(importance_matrix = impt.xgb0,top_n = min(30,fit.xgb0$nfeatures), measure =  "Gain")
xgb.ggplot.deepness(model = fit.xgb0,which = "2x1")


#' 参数调优
paramT.xgb <- list(
  seed = 0,
  silent = 1,
  nthread = 2,
  booster = "gbtree",
  objective = "reg:linear",
  eval_metric = "rmse",
  
  eta = 0.01,
  
  gamma = 0,
  max_depth = 5,
  min_child_weight = 10,
  max_delta_step = 0,
  
  subsample = 0.8,
  colsample_bytree = 0.7,
  colsample_bylevel = 0.6,
  
  lambda = 10,
  alpha = 0
  
  #scale_pos_weight = 
)

fit.xgbT <- xgb.train(
  params = paramT.xgb,
  data = dtrain,
  watchlist = list(train = dtrain, val = dval),
  print_every_n = 1,
  early_stopping_rounds = 10,
  nrounds = 1000
)

fit.xgbT

iter.errorT <-  data.frame(iter = rep(1:fit.xgbT$niter,time=2),
                          error_set = rep(c("train","val"),each=fit.xgbT$niter),
                          error_p0 = c(fit.xgbT$evaluation_log$train_rmse,fit.xgbT$evaluation_log$val_rmse))
ggplot(data = iter.errorT, aes(x = iter, y = error_p0, colour = error_set)) + 
  geom_line(size = 1) 

impt.xgbT <- xgb.importance(feature_names = fit.xgbT$feature_names, model = fit.xgbT)
xgb.ggplot.importance(importance_matrix = impt.xgbT,top_n = min(30,fit.xgb0$nfeatures), measure =  "Gain")
xgb.ggplot.deepness(model = fit.xgbT,which = "2x1")

#' 用test dataset进行验证
iter.errorT %>% filter(iter == fit.xgbT$best_iteration)

pred_val <- predict(fit.xgbT, dval, ntreelimit = fit.xgbT$best_iteration)
sum((getinfo(dval,"label") - pred_val) ^ 2) / length(pred_val)  #1.028123

pred_test <- predict(fit.xgbT, dtest, ntreelimit = fit.xgbT$best_iteration)
sum((getinfo(dtest,"label") - pred_test) ^ 2) / length(pred_test) #1.031232

#' 保存模型
xgb.save(fit.xgbT, "./model/model_20181106.xgb")
fit.xgbT <- xgb.load("./model/model_20181106.xgb")


############################################################1.3. Exam Predict############################################################
dExam; Exam
fit.xgbT <- xgb.load("./model/model_20181106.xgb")
pred_Exam <- predict(fit.xgbT, dExam, ntreelimit = fit.xgbT$best_iteration)
pred_Exam <- ifelse(pred_Exam < 0, 0, pred_Exam)
nrow(Exam) == length(pred_Exam)

all2_v1_pred <- tibble(fullVisitorId = Exam$fullVisitorId,
                       PredictedLogRevenue = pred_Exam)
nrow(all2_v1_pred) == length(pred_Exam)

write_csv(all2_v1_pred,"./data/predRlt_v1_20181107.csv")


############################################################2. RNN for visitNumbers############################################################
############################################################2.1. Data Preparation P1############################################################
all2 <- read_csv("./data/all3.csv"); 
all2 <- all2 %>% select(-tt_transactionRevenue2)


vn_keep <- 13



user_vn <- 
  all2 %>% select(visitNumber, fullVisitorId) %>% 
  group_by(fullVisitorId) %>% 
  summarise(n = n(), min_vn = min(visitNumber), max_vn = max(visitNumber))
#View(user_vn[1:1000,])
user_vn$only_v1 <- ifelse(user_vn$n == 1 & user_vn$min_vn == 1 & user_vn$max_vn == 1, "Y","N")
#table(user_vn$only_v1)
all2_vn <- all2 %>% filter(!(fullVisitorId %in% user_vn$fullVisitorId[user_vn$only_v1 == "Y"]))
#nrow(all2_vn)
#glimpse(all2_vn)

rm(list = c("all2","user_vn")); gc()

# 统计: unique用户-# of visitnumber-sum(tt_transaction)-是否在train和test中都出现
stat_vn <- 
  all2_vn %>% select(fullVisitorId, visitNumber, flag, tt_transactionRevenue) %>% 
  group_by(fullVisitorId) %>%
  summarise(n_records = n(), 
            min_vn = min(visitNumber), 
            mx_vn = max(visitNumber), 
            min_flag = min(flag), 
            max_flag = max(flag), 
            sum_tran = sum(tt_transactionRevenue))
stat_vn$which_ds <- ifelse(stat_vn$min_flag == 1 & stat_vn$max_flag == 1, "Dev", 
                           ifelse(stat_vn$min_flag == 0 & stat_vn$max_flag == 0, "Exam", "Both"))
#View(stat_vn)

# 一共有194307个unique user, Dev(96150)/Exam(91838)/Both(6319)
# 解决方案一: 把both的user的tt_transaction设置为0, flag设置为1。(相当于这部分数据只做test,不做train)
all2_vn$tt_transactionRevenue[all2_vn$fullVisitorId %in% stat_vn$fullVisitorId[stat_vn$which_ds == "Both"]] <- NA
all2_vn$flag[all2_vn$fullVisitorId %in% stat_vn$fullVisitorId[stat_vn$which_ds == "Both"]] <- 0
# 解决方案二: 暂时先不考虑Both, 看是否有其他的方法可以
#nrow(stat_vn); table(stat_vn$which_ds); 
#View(all2_vn %>% filter(fullVisitorId == "0000436683523507380")) 
#length(unique(all2_vn$fullVisitorId))
#nrow(all2_vn %>% distinct(fullVisitorId,flag))

# 先来一轮空置处理，同时考虑哪些变量是不需要的
if(1==1){
  ct_all2 <- nrow(all2_vn)
  ct_all2_1 <- nrow(all2_vn %>% filter(flag == 1, tt_transactionRevenue > 0))
  name_attrs <- names(all2_vn)
  len_attrs <- length(name_attrs)
  dtypes <- sapply(all2_vn,typeof)
  na_len <- round(sapply(all2_vn, function(x){sum(as.integer(is.na(x)))}) / ct_all2, 5)
  na_len_1 <-  round(sapply(all2_vn %>% filter(flag == 1, tt_transactionRevenue > 0),function(x){sum(as.integer(is.na(x)))}) /  ct_all2_1, 5)
  unique_len <- sapply(all2_vn, function(x){ifelse(typeof(x)=="character", length(unique(x)), NA)})
  ds_each_attr <- tibble(var_name = name_attrs, 
                         DType = dtypes, 
                         NA_prop = na_len, 
                         NA_prop_1 = na_len_1,
                         unq_ct = unique_len)
  ds_each_attr$DType[ds_each_attr$var_name %in% c("tfcS_is_campaign",
                                                  "tfcS_isTrueDirect","tfcS_adwordsClickInfo.page",
                                                  "tfcS_adwordsClickInfo.isVideoAd","tt_newVisits")] <- "bool"
}
#View(ds_each_attr)

# remove some variables
if(1==1){
  all2_vn <- all2_vn %>% select(-dvc_isMobile)
  all2_vn <- all2_vn %>% select(-geo_metro,-geo_region,-geo_continent,-geo_city)
  all2_vn <- all2_vn %>% select(-tfcS_campaign,-tfcS_keyword, -tfcS_adContent, -tfcS_adwordsClickInfo.slot, -tfcS_adwordsClickInfo.adNetworkType)
}

# missing value handling
NA_VALUE <- "UNKOWN"
if(1==1){
  all2_vn$time_last[is.na(all2_vn$time_last)] <- 0
  #all2_vn$geo_continent[is.na(all2_vn$geo_continent)] <- NA_VALUE
  all2_vn$geo_subContinent[is.na(all2_vn$geo_subContinent)] <- NA_VALUE
  all2_vn$geo_country[is.na(all2_vn$geo_country)] <- NA_VALUE
  #all2_vn$geo_region[is.na(all2_vn$geo_region)] <- NA_VALUE
  #all2_vn$geo_metro[is.na(all2_vn$geo_metro)] <- NA_VALUE
  #all2_vn$geo_city[is.na(all2_vn$geo_city)] <- NA_VALUE
  #all2_vn$tfcS_campaign[is.na(all2_vn$tfcS_campaign)] <- NA_VALUE
  all2_vn$tfcS_medium[is.na(all2_vn$tfcS_medium)] <- NA_VALUE
  #all2_vn$tfcS_keyword[is.na(all2_vn$tfcS_keyword)] <- NA_VALUE
  all2_vn$tfcS_referralPath[is.na(all2_vn$tfcS_referralPath)] <- NA_VALUE
  #all2_vn$tfcS_adContent[is.na(all2_vn$tfcS_adContent)] <- NA_VALUE
  #all2_vn$tfcS_adwordsClickInfo.slot[is.na(all2_vn$tfcS_adwordsClickInfo.slot)] <- NA_VALUE
  #all2_vn$tfcS_adwordsClickInfo.adNetworkType[is.na(all2_vn$tfcS_adwordsClickInfo.adNetworkType)] <- NA_VALUE
}

# 减少分类变量的类别
if(1==1){
  weekdays <- all2_vn %>% filter(flag == 1) %>% group_by(weekday) %>% summarise(n = n(), avg_tran = log(mean(tt_transactionRevenue)+1)) %>% arrange(desc(avg_tran))
  #View(weekdays)
  
  channelGroupings<- all2_vn %>% filter(flag == 1) %>% group_by(channelGrouping) %>% summarise(n = n(), avg_tran = log(mean(tt_transactionRevenue)+1)) %>% arrange(desc(avg_tran))
  #View(channelGroupings)
  
  dvc_browsers<- all2_vn %>% filter(flag == 1) %>% group_by(dvc_browser) %>% summarise(n = n(), avg_tran = log(mean(tt_transactionRevenue)+1)) %>% arrange(desc(avg_tran))
  #View(dvc_browsers)
  
  dvc_operatingSystems<- all2_vn %>% filter(flag == 1) %>% group_by(dvc_operatingSystem) %>% summarise(n = n(), avg_tran = log(mean(tt_transactionRevenue)+1)) %>% arrange(desc(avg_tran))
  #View(dvc_operatingSystems)
  
  dvc_deviceCategorys<- all2_vn %>% filter(flag == 1) %>% group_by(dvc_deviceCategory) %>% summarise(n = n(), avg_tran = log(mean(tt_transactionRevenue)+1)) %>% arrange(desc(avg_tran))
  #View(dvc_deviceCategorys)
  
  geo_subContinents<- all2_vn %>% filter(flag == 1) %>% group_by(geo_subContinent) %>% summarise(n = n(), avg_tran = log(mean(tt_transactionRevenue)+1)) %>% arrange(desc(avg_tran))
  #View(geo_subContinents)
  
  geo_countrys <- all2_vn %>% filter(flag == 1) %>% group_by(geo_country) %>% summarise(n = n(), avg_tran = log(mean(tt_transactionRevenue)+1)) %>% arrange(desc(avg_tran))
  #View(geo_countrys)
  all2_vn$geo_country[all2_vn$geo_country %in% geo_countrys$geo_country[geo_countrys$n<100]] <- "Other"
  all2_vn$geo_country[all2_vn$geo_country %in% geo_countrys$geo_country[geo_countrys$avg_tran<0.001]] <- "avg_0"
  
  referralPaths <- all2_vn %>% filter(flag == 1) %>% group_by(tfcS_referralPath) %>% summarise(n = n(), avg_tran = log(mean(tt_transactionRevenue)+1)) %>% arrange(desc(avg_tran))
  #View(referralPaths)
  all2_vn$tfcS_referralPath[all2_vn$tfcS_referralPath %in% referralPaths$tfcS_referralPath[referralPaths$n<100]] <- "Other"
  all2_vn$tfcS_referralPath[all2_vn$tfcS_referralPath %in% referralPaths$tfcS_referralPath[referralPaths$avg_tran<0.001]] <- "avg_0"
  
  
  tfcS_sources <- all2_vn %>% filter(flag == 1) %>% group_by(tfcS_source) %>% summarise(n = n(), avg_tran = log(mean(tt_transactionRevenue)+1)) %>% arrange(desc(avg_tran))
  #View(tfcS_sources)
  all2_vn$tfcS_source[all2_vn$tfcS_source %in% tfcS_sources$tfcS_source[tfcS_sources$n<100]] <- "Other"
  all2_vn$tfcS_source[all2_vn$tfcS_source %in% tfcS_sources$tfcS_source[tfcS_sources$avg_tran<0.001]] <- "avg_0"
  
}







# 访问次数与人数和平均消费的关系 
#View(all2_vn %>% select(visitNumber,tt_transactionRevenue) %>% group_by(visitNumber) %>%  summarise(n = n(), avg_tran = mean(tt_transactionRevenue, na.rm = TRUE)))

# 这里有一个重要的变量需要考虑 'vn_keep', 而且需要不断的调试
#vn_keep <- 13
uf_padding_RNN <- function(all2_vn = all2_vn, vn_keep = 20){
  ###这里有两个问题要注意:
  #'1. padding (Exam中有的记录的tt_transactionRevenue是有值的)
  #'2. > vn_keep的数据的合并
  #e.g. View(all2_vn %>% filter(fullVisitorId == "9377429831454005466") %>% arrange(fullVisitorId,visitNumber))
  
  data_vn_big <- all2_vn %>% filter(visitNumber > vn_keep)   #把vn>vn_keep的记录先存在一边
  all2_vn_keep <- all2_vn %>% filter(visitNumber <= vn_keep)
  all2_vn_keep <- all2_vn_keep %>% arrange(fullVisitorId, visitNumber);  #length(unique(all2_vn_keep$fullVisitorId)) 194181
  unique_users <- all2_vn_keep %>% distinct(fullVisitorId,flag)          #nrow(unique_users) 194181
  #194181 < 194307, 说明有一部分用户只有visitNumber>20的记录
  #View(all2_vn_keep[1:1000,])
  
  sbe_ <- tibble(fullVisitorId = rep(unique_users$fullVisitorId,each = vn_keep + 1),
                 flag = rep(unique_users$flag, each = vn_keep + 1),
                 visitNumber = rep(c(1:(vn_keep + 1)),times = length(unique_users$fullVisitorId))
                 )
  org_ <- tibble(fullVisitorId = all2_vn_keep$fullVisitorId,
                 flag = all2_vn_keep$flag,
                 visitNumber = all2_vn_keep$visitNumber)
  diff_ <- dplyr::setdiff(sbe_,org_) 
  #nrow(org_) + nrow(diff_) == nrow(sbe_)
  
  diff_$tt_transactionRevenue <- ifelse(diff_$flag == 0, NA, 0)
  
  NA_CHAR <- "UNKOWN"
  if(1==1){
    diff_$visitId <- NA
    diff_$visitStartTime <- NA                       
    diff_$dateInt <- NA                             
    diff_$year <- 0                                 
    diff_$month <- 0                               
    diff_$day <- 0                                 
    diff_$hour <- 0                                
    diff_$weekday <- NA_CHAR                              
    diff_$time_last <- 0                          
    diff_$channelGrouping <- NA_CHAR                      
    diff_$dvc_browser <- NA_CHAR                         
    diff_$dvc_operatingSystem <- NA_CHAR                  
    #diff_$dvc_isMobile <- NA                        
    diff_$dvc_deviceCategory <- NA_CHAR                  
    #diff_$geo_continent <- NA_CHAR                       
    diff_$geo_subContinent <- NA_CHAR                     
    diff_$geo_country <- NA_CHAR                         
    #diff_$geo_region <- NA_CHAR                           
    #diff_$geo_metro <- NA_CHAR                           
    #diff_$geo_city <- NA_CHAR                             
    #tfcS_campaign <- NA_CHAR                       
    diff_$tfcS_is_campaign <- 0                    
    diff_$tfcS_source <- NA_CHAR                         
    diff_$tfcS_medium <- NA_CHAR                          
    #diff_$tfcS_keyword <- NA_CHAR                        
    diff_$tfcS_isTrueDirect <- 0                   
    diff_$tfcS_referralPath <- NA_CHAR                   
    #diff_$tfcS_adContent <- NA_CHAR                       
    diff_$tfcS_adwordsClickInfo.page <- 0         
    #diff_$tfcS_adwordsClickInfo.slot <- NA_CHAR           
    #diff_$tfcS_adwordsClickInfo.adNetworkType <- NA_CHAR 
    diff_$tfcS_adwordsClickInfo.isVideoAd <- 0      
    diff_$tt_hits <- 0                             
    diff_$tt_pageviews <- 0                         
    diff_$tt_bounces <- 0                          
    diff_$tt_newVisits <- 0
  }
  
  diff_ <- diff_ %>% select(2,4,1,5,3,6:length(names(diff_)))
  #all(names(diff_) == names(all2_vn_keep))
  
  all2_vn_keep <- union_all(all2_vn_keep, diff_)
  #nrow(all2_vn_keep) == nrow(sbe_)
  
  #padding result check, nrow of following should be 0
  #all2_vn_keep %>% select(fullVisitorId, visitNumber) %>% group_by(fullVisitorId) %>% 
  #summarise(n = n(), sum_vn = sum(visitNumber), min_vn = min(visitNumber), max_vn = max(visitNumber)) %>% 
  #filter(n != vn_keep + 1, sum_vn != sum(1:(vn_keep+1)), min_vn != 1, max_vn != vn_keep + 1)
  
  #处理vn>vn_keep的
  if(1==1){
    data_vn_big <- 
      data_vn_big %>% group_by(fullVisitorId) %>%
      summarise(flag = max(flag),                                 
                tt_transactionRevenue = sum(tt_transactionRevenue),          
                visitId = NA,                              
                visitNumber = vn_keep + 1,                        
                visitStartTime = NA,                       
                dateInt = NA,                             
                year = max(year),                                 
                month = max(month),                               
                day = max(day),                                 
                hour = max(hour),                                
                weekday = max(weekday),                              
                time_last = max(time_last),                          
                channelGrouping = max(channelGrouping),                    
                dvc_browser = max(dvc_browser)  ,                         
                dvc_operatingSystem = max(dvc_operatingSystem)  ,                  
                #dvc_isMobile = NA,                        
                dvc_deviceCategory = max(dvc_deviceCategory)  ,                  
                #geo_continent = NA_CHAR,                       
                geo_subContinent = max(geo_subContinent)  ,                     
                geo_country = max(geo_country)  ,                         
                #geo_region = NA_CHAR,                           
                #geo_metro = NA_CHAR,                           
                #geo_city = max(geo_city)  ,                             
                #tfcS_campaign = NA_CHAR,                       
                tfcS_is_campaign = max(tfcS_is_campaign)  ,                    
                tfcS_source = max(tfcS_source)  ,                         
                tfcS_medium = max(tfcS_medium)  ,                          
                #tfcS_keyword = NA_CHAR,                        
                tfcS_isTrueDirect = max(tfcS_isTrueDirect)  ,                   
                tfcS_referralPath = max(tfcS_referralPath)  ,                   
                #tfcS_adContent = NA_CHAR,                       
                tfcS_adwordsClickInfo.page = max(tfcS_adwordsClickInfo.page)  ,         
                #tfcS_adwordsClickInfo.slot = NA_CHAR,           
                #tfcS_adwordsClickInfo.adNetworkType = NA_CHAR, 
                tfcS_adwordsClickInfo.isVideoAd = max(tfcS_adwordsClickInfo.isVideoAd)  ,      
                tt_hits = sum(tt_hits),                             
                tt_pageviews = sum(tt_pageviews),                         
                tt_bounces = max(3,sum(tt_bounces)),                          
                tt_newVisits = 0)
    
    data_vn_big <- data_vn_big %>% select(2,3,1,4:length(names(data_vn_big)))
    data_vn_big <- data_vn_big %>% filter((fullVisitorId %in% all2_vn_keep$fullVisitorId))
  }
  #nrow(data_vn_big)
  
  #sample
  #View(all2_vn %>% filter(fullVisitorId == "9377429831454005466") %>% arrange(fullVisitorId,visitNumber))
  #View(all2_vn_keep %>% filter(fullVisitorId == "9377429831454005466") %>% arrange(fullVisitorId,visitNumber))
  #View(data_vn_big %>% filter(fullVisitorId == "9377429831454005466"))
  
  # 把data_vn_big中的21更新到all2_vn_keep中
  all2_vn_keep <- all2_vn_keep %>% filter(!(visitNumber == vn_keep + 1 & fullVisitorId %in% data_vn_big$fullVisitorId))
  all2_vn_keep <- union_all(all2_vn_keep, data_vn_big)
  
  
  # 检验all2_vn_keep
  #nrow(all2_vn_keep) == (1+vn_keep) * nrow(unique_users)
  #all2_vn_keep %>% select(fullVisitorId, visitNumber) %>% group_by(fullVisitorId) %>% 
  #summarise(n = n(), sum_vn = sum(visitNumber), min_vn = min(visitNumber), max_vn = max(visitNumber)) %>% 
  #filter(n != vn_keep + 1, sum_vn != sum(1:(vn_keep+1)), min_vn != 1, max_vn != vn_keep + 1)
  
  all2_vn_keep <- all2_vn_keep %>% select(-c("visitId","visitStartTime","dateInt"))
  all2_vn_keep <- all2_vn_keep %>% arrange(flag, fullVisitorId, visitNumber)
  
  return(all2_vn_keep)
}

all2_vn_pad <- uf_padding_RNN(all2_vn,vn_keep)

# 检验all2_vn_keep
all2_vn_pad %>% select(fullVisitorId, visitNumber) %>% group_by(fullVisitorId) %>% summarise(n = n(), sum_vn = sum(visitNumber), min_vn = min(visitNumber), max_vn = max(visitNumber)) %>% filter(n != vn_keep + 1, sum_vn != sum(1:(vn_keep+1)), min_vn != 1, max_vn != vn_keep + 1)
#View(all2_vn_pad[1:100,])
all2_vn_pad_bk <- all2_vn_pad %>% select(flag,tt_transactionRevenue,fullVisitorId,visitNumber)

rm(list = ls()[!(ls() %in% c("all2_vn_pad","all2_vn_pad_bk","vn_keep"))]); gc()
#format(object.size(all2_vn_pad_bk), units = "auto")

# all2_vn_pad 分为Dev和Exam 数据集
Dev <- all2_vn_pad %>% filter(flag == 1)
Exam <- all2_vn_pad %>% filter(flag == 0)
Dev_y <- Dev$tt_transactionRevenue
Exam_y <- Exam$tt_transactionRevenue

rm(list = c("all2_vn_pad")); gc()
#View(Dev[1:100,])

# 去掉不需要的变量
Dev <- Dev %>% select(-c("flag","fullVisitorId","visitNumber"))
Exam <- Exam %>% select(-c("flag","fullVisitorId","visitNumber"))

# vectorization - to matrix
options("na.action"); options(na.action="na.pass"); 
M_Dev_x <- model.matrix(tt_transactionRevenue~.-1, data = Dev) 
rm(list = c("Dev")); gc()
M_Exam_x <- model.matrix(tt_transactionRevenue~.-1, data = Exam)
rm(list = c("Exam")); gc()
options(na.action="na.omit"); options("na.action");
#nrow(M_Dev_x) + nrow(M_Exam_x) == nrow(all2_vn_pad_bk)

# 删除不同时在Dev和Exam里面的列
if(1==1){
  var_list_Dev <- attributes(M_Dev_x)$dimnames[[2]]                         #365
  var_list_Exam <-  attributes(M_Exam_x)$dimnames[[2]]                      #338
  its_DevExam <- dplyr::intersect(var_list_Dev, var_list_Exam)              #338
  saveRDS(M_Exam_x,"./Data/M_Exam_x.rds");rm(list = c("M_Exam_x")); gc()
  M_Dev_x <- M_Dev_x[,-which(!(var_list_Dev %in% its_DevExam))]
  saveRDS(M_Dev_x,"./Data/M_Dev_x.rds");rm(list = c("M_Dev_x")); gc()
  M_Exam_x <- readRDS("./Data/M_Exam_x.rds")
  #M_Exam_x <- M_Exam_x[,-which(!(var_list_Exam %in% its_DevExam))]          #不能执行 (因为-which(!(var_list_Exam %in% its_DevExam))为空)
  M_Dev_x <- readRDS("./Data/M_Dev_x.rds")
  all(attributes(M_Dev_x)$dimnames[[2]] == attributes(M_Exam_x)$dimnames[[2]])       #检验结果为TRUE
  
}

# 因为内存不足的原因，先把Object存起来
#saveRDS(M_Dev_x,"./Data/M_Dev_x.rds");rm(list = c("M_Dev_x")); gc()
#saveRDS(M_Exam_x,"./Data/M_Exam_x.rds");rm(list = c("M_Exam_x")); gc()

############################ 一个一个的处理 M_Dev_x
#M_Dev_x <- readRDS("./Data/M_Dev_x.rds")
attributes(M_Dev_x)$dimnames <- NULL;
attributes(M_Dev_x)$assign <- NULL
attributes(M_Dev_x)$contrasts <- NULL

str(Dev_y)
dim(M_Dev_x); #str(M_Dev_x); class(M_Dev_x);M_Dev_x[1,];attributes(M_Dev_x)

#normalization
if(1==1){
  wide <- dim(M_Dev_x)[[2]]
  mean_sds <- tibble(varID = integer(0), mean = numeric(0), sd = numeric(0))
  for(j in 1:wide){
    print(j)
    var_unq <- unique(M_Dev_x[,j])
    var_unq <- sort(var_unq[!is.na(var_unq)])
    if(any(is.na(M_Dev_x[,j]))){
      M_Dev_x[is.na(M_Dev_x[,j]),j] <- 0        #set NA to 0
    }
    
    if(any(var_unq != c(0,1))){
      mean_sds <- union_all(mean_sds, tibble(varID = j,
                                             mean = base::mean(M_Dev_x[,j], na.rm = T),
                                             sd = sd(M_Dev_x[,j], na.rm = T)))
    }
  }
  scale_len <- nrow(mean_sds)
  for(k in 1:scale_len){
    i_var <- as.integer(mean_sds[k,1])  
    M_Dev_x[,i_var] <- scale(M_Dev_x[,i_var],center = as.double(mean_sds[k,2]) , scale = as.double(mean_sds[k,3])) 
    #M_valid_x[,i_var] <- scale(M_valid_x[,i_var],center = as.double(mean_sds[k,2]) , scale = as.double(mean_sds[k,3])) 
  }
}

#reshape
if(1==1){
  rm(list = ls()[!(ls() %in% c("all2_vn_pad_bk","Dev_y","M_Dev_x","vn_keep"))]); gc()
  len_series <- vn_keep + 1
  str(M_Dev_x)
  M_Dev_x <- keras::array_reshape(M_Dev_x,dim = c(dim(M_Dev_x)[[1]] / len_series,len_series,dim(M_Dev_x)[[2]])); gc()
  str(M_Dev_x)
}

#View(M_Dev_x[1,,])


## 将Dev数据集分割成train, val, test (方案一, after reshape)
if(1==1){
  unique_users <- 
    all2_vn_pad_bk %>% filter(flag == 1) %>% group_by(fullVisitorId) %>% 
    summarise(sum_tran = sum(tt_transactionRevenue)) %>%
    mutate(is_Tran = ifelse(sum_tran > 0, 1, 0))
  table(unique_users$is_Tran)
  
  set.seed(1000)
  str(M_Dev_x)
  
  total_seq <- c(1:dim(M_Dev_x)[[1]])
  seq_lb <- unique_users$is_Tran; table(seq_lb)
  lb0_seq <- total_seq[seq_lb == 0]
  lb1_seq <- total_seq[seq_lb == 1]
  len_lb0_seq <- length(lb0_seq)
  len_lb1_seq <- length(lb1_seq)
  
  splt_rate <- c(0.65,0.2,0.15)
  attr(splt_rate,"setType") <- c("train","val","test")
  attr(splt_rate,"type_len_0") <- c(round(len_lb0_seq * splt_rate) - 1)
  attr(splt_rate,"type_len_1") <- c(round(len_lb1_seq * splt_rate) - 1)
  
  lb0_seq_suffle <- lb0_seq[sample(1:len_lb0_seq,size = len_lb0_seq, replace = F)]
  lb1_seq_suffle <- lb1_seq[sample(1:len_lb1_seq,size = len_lb1_seq, replace = F)]
  
  train_seq <- c(lb0_seq_suffle[1:attr(splt_rate,"type_len_0")[1]], lb1_seq_suffle[1:attr(splt_rate,"type_len_1")[1]])
  val_seq <- c(lb0_seq_suffle[(attr(splt_rate,"type_len_0")[1]+1):(sum(attr(splt_rate,"type_len_0")[1:2]))],lb1_seq_suffle[(attr(splt_rate,"type_len_1")[1]+1):(sum(attr(splt_rate,"type_len_1")[1:2]))])
  test_seq <- c(lb0_seq_suffle[(sum(attr(splt_rate,"type_len_0")[1:2])+1):(sum(attr(splt_rate,"type_len_0")[1:3]))],lb1_seq_suffle[(sum(attr(splt_rate,"type_len_1")[1:2])+1):(sum(attr(splt_rate,"type_len_1")[1:3]))])
  
  train <- M_Dev_x[train_seq,,]
  val <- M_Dev_x[val_seq,,]
  test <- M_Dev_x[test_seq,,]
  train_y <- Dev_y[rep(train_seq, each = len_series)]
  val_y <- Dev_y[rep(val_seq, each = len_series)]
  test_y <- Dev_y[rep(test_seq, each = len_series)]
  #dim(train)[[1]] + dim(val)[[1]] + dim(test)[[1]] - dim(M_Dev_x)[[1]]
  #length(train_y) + length(val_y) + length(test_y) - length(Dev_y)
  #nrow(train); length(train_y) / 11
}

## 将Dev数据集分割成train, val, test (方案二, before reshape)
if(1!=1){
  #dim(M_Dev_x)
  unique_users <- 
    all2_vn_pad_bk %>% filter(flag == 1) %>% group_by(fullVisitorId) %>% 
    summarise(sum_tran = sum(tt_transactionRevenue)) %>%
    mutate(is_Tran = ifelse(sum_tran > 0, 1, 0))
  table(unique_users$is_Tran)
  
  set.seed(1000)
  
  total_seq <- c(1:nrow(unique_users))
  seq_lb <- unique_users$is_Tran; table(seq_lb)
  lb0_seq <- total_seq[seq_lb == 0]
  lb1_seq <- total_seq[seq_lb == 1]
  len_lb0_seq <- length(lb0_seq)
  len_lb1_seq <- length(lb1_seq)
  
  splt_rate <- c(0.65,0.2,0.15)
  attr(splt_rate,"setType") <- c("train","val","test")
  attr(splt_rate,"type_len_0") <- c(round(len_lb0_seq * splt_rate) - 1)
  attr(splt_rate,"type_len_1") <- c(round(len_lb1_seq * splt_rate) - 1)
  
  lb0_seq_suffle <- lb0_seq[sample(1:len_lb0_seq,size = len_lb0_seq, replace = F)]
  lb1_seq_suffle <- lb1_seq[sample(1:len_lb1_seq,size = len_lb1_seq, replace = F)]
  
  train_seq <- c(lb0_seq_suffle[1:attr(splt_rate,"type_len_0")[1]], lb1_seq_suffle[1:attr(splt_rate,"type_len_1")[1]])
  val_seq <- c(lb0_seq_suffle[(attr(splt_rate,"type_len_0")[1]+1):(sum(attr(splt_rate,"type_len_0")[1:2]))],lb1_seq_suffle[(attr(splt_rate,"type_len_1")[1]+1):(sum(attr(splt_rate,"type_len_1")[1:2]))])
  test_seq <- c(lb0_seq_suffle[(sum(attr(splt_rate,"type_len_0")[1:2])+1):(sum(attr(splt_rate,"type_len_0")[1:3]))],lb1_seq_suffle[(sum(attr(splt_rate,"type_len_1")[1:2])+1):(sum(attr(splt_rate,"type_len_1")[1:3]))])
  
  Dev_seq <- ceiling(c(1:dim(M_Dev_x)[[1]]) / (vn_keep + 1))
  
  train <- M_Dev_x[Dev_seq %in% train_seq,]
  val <- M_Dev_x[Dev_seq %in% val_seq,]
  test <- M_Dev_x[Dev_seq %in% test_seq,]
  train_y <- Dev_y[Dev_seq %in% train_seq]; 
  val_y <- Dev_y[Dev_seq %in% val_seq]
  test_y <- Dev_y[Dev_seq %in% test_seq]
  #dim(train)[[1]] + dim(val)[[1]] + dim(test)[[1]] - dim(M_Dev_x)[[1]]
  #length(train_y) + length(val_y) + length(test_y) - length(Dev_y)
  #nrow(train); length(train_y) / 11
}

rm(list=ls()[!(ls() %in% c("train","val","test","train_y","val_y","test_y","vn_keep"))]);gc()
#rm(list=ls()[!(ls() %in% c("M_Dev_x","Dev_y","vn_keep"))]);gc()




################################ 一个一个的处理 M_Exam_x
#normalization
if(1==1){
  wide <- dim(M_Dev_x)[[2]]
  mean_sds <- tibble(varID = integer(0), mean = numeric(0), sd = numeric(0))
  for(j in 1:wide){
    print(j)
    var_unq <- unique(M_Dev_x[,j])
    var_unq <- sort(var_unq[!is.na(var_unq)])
    if(any(is.na(M_Dev_x[,j]))){
      M_Dev_x[is.na(M_Dev_x[,j]),j] <- 0        #set NA to 0
    }
    
    if(any(var_unq != c(0,1))){
      mean_sds <- union_all(mean_sds, tibble(varID = j,
                                             mean = base::mean(M_Dev_x[,j], na.rm = T),
                                             sd = sd(M_Dev_x[,j], na.rm = T)))
    }
  }
  scale_len <- nrow(mean_sds)
  for(k in 1:scale_len){
    i_var <- as.integer(mean_sds[k,1])  
    M_Exam_x[,i_var] <- scale(M_Exam_x[,i_var],center = as.double(mean_sds[k,2]) , scale = as.double(mean_sds[k,3])) 
    #M_valid_x[,i_var] <- scale(M_valid_x[,i_var],center = as.double(mean_sds[k,2]) , scale = as.double(mean_sds[k,3])) 
  }
}

rm(list = c("M_Dev_x","Dev_y")); gc()

attributes(M_Exam_x)$dimnames <- NULL;
attributes(M_Exam_x)$assign <- NULL
attributes(M_Exam_x)$contrasts <- NULL

str(Exam_y)
dim(M_Exam_x); 
#M_Exam_x[1,]

#reshape
if(1==1){
  rm(list = ls()[!(ls() %in% c("all2_vn_pad_bk","Exam_y","M_Exam_x","vn_keep"))]); gc()
  len_series <- vn_keep + 1
  str(M_Exam_x)
  M_Exam_x <- keras::array_reshape(M_Exam_x,dim = c(dim(M_Exam_x)[[1]] / len_series,len_series,dim(M_Exam_x)[[2]])); gc()
  str(M_Exam_x)
}



############################################################2.2. RNN Model############################################################
# generator (方案一, after reshape)
generator <- function(data,y, max_index = NULL, batch_size = 64, len_series = 5,shuffle = FALSE){
  if(is.null(max_index)){
    max_index <- nrow(data)
  }
  max_bn <- round(max_index / batch_size) - 1
  bn <- 1
  
  function(){
    if(bn > max_bn){bn <<- 1}
    
    samples <- data[((bn-1)*batch_size+1):(bn*batch_size),,]; 
    
    if(1!=1){
      vv <- y[((bn-1)*batch_size*len_series+1):(bn*batch_size*len_series)]
      seq_vv <- seq_along(vv)
      targets <- tapply(vv,rep(seq_vv,each=len_series)[seq_vv],FUN=sum)
      targets <- log(targets+1)
    }
    
    if(1==1){
      df_targ <- tibble(targets_flat = y[((bn-1)*batch_size*len_series+1):(bn*batch_size*len_series)],
                        bn_i = rep(c(1:batch_size), each = len_series))
      df_targ_1 <- df_targ %>% group_by(bn_i) %>% summarise(targets = sum(targets_flat)) %>% arrange(bn_i)
      targets <- log(df_targ_1$targets+1)
    }
    
    
    bn <<- bn + 1
    
    list(samples, targets)
  }
  
}

# generator (方案二, before reshape)
generator <- function(data,y, max_index = NULL, batch_size = 16, len_series = 15,shuffle = FALSE){
  if(is.null(max_index)){
    max_index <- nrow(data) / len_series
  }
  max_bn <- floor(max_index / batch_size)
  bn <- 1
  
  function(){
    if(bn > max_bn){bn <<- 1}
    
    samples <- data[((bn-1)*batch_size*len_series+1):(bn*batch_size*len_series),]
    samples <- keras::array_reshape(samples,dim = c(dim(samples)[[1]] / len_series,len_series,dim(samples)[[2]]))
    
    df_targ <- tibble(targets_flat = y[((bn-1)*batch_size*len_series+1):(bn*batch_size*len_series)],
                      bn_i = rep(c(1:batch_size), each = len_series))
    df_targ_1 <- df_targ %>% group_by(bn_i) %>% summarise(targets = sum(targets_flat)) %>% arrange(bn_i)
    
    targets <- log(df_targ_1$targets+1)
    
    bn <<- bn + 1
    
    list(samples, targets)
  }
  
}

len_series = vn_keep + 1
batch_size = 16
train_gen <- generator(train,
                       train_y,
                       batch_size = batch_size,
                       len_series = len_series)
val_gen <- generator(val,
                     val_y,
                     batch_size = batch_size,
                     len_series = len_series)
test_gen <- generator(test,
                      test_y,
                     batch_size = batch_size,
                     len_series = len_series)

# for 2d
val_steps <- floor(dim(val)[[1]] / len_series /batch_size)
train_steps <- floor(dim(train)[[1]] / len_series / batch_size)
test_steps <- floor(dim(test)[[1]] / len_series / batch_size)

# for 3d
val_steps <- floor(dim(val)[[1]] /batch_size)
train_steps <- floor(dim(train)[[1]] / batch_size)
test_steps <- floor(dim(test)[[1]]  / batch_size)


# model2
model2 <- keras_model_sequential() %>%
  layer_gru(units = 32, 
            dropout = 0.1,
            recurrent_dropout = 0.3,
            return_sequences = TRUE,
            input_shape = list(NULL, dim(train)[[length(dim(train))]])) %>%
  layer_gru(units = 32, 
            activation = "relu",
            dropout = 0.1,
            recurrent_dropout = 0.3) %>%
  layer_dense(units = 1)

summary(model2)

model2 %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mse"
)

history2 <- model2 %>% fit_generator(
  train_gen,
  steps_per_epoch = train_steps,
  epochs = 3,
  validation_data = val_gen,
  validation_steps = val_steps
)

plot(history2)
summary(history2)

# predict val
pred_val <- model2 %>% predict(val)
str(val)
str(pred_val)
val_y_tb <- tibble(val_y = val_y,
                    bn_i = rep(c(1:dim(val)[[1]]), each = len_series))
val_y_log <- val_y_tb %>% group_by(bn_i) %>% summarise(val_y = sum(val_y)) %>% arrange(bn_i)
dim(val_y_log)
val_y_act <- log(val_y_log$val_y + 1)
sum((val_y_act - pred_val)^2)/length(val_y_act)


# predict test
pred_test <- model2 %>% predict(test)
str(test)
str(pred_test)
test_y_tb <- tibble(test_y = test_y,
                   bn_i = rep(c(1:dim(test)[[1]]), each = len_series))
test_y_log <- test_y_tb %>% group_by(bn_i) %>% summarise(test_y = sum(test_y)) %>% arrange(bn_i)
dim(test_y_log)
test_y_act <- log(test_y_log$test_y + 1)
sqrt(sum((test_y_act - pred_test)^2)/length(test_y_act))

# save model
keras::save_model_hdf5(model2,"./model/model_20181107_rnn.hdf5")




############################################################2.3. Exam Predict############################################################
# predict Exam
str(M_Exam_x)
model2 <- keras::load_model_hdf5("./model/model_20181107_rnn.hdf5")

pred_Exam <- model2 %>% predict(M_Exam_x)
str(M_Exam_x)
str(pred_Exam)

nrow(all2_vn_pad_bk %>% filter(flag == 0)) / (vn_keep + 1) == length(pred_Exam)

all2_vn_pred <- tibble(fullVisitorId = all2_vn_pad_bk$fullVisitorId[all2_vn_pad_bk$flag == 0],
                       PredictedLogRevenue = rep(pred_Exam, each = vn_keep + 1))
all2_vn_pred <- all2_vn_pred %>% distinct(fullVisitorId,PredictedLogRevenue)
nrow(all2_vn_pred) == length(pred_Exam)

write_csv(all2_vn_pred,"./data/predRlt_vn_20181107.csv")


############################################################3. Combine Prediction############################################################
all2_Exam <- read_csv("./data/test.csv"); 
all2_Exam <- all2_Exam %>% distinct(fullVisitorId)

all2_vn_pred <- read_csv("./data/predRlt_vn_20181107.csv")
all2_v1_pred <- read_csv("./data/predRlt_v1_20181107.csv")
dplyr::intersect(all2_vn_pred$fullVisitorId, all2_v1_pred$fullVisitorId)
all2_pred <- union_all(all2_vn_pred,all2_v1_pred)
all2_miss_pred <- tibble(fullVisitorId = all2_Exam$fullVisitorId[!(all2_Exam$fullVisitorId %in% all2_pred$fullVisitorId)],
                         PredictedLogRevenue = 0)
all2_pred <- union_all(all2_pred,all2_miss_pred)

nrow(all2_pred) == nrow(all2_Exam)

write_csv(all2_pred,"./data/predRlt_20181107.csv")




