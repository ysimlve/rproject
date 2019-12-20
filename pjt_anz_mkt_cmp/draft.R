getwd()
# Load librayies
library(tidyverse)
library(xgboost)
library(keras)
library(scales)
library(Rmisc)
library(ggalluvial)
library(Matrix)
library(magrittr)

# Load data
dev <- read_csv("./data/bank-additional-train.csv")
str(dev)
head(dev)

# Global variables and functions
anz_color1 <- "dodgerblue4"
anz_color2 <- "deepskyblue2"

is_na_val <- function(x) ifelse(x == "unknown", T, F)

uf_gridsearch_xgb <- function(data, fold = 10){
  rlt <- tibble(id = 0,
                eta = 0,
                gamma = 0,
                max_depth = 0,
                min_child_weight = 0,
                max_delta_step = 0,
                subsample = 0,
                colsample_bytree = 0,
                colsample_bylevel = 0,
                lambda = 0,
                scale_pos_weight = 0,
                best_iteration = 0,
                best_ntreelimit = 0,
                niter = 0,
                train_error_mean = 0,
                train_error_std = 0,
                test_error_mean = 0,
                test_error_std= 0)[0]
  
  eta <- c(0.01,0.05) #c(0.01,0.03,0.05,0.1);                
  len_eta <- length(eta)
  gamma <- c(0);                               
  len_gamma <- length(gamma)
  max_depth <- c(4)#c(4,5,6,7,8,9);                 
  len_max_depth <- length(max_depth)
  min_child_weight <- c(5,6)#c(1,2,3,4,5,6,7);        
  len_min_child_weight <- length(min_child_weight)
  max_delta_step <- 0;                       
  subsample <- c(.8,.7)#c(1,.9,.8,.7,.6,.5);            
  len_subsample <- length(subsample)
  colsample_bytree <- c(.8)#c(1,.9,.8,.7,.6,.5);     
  len_colsample_bytree <- length(colsample_bytree)
  colsample_bylevel <- c(.8,.9) #c(1,.9,.8,.7,.6,.5);    
  len_colsample_bylevel <- length(colsample_bylevel)
  lambda <- c(1,5)#c(0,5,10,15,20);                           
  len_lambda <- length(lambda)
  scale_pos_weight <- c(1,8)#c(1,.4,.5,.6,.7,.8);     
  len_scale_pos_weight <- length(scale_pos_weight)
  
  total_loop <- len_eta*len_gamma*len_max_depth*len_min_child_weight*len_subsample*len_colsample_bytree*len_colsample_bylevel*len_lambda
  cat("totla loop: ", total_loop, "   ")
  
  loop_i <- 0
  for(e in 1:len_eta){
    for(g in 1:len_gamma){
      for(md in 1:len_max_depth){
        for(mcw in 1:len_min_child_weight){
          for(ss in 1:len_subsample){
            for(cbt in 1:len_colsample_bytree){
              for(cbl in 1:len_colsample_bylevel){
                for(l in 1:len_lambda){
                  for(spw in 1:len_scale_pos_weight){
                    
                    cat("Round ", loop_i + 1, " / ",total_loop)
                    
                    hp <- list(
                      seed = 0,
                      silent = 1,
                      nthread = 2,
                      booster = "gbtree",
                      objective = "binary:logistic",
                      eval_metric = "auc",
                      
                      eta = eta[e],
                      
                      gamma = gamma[g],
                      max_depth = max_depth[md],
                      min_child_weight = min_child_weight[mcw],
                      max_delta_step = 0,
                      
                      subsample = subsample[ss],
                      colsample_bytree = colsample_bytree[cbt],
                      colsample_bylevel = colsample_bylevel[cbl],
                      
                      lambda = lambda[l],
                      alpha = 0,
                      
                      scale_pos_weight = scale_pos_weight[spw]
                    )
                    
                    fit.cv <- xgb.cv(
                      hp,
                      data,
                      missing = NA,
                      nrounds = 100,
                      nfold = fold,
                      prediction = TRUE,
                      showsd = TRUE,
                      stratified = TRUE,
                      verbose = FALSE,
                      print_every_n = 1,
                      early_stopping_rounds = 10
                    )
                    
                    rlt <- union_all(
                      rlt,
                      tibble(id = loop_i + 1,
                             eta = eta[e],
                             gamma = gamma[g],
                             max_depth = max_depth[md],
                             min_child_weight = min_child_weight[mcw],
                             max_delta_step = 0,
                             subsample = subsample[ss],
                             colsample_bytree = colsample_bytree[cbt],
                             colsample_bylevel = colsample_bylevel[cbl],
                             lambda = lambda[l],
                             scale_pos_weight = scale_pos_weight[spw],
                             best_iteration = fit.cv$best_iteration,
                             best_ntreelimit = fit.cv$best_ntreelimit,
                             niter = fit.cv$niter,
                             train_error_mean = fit.cv$evaluation_log[fit.cv$best_iteration,]$train_auc_mean,
                             train_error_std = fit.cv$evaluation_log[fit.cv$best_iteration,]$train_auc_std,
                             test_error_mean = fit.cv$evaluation_log[fit.cv$best_iteration,]$test_auc_mean,
                             test_error_std= fit.cv$evaluation_log[fit.cv$best_iteration,]$test_auc_std)
                    )
                    loop_i <- loop_i + 1
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  cat("Grid search done")
  return(rlt)
}

# Distribution of target variables (Label)
dev %>% select(y) %>% 
  group_by(y) %>% dplyr::count() %>%
  ggplot(aes(x = y, y =n)) + 
  geom_bar(stat = "identity", fill = anz_color1, width = .4) +
  labs(x = "Bank term deposit be subscribed", y = "count", title = "Distribution of target variable values") +
  theme_light() +
  scale_y_continuous(labels = comma) + 
  geom_text(aes(label = n),vjust = 1.6, colour = "white", size = 4) +
  geom_text(aes(label = ifelse(y == "yes","Only 12.6% of all samples with label 'yes'", "")), 
            colour = "red", vjust = -5, size = 5)
  
dev$y <- ifelse(dev$y == "no", 0, 1)
table(dev$y)

# Missing value check
dev %>% summarise_all(funs(sum(is_na_val(.))/n() * 100)) %>%  
  gather(key = "feature", value = missing_pct) %>%
  ggplot(aes(x = reorder(feature, missing_pct), y = missing_pct)) + 
  geom_bar(stat = "identity", fill = anz_color1) +
  labs(y = "missing %", x = "feature") +
  coord_flip() + 
  theme_minimal()


# Duplication check
dev$md5key <- apply(dev %>% select(-one_of("y")), 1, function(x){openssl::md5(str_c(x,collapse = ""))}) #generate 'md5' key for each sample
dev_dup <- dev %>% group_by(md5key) %>% dplyr::count() %>% filter(n > 1)            # find out duplicate records
if(nrow(dev %>% filter(md5key %in% dev_dup$md5key) %>% select(md5key,y) %>%         # make sure duplicate records has same 'y'
        group_by(md5key) %>% dplyr::summarise(sum_y = sum(y)) %>% filter(sum_y == 1)) == 0){
  cat("There are ", nrow(dev_dup), " records have duplicate, but none of them has different 'y'")
}
dev %>% filter(md5key == "4bd6f9da6febbb8a4bb4206f0207020b")

# Time series check
time_series_check <- function(dt, duration){
  dt$seq <- c(1:nrow(dt))
  dt$seq_duration <- ceiling(dt$seq / duration) 
  agv_deal <- floor(mean((dt %>% group_by(seq_duration) %>% dplyr::summarise(sum_y = sum(y)))$sum_y))
  dt %>% 
    group_by(seq_duration) %>% 
    dplyr::summarise(sum_y = sum(y)) %>% 
    ggplot(aes(x = seq_duration, y = sum_y)) +
    geom_line(size = 1, colour = anz_color2) +
    labs(x = "period", y = "# of term deposit") +
    geom_smooth(method = "loess", colour = anz_color1, size = 1.2) + 
    geom_hline(yintercept = agv_deal, colour = "red", size = 1, linetype = 2) +
    theme_minimal() + 
    theme(plot.margin = unit(c(0,0,0,0),"cm"))
}

time_series_check(dev, 100)

# EDA -Age
p1 <- 
  dev %>% 
  ggplot(aes(x = age)) + 
  geom_histogram(binwidth = 5, fill = anz_color1) +
  labs(x = "customer age", title = "Distribution of customer age") +
  geom_vline(xintercept = c(25,55), colour = "red1", size = .7) +
  theme_minimal()
p2 <- 
  dev %>% 
  ggplot(aes(x = age, fill = as.factor(y))) + 
  geom_histogram(binwidth = 5, position = "fill") +
  labs(x = "customer age", y = "rate",title = "Correleation between age & y") +
  geom_vline(xintercept = c(25,55), colour = "red1", size = .7) +
  scale_fill_manual(values=c(anz_color2, anz_color1), name = "y") +
  theme_minimal()

multiplot(p1,p2,cols = 2)

# Alluvial diagram for categrical variables 
(catVars <- 
    tibble(var_name = c("job","marital","education","default","housing","loan","contact"),
           nm_unique_values = sapply(dev %>% select(c("job","marital","education","default","housing","loan","contact")),
                                     function(x){length(unique(x))}),
           chi_test_pValue = c(chisq.test(xtabs(~y+job, data = dev), correct = T)$p.value,
                               chisq.test(xtabs(~y+marital, data = dev), correct = T)$p.value,
                               chisq.test(xtabs(~y+education, data = dev), correct = T)$p.value,
                               chisq.test(xtabs(~y+default, data = dev), correct = T)$p.value,
                               chisq.test(xtabs(~y+housing, data = dev), correct = T)$p.value,
                               chisq.test(xtabs(~y+loan, data = dev), correct = T)$p.value,
                               chisq.test(xtabs(~y+contact, data = dev), correct = T)$p.value)) %>%
    arrange(chi_test_pValue))


dev %>% 
  select(c("job","contact","default","marital")) %>%
  dplyr::mutate_all(factor) %>%
  dplyr::mutate_all(fct_lump,4) %>%
  bind_cols(tibble(deposit = ifelse(dev$y == 0, "no","yes") %>% factor)) %>%
  na.omit() %>%
  group_by_all() %>%
  dplyr::count() %>%
  ggplot(aes(y = n, 
             axis1 = job, axis2 = contact, axis3 = default,
             axis5 = marital)) +
  geom_alluvium(aes(fill = deposit), width = 1/12) +
  scale_fill_manual(values=c("gray77", anz_color1), name = "y") +
  geom_stratum(width = 1/20, fill = "black", color = "grey") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:4, labels = c("job", "contact", "default",
                                              "marital")) +
  theme(legend.position = "none")


# Correlation analysis
cor_numVar <- stats::cor(dev %>% 
                           select(one_of(c("emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed"))),
                         use = "pairwise.complete.obs")

cor_sorted <- as.matrix(sort(cor_numVar[,'euribor3m'], decreasing = TRUE))

CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.1)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot::corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt",mar = c(0, 0,0, 0))






# split data into train,validation(k-fold),test
set.seed(1000)
# split into 'train_val' & 'test'
total_seq <- c(1:nrow(dev))
seq_lb <- dev$y
lb0_seq <- total_seq[seq_lb == 0]
lb1_seq <- total_seq[seq_lb == 1]
len_lb0_seq <- length(lb0_seq)
len_lb1_seq <- length(lb1_seq)

splt_rate <- c(0.8,0.2)
attr(splt_rate,"setType") <- c("train_val","test")
attr(splt_rate,"type_len_0") <- c(floor(len_lb0_seq * splt_rate))
attr(splt_rate,"type_len_1") <- c(floor(len_lb1_seq * splt_rate))

lb0_seq_suffle <- lb0_seq[sample(1:len_lb0_seq,size = len_lb0_seq, replace = F)]
lb1_seq_suffle <- lb1_seq[sample(1:len_lb1_seq,size = len_lb1_seq, replace = F)]

train_val_seq <- c(lb0_seq_suffle[1:attr(splt_rate,"type_len_0")[1]], lb1_seq_suffle[1:attr(splt_rate,"type_len_1")[1]])
train_val_seq <- train_val_seq[sample(1:length(train_val_seq),size = length(train_val_seq), replace = F)]
test_seq <- c(lb0_seq_suffle[(attr(splt_rate,"type_len_0")[1]+1):(sum(attr(splt_rate,"type_len_0")[1:2]))],lb1_seq_suffle[(attr(splt_rate,"type_len_1")[1]+1):(sum(attr(splt_rate,"type_len_1")[1:2]))])
test_seq <- test_seq[sample(1:length(test_seq),size = length(test_seq), replace = F)]

cat("There are ", length(train_val_seq), " rows for train/val, and ", length(test_seq), " rows for test.")



# Model - XGBoost
dev_xgb <- dev %>% select(-md5key)
dev_xgb %<>% mutate_all(funs(ifelse(is_na_val(.), NA, .)))   #replace 'unknown' to NA
options(na.action="na.pass"); 
spM_Dev_x <- sparse.model.matrix(y~.-1, data = dev_xgb, na.action = "na.pass")
options(na.action="na.omit");
label_Dev <- dev_xgb$y

# split dataset 'dev_xgb' into train, val(k-fold), test

spM_train_val_x <- spM_Dev_x[train_val_seq,]
spM_test_x <- spM_Dev_x[test_seq,]
label_train_val <- label_Dev[train_val_seq]
label_test <- label_Dev[test_seq]

# generate k-folder validation dataset
set.seed(2000)
total_seq <- c(1:length(label_train_val))
seq_lb <- label_train_val
lb0_seq <- total_seq[seq_lb == 0]
lb1_seq <- total_seq[seq_lb == 1]
len_lb0_seq <- length(lb0_seq)
len_lb1_seq <- length(lb1_seq)

splt_rate <- c(0.8,0.2)
attr(splt_rate,"setType") <- c("train","val")
attr(splt_rate,"type_len_0") <- c(floor(len_lb0_seq * splt_rate))
attr(splt_rate,"type_len_1") <- c(floor(len_lb1_seq * splt_rate))

lb0_seq_suffle <- lb0_seq[sample(1:len_lb0_seq,size = len_lb0_seq, replace = F)]
lb1_seq_suffle <- lb1_seq[sample(1:len_lb1_seq,size = len_lb1_seq, replace = F)]

train_seq <- c(lb0_seq_suffle[1:attr(splt_rate,"type_len_0")[1]], lb1_seq_suffle[1:attr(splt_rate,"type_len_1")[1]])
train_seq <- train_seq[sample(1:length(train_seq),size = length(train_seq), replace = F)]
val_seq <- c(lb0_seq_suffle[(attr(splt_rate,"type_len_0")[1]+1):(sum(attr(splt_rate,"type_len_0")[1:2]))],lb1_seq_suffle[(attr(splt_rate,"type_len_1")[1]+1):(sum(attr(splt_rate,"type_len_1")[1:2]))])
val_seq <- val_seq[sample(1:length(val_seq),size = length(val_seq), replace = F)]

spM_train_x <- spM_train_val_x[train_seq,]
spM_val_x <- spM_train_val_x[val_seq,]
label_train <- label_train_val[train_seq]
label_val <- label_train_val[val_seq]

dtrain_val <- xgb.DMatrix(data = spM_train_val_x, label = label_train_val, missing = NA)
cat("The infor of train dataset - ")
((dtrain <- xgb.DMatrix(data = spM_train_x, label = label_train, missing = NA)))
cat("The infor of validation dataset - ")
(dval <- xgb.DMatrix(data = spM_val_x, label = label_val, missing = NA))
cat("The infor of test dataset - ")
(dtest <- xgb.DMatrix(data = spM_test_x, label = label_test, missing = NA))

# Build Model - grid search
gs_rlt <- uf_gridsearch_xgb(dtrain_val,5)
View(gs_rlt)

(gs_rlt_top10 <- gs_rlt %>% filter(!is.na(eta)) %>% 
    select(test_error_mean,test_error_std,train_error_mean,train_error_std,2:12) %>% 
    dplyr::arrange((test_error_mean+test_error_std)) %>% top_n(10))

# Train the xgb model with selected parameters
xgb.params <- list(seed = 0,
                   silent = 1,
                   nthread = 2,
                   booster = "gbtree",
                   objective = "binary:logistic",
                   eval_metric = "auc",
                   
                   eta = 0.05,
                   
                   gamma = 0,
                   max_depth = 4,
                   min_child_weight = 5,
                   max_delta_step = 0,
                   
                   subsample = .8,
                   colsample_bytree = .8,
                   colsample_bylevel = .7,
                   
                   lambda = 20,
                   alpha = 0,
                   
                   scale_pos_weight = 1)
fit.xgb <- xgb.train(
  params = xgb.params,
  data = dtrain,
  watchlist = list(train = dtrain, val = dval),
  print_every_n = 100,
  early_stopping_rounds = 100,
  nrounds = 1000,
  verbose = TRUE)

# Check the information of trained xgb model
cat("The best score(val_error) is: ", fit.xgb$best_score, "; and the best iteration is: ", fit.xgb$best_iteration)

iter.error <-  data.frame(iter = rep(1:fit.xgb$niter,time=2),
                          error_set = rep(c("train","val"),each=fit.xgb$niter),
                          error_p0 = c(fit.xgb$evaluation_log$train_auc,fit.xgb$evaluation_log$val_auc))
ggplot(data = iter.error, aes(x = iter, y = error_p0, colour = error_set)) + 
  geom_line(size = 1) + 
  labs(x = "iteration", y = "error") +
  geom_vline(xintercept = fit.xgb$best_iteration, colour = anz_color1, size = .7) +
  geom_text(aes(label = ifelse(iter == fit.xgb$best_iteration & error_set == "val","trade-off point of bias and variance","")),size = 3, vjust = 1, hjust = 1, colour = "black") +
  theme(legend.position = "none") +
  theme_minimal()

# Check the importance of variables
impt.xgb <- xgb.importance(feature_names = fit.xgb$feature_names, model = fit.xgb)
xgb.ggplot.importance(importance_matrix = impt.xgb,top_n = min(30,fit.xgb$nfeatures), measure =  "Gain")

# Validate on 'test' dataset
pred_test_xgb <- predict(fit.xgb, dtest, ntreelimit = fit.xgb$best_iteration)
pred_test_bin_xgb <- ifelse(pred_test_xgb > .5, 1, 0)
conf_mtx <- table(pred_test_bin_xgb, label_test)
acc_xgb <- round(sum(conf_mtx[1,1]+conf_mtx[2,2]) / length(pred_test_bin_xgb),4)
conf_mtx

cat("The predict accuracy is: ", acc_xgb)

# Model - Nueral network (TensorFlow/Keras)
dev$log_duration <- log1p(dev$duration) 
dev_keras <- dev %>% select(-md5key)


# normalization
dev_nor <- dev_keras %>% select(one_of(c("age","campaign","pdays","previous","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed","log_duration","duration")))
means <- apply(dev_nor, 2, mean)
sds <- apply(dev_nor, 2, sd)
dev_nor <- scale(dev_nor, center = means, scale = sds)
dev_keras %<>% dplyr::mutate(age = dev_nor[,1],
                             campaign = dev_nor[,2],
                             pdays = dev_nor[,3],
                             previous = dev_nor[,4],
                             emp.var.rate = dev_nor[,5],
                             cons.price.idx = dev_nor[,6],
                             cons.conf.idx = dev_nor[,7],
                             euribor3m = dev_nor[,8],
                             nr.employed = dev_nor[,9],
                             log_duration = dev_nor[,10],
                             duration = dev_nor[,11])
# vectorization - to matrix
options(na.action="na.pass"); 
M_Dev_x <- model.matrix(y~.-1, data = dev_keras) 
options(na.action="na.omit");

Dev_y <- dev_keras$y

attributes(M_Dev_x)$dimnames <- NULL;
attributes(M_Dev_x)$assign <- NULL
attributes(M_Dev_x)$contrasts <- NULL

# split into train_val, test
M_train_val_x <- M_Dev_x[train_val_seq,]
M_test_x <- M_Dev_x[test_seq,]
train_val_y <- Dev_y[train_val_seq]
test_y <- Dev_y[test_seq]

# generate k-folder validation dataset
set.seed(3000)
total_seq <- c(1:length(train_val_y))
seq_lb <- train_val_y
lb0_seq <- total_seq[seq_lb == 0]
lb1_seq <- total_seq[seq_lb == 1]
len_lb0_seq <- length(lb0_seq)
len_lb1_seq <- length(lb1_seq)

splt_rate <- c(0.8,0.2)
attr(splt_rate,"setType") <- c("train","val")
attr(splt_rate,"type_len_0") <- c(floor(len_lb0_seq * splt_rate))
attr(splt_rate,"type_len_1") <- c(floor(len_lb1_seq * splt_rate))

lb0_seq_suffle <- lb0_seq[sample(1:len_lb0_seq,size = len_lb0_seq, replace = F)]
lb1_seq_suffle <- lb1_seq[sample(1:len_lb1_seq,size = len_lb1_seq, replace = F)]

train_seq <- c(lb0_seq_suffle[1:attr(splt_rate,"type_len_0")[1]], lb1_seq_suffle[1:attr(splt_rate,"type_len_1")[1]])
train_seq <- train_seq[sample(1:length(train_seq),size = length(train_seq), replace = F)]
val_seq <- c(lb0_seq_suffle[(attr(splt_rate,"type_len_0")[1]+1):(sum(attr(splt_rate,"type_len_0")[1:2]))],lb1_seq_suffle[(attr(splt_rate,"type_len_1")[1]+1):(sum(attr(splt_rate,"type_len_1")[1:2]))])
val_seq <- val_seq[sample(1:length(val_seq),size = length(val_seq), replace = F)]

M_train_x <- M_train_val_x[train_seq,]
M_val_x <- M_train_val_x[val_seq,]
train_y <- train_val_y[train_seq]
val_y <- train_val_y[val_seq]

cat("The infor of train dataset - ")
dim(M_train_x)
cat("The infor of validation dataset - ")
dim(M_val_x)
cat("The infor of test dataset - ")
dim(M_test_x)



# Model buid and tuning
bs <- 64
in_sp <- dim(M_train_x)[[2]]
model <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = in_sp) %>%
  layer_dropout(rate = 0.2) %>%  
  layer_dense(units = 32, activation = "relu") %>%
  layer_dropout(rate = 0.1) %>%  
  layer_dense(units = 1, activation = "sigmoid")
model %>% compile(
  optimizer = optimizer_rmsprop(lr = 0.001),                    #optimizer_rmsprop
  loss = loss_binary_crossentropy,                              #loss_binary_crossentropy
  metrics = c("accuracy")                                       #metric_binary_accuracy
)

# start training
history <- model %>% fit(
  M_train_x, 
  train_y,
  validation_data = list(M_val_x, val_y),
  shuffle = TRUE,
  verbose = TRUE,
  batch_size = bs,
  epochs = 10
)

#str(history)    #start to overfit after 2nd epoch
#history$params
#history$metrics
plot(history)

# Validate on 'test' dataset
pred_test_keras <- model %>% predict(M_test_x)
pred_test_keras <- pred_test_keras[,1]
pred_test_bin_keras <- ifelse(pred_test_keras > .5, 1, 0)
conf_mtx_keras <- table(pred_test_bin_keras, label_test)
acc_keras <- model %>% evaluate(M_test_x,test_y,batch_size = bs)
acc_keras <- acc_keras[2][[1]]
conf_mtx_keras

cat("The predict accuracy is: ", acc_keras)


# Final model selection - XGBoost vs Keras
ds.pred.rlt <- data.frame(acc = label_test, xgb_score = pred_test_xgb, keras_score = pred_test_keras)
roc_xgb <- pROC::roc(as.factor(ds.pred.rlt$acc),ds.pred.rlt$xgb_score)
roc_keras <- pROC::roc(as.factor(ds.pred.rlt$acc),ds.pred.rlt$keras_score)
g2 <- pROC::ggroc(list(xgb=roc_xgb,keras=roc_keras))  # x(FPR) = FP / (TN+FP); y(TPR) = TP / (TP+FN)
g2

cat("AUC of xgb model is:", pROC::auc(roc_xgb), " and AUC of keras model is:", pROC::auc(roc_keras))

cat("The confusion matrix of keras model: \n")
conf_mtx_keras

cat("The confusion matrix of xgboost: \n")
conf_mtx





