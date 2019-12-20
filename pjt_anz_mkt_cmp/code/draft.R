getwd()
options(warn=-1)
#############################################1. Preparations###########################################################
#############################################1.1. Load libraries###########################################################
# if(!require(caret)) install.packages("pROC",repos=getOption("repos"), lib = .libPaths())
library(xgboost)       
library(keras)
library(tidyverse)
library(h2o)           # R interface for 'H2O', the scalable open source machine learning platform that offers parallelized implementations of many supervised and unsupervised machine learning algorithms. Reference: https://www.h2o.ai/
library(caret)         # caret(Classification And Regression Training) is a set of functions that attempt to streamline the process for creating predictive models. Reference: http://topepo.github.io/caret/index.html
library(lme4)          # Fit linear and generalized linear mixed-effects models. 
library(ggalluvial)    # Alluvial Diagrams in 'ggplot2'
library(jsonlite)      # A Robust, High Performance JSON Parser and Generator for R
library(lubridate)     # Make Dealing with Dates a Little Easier in R
library(knitr)         # A General-Purpose Package for Dynamic Report Generation in R
library(Rmisc)         # Rmisc is a colletion of functions useful for data analysis and utility operations
library(scales)        # Graphical scales map data to aesthetics, and provide methods for automatically determining breaks and labels for axes and legends.
library(countrycode)   # Convert Country Names and Country Codes
library(highcharter)   # R wrapper for highcharts based on htmlwidgets. Reference: http://www.htmlwidgets.org/showcase_highcharts.html
library(glmnet)        # Extremely efficient procedures for fitting the entire lasso or elastic-net regularization path for linear regression, logistic and multinomial regression models....
library(forecast)      # Forecasting Functions for Time Series and Linear Models
library(zoo)           # S3 Infrastructure for Regular and Irregular Time Series 
library(magrittr)      # %>%
library(vcd)
library(pROC)

#############################################1.2. Load data###########################################################
set.seed(0)
dev <- read_csv("./data/bank-additional-train.csv")
View(dev)
str(dev)
names(dev)

#############################################1.3. Global Vars & Funs###########################################################
anz_color1 <- "dodgerblue4"
anz_color2 <- "deepskyblue2"

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
  
  eta <- c(0.01) #c(0.01,0.03,0.05,0.1);                
  len_eta <- length(eta)
  gamma <- c(0);                               
  len_gamma <- length(gamma)
  max_depth <- c(6)#c(4,5,6,7,8,9);                 
  len_max_depth <- length(max_depth)
  min_child_weight <- c(1)#c(1,2,3,4,5,6,7);        
  len_min_child_weight <- length(min_child_weight)
  max_delta_step <- 0;                       
  subsample <- c(.9,.8)#c(1,.9,.8,.7,.6,.5);            
  len_subsample <- length(subsample)
  colsample_bytree <- c(.8)#c(1,.9,.8,.7,.6,.5);     
  len_colsample_bytree <- length(colsample_bytree)
  colsample_bylevel <- c(.7) #c(1,.9,.8,.7,.6,.5);    
  len_colsample_bylevel <- length(colsample_bylevel)
  lambda <- c(0)#c(0,5,10,15,20);                           
  len_lambda <- length(lambda)
  scale_pos_weight <- c(1)#c(1,.4,.5,.6,.7,.8);     
  len_scale_pos_weight <- length(scale_pos_weight)
  
  total_loop <- len_eta*len_gamma*len_max_depth*len_min_child_weight*len_subsample*len_colsample_bytree*len_colsample_bylevel*len_lambda
  cat("totla loop: ", total_loop)
  
  for(e in 1:len_eta){
    for(g in 1:len_gamma){
      for(md in 1:len_max_depth){
        for(mcw in 1:len_min_child_weight){
          for(ss in 1:len_subsample){
            for(cbt in 1:len_colsample_bytree){
              for(cbl in 1:len_colsample_bylevel){
                for(l in 1:len_lambda){
                  for(spw in 1:len_scale_pos_weight){
                    loop_id = e*g*md*mcw*ss*cbt*cbl*l*spw
                    #cat("Round ", loop_id, " / ",total_loop)
                    
                    hp <- list(
                      seed = 0,
                      silent = 1,
                      nthread = 2,
                      booster = "gbtree",
                      objective = "binary:logistic",
                      eval_metric = "error",
                      
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
                      tibble(id = loop_id,
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
                             train_error_mean = fit.cv$evaluation_log[fit.cv$best_iteration,]$train_error_mean,
                             train_error_std = fit.cv$evaluation_log[fit.cv$best_iteration,]$train_error_std,
                             test_error_mean = fit.cv$evaluation_log[fit.cv$best_iteration,]$test_error_mean,
                             test_error_std= fit.cv$evaluation_log[fit.cv$best_iteration,]$test_error_std)
                    )
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  return(rlt)
}

#############################################2. Peek at the dataset###########################################################
#############################################2.1. General info###########################################################
glimpse(dev)
kable(head(dev[,1:10],2))

#############################################2.2. Distribution of target variable###########################################################
dev %>% select(y) %>% 
  group_by(y) %>% dplyr::count() %>%
  ggplot(aes(x = y, y = n)) +
  geom_bar(stat = "identity",fill = anz_color1, width = .4) +
  labs(x = "Bank term deposit be subscribed", y = "count", title = "Distribution of target variable values") +
  theme_minimal() +
  scale_y_continuous(labels = comma) + 
  geom_text(aes(label = n),vjust = 1.6, colour = "white", size = 4) +
  geom_text(aes(label = ifelse(y == "yes","Only 12.6% of all samples with label as 'yes'", "")), 
            colour = "red", vjust = -5, size = 5)

#Safely transform variable 'y' to binary type
dev$y <- ifelse(dev$y == "yes",1,0)

#############################################2.3. Missing value check###########################################################
is_na_val <- function(x) ifelse(x == "unknown", T, F)

## check the missing percentage of each column
dev %>% summarise_all(funs(sum(is_na_val(.))/n() * 100)) %>%  
  gather(key = "feature", value = missing_pct) %>% 
  ggplot(aes(x = reorder(feature, missing_pct), y = missing_pct)) + 
  geom_bar(stat = "identity", fill = anz_color1) +
  labs(y = "missing %", x = "feature") +
  coord_flip() + 
  theme_minimal()

#############################################2.4. Duplication check###########################################################
dev$md5key <- apply(dev %>% select(-one_of("y")), 1, function(x){openssl::md5(str_c(x,collapse = ""))}) #generate 'md5' key for each sample
dev_dup <- dev %>% group_by(md5key) %>% dplyr::count() %>% filter(n > 1)            # find out duplicate records
if(nrow(dev %>% filter(md5key %in% dev_dup$md5key) %>% select(md5key,y) %>%         # make sure duplicate records has same 'y'
        group_by(md5key) %>% dplyr::summarise(sum_y = sum(y)) %>% filter(sum_y == 1)) == 0){
  cat("There are ", nrow(dev_dup), " records have duplicate, but none of them has different 'y'")
}
kable(dev %>% filter(md5key == "4bd6f9da6febbb8a4bb4206f0207020b"))


#############################################2.5. Time series check###########################################################
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

#############################################3. EDA & Data Tidy###########################################################
#############################################3.1. Age###########################################################
p1 <- 
  dev %>% 
  ggplot(aes(x = age)) + 
  geom_histogram(binwidth = 5, fill = anz_color1) +
  labs(x = "customer age", title = "Distribution of customer age") +
  geom_vline(xintercept = c(25,55), colour = "red1", size = .8, linetype = 2) +
  theme_minimal()
p2 <- 
  dev %>% 
  ggplot(aes(x = age, fill = as.factor(y))) + 
  geom_histogram(binwidth = 5, position = "fill") +
  labs(x = "customer age", y = "rate",title = "Correleation between age & y") +
  geom_vline(xintercept = c(25,55), colour = "red1", size = .8, linetype = 2) +
  scale_fill_manual(values=c(anz_color2, anz_color1), name = "y") +
  theme_minimal()

multiplot(p1,p2,cols = 2)

#############################################3.2. Job###########################################################
p1 <- 
  dev %>% group_by(job) %>% dplyr::count() %>%
  ggplot(aes(x = job, y = n)) + 
  geom_bar(stat = "identity", fill = anz_color1) + 
  geom_text(aes(label = n),hjust = 1, colour = "white", size = 3) +
  labs(x = "job") +
  coord_flip() + 
  theme_minimal() 

p2 <- dev %>% 
  ggplot(aes(x = job, fill = as.factor(y))) + 
  geom_bar(position = "fill") + 
  scale_fill_manual(values=c(anz_color2, anz_color1), name = "y") +
  labs(x = "job", y = "rate") +
  theme_minimal() + coord_flip()

multiplot(p1,p2,cols = 2)

#############################################3.3. Client categrical attributes###########################################################
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

#############################################3.4. duration###########################################################
p1 <- 
  dev %>% 
  filter(duration > 0) %>% 
  ggplot(aes(x = duration)) + 
  geom_histogram(binwidth = 60, fill = anz_color1) +
  labs(x = "last contact duration(exclude 0)") +
  theme_minimal()
p2 <- 
  dev %>% 
  filter(duration > 0) %>% 
  ggplot(aes(x = duration, fill = as.factor(y))) + 
  geom_histogram(binwidth = 60, position = "fill") +
  scale_fill_manual(values=c(anz_color2, anz_color1), name = "y") +
  labs(x = "last contact duration(exclude 0)", y = "rate") +
  theme_minimal()
p3 <- 
  dev %>% 
  filter(duration > 0) %>% 
  ggplot(aes(x = log1p(duration))) + 
  geom_histogram(binwidth = .5, fill = anz_color1) +
  labs(x = "log1p(last contact duration)(exclude 0)") +
  theme_minimal()

p4 <- 
  dev %>% 
  filter(duration > 0) %>% 
  ggplot(aes(x = log1p(duration), fill = as.factor(y))) + 
  geom_histogram(binwidth = .5, position = "fill") +
  labs(x = "log1p(last contact duration)(exclude 0)", y = "rate") +
  scale_fill_manual(values=c(anz_color2, anz_color1), name = "y") +
  theme_minimal()

multiplot(p1,p2,p3,p4,cols = 2)

dev$log_duration <- log1p(dev$duration) 








































































#############################################remember to add#############################################
dev %<>% select(-md5key)
dev %<>% select(-seq, -seq_duration)
dev %<>% select(-is_previous,-is_pdays,-is_contacted) 

