getwd()

#### packages
library(tidyverse)
library(xgboost)
library(keras)
library(jsonlite)
library(stringr)
library(lubridate)
library(Matrix)

set.seed(0)

#### data loading
train <- read_csv("./data/train.csv");  nrow(train)  # 903653
test <- read_csv("./data/test.csv");    nrow(test)   # 804684

str(train)
head(train)
View(train[1:100,])

#### Json process
flatten_json <- . %>% 
  str_c(., collapse = ",") %>%
  str_c("[", ., "]") %>% 
  fromJSON(flatten = T)

parse <- . %>%
  bind_cols(flatten_json(.$device) %>% rename_all(function(x) str_c("dvc_",x))) %>%
  bind_cols(flatten_json(.$geoNetwork) %>% rename_all(function(x) str_c("geo_",x))) %>%
  bind_cols(flatten_json(.$trafficSource) %>% rename_all(function(x) str_c("tfcS_",x))) %>%
  bind_cols(flatten_json(.$totals) %>% rename_all(function(x) str_c("tt_",x))) %>%
  select(-device, -geoNetwork, -trafficSource, -totals)

train <- parse(train)
test <- parse(test)

str(train)
View(train[1:1000,])

# remove attributes which has only one value
ft_uniqValue <- sapply(train, n_distinct)
(ft_rm <- names(ft_uniqValue[ft_uniqValue==1]))
train <- train %>% select(-ft_rm)
test <- test %>% select(-ft_rm)

# check percentage of null value of each attribute
is_na_val <- function(x) x %in% c("not available in demo dataset", "(not provided)",
                                  "(not set)", "<NA>", "unknown.unknown",  "(none)")

train <- train %>% mutate_all(funs(ifelse(is_na_val(.),NA,.)))
train <- test %>% mutate_all(funs(ifelse(is_na_val(.),NA,.)))









  
