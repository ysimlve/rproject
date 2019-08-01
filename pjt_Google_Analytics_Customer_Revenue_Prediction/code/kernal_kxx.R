# reference - https://www.kaggle.com/kailex/r-eda-for-gstore-glm-keras-xgb/code


#title: "Google Analytics Customer Revenue Prediction EDA"
#Introduction: 
#  Our task is to build a algorithm that predicts the natual log of the sum of all transactions per user.


##0 load required libraries
library(xgboost)       
library(keras)
library(tidyverse)

library(h2o)           #R interface for 'H2O', the scalable open source machine learning platform that offers parallelized implementations of many supervised and unsupervised machine learning algorithms. Reference: https://www.h2o.ai/
library(caret)         #caret(_C_lassification _A_nd _RE_gression _T_raining) is a set of functions that attempt to streamline the process for creating predictive models. Reference: http://topepo.github.io/caret/index.html
library(lme4)          #Fit linear and generalized linear mixed-effects models. 
library(ggalluvial)    #Alluvial Diagrams in 'ggplot2'
library(jsonlite)      #A Robust, High Performance JSON Parser and Generator for R
library(lubridate)     #Make Dealing with Dates a Little Easier in R
library(knitr)         #A General-Purpose Package for Dynamic Report Generation in R
library(Rmisc)         #Rmisc is a colletion of functions useful for data analysis and utility operations
library(scales)        #Graphical scales map data to aesthetics, and provide methods for automatically determining breaks and labels for axes and legends.
library(countrycode)   #Convert Country Names and Country Codes
library(highcharter)   #R wrapper for highcharts based on htmlwidgets. Reference: http://www.htmlwidgets.org/showcase_highcharts.html
library(glmnet)        #Extremely efficient procedures for fitting the entire lasso or elastic-net regularization path for linear regression, logistic and multinomial regression models....
library(forecast)      #Forecasting Functions for Time Series and Linear Models
library(zoo)           #S3 Infrastructure for Regular and Irregular Time Series 
library(magrittr)      # %>%


##1 load data
set.seed(0)

tr <- read_csv("./data/train.csv")
te <- read_csv("./data/test.csv")

subm <- read_csv("./data/sample_submission.csv")

##2 peek at the dataset

#2.1 general information
cat("Train set file size:", file.size("./data/train.csv") / 1024 / 1024, "Mb")
glimpse(tr)
cat("Test set file size:", file.size("./data/test.csv") / 1024 / 1024, "Mb")
glimpse(te)

#2.2 distribution of transactions dates
p1_tr <- 
  tr %>% select(date) %>%
  mutate(date = ymd(date),
         year_month = make_date(year(date), month(date))) %>%
  group_by(year_month) %>% dplyr::count() %>%
  ggplot(aes(x = year_month, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "", y = "transactions", title = "Train") +
  theme_minimal() +
  scale_x_date(labels = date_format("%Y - %m")) +
  theme(axis.title.x = element_text(angle = 45, hjust = 1)) + 
  geom_vline(aes(xintercept = max(year_month), colour = "red"), size = 1) +
  theme(legend.position = "none")

p1_te <- 
  te %>% select(date) %>%
  mutate(date = ymd(date),
         year_month = make_date(year(date), month(date))) %>%
  group_by(year_month) %>% dplyr::count() %>%
  ggplot(aes(x = year_month, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "", y = "transactions", title = "Test") +
  theme_minimal() +
  scale_x_date(labels = date_format("%Y - %m")) +
  theme(axis.title.x = element_text(angle = 45, hjust = 1)) 

multiplot(p1_tr, p1_te, cols = 2)

#infor from the plot: 
#the data was splited by 'time' into Train and Test. So it make scense to create time-based splits for train/validation sets.

#2.3 datasets columns
glimpse(tr)
tr %>% select(fullVisitorId,channelGrouping,date, device, geoNetwork,
              sessionId, socialEngagementType, visitId, totals, trafficSource,
              visitNumber, visitStartTime) %>%
  map_df(n_distinct) %>%
  gather() %>%
  ggplot(aes(x = reorder(key, -value), y = value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_log10(breaks = c(5,50,250,500,1000,10000,50000)) +
  geom_text(aes(label = value), vjust = 1.6, colour = "white", size = 3.5) +
  theme_minimal() + 
  labs(x = "features", y = "Number of unique values") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#infor from the plot:
#column 'socialEngagementType' has only one value, so it is useless, can be removed

##3. JSON data processing
#Actually the columns **device**, **geoNetwork**, **trafficSource**, **totals** contain data in JSON format. 
#3.1 define function to deal with json format
flatten_json <- . %>% 
  str_c(., collapse = ",") %>% 
  str_c("[", ., "]") %>% 
  fromJSON(flatten = T)

parse <- . %>% 
  bind_cols(flatten_json(.$device)) %>%
  bind_cols(flatten_json(.$geoNetwork)) %>% 
  bind_cols(flatten_json(.$trafficSource)) %>% 
  bind_cols(flatten_json(.$totals)) %>% 
  select(-device, -geoNetwork, -trafficSource, -totals)

#3.2 convert train and test sets to the tidy format
tr <- parse(tr)
te <- parse(te)
kable(head(tr,2))
kable(head(te,2))
kable(head(subm,5))


##4. check on transformed data
#attributes diff between train and test data
setdiff(names(tr),names(te))
tr %<>% select(-one_of("campaignCode"))   #remove from tr set

#are there any variable has only one value?
fea_uniq_values <- sapply(tr, n_distinct)
(fea_del <- names(fea_uniq_values[fea_uniq_values == 1]))

tr %<>% select(-one_of(fea_del))   #remove these columns from tr
te %<>% select(-one_of(fea_del))   #remove these columns from te

##5. missing value
#the following values can be treat as NA
is_na_val <- function(x) x %in% c("not available in demo dataset", "(not provided)",
                                  "(not set)", "<NA>", "unknown.unknown",  "(none)")

tr %<>% mutate_all(funs(ifelse(is_na_val(.), NA, .)))  #replace above values in all columns to NA
te %<>% mutate_all(funs(ifelse(is_na_val(.), NA, .)))

# check the missing percentage of each column
tr %>% summarise_all(funs(sum(is.na(.))/n() * 100)) %>%   #same as - sapply(tr, FUN = function(x){sum(is.na(x))/length(x)*100})
  gather(key = "feature", value = missing_pct) %>%
  ggplot(aes(x = reorder(feature, -missing_pct), y = missing_pct)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(y = "missing %", x = "feature") +
  coord_flip() + 
  theme_minimal()

#infor from the plot
#there are some columns are almost all NA, e.g.'adContent'

##6. simple transformation
#-- View(tr[1:100,])
tr %<>% mutate(date = ymd(date),
              hits = as.integer(hits),
              pageviews = as.integer(pageviews),
              bounces = as.integer(bounces),
              newVisits = as.integer(newVisits),
              transactionRevenue = as.numeric(transactionRevenue))
te %<>% mutate(date = ymd(date),
              hits = as.integer(hits),
              pageviews = as.integer(pageviews),
              bounces = as.integer(bounces),
              newVisits = as.integer(newVisits))
y <- tr$transactionRevenue
tr$transactionRevenue <- NULL
summary(y)
y[is.na(y)] <- 0   #set to 0 if NA in train

p1 <- as_tibble(y) %>%
  ggplot(aes(x = log1p(value))) +                     # log1p(x) = log(x+1) 
  geom_histogram(bins = 30, fill = "steelblue") +
  labs(x = "transaction revenue") +
  theme_minimal()

p2 <- as_tibble(y[y>0]) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill="steelblue") + 
  labs(x = "non-zero transaction revenue") +
  theme_minimal()

multiplot(p1, p2, cols = 2)

as_tibble(log1p(y[y>0] / 1e6)) %>% 
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill="steelblue") + 
  labs(x = "log(non-zero transaction revenue / 1e6)") +
  theme_minimal()

#infor from the plot:
#tranform the target variable(transactionRevenue) with log

#how many transaction have non-zero revenue
length(y[y>0]) / length(y)      #only 1.27% transaction has non-zero revenue 


##7. EDA - channelGrouping
tr %>% 
  bind_cols(as_tibble(y)) %>% 
  group_by(channelGrouping) %>% 
  dplyr::summarise(revenue = sum(value)) %>%
  ggplot(aes(x = channelGrouping, y = revenue)) +
  geom_point(color="steelblue", size=2) +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# first visit users generate more total revenue, but be aware of there are much more users visit only once
tr %>% 
  bind_cols(as_tibble(y)) %>%
  group_by(visitNumber) %>%
  dplyr::summarise(revenue = sum(value)) %>%
  ggplot(aes(x = visitNumber, y = revenue)) +
  geom_point(color="steelblue", size=0.5) +
  theme_minimal() + 
  scale_x_continuous(breaks = c(1,3,5,10,15,25,50,100), limits = c(0,105)) +
  scale_y_continuous(labels = comma)


# How target variable changes in time
## The revenue itself can be viewed as a timeseries. There seems to be a pattern of peaks.
p1 <- tr %>% 
  bind_cols(as_tibble(y)) %>%
  group_by(date) %>%
  dplyr::summarise(visits = n()) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = visits)) +
  geom_line() +
  stat_smooth() + 
  labs(x = "") +
  theme_minimal()

p2 <- tr %>% 
  bind_cols(as_tibble(y)) %>% 
  group_by(date) %>% 
  dplyr::summarize(revenue = mean(value)) %>% 
  ungroup()  %>% 
  ggplot(aes(x = date, y = revenue)) + 
  geom_line() +
  stat_smooth() +
  labs(x = "") +
  theme_minimal()

multiplot(p1, p2, cols = 1)   


# There is an interesting separation in target variable by **isTrueDirect** feature
tr %>% 
  bind_cols(as_tibble(y)) %>% 
  group_by(date, isTrueDirect) %>% 
  dplyr::summarize(revenue = mean(value)) %>% 
  ungroup()  %>% 
  ggplot(aes(x = date, y = revenue, color = isTrueDirect)) + 
  #geom_line() +
  stat_smooth(aes(color = isTrueDirect)) +
  labs(x = "") +
  theme_minimal()


# Revenue forecasting
## Let's see if we can predict log-transformed mean daily revenue using timeseries.
## Here we use [zoo] and [forecast] packages for timeseries modelling
ds_rev_by_date <- 
  tr %>% 
    bind_cols(tibble(revenue = y)) %>% 
    group_by(date) %>% 
    dplyr::summarize(mean_revenue = log1p(mean(revenue/1e6))) %>% 
    ungroup() 

revenue <- with(ds_rev_by_date, zoo(mean_revenue, order.by = date))
class(revenue); attributes(revenue)
plot(revenue,ds_rev_by_date$mean_revenue)

range(te$date); range(tr$date)
h <- max(te$date) - min(te$date) + 1

revenue %>% 
  autoplot() + 
  geom_line() +
  geom_smooth() + 
  labs(x = "", y = "log(revenue)") +
  theme_minimal()

m_aa <- auto.arima(revenue)
summary(m_aa)

forecast(m_aa, h = h) %>% 
  autoplot() + 
  theme_minimal()  #Clearly, this model is of no use for long time period forecasting. 

# add a regression term **mean pageviews**:
tr %>% 
  group_by(date) %>% 
  dplyr::summarize(mean_pv = log1p(mean(pageviews, na.rm=TRUE))) %>% 
  ungroup() %$% 
  mean_pv ->
  mean_pv_tr


te %>% 
  group_by(date) %>% 
  dplyr::summarize(mean_pv = log1p(mean(pageviews, na.rm=TRUE))) %>% 
  ungroup() %$% 
  mean_pv ->
  mean_pv_te

m_aa_reg <- auto.arima(revenue, xreg = mean_pv_tr)  # added a regression term
summary(m_aa_reg)

forecast(m_aa_reg, h = h, xreg = mean_pv_te) %>% 
  autoplot() + 
  theme_minimal()   #This forecast looks much better.


# Time features
## The dataset contains the timestamp column **visitStartTime** expressed as POSIX time.
## It allows us to create a bunch of features. Let's check symmetric differences(对称差集) of the time features from the train and test sets.
tr_vst <- as_datetime(tr$visitStartTime)
te_vst <- as_datetime(te$visitStartTime)

symdiff <- function(x, y) setdiff(union(x, y), intersect(x, y))               #对称差集的计算公式

symdiff(tr_vst %>% year %>% unique, te_vst %>% year %>% unique)               #对称差集 - Year
symdiff(tr_vst %>% month %>% unique, te_vst %>% month %>% unique)             #对称差集 - Month
symdiff(tr_vst %>% day %>% unique, te_vst %>% day %>% unique)                 #对称差集 - Day
symdiff(tr_vst %>% week %>% unique, te_vst %>% week %>% unique)               #对称差集 - Week
symdiff(tr_vst %>% yday %>% unique, te_vst %>% yday %>% unique)               #对称差集 - Day of the year
symdiff(tr_vst %>% hour %>% unique, te_vst %>% hour %>% unique)               #对称差集 - Hour

#Known from the symmetric differences(对称差集):
# We can see that some time features (week, month, day of the year) from the train and test sets 
# differ notably. Thus, they can cause overfitiing, but **year** and **hour** can be useful.

# Distribution of visits and revenue by attributes 'channelGrouping'
# The most frequent channels are **OrganicSearch** and **Social**.
tr %>% 
  bind_cols(as_tibble(y)) %>% 
  group_by(channelGrouping) %>% 
  dplyr::summarize(visits = n(), mean_revenue = mean(value), total_revenue = sum(value)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(channelGrouping = reorder(channelGrouping, -visits)) %>% 
  data.table::melt(id.vars = c("channelGrouping")) %>% 
  ggplot(aes(channelGrouping, value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ variable, scales = "free") + 
  theme_minimal() +
  labs(x = "channel grouping", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") 

# Distribution of visits and revenue by attributes 'browser'
# Chrome is the most popular browser and its users produce the highest total revenue.
tr %>% 
  bind_cols(as_tibble(y)) %>% 
  dplyr::mutate(browser = factor(browser) %>% fct_lump(prop=0.01)) %>% 
  group_by(browser) %>% 
  dplyr::summarize(visits = n(), mean_revenue = mean(value), total_revenue = sum(value)) %>% 
  ungroup() %>% 
  mutate(browser = reorder(browser, -visits)) %>% 
  data.table::melt(id.vars = c("browser")) %>% 
  ggplot(aes(browser, value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ variable, scales = "free") + 
  theme_minimal() +
  labs(x = "browser", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") 

# Distribution of visits and revenue by attributes 'operatingSystem'
# Windows and MacOS are the most popular operating systems. It's interesting that ChromeOS users yield the highest mean revenue.
tr %>% 
  bind_cols(as_tibble(y)) %>% 
  dplyr::mutate(operatingSystem = factor(operatingSystem) %>% fct_lump(prop=0.01)) %>% 
  group_by(operatingSystem) %>% 
  dplyr::summarize(visits = n(), mean_revenue = mean(value), total_revenue = sum(value)) %>% 
  ungroup() %>% 
  mutate(operatingSystem = reorder(operatingSystem, -visits)) %>% 
  data.table::melt(id.vars = c("operatingSystem")) %>% 
  ggplot(aes(operatingSystem, value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ variable, scales = "free") + 
  theme_minimal() +
  labs(x = "operating system", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") 

# Distribution of visits and revenue by attributes 'deviceCategory'
# Desktops are still in the ranks.
tr %>% 
  bind_cols(as_tibble(y)) %>% 
  dplyr::mutate(deviceCategory = factor(deviceCategory) %>% fct_lump(prop=0.01)) %>% 
  group_by(deviceCategory) %>% 
  dplyr::summarize(visits = n(), mean_revenue = mean(value), total_revenue = sum(value)) %>% 
  ungroup() %>% 
  mutate(deviceCategory = reorder(deviceCategory, -visits)) %>% 
  data.table::melt(id.vars = c("deviceCategory")) %>% 
  ggplot(aes(deviceCategory, value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ variable, scales = "free") + 
  theme_minimal() +
  labs(x = "device category", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") 

# Distribution of visits and revenue by attributes 'country'
# The US users yield the most of the total revenue.
tr %>% 
  bind_cols(as_tibble(y)) %>% 
  dplyr::mutate(country = factor(country) %>% fct_lump(prop=0.023)) %>%   #here, how to decide the value of prop?
  group_by(country) %>% 
  dplyr::summarize(visits = n(), mean_revenue = mean(value), total_revenue = sum(value)) %>% 
  ungroup() %>% 
  mutate(country = reorder(country, -visits)) %>% 
  data.table::melt(id.vars = c("country")) %>% 
  ggplot(aes(country, value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ variable, scales = "free") + 
  theme_minimal() +
  labs(x = "country", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") 

# Distribution of visits and revenue by attributes 'city'
tr %>% 
  bind_cols(as_tibble(y)) %>% 
  dplyr::mutate(city = factor(city) %>% fct_lump(prop=0.01)) %>% 
  group_by(city) %>% 
  dplyr::summarize(visits = n(), mean_revenue = mean(value), total_revenue = sum(value)) %>% 
  ungroup() %>% 
  mutate(city = fct_explicit_na(city, na_level = "Other") %>% reorder(-visits)) %>% 
  data.table::melt(id.vars = c("city")) %>% 
  ggplot(aes(city, value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ variable, scales = "free") + 
  theme_minimal() +
  labs(x = "city", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") 

# Distribution of visits and revenue by attributes 'networkDomain'
# Usually netwok domain is unknown.
tr %>% 
  bind_cols(as_tibble(y)) %>% 
  dplyr::mutate(networkDomain = factor(networkDomain) %>% fct_lump(prop=0.01)) %>% 
  group_by(networkDomain) %>% 
  dplyr::summarize(visits = n(), mean_revenue = mean(value), total_revenue = sum(value)) %>% 
  ungroup() %>% 
  mutate(networkDomain = fct_explicit_na(networkDomain, na_level = "Other") %>% reorder(-visits)) %>% 
  data.table::melt(id.vars = c("networkDomain")) %>% 
  ggplot(aes(networkDomain, value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ variable, scales = "free") + 
  theme_minimal() +
  labs(x = "network domain", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") 

# Distribution of visits and revenue by attributes 'medium'
# **organic** and **referral** are the most popular mediums.
tr %>% 
  bind_cols(as_tibble(y)) %>% 
  dplyr::mutate(medium = factor(medium) %>% fct_lump(prop=0.005)) %>% 
  group_by(medium) %>% 
  dplyr::summarize(visits = n(), mean_revenue = mean(value), total_revenue = sum(value)) %>% 
  ungroup() %>% 
  mutate(medium = fct_explicit_na(medium, na_level = "Other") %>% reorder(-visits)) %>% 
  data.table::melt(id.vars = c("medium")) %>% 
  ggplot(aes(medium, value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ variable, scales = "free") + 
  theme_minimal() +
  labs(x = "medium", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") 



# Alluvial diagram
tr %>% 
  select(country, networkDomain, browser, deviceCategory, channelGrouping) %>% 
  dplyr::mutate(networkDomain = str_split(networkDomain, "\\.") %>% map(~ .x[[length(.x)]]) %>% unlist) %>% 
  dplyr::mutate_all(factor) %>% 
  dplyr::mutate_all(fct_lump, 4) %>% 
  bind_cols(tibble(revenue = ifelse(y == 0, "Zero", "Non-zero") %>% factor)) %>% 
  na.omit() %>% 
  group_by_all() %>% 
  dplyr::count() %>% 
  ggplot(aes(y = n, 
             axis1 = country, axis2 = deviceCategory, axis3 = browser,   
             axis4 = channelGrouping, axis5 = networkDomain)) +
  geom_alluvium(aes(fill = revenue), width = 1/12) +
  geom_stratum(width = 1/10, fill = "black", color = "grey") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:5, labels = c("country", "deviceCategory", "browser",
                                              "channelGrouping", "networkDomain"))



# Bot or not? (机器人)
# We are interested in traffic which yields zero transaction revenue.
bot_browsers <- c("Mozilla Compatible Agent", "Seznam", "User Agent", 
                  "Changa 99695759","ThumbSniper","LYF_LS_4002_12",
                  "[Use default User-agent string] LIVRENPOCHE", "no-ua",
                  "YE", "0","subjectAgent: NoticiasBoom")
tr %>% 
  bind_cols(tibble(revenue = y)) %>% 
  filter(revenue == 0 & browser %in% bot_browsers) %>% 
  group_by(browser)%>% 
  dplyr::summarize(visits = n()) %>% 
  ungroup() %>% 
  ggplot(aes(reorder(browser, -visits), visits)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(x = "browser", y = "visits") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.position="none")+
  geom_text(aes(label = visits), vjust = -0.2, color = "black", size=2.5) 


# The absolute leader here is "Mozilla Compatible Agent" - very often bots can be identified by
# this user agent. Here is a time series of the bots visits:
tr %>%
  bind_cols(tibble(revenue = y)) %>%
  filter(revenue == 0) %>%
  filter(browser == "Mozilla Compatible Agent") %>%
  group_by(date) %>%
  dplyr::summarize(visits = n()) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = visits)) +
  geom_line() +
  geom_smooth() +
  labs(x = "") +
  theme_minimal()

# We can clearly observe some peaks. Let's detect dates of these peaks:
tr %>%
  bind_cols(tibble(revenue = y)) %>%
  filter(revenue == 0) %>%
  filter(browser == "Mozilla Compatible Agent") %>%
  group_by(date) %>%
  dplyr::summarize(visits = n()) %>%
  ungroup() %>%
  arrange(-visits) %>% 
  top_n(10)


tr %>%
  bind_cols(tibble(revenue = y)) %>%
  filter(revenue == 0) %>%
  filter(browser == "Mozilla Compatible Agent") %>%
  select(date, networkDomain, country) %>% 
  dplyr::mutate(networkDomain = str_split(networkDomain, "\\.") %>% map(~ .x[[length(.x)]]) %>% unlist) %>% 
  group_by_all() %>% 
  dplyr::summarize(visits = n()) %>% 
  ungroup() %>% 
  arrange(-visits) %>% 
  na.omit() %>% 
  top_n(10)


p1 <- tr %>%
  bind_cols(tibble(revenue = y)) %>%
  filter(revenue == 0) %>%
  filter(browser == "Mozilla Compatible Agent") %>%
  group_by(country) %>% 
  dplyr::summarize(visits = n()) %>% 
  ungroup() %>% 
  arrange(-visits) %>% 
  na.omit() %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(country, -visits), visits)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(x = "country", y = "visits") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.position="none")+
  geom_text(aes(label = visits), vjust = -0.2, color = "black", size=2.5) 

p2 <- tr %>%
  bind_cols(tibble(revenue = y)) %>%
  filter(revenue == 0) %>%
  filter(browser == "Mozilla Compatible Agent") %>%
  dplyr::mutate(networkDomain = str_split(networkDomain, "\\.") %>% map(~ .x[[length(.x)]]) %>% unlist) %>% 
  group_by(networkDomain) %>% 
  dplyr::summarize(visits = n()) %>% 
  ungroup() %>% 
  arrange(-visits) %>% 
  na.omit() %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(networkDomain, -visits), visits)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(x = "networkDomain", y = "visits") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.position="none")+
  geom_text(aes(label = visits), vjust = -0.2, color = "black", size=2.5) 

multiplot(p1, p2, cols=2)

# In general, the traffic from the USA and .net domains is the main contributor to the visits with "Mozilla Compatible Agent" user agent


# World map: important values
highchart(type = "map") %>%
  hc_add_series_map(worldgeojson,
                    tr %>% 
                      bind_cols(as_tibble(y)) %>% 
                      group_by(country) %>% 
                      dplyr::summarise(revenue = log1p(sum(value))) %>% 
                      ungroup() %>% 
                      dplyr::mutate(iso2 = countrycode(country, origin="country.name", destination="iso2c")),
                    value = "revenue", joinBy = "iso2") %>%
  hc_title(text = "log Revenue by country") %>%
  hc_tooltip(useHTML = TRUE, headerFormat = "",
             pointFormat = "{point.country}: {point.revenue:.0f}") %>% 
  hc_colorAxis(minColor = "#e8eded", maxColor = "#4c735e")

highchart(type = "map") %>%
  hc_add_series_map(worldgeojson, 
                    tr %>% 
                      group_by(country) %>% 
                      dplyr::summarise(pageviews = sum(pageviews)) %>% 
                      ungroup() %>% 
                      dplyr::mutate(iso2 = countrycode(country, origin="country.name", destination="iso2c")),
                    value = "pageviews", 
                    joinBy = "iso2") %>%
  hc_title(text = "Pageviews by country") %>%
  hc_tooltip(useHTML = TRUE, headerFormat = "",
             pointFormat = "{point.country}: {point.pageviews}") %>% 
  hc_colorAxis(minColor = "#e8eded", maxColor = "#4c735e")

highchart(type = "map") %>%
  hc_add_series_map(worldgeojson, 
                    tr %>% 
                      group_by(country) %>% 
                      dplyr::summarise(hits = sum(hits)) %>% 
                      ungroup() %>% 
                      dplyr::mutate(iso2 = countrycode(country, origin="country.name", destination="iso2c")),
                    value = "hits", 
                    joinBy = "iso2") %>%
  hc_title(text = "Hits by country") %>%
  hc_tooltip(useHTML = TRUE, headerFormat = "",
             pointFormat = "{point.country}: {point.hits}") %>% 
  hc_colorAxis(minColor = "#e8eded", maxColor = "#4c735e")


# Correlations between revenue and features

m <- tr %>% 
  dplyr::mutate(year = year(date),
               month = month(date),
               day = day(date),
               isMobile = ifelse(isMobile, 1L, 0L),
               isTrueDirect = ifelse(isMobile, 1L, 0L)) %>% 
  mutate_all(funs(ifelse(is.na(.), 0, .))) %>% 
  select(-date, -fullVisitorId, -visitId, -sessionId) %>% 
  mutate_if(is.character, factor) %>% 
  mutate_if(is.factor, fct_lump, prop = 0.01) %>% 
  model.matrix(~ . - 1, .) %>% 
  cor(y) %>% 
  data.table::as.data.table(keep.rownames=TRUE) %>% 
  purrr::set_names("Feature", "rho") %>% 
  arrange(-rho) 

m %>% 
  ggplot(aes(x = rho)) +
  geom_histogram(bins = 50, fill="steelblue") + 
  labs(x = "correlation") +
  theme_minimal()

# The values of the correlation coefficient are concentrated around zero, but there are several values bigger than 0.1
m %>% 
  filter(rho > 0.1) %>% 
  kable()


p1 <- tr %>% 
  select(pageviews) %>% 
  bind_cols(as_tibble(y)) %>% 
  filter(value > 0) %>% 
  ggplot(aes(x = pageviews, y = log1p(value))) +
  geom_point() +
  labs(x = "pageviews", y = "transaction revenue") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() 

p2 <- tr %>% 
  select(hits) %>% 
  bind_cols(as_tibble(y)) %>% 
  filter(value > 0) %>% 
  ggplot(aes(x = hits, y = log1p(value))) +
  geom_point() +
  labs(x = "hits", y = "transaction revenue") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() 

multiplot(p1, p2, cols = 2)

# Simple autoencoder(自编码)
# To train an autoencoder we can use [h2o package]
h2o.no_progress()
h2o.init(nthreads = 4, max_mem_size = "10G")

names(tr)
tr_h2o <- as.h2o(tr)
te_h2o <- as.h2o(te)

n_ae <- 4
# Let’s train a simple model, which compresses the input space to `r n_ae` components
m_ae <- h2o.deeplearning(training_frame = tr_h2o,
                         x = 1:ncol(tr_h2o),
                         autoencoder = T,
                         activation="Rectifier",
                         reproducible = TRUE,
                         seed = 0,
                         sparse = T,
                         standardize = TRUE,
                         hidden = c(32, n_ae, 32),
                         max_w2 = 5,
                         epochs = 25)
tr_ae <- h2o.deepfeatures(m_ae, tr_h2o, layer = 2) %>% as_tibble
te_ae <- h2o.deepfeatures(m_ae, te_h2o, layer = 2) %>% as_tibble

rm(tr_h2o, te_h2o, m_ae); invisible(gc())
h2o.shutdown(prompt = FALSE)

plot.pairs <- function(x, y, n=5, b=20){
  pairs(x[, 1:n], cex = 0.15,
        col=alpha(rainbow(b)[as.numeric(cut(y, breaks=b))], 0.2), asp=1)
}

plot.pairs(tr_ae, log1p(y), n_ae)
plot.pairs(te_ae, 1, n_ae)
















































































































































#####################draft#####################
test <- tibble(x1 = c("a","b","not available in demo dataset","c"),
               x2 = c(1,2,3,NA),
               x3 = c("a","(not provided)","<NA>","d"),
               x4 = c("e","(not set)",NA,"(none)"))

test %>% mutate_all()

























































  