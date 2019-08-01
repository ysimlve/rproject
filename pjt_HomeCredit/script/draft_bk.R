library(tidyverse)
library(magrittr)

app_tr <- read_csv("./data/application_train.csv")   # [1 : 307511]
app_te <- read_csv("./data/application_test.csv")    # [307512 : 356255]
app_te %<>% mutate(TARGET = NA) %>% select(SK_ID_CURR,TARGET,2:121)
app <- union_all(app_tr,app_te) %>% select(-TARGET)
y <- app_tr$TARGET

glimpse(app)
nrow(app) == length(unique(app$SK_ID_CURR))       #TRUE

# SK_ID_CURR - ID of loan

# TARGET distribution
prop.table(table(y)) 
tibble(target = y) %>% 
  ggplot(aes(x = as.factor(target))) +
  geom_bar() +
  labs(x = "TARGET")

# APPLICATION - NAME_CONTRACT_TYPE
prop.table(table(app$NAME_CONTRACT_TYPE))
prop.table(table(app$NAME_CONTRACT_TYPE[1:307511], y), margin = 1)

# CLINET - CODE_GENDER
unique(app$CODE_GENDER)
#-- app$CODE_GENDER <- ifelse(app$CODE_GENDER == "XNA", NA, app$CODE_GENDER)
prop.table(table(app$CODE_GENDER))
prop.table(table(app$CODE_GENDER[1:307511], y), margin = 1)

# CLINET - FLAG_OWN_CAR
unique(app$FLAG_OWN_CAR)
prop.table(table(app$FLAG_OWN_CAR))
prop.table(table(app$FLAG_OWN_CAR[1:307511], y), margin = 1)

# CLIENT - FLAG_OWN_REALTY
unique(app$FLAG_OWN_REALTY)
prop.table(table(app$FLAG_OWN_REALTY))
prop.table(table(app$FLAG_OWN_REALTY[1:307511], y), margin = 1)

# CLIENT - CNT_CHILDREN
unique(app$CNT_CHILDREN)
prop.table(table(app$CNT_CHILDREN))
prop.table(table(app$CNT_CHILDREN[1:307511], y), margin = 1)
#-- app$CNT_CHILDREN <- ifelse(app$CNT_CHILDREN > 6, "more", as.character(app$CNT_CHILDREN))

# CLIENT - AMT_INCOME_TOTAL -> log_AMT_INCOME_TOTAL
summary(app$AMT_INCOME_TOTAL)
summary(log1p(app$AMT_INCOME_TOTAL))
app %>% 
  ggplot(aes(x = log1p(AMT_INCOME_TOTAL))) +
  geom_histogram(bins = 30)
#-- app %<>% mutate(log_AMT_INCOME_TOTAL = log1p(AMT_INCOME_TOTAL))


# APPLICATION - AMT_CREDIT
summary(app$AMT_CREDIT)
#-- app %<>% mutate(log_AMT_CREDIT = log1p(AMT_CREDIT))
app %>% 
  ggplot(aes(x = AMT_CREDIT)) +
  geom_histogram(bins = 30)
app %>% 
  ggplot(aes(x = log_AMT_CREDIT)) +
  geom_histogram(bins = 30)


app %>%
  ggplot(aes(x = log_AMT_INCOME_TOTAL, y = AMT_CREDIT)) + 
  geom_point()
app %>%
  ggplot(aes(x = log_AMT_INCOME_TOTAL, y = log_AMT_CREDIT)) + 
  geom_point()

# APPLICATION - AMT_ANNUITY
summary(app$AMT_ANNUITY)
#-- app %<>% mutate(CREDIT_ANNUITY = round(AMT_CREDIT / AMT_ANNUITY))
#-- app %<>% mutate(log_AMT_ANNUITY = ifelse(is.na(AMT_ANNUITY),0,log1p(AMT_ANNUITY)))
app %>% 
  ggplot(aes(x = AMT_ANNUITY)) +
  geom_histogram(bins = 30)
app %>% 
  ggplot(aes(x = log1p(AMT_ANNUITY))) +
  geom_histogram(bins = 30)

app %>%
  filter(!is.na(AMT_ANNUITY)) %>%
  ggplot(aes(x = AMT_ANNUITY, y = AMT_CREDIT)) +
  geom_point()

app %>% filter(!is.na(AMT_ANNUITY)) %>%
  group_by(CREDIT_ANNUITY) %>% 
  count() %>%
  arrange(desc(n))

#-- app$CREDIT_ANNUITY[is.na(app$CREDIT_ANNUITY)] <- 20
#-- app$AMT_ANNUITY[is.na(app$AMT_ANNUITY)] <- app$AMT_CREDIT[is.na(app$AMT_ANNUITY)] / 20


# APPLICATION - AMT_GOODS_PRICE - highly correlated with AMT_CREDIT
summary(app$AMT_GOODS_PRICE)
#-- app %<>% select(-AMT_GOODS_PRICE) 

# correlation analysis
app %>% filter(!is.na(AMT_GOODS_PRICE)) %>%
  select(AMT_INCOME_TOTAL,AMT_CREDIT,log_AMT_INCOME_TOTAL,
         log_AMT_CREDIT,AMT_ANNUITY,log_AMT_ANNUITY,CREDIT_ANNUITY,AMT_GOODS_PRICE) %>%
  cor()

# APPLICATION - NAME_TYPE_SUITE
unique(app$NAME_TYPE_SUITE)
prop.table(table(app$NAME_TYPE_SUITE))
prop.table(table(app$NAME_TYPE_SUITE[1:307511], y), margin = 1)
sum(as.integer(is.na(app$NAME_TYPE_SUITE)))  #2203 rows with NA
#-- app$NAME_TYPE_SUITE[is.na(app$NAME_TYPE_SUITE)] <- "Unaccompanied"

# CLINET - NAME_INCOME_TYPE
unique(app$NAME_INCOME_TYPE)
prop.table(table(app$NAME_INCOME_TYPE))
prop.table(table(app$NAME_INCOME_TYPE[1:307511], y), margin = 1)

# CLINET - OCCUPATION_TYPE
unique(app$OCCUPATION_TYPE)
prop.table(table(app$OCCUPATION_TYPE))
prop.table(table(app$OCCUPATION_TYPE[1:307511], y), margin = 1)
nrow(app[is.na(app$OCCUPATION_TYPE),]) / nrow(app)   #31.5% is NA
app[is.na(app$OCCUPATION_TYPE),"NAME_INCOME_TYPE"]


# CLINET - NAME_EDUCATION_TYPE
unique(app$NAME_EDUCATION_TYPE)
prop.table(table(app$NAME_EDUCATION_TYPE))
prop.table(table(app$NAME_EDUCATION_TYPE[1:307511], y), margin = 1)

# CLINET - NAME_FAMILY_STATUS
unique(app$NAME_FAMILY_STATUS)
prop.table(table(app$NAME_FAMILY_STATUS))
prop.table(table(app$NAME_FAMILY_STATUS[1:307511], y), margin = 1)
#-- app$NAME_FAMILY_STATUS[app$NAME_FAMILY_STATUS == "Unknown"] <- "Married"

# CLINET - NAME_HOUSING_TYPE
unique(app$NAME_HOUSING_TYPE)
prop.table(table(app$NAME_HOUSING_TYPE))
prop.table(table(app$NAME_HOUSING_TYPE[1:307511], y), margin = 1)

# CLINET - DAYS_BIRTH
summary(app$DAYS_BIRTH)
app %>%
  ggplot(aes(x = DAYS_BIRTH)) +
  geom_histogram(bins = 30)
#-- app %<>% mutate(AGE_AT_APP = -round(DAYS_BIRTH / 365))
summary(app$AGE_AT_APP)

# CLINET - DAYS_REGISTRATION
summary(app$DAYS_REGISTRATION)
app %>%
  ggplot(aes(x = DAYS_REGISTRATION)) +
  geom_histogram(bins = 30)

# CLINET - DAYS_ID_PUBLISH
summary(app$DAYS_ID_PUBLISH)
app %>%
  ggplot(aes(x = DAYS_ID_PUBLISH)) +
  geom_histogram(bins = 30)

# CLINET - OWN_CAR_AGE
summary(app$OWN_CAR_AGE)
sum(as.integer(is.na(app$OWN_CAR_AGE))) / nrow(app)  #66% is NA
#-- app %<>% select(-OWN_CAR_AGE)

# CLINET - FLAG_MOBIL
unique(app$FLAG_MOBIL)
table(app$FLAG_MOBIL)
#-- app %<>% select(-FLAG_MOBIL)

# CLINET - FLAG_EMP_PHONE
unique(app$FLAG_EMP_PHONE)
prop.table(table(app$FLAG_EMP_PHONE))
prop.table(table(app$FLAG_EMP_PHONE[1:307511], y), margin = 1)

# CLINET - FLAG_CONT_MOBILE
unique(app$FLAG_CONT_MOBILE)
prop.table(table(app$FLAG_CONT_MOBILE))
prop.table(table(app$FLAG_CONT_MOBILE[1:307511], y), margin = 1)

# CLINET - FLAG_PHONE
unique(app$FLAG_PHONE)
prop.table(table(app$FLAG_PHONE))
prop.table(table(app$FLAG_PHONE[1:307511], y), margin = 1)

# CLINET - FLAG_EMAIL
unique(app$FLAG_EMAIL)
prop.table(table(app$FLAG_EMAIL))
prop.table(table(app$FLAG_EMAIL[1:307511], y), margin = 1)

# CLINET - CNT_FAM_MEMBERS
summary(app$CNT_FAM_MEMBERS)
#-- app$CNT_FAM_MEMBERS[is.na(app$CNT_FAM_MEMBERS)] <- 1

# CLINET - REGION_POPULATION_RELATIVE
summary(app$REGION_POPULATION_RELATIVE)
app %>%
  ggplot(aes(x = REGION_POPULATION_RELATIVE)) +
  geom_histogram(bins = 30)

# CLINET - REGION_RATING_CLIENT
unique(app$REGION_RATING_CLIENT)
prop.table(table(app$REGION_RATING_CLIENT))
prop.table(table(app$REGION_RATING_CLIENT[1:307511], y), margin = 1)

# CLINET - REGION_RATING_CLIENT_W_CITY
unique(app$REGION_RATING_CLIENT_W_CITY)
prop.table(table(app$REGION_RATING_CLIENT_W_CITY))
prop.table(table(app$REGION_RATING_CLIENT_W_CITY[1:307511], y), margin = 1)
#-- app$REGION_RATING_CLIENT_W_CITY[app$REGION_RATING_CLIENT_W_CITY == -1] <- 2

#-- app %<>% mutate(REGION_RATING = str_c(REGION_RATING_CLIENT_W_CITY,"_",REGION_RATING_CLIENT))
#-- app %<>% select(-REGION_RATING_CLIENT_W_CITY,-REGION_RATING_CLIENT)

unique(app$REGION_RATING)
prop.table(table(app$REGION_RATING))
prop.table(table(app$REGION_RATING[1:307511], y), margin = 1)

# APPLICATION - WEEKDAY_APPR_PROCESS_START
unique(app$WEEKDAY_APPR_PROCESS_START)
prop.table(table(app$WEEKDAY_APPR_PROCESS_START))
prop.table(table(app$WEEKDAY_APPR_PROCESS_START[1:307511], y), margin = 1)

# APPLICATION - HOUR_APPR_PROCESS_START
unique(app$HOUR_APPR_PROCESS_START)
prop.table(table(app$HOUR_APPR_PROCESS_START))
prop.table(table(app$HOUR_APPR_PROCESS_START[1:307511], y), margin = 1)

# CLINET - REG_REGION_NOT_LIVE_REGION
# CLINET - REG_REGION_NOT_WORK_REGION
# CLINET - LIVE_REGION_NOT_WORK_REGION
# CLINET - REG_CITY_NOT_LIVE_CITY
# CLINET - REG_CITY_NOT_WORK_CITY
# CLINET - LIVE_CITY_NOT_WORK_CITY
prop.table(table(app$LIVE_CITY_NOT_WORK_CITY[1:307511], y), margin = 1)

#-- app$SUM_CITY_MATCH <- app$REG_CITY_NOT_LIVE_CITY+app$REG_CITY_NOT_WORK_CITY+app$LIVE_CITY_NOT_WORK_CITY
#-- app$SUM_REGION_MATCH <- app$REG_REGION_NOT_LIVE_REGION+app$REG_REGION_NOT_WORK_REGION+app$LIVE_REGION_NOT_WORK_REGION
#-- app$SUM_CITY_REGION_MATCH <- app$SUM_CITY_MATCH + app$SUM_REGION_MATCH

# CLIENT - ORGANIZATION_TYPE
unique(app$ORGANIZATION_TYPE)
prop.table(table(app$ORGANIZATION_TYPE))
prop.table(table(app$ORGANIZATION_TYPE[1:307511], y), margin = 1)




























