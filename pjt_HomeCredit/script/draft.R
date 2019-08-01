getwd()
library(tidyverse)
library(scales)
set.seed(0)

# data load
app_dev <- read_csv("./data/application_train.csv")
app_test <- read_csv("./data/application_test.csv")

str(app_dev)
summary(app_dev)
head(app_dev)

# TARGET - highly unbalanced
prop.table(table(app_dev$TARGET)) 
app_dev %>% select(TARGET) %>% 
  group_by(TARGET) %>% dplyr::count() %>%
  ggplot(aes(x = TARGET, y = n)) +
  geom_bar(stat = "identity",fill = "dodgerblue4", width = .4) +
  labs(x = "Target", y = "count", title = "Distribution of target variable values") +
  theme_minimal() +
  scale_y_continuous(labels = comma) + 
  geom_text(aes(label = n),vjust = 1.6, colour = "white", size = 4) +
  geom_text(aes(label = ifelse(TARGET == 1,"Only 8% of all samples with label '1'", "")), 
            colour = "red", vjust = -5, size = 5)

## check the missing percentage of each column
app_dev %>% summarise_all(funs(sum(is.na(.))/n() * 100)) %>%  
  gather(key = "feature", value = missing_pct) %>%
  filter(missing_pct > 0) %>%
  ggplot(aes(x = reorder(feature, missing_pct), y = missing_pct)) + 
  geom_bar(stat = "identity", fill = "dodgerblue4") +
  labs(y = "missing %", x = "feature") +
  coord_flip() + 
  theme_minimal()





