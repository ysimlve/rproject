getwd()

library(bupaR)
library(readr)
library(tidyverse)
library(DiagrammeR)
library(ggplot2)
library(stringr)
library(lubridate)
# summary informartion of event logs
eventdataR::patients
patients %>% summary()

# metadata
patients %>% mapping()

patients %>% cases() %>% filter(patient == 1)

patients$patient
patients$handling

# activity filter
patients %>% filter_activity(c("Blood test","X-Ray")) %>% activities()

patients %>%
  filter_activity_frequency(percentage = 0.5, reverse = T) %>%
  activities

patients %>%
  filter_activity_frequency(interval = c(300,500)) %>%
  activities

# resource filter
patients %>%
  filter_resource_frequency(perc = 0.80) %>%
  resources()

# Trim cases
patients %>%
  filter_trim(start_activities = "Registration", end_activities =  c("MRI SCAN","X-Ray")) %>%
  process_map(type = performance())

# Trim to time window
patients %>%
  filter_time_period(interval = ymd(c(20171201, 20171231)), filter_method = "trim") %>%
  summary()

# case filter
patients %>%
  filter_trace_length(interval = c(2, 5)) %>%
  trace_length(units = "hours")

# Activity presence 
patients %>%
  filter_activity_presence(c("X-Ray", "MRI SCAN"), method = "one_of")  %>%
  traces

patients %>%
  filter_endpoints(start_activities = "Registration", end_activities = "Check-out") %>%
  process_map()

# Is-a aggregation
patients %>% 
  process_map()
# 把几个状态合并成一个状态
patients %>%
  act_unite(Scan = c("MRI SCAN","X-Ray")) %>%
  process_map()

patients %>% 
  act_collapse(Testing = c("MRI SCAN","X-Ray","Blood test")) %>%
  process_map()

patients %>%
  act_recode("Check-in" = "Registration",
             "MRI Scan" = "MRI SCAN") %>%
  process_map()


sepsis %>%
  group_by_case() %>%
  first_n(3) %>%
  trace_explorer(coverage = 0.95)




###### Exploring event data
patients %>% 
  processing_time("activity") %>%
  plot

patients %>%
  throughput_time("log") %>%
  plot()

patients %>% activity_presence() %>%
  plot

patients %>%
  activity_frequency("activity")

patients %>%
  trace_coverage("trace") %>%
  plot()
patients %>%
  trace_length("log") %>%
  plot


patients %>%
  precedence_matrix(type = "absolute") %>%
  plot
