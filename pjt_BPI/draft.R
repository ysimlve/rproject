getwd()

install.packages("bupaR")
install.packages("edeaR")
install.packages("eventdataR")
install.packages("processmapR")
install.packages("processmonitR")
install.packages("xesreadR")
install.packages("petrinetR")
install.packages("DiagrammeRsvg")
install.packages("heuristicsmineR")
install.packages("petrinetR")

library(bupaR)
library(edeaR)
library(processmapR)
library(processmonitR)
library(eventdataR)
library(readr)
library(tidyverse)
library(DiagrammeR)
library(ggplot2)
library(stringr)
library(lubridate)
library(heuristicsmineR)
library(petrinetR)
library(plotly)

############ basic infor ############ 
events <- xesreadR::read_xes("./data/financial_log.xes")
events <- events %>% mutate(resource_id = ifelse(is.na(resource_id), "000", resource_id))
class(events)
events %>% mapping()
#Case identifier:		CASE_concept_name 
#Activity identifier:		activity_id 
#Resource identifier:		resource_id 
#Activity instance identifier:	activity_instance_id 
#Timestamp:			timestamp 
#Lifecycle transition:		lifecycle_id

# Eventlog overview
events %>% summary()
#Number of events:  262200
#Number of cases:  13087 = 7635(A_DECLINED) + 2807(A_CANCELLED) + 2246(A_APPROVED); 
#Number of traces:  4366
#Number of distinct activities:  24
#Number of resource: 69
#Average trace length:  20.03515 = 262200 / 13087
#Events occurred from 2011-09-30 22:38:44 until 2012-03-14 15:04:54 

event_cases <- events %>% cases()
event_cases # 基于这个数据集可以做很多分析,e.g. trace_length 分布, 时间分析, number_of_activities 分布 (不同结果的对比,A_DECLINED/A_CANCELLED/A_APPROVED)
event_cases %>% ggplot(mapping = aes(x=trace_length)) + geom_histogram()

######draft ##############################################################################################################################
events

trace_explorer <- events %>%
  trace_explorer(coverage = 0.5)

events %>%
  filter_trace_frequency(percentage = .80) %>%    # show only the most frequent traces
  group_by(activity_id) %>% 
  throughput_time('log', units = 'hours') %>% plot()

events %>% 
  processing_time("resource",units = "sec") %>%
  plot

min(events$timestamp)
all(!is.na(events$timestamp))
patients$time
patients %>% 
  processing_time("activity") %>%
  plot

events %>%
  trace_length("log") %>%
  plot

View(events %>% activities())
events %>% filter_activity(c("A_SUBMITTED","A_PARTLYSUBMITTED","A_DECLINED","A_PREACCEPTED","A_ACCEPTED","A_FINALIZED","A_CANCELLED","A_ACTIVATED","A_APPROVED","A_REGISTERED")) %>%
  process_map()

events %>% filter_activity(c("A_SUBMITTED","A_PARTLYSUBMITTED","A_DECLINED","A_PREACCEPTED","A_ACCEPTED","A_FINALIZED","A_CANCELLED","A_ACTIVATED","A_APPROVED","A_REGISTERED")) %>%
  act_collapse(A_Success = c("A_ACTIVATED","A_APPROVED","A_REGISTERED"),
               A_SUBMITTED = c("A_SUBMITTED","A_PARTLYSUBMITTED")) %>%
  process_map()

events %>% filter_activity(c("A_SUBMITTED","A_PARTLYSUBMITTED","A_DECLINED","A_PREACCEPTED","A_ACCEPTED","A_FINALIZED","A_CANCELLED","A_ACTIVATED","A_APPROVED","A_REGISTERED")) %>%
  precedence_matrix(type = "absolute") %>% plot()

events_declined <- 
events %>% 
  filter_activity(c("A_PARTLYSUBMITTED","A_DECLINED","A_PREACCEPTED","A_ACCEPTED","A_FINALIZED","W_Afhandelen leads","W_Completeren aanvraag",
                    "W_Nabellen offertes","W_Valideren aanvraag","W_Nabellen incomplete dossiers","W_Beoordelen fraude","W_Wijzigen contractgegevens")) %>%
  filter_trim(start_activities = "A_PARTLYSUBMITTED",end_activities = c("A_DECLINED"))
events_declined %>% process_map(type = frequency())

events_declined_cases <- events_declined %>% cases()
events_declined_cases$declined_reason <- ""
temp_1 <- unlist(lapply(gregexpr(",",gsub(",A_DECLINED","",events_declined_cases_traces)),max))

for (x in 1:nrow(events_declined_cases)) {
  if(events_declined_cases$trace_length[x] <=2)
    events_declined_cases$declined_reason[x] <- "Directly"
  else{
    events_declined_cases$declined_reason[x] <- 
    substr(gsub(",A_DECLINED","",events_declined_cases$trace[x]),
           max(unlist(gregexpr(",",gsub(",A_DECLINED","",events_declined_cases$trace[x])))) + 1,
           nchar(events_declined_cases$trace[x]))
  }
    
}
View(events_declined_cases)


cases <- events %>% cases() 

n_cs <- nrow(cases)
cases$final_status <- ""
for (x in 1:n_cs) {
  if(regexpr("A_DECLINED",cases$trace[x]) > 0)
    cases$final_status[x] <- "DECLINED"
  else if(regexpr("A_CANCELLED",cases$trace[x]) > 0)
    cases$final_status[x] <- "CANCELLED"
  else if(regexpr("A_APPROVED",cases$trace[x]) > 0 | regexpr("A_REGISTERED",cases$trace[x]) > 0 | regexpr("A_ACTIVATED",cases$trace[x]) > 0)
    cases$final_status[x] <- "SUCCEED"
  else
    cases$final_status[x] <- "OTHER"
}

View(cases)
cases %>% ggplot(aes(x = final_status)) + geom_bar()

cases %>% ggplot(mapping = aes(x = duration_in_days, color=final_status)) + geom_density() + geom_vline(aes(xintercept = mean(duration_in_days)),colour = anz_color1) +
  labs(x = "Durantion In Days", y = "",title = "Distribution of Application Durantion") + theme_minimal() 
cases %>% filter(final_status != "OTHER") %>% ggplot(aes(x = factor(final_status), y = duration_in_days)) + geom_boxplot() +
  labs(x = "Final Status", y = "",title = "Boxplot of Duration in Days") + theme_minimal() 

mean(cases$duration_in_days[cases$final_status == "DECLINED"]) # 2.048094
mean(cases$duration_in_days[cases$final_status == "CANCELLED"]) # 18.59956
mean(cases$duration_in_days[cases$final_status == "SUCCEED"]) # 16.73539

# 7635 was declined in total
# 3429 was declined derectly
# 2234 was declined after 'W_Afhandelen leads' (Following up on incomplete initial submissions)
# 25 was declined after 'A_ACCEPTED' --?
# 57 was declined after 'W_Beoordelen fraude' (Investigating suspect fraud cases)
# 1088 was declined after W_Completeren aanvraag (Completing pre-accepted applications)
# 86 was declined after W_Nabellen incomplete dossiers (Seeking additional information during assessment phase)
# 668 was declined after W_Valideren aanvraag (Assessing the application)
# 48 was declined after W_Nabellen offertes (Follow up after transmitting offers to qualified applicants)



7635 = 3429 + 2234 + 25 + 57 + 1088 + 86 + 668 + 48
Total cases (13087) = 2807(A_CANCELLED) + 2246(A_Succeed) + 7635(A_DECLINED) + 69(A_PREACCEPTED) + 3(A_ACCEPTED) + 327(A_FINALIZED)

3429 / 7635

x <- list("Total","CANCELLED","PREACCEPTED","Direct_Decline","Incomplete","Suspect_fraud","Completeren","Nabellen","Assessing","Qualified","ACCEPTED","Succeed")
measure <- c("Total","relative","relative","relative","relative","relative","relative","relative","relative","relative","relative","relative")
text <- c("13087","-2807","-399","-3429","-2234","-57","-1088","-86","-668","-48","-25","2246")
y <- c(13087,-2807,-399,-3429,-2234,-57,-1088,-86,-668,-48,-25,-2246)
wf_data <- data.frame(x=factor(x,levels = x),measure,text,y)

p <- plot_ly(
  wf_data, name = "20", type = "waterfall", measure = ~measure,
  x = ~x, textposition = "outside", y= ~y, text =~text,
  connector = list(line = list(color= "rgb(63, 63, 63)"))) %>%
  layout(title = "Profit and loss statement 2018",
         xaxis = list(title = ""),
         yaxis = list(title = ""),
         autosize = TRUE,
         showlegend = TRUE)
p

dependency_matrix(events,threshold = .9) %>% render_dependency_matrix()
causal_net(events) %>% render_causal_net()



############ data preprocessing ############ 
## event filter
View(events %>% activities())
events %>% filter_activity(c("A_DECLINED","A_CANCELLED","A_APPROVED")) %>% activities()
events %>% filter_activity_frequency(percentage = .8) %>% activities()
events %>% filter_activity_frequency(interval = c(10000,20000)) %>% activities()

events %>% resources()
events %>% filter_resource_frequency(percentage = .1) %>% resources()

events %>% filter_trim(start_activities = "A_SUBMITTED",end_activities = c("A_APPROVED")) %>% process_map(type = frequency())

events %>% filter_time_period(interval = ymd(20111230,20111231), filter_method = "trim") %>% summary()

## case filter
# processing time
events %>% filter_processing_time(interval = c(2,5),units = "days") %>% processing_time(units = "days")
# Trace length
events %>% filter_trace_length(interval = c(50,60)) %>% trace_length(units = "days")
# activity presence
events %>% filter_activity_presence("A_APPROVED") %>% traces()  #成功的case也有2000多种不同的trace
events %>% filter_activity_presence("A_DECLINED") %>% traces()
events %>% filter_activity_presence(c("A_APPROVED","W_Completeren aanvraag"), method = "all") %>% traces()  # method = "one_of"
# Precedence
events %>% filter_precedence(antecedents = "A_SUBMITTED",
                             consequents = "A_APPROVED",
                             precedence_type = "directly_follows") %>% traces  # precedence_type = "eventually_follows"
patients %>%
  filter_precedence(antecedents = "Triage and Assessment",
                    consequents = c("Blood test", "X-Ray"),
                    precedence_type = "eventually_follows",
                    filter_method = "all") %>%
  traces

# time period
events %>% filter_time_period(interval = ymd(20111230,20111231), filter_method = "start") %>% dotted_chart

## Is-a aggregation
events %>% process_map()

# zoom out
events %>% 
  act_unite(Work = c("W_Completeren aanvraag","W_Nabellen offertes","W_Nabellen incomplete dossiers","W_Valideren aanvraag",
                              "W_Afhandelen leads","W_Beoordelen fraude","W_Wijzigen contractgegevens"),
            Offer = c("O_CREATED","O_SELECTED","O_SENT","O_CANCELLED","O_SENT_BACK","O_ACCEPTED","O_DECLINED"),
            ) %>%
  
  process_map()


############ EDA (Exploring Data Analysis) ############ 
# Time perspective - Idel time
events %>% idle_time(level = "resource", units = "days") %>% plot()

# Time perspective - Processing time
events %>% mutate(resource_id = "110") %>% processing_time(level = "activity",units = "sec") %>% plot()
# Time perspective - Organizational Perspective
events %>% throughput_time("log") %>% plot()

# Organizational Perspective
events %>% resource_frequency("resource") 
events %>% resource_involvement("resource") %>% plot # 资源利用问题

# Structuredness
events %>% activity_presence() %>% plot

events %>% trace_coverage("trace") %>% plot()

events %>% trace_length("log") %>% plot

events %>% process_map(type = frequency("relative"))
events %>% process_map(type = frequency("absolute_case"))
events %>% process_map(performance(median, "days"))
events %>%
  process_map(type_nodes = frequency("relative_case"),
              type_edges = performance(mean))

## Precedence diagrams
events %>%
events %>%
  precedence_matrix(type = "relative") %>% plot()
events %>%
  precedence_matrix(type = "absolute-antecedent") %>%
  plot

events %>%
  dotted_chart(x = "absolute", y = "start")


## Social network analysis
events %>%
  resource_map()

# dashboard
events %>% processmonitR::performance_dashboard()

############ Process Discovery ############ 
dependency_matrix(events) %>% render_dependency_matrix()
causal_net(events) %>% render_causal_net()
m <- precedence_matrix_absolute(L_heur_1)
as.matrix(m)

dependency_matrix(L_heur_1, threshold = .7) %>% render_dependency_matrix()
causal_net(L_heur_1, threshold = .7) %>% render_causal_net()















events %>% filter_activity(activities = c(""))
















































