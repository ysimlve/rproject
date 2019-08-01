
########################################################Programm Start########################################################
setwd("C:/YuanLe/R/RWkDir/01. MDM_RegExp")
#ls()
#rm(list=ls())
#install.packages("geosphere")

options(stringsAsFactors = FALSE)
options(java.parameters = "-Xmx4g")
options(digits=9)

library(readxl)            ##import data from Excel
library(dplyr)             ##data manipulation, e.g. select, filter
library(stringdist)        ##string distance calculation and approximate string matching
library(RCurl)             ##use for processing Google Geocoding API and JSON file
library(RJSONIO)           ##use for processing Google Geocoding API and JSON file
library(xlsx)
library(geosphere)


########################################################Process########################################################
#GEO_LAT_GAPI
#GEO_LONG_GAPI
#GEO_LAT_BAPI
#GEO_LONG_BAPI

df.all <- uf_xlsxDataImport("Andy's Data.xlsx","Sheet1")
count_all <- nrow(df.all)   #Total Records - 2200
View(df.all)

#Both Baidu & Google can't retrieve Geocode - 164
View(df.all[which((df.all$GEO_LAT_GAPI == 0 | df.all$GEO_LAT_GAPI == "" | is.na(df.all$GEO_LAT_GAPI)) & 
                  (df.all$GEO_LAT_BAPI == 0 | df.all$GEO_LAT_BAPI == "" | is.na(df.all$GEO_LAT_BAPI))),])

#Google can retrieve geocode - 1906
View(df.all[which(df.all$GEO_LAT_GAPI !=0 & df.all$GEO_LAT_GAPI != "" & !is.na(df.all$GEO_LAT_GAPI)),])

#Baidu can retrieve geocode - 1716
View(df.all[which(df.all$GEO_LAT_BAPI !=0 & df.all$GEO_LAT_BAPI != "" & !is.na(df.all$GEO_LAT_BAPI)),])


#Baidu can retrieve geocode, Google can't - 130
View(df.all[which((df.all$GEO_LAT_BAPI !=0 & df.all$GEO_LAT_BAPI != "" & !is.na(df.all$GEO_LAT_BAPI)) & 
                  (df.all$GEO_LAT_GAPI ==0 | df.all$GEO_LAT_GAPI == "" | is.na(df.all$GEO_LAT_GAPI))),])


1906 + 130 + 164

################################################################################################################
#Both Baidu & Google can retrieve Geocode - 1586
df.both <- df.all[which((df.all$GEO_LAT_GAPI != 0 & df.all$GEO_LAT_GAPI != "" & !is.na(df.all$GEO_LAT_GAPI)) & 
                       (df.all$GEO_LAT_BAPI != 0 & df.all$GEO_LAT_BAPI != "" & !is.na(df.all$GEO_LAT_BAPI))),]

count_both <- nrow(df.both)

colnames(df.both)[3] <- "G_LAT"
colnames(df.both)[4] <- "G_LONG"
colnames(df.both)[5] <- "B_LAT"
colnames(df.both)[6] <- "B_LONG"



df.both$G_LAT_Conv_B <- rep(0.0,count_both)
df.both$G_LONG_Conv_B <- rep(0.0,count_both)
df.both$B_LAT_Conv_G <- rep(0.0,count_both)
df.both$B_LONG_Conv_G <- rep(0.0,count_both)
df.both$G_B_SQL_LAT <- rep(0.0,count_both)
df.both$G_B_SQL_LONG <- rep(0.0,count_both)
df.both$B_G_SQL_LAT <- rep(0.0,count_both)
df.both$B_G_SQL_LONG <- rep(0.0,count_both)

df.both$Dist_Org <- rep(0,count_both)
df.both$Dist_G_Org_B_Conv <- rep(0,count_both)
df.both$Dist_G_Conv_B_Org <- rep(0,count_both)
df.both$Dist_G_Conv_B_Org_SQL <- rep(0,count_both)
df.both$Dist_B_Conv_G_Org_SQL <- rep(0,count_both)

for(i in 1:count_both){
  G_LAT <- as.numeric(df.both$G_LAT[i])
  G_LONG <- as.numeric(df.both$G_LONG[i])
  B_LAT <- as.numeric(df.both$B_LAT[i])
  B_LONG <- as.numeric(df.both$B_LONG[i])
    
  G_Conv <- geoChina::conv(G_LAT,G_LONG,from = "GCJ-02",to = "BD-09")
  G_LAT_Conv_B <- df.both$G_LAT_Conv_B[i] <- G_Conv[[1]]
  G_LONG_Conv_B <- df.both$G_LONG_Conv_B[i] <- G_Conv[[2]]
  
  B_Conv <- geoChina::conv(B_LAT,B_LONG,from = "BD-09",to = "GCJ-02")
  B_LAT_Conv_G <- df.both$B_LAT_Conv_G[i] <- B_Conv[[1]]
  B_LONG_Conv_G <- df.both$B_LONG_Conv_G[i] <- B_Conv[[2]]
  
  #G_B_SQL_LAT <- df.both$G_B_SQL_LAT[i] <- (sqrt(G_LONG*G_LONG+G_LAT*G_LAT)+0.00002*sin(G_LAT*X_PI))*sin(atan2(G_LAT,G_LONG)+0.000003*cos(G_LONG*X_PI)) + 0.006
  #G_B_SQL_LONG <- df.both$G_B_SQL_LONG[i] <- (sqrt(G_LONG*G_LONG+G_LAT*G_LAT)+0.00002*sin(G_LAT*X_PI))*cos(atan2(G_LAT,G_LONG)+0.000003*cos(G_LONG*X_PI)) + 0.0065
  
  df.both$Dist_Org[i] <- round(distm(c(G_LONG,G_LAT), c(B_LONG,B_LAT), fun = distHaversine),2)
  df.both$Dist_G_Conv_B_Org[i] <- round(distm(c(G_LONG_Conv_B,G_LAT_Conv_B), c(B_LONG,B_LAT), fun = distHaversine),2)
  df.both$Dist_G_Org_B_Conv[i] <- round(distm(c(G_LONG,G_LAT), c(B_LONG_Conv_G,B_LAT_Conv_G), fun = distHaversine),2)
  #df.both$Dist_G_Conv_B_Org_SQL[i] <- round(distm(c(G_B_SQL_LONG,G_B_SQL_LAT), c(B_LONG,B_LAT), fun = distHaversine),2)
  
}


df.both_good <- filter(df.both,df.both$Dist_Org < 2000)
#View(df.both_good)

round(mean(df.both_good$Dist_Org),2) #905.91
round(sd(df.both_good$Dist_Org),2)   #141.6
length(df.both_good$Dist_Org[df.both_good$Dist_Org > 200])/length(df.both_good$Dist_Org) #99.93%
median(df.both_good$Dist_Org)        #901.7


round(mean(df.both_good$Dist_G_Conv_B_Org),2) #107.38
round(sd(df.both_good$Dist_G_Conv_B_Org),2)   #224.77
length(df.both_good$Dist_G_Conv_B_Org[df.both_good$Dist_G_Conv_B_Org > 200])/length(df.both_good$Dist_G_Conv_B_Org) #10.53%
median(df.both_good$Dist_G_Conv_B_Org)        #40

#########erase deviation point
round(mean(df.both_good$Dist_G_Conv_B_Org[df.both_good$Dist_G_Conv_B_Org < 200]),2) #49.22
round(sd(df.both_good$Dist_G_Conv_B_Org[df.both_good$Dist_G_Conv_B_Org < 200]),2)   #42.34
median(df.both_good$Dist_G_Conv_B_Org[df.both_good$Dist_G_Conv_B_Org < 200])        #35.23
range(df.both_good$Dist_G_Conv_B_Org[df.both_good$Dist_G_Conv_B_Org < 200])

count1 <- length(df.both_good$Dist_G_Conv_B_Org[df.both_good$Dist_G_Conv_B_Org < 200])
plot(c(1:count1),df.both_good$Dist_G_Conv_B_Org[df.both_good$Dist_G_Conv_B_Org < 200])


round(mean(df.both_good$Dist_G_Org_B_Conv),2) #107.38
round(sd(df.both_good$Dist_G_Org_B_Conv),2)   #224.71
length(df.both_good$Dist_G_Org_B_Conv[df.both_good$Dist_G_Org_B_Conv > 200])/length(df.both_good$Dist_G_Org_B_Conv) #10.46%
median(df.both_good$Dist_G_Org_B_Conv)        #40

#########erase deviation point
round(mean(df.both_good$Dist_G_Org_B_Conv[df.both_good$Dist_G_Org_B_Conv < 200]),2) #49.33
round(sd(df.both_good$Dist_G_Org_B_Conv[df.both_good$Dist_G_Org_B_Conv < 200]),2)   #42.53
median(df.both_good$Dist_G_Org_B_Conv[df.both_good$Dist_G_Org_B_Conv < 200])        #35.305

count1 <- length(df.both_good$Dist_G_Org_B_Conv[df.both_good$Dist_G_Org_B_Conv < 200])
plot(c(1:count1),df.both_good$Dist_G_Org_B_Conv[df.both_good$Dist_G_Org_B_Conv < 200])



round(mean(df.both_good$Dist_G_Conv_B_Org_SQL),2) #107.38
round(sd(df.both_good$Dist_G_Conv_B_Org_SQL),2)   #224.77
length(df.both_good$Dist_G_Conv_B_Org_SQL[df.both_good$Dist_G_Conv_B_Org_SQL > 200])/length(df.both_good$Dist_G_Conv_B_Org_SQL) #10.53%
median(df.both_good$Dist_G_Conv_B_Org_SQL)        #40

uf_xlsxDataExport(filter(df.both_good,Dist_G_Org_B_Conv > 200), "DIFF_G_B.xlsx","sheet1") 












