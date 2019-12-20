########################################################1. 开始########################################################
setwd("C:/YuanLe/R/RWkDir/01. MDM_RegExp")
#ls()
#rm(list=ls())
#install.packages("Hmisc")

options(stringsAsFactors = FALSE)
options(java.parameters = "-Xmx4g")

library(readxl)
library(xlsx)
library(dplyr)
library(Kendall)
library(Hmisc)

########################################################2. 加载和处理原始数据########################################################
uf_xlsxDataImport <- function(xlsx_file,spreedsheet){
  df_xlsx <- read.xlsx(xlsx_file,sheetName = spreedsheet,header=TRUE,encoding = "UTF-8")
  return(df_xlsx)
}
uf_xlsxDataExport <- function(p_df, xlsx_file, spreedsheet, append = FALSE){
  xlsx::write.xlsx(p_df,xlsx_file,sheetName = spreedsheet, append = append)
}

df.address.element <- uf_xlsxDataImport("./Dalton_Test_Output/ReferenceData.xlsx","Address_Element")

org.data.1st <- uf_xlsxDataImport("./Dalton_Test_Output/Dalton_China_Test_Data_Both.xlsx","Chinese")
org.data.2nd <- uf_xlsxDataImport("./Dalton_Test_Output/Dalton_China_Test_Data_Both_Lookup1.xlsx","Chinese")
names(org.data.1st)
names(org.data.2nd)
nrow(org.data.1st)
nrow(org.data.2nd)

#merge(op.data.1st,op.data.2nd,by = "SRC_SYS_ID",all = TRUE)
uf_trans_score <- function(org_score,type){
  if(type == "normalize"){
    trans_score = "ANDY"
    if(any(org_score == c("0","1","2"))){
      trans_score <- "NotCheck"
    }else if(any(org_score == c("3","4"))){
      trans_score <- "JustChecked"
    }else if(any(org_score == c("5","6"))){
      trans_score <- "JustValidated"
    }else if(any(org_score == c("7","8","9"))){
      trans_score <- "Corrected"
    }else if(any(org_score == c("C","D","E"))){
      trans_score <- "Verified"
    }else if(any(org_score == c("F"))){
      trans_score <- "Perfect"
    }
  }else if(type == "trans"){
    trans_score <- org_score
    if(org_score == "C"){
      trans_score <- "10"
    }else if(org_score == "D"){
      trans_score <- "11"
    }else if(org_score == "E"){
      trans_score <- "12"
    } else if(org_score == "F"){
      trans_score <- "13"
    }
  }
  
  return(trans_score)
}
#orgData <- org.data.2nd
uf_orgData_process <- function(orgData, filterRs = F, scoreAsFactor = F, elmtAsFactor = F,defaultScoreLevel = F,defaultElmtLevel = F,
                               scoreLevel = c("0","1","2","3","4","5"),
                               elmtLevel = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13"),transType = "trans"){
  p_data <- orgData
  if(filterRs){p_data <- filter(orgData,ADDR_RESOLUTION_CD == "00000000000000000000")}
  analysis.data <- select(p_data,
                          SRC_SYS_ID,ADDR_MAIL_SCORE,ADDR_MATCH_CD,ADDR_RESOLUTION_CD,ADDR_ELEMENT_STAT)
  count.analysis <- nrow(analysis.data)
  #View(analysis.data)
  #names(analysis.data)
  
  new.col.rsl.code <- paste0("rslCode_",as.character(c(1:20)),"_",df.address.element$Addr_Elmt_Code)
  analysis.data[,c((ncol(analysis.data) + 1):(ncol(analysis.data) + 20))] <- rep("-1",count.analysis)
  names(analysis.data)[(ncol(analysis.data) - 20 + 1) : ncol(analysis.data)] <- new.col.rsl.code
  
  new.col.elmt.status <- paste0("elmtStat_",as.character(c(1:20)),"_",df.address.element$Addr_Elmt_Code)
  analysis.data[,c((ncol(analysis.data) + 1):(ncol(analysis.data) + 20))] <- rep("-1",count.analysis)
  names(analysis.data)[(ncol(analysis.data) - 20 + 1) : ncol(analysis.data)] <- new.col.elmt.status
  
  new.col.start.position <- which(names(analysis.data) == "rslCode_1_Postal_Level_0")
  
  for(i in 1:count.analysis){
    #print(i) 
    ADDR_RESOLUTION_CD <- ifelse(is.na(analysis.data$ADDR_RESOLUTION_CD[i]),"--------------------",analysis.data$ADDR_RESOLUTION_CD[i])
    ADDR_ELEMENT_STAT <- ifelse(is.na(analysis.data$ADDR_ELEMENT_STAT[i]),"--------------------",analysis.data$ADDR_ELEMENT_STAT[i])
    
    for(k in 1:20){
      analysis.data[i,(new.col.start.position + k - 1)] <- substr(ADDR_RESOLUTION_CD,k,k)
    }
    
    for(p in 1:20){
      analysis.data[i,(new.col.start.position + 20 + p - 1)] <- as.character(uf_trans_score(substr(ADDR_ELEMENT_STAT,p,p),transType))
    }
  }
  analysis.data$ADDR_MAIL_SCORE <- as.character(analysis.data$ADDR_MAIL_SCORE)
  
  if(scoreAsFactor){
    if(defaultScoreLevel){
      analysis.data$ADDR_MAIL_SCORE <- factor(analysis.data$ADDR_MAIL_SCORE,levels = scoreLevel,ordered = T)
    }else{
      analysis.data$ADDR_MAIL_SCORE <- factor(analysis.data$ADDR_MAIL_SCORE,
                                              levels = unique(analysis.data$ADDR_MAIL_SCORE)[order(unique(analysis.data$ADDR_MAIL_SCORE))],
                                              ordered = T)
    }
    
  }
  
  if(elmtAsFactor){
    if(defaultElmtLevel){
      if(transType == "trans"){elmtLevel = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13")}else if(transType == "normalize"){
        elmtLevel = c("NotCheck","JustChecked","JustValidated","Corrected","Verified","Perfect")
      }
      analysis.data$Elmt_Stat_1 <- factor(analysis.data$Elmt_Stat_1,levels = elmtLevel,ordered = T)
      analysis.data$Elmt_Stat_2 <- factor(analysis.data$Elmt_Stat_2,levels = elmtLevel,ordered = T)
      analysis.data$Elmt_Stat_3 <- factor(analysis.data$Elmt_Stat_3,levels = elmtLevel,ordered = T)
      analysis.data$Elmt_Stat_4 <- factor(analysis.data$Elmt_Stat_4,levels = elmtLevel,ordered = T)
      analysis.data$Elmt_Stat_5 <- factor(analysis.data$Elmt_Stat_5,levels = elmtLevel,ordered = T)
      analysis.data$Elmt_Stat_6 <- factor(analysis.data$Elmt_Stat_6,levels = elmtLevel,ordered = T)
      analysis.data$Elmt_Stat_7 <- factor(analysis.data$Elmt_Stat_7,levels = elmtLevel,ordered = T)
      analysis.data$Elmt_Stat_8 <- factor(analysis.data$Elmt_Stat_8,levels = elmtLevel,ordered = T)
      analysis.data$Elmt_Stat_9 <- factor(analysis.data$Elmt_Stat_9,levels = elmtLevel,ordered = T)
      analysis.data$Elmt_Stat_10 <- factor(analysis.data$Elmt_Stat_10,levels = elmtLevel,ordered = T)
      analysis.data$Elmt_Stat_11 <- factor(analysis.data$Elmt_Stat_11,levels = elmtLevel,ordered = T)
      analysis.data$Elmt_Stat_12 <- factor(analysis.data$Elmt_Stat_12,levels = elmtLevel,ordered = T)
      analysis.data$Elmt_Stat_13 <- factor(analysis.data$Elmt_Stat_13,levels = elmtLevel,ordered = T)
      analysis.data$Elmt_Stat_14 <- factor(analysis.data$Elmt_Stat_14,levels = elmtLevel,ordered = T)
      analysis.data$Elmt_Stat_15 <- factor(analysis.data$Elmt_Stat_15,levels = elmtLevel,ordered = T)
      analysis.data$Elmt_Stat_16 <- factor(analysis.data$Elmt_Stat_16,levels = elmtLevel,ordered = T)
      analysis.data$Elmt_Stat_17 <- factor(analysis.data$Elmt_Stat_17,levels = elmtLevel,ordered = T)
      analysis.data$Elmt_Stat_18 <- factor(analysis.data$Elmt_Stat_18,levels = elmtLevel,ordered = T)
      analysis.data$Elmt_Stat_19 <- factor(analysis.data$Elmt_Stat_19,levels = elmtLevel,ordered = T)
      analysis.data$Elmt_Stat_20 <- factor(analysis.data$Elmt_Stat_20,levels = elmtLevel,ordered = T)
    }
  }
  
  return(analysis.data)
}

analysis.data.1st <- uf_orgData_process(org.data.1st,F,F,F,transType = "trans")
analysis.data.2nd <- uf_orgData_process(org.data.2nd,F,F,F,transType = "trans")

nrow(analysis.data.1st)
nrow(analysis.data.2nd)

#View(analysis.data.1st)
#View(analysis.data.2nd)

########################################################4. Validation did not occur########################################################
View(analysis.data.2nd)
names(analysis.data.2nd)
unique(analysis.data.2nd$ADDR_RESOLUTION_CD)

analyData.2nd.NonValidate <- filter(analysis.data.2nd, ADDR_RESOLUTION_CD != "00000000000000000000", !is.na(ADDR_RESOLUTION_CD))
#none of the records in analyData.2nd.NonValidate has score > 2
count.NonValidate <- nrow(analyData.2nd.NonValidate)  #873
unique(analyData.2nd.NonValidate$ADDR_RESOLUTION_CD)

View(analyData.2nd.NonValidate)

#选出有哪些address element 有问题
issue_addr_elmts <- character(0)
for(k in 6:25){
  addr_elmt <-  gsub("^_","",substr(names(analyData.2nd.NonValidate)[k],nchar("rslCode_1_") + 1, 100))
  if(!all(analyData.2nd.NonValidate[,k] == "0" | analyData.2nd.NonValidate[,k] == "-")){
    seq <- length(issue_addr_elmts) + 1
    issue_addr_elmts[seq] <- addr_elmt
    attr(issue_addr_elmts,"num")[seq] <- k
  }
}

anlOutput <- data.frame(SRC_SYS_ID = "00000")
for(n in 1:length(issue_addr_elmts)){
  anlOutput <- cbind(anlOutput,x = "")
  names(anlOutput)[n + 1] <- issue_addr_elmts[n]
}
anlOutput <- anlOutput[0,]

Rsl_Code_Desc <- c("Address Element of input is missing", 
                   "Street Number of input is outside the valid range", 
                   "Input address contains more than one instance of the element",
                   "Address Element of input is ambiguous",
                   "Address Element of input is contradict with another element", 
                   "Address Element cannot be correctted",
                   "Address Element does not conform to mail carrier validation rules")
attr(Rsl_Code_Desc,"Code") <- c(2:8)

#insert result into anlOutput
for(i in 1:count.NonValidate){
  single_row <- analyData.2nd.NonValidate[i,c(1:25)]
  for(j in attr(issue_addr_elmts,"num")){
    if(single_row[1,j] != "0"){
      issue_item <- rep("",length(issue_addr_elmts))
      whch <- which(attr(issue_addr_elmts,"num") == j)
      issue_item[whch] <- Rsl_Code_Desc[which(attr(Rsl_Code_Desc,"Code") == single_row[1,j])[1]]
      
      anlOutput <- rbind(anlOutput,c(single_row$SRC_SYS_ID,issue_item))
      
    }
  }
  names(anlOutput) <- c("SRC_SYS_ID",issue_addr_elmts)
}
View(anlOutput)
names(anlOutput)
nrow(anlOutput)

nrow(filter(anlOutput,Postal_Level_0 != ""))
nrow(filter(anlOutput,Locality_City_0 != ""))
nrow(filter(anlOutput,Locality_District_1 != ""))
nrow(filter(anlOutput,Street_Level_0 != ""))
nrow(filter(anlOutput,Street_Num_0 != ""))
nrow(filter(anlOutput,Building_Level_0 != ""))

summarise(group_by(filter(anlOutput,Postal_Level_0 != ""),Postal_Level_0),n=n())
summarise(group_by(filter(anlOutput,Locality_City_0 != ""),Locality_City_0),n=n())
summarise(group_by(filter(anlOutput,Locality_District_1 != ""),Locality_District_1),n=n())
summarise(group_by(filter(anlOutput,Street_Level_0 != ""),Street_Level_0),n=n())
summarise(group_by(filter(anlOutput,Street_Num_0 != ""),Street_Num_0),n=n())
summarise(group_by(filter(anlOutput,Building_Level_0 != ""),Building_Level_0),n=n())

anlOutput_final <- dplyr::left_join(anlOutput,org.data.2nd,by="SRC_SYS_ID")
nrow(anlOutput_final)
View(anlOutput_final)
uf_xlsxDataExport(anlOutput_final,"./Dalton_Test_Output/Analysis.xlsx","NonValidated",append = T)



########################################################3. 相关性分析########################################################
########################################################3.1 卡方检验########################################################
########################################################3.1.1 第一轮output数据########################################################
str(analysis.data.1st)

scoreLevel = unique(analysis.data$ADDR_MAIL_SCORE)[order(unique(analysis.data$ADDR_MAIL_SCORE))]
elmtLevel = c("NotCheck","JustChecked","JustValidated","Corrected","Verified","Perfect")
df <- (length(scoreLevel) - 1) * (length(elmtLevel) - 1)

t_score_elmt_1 <- table(Score = analysis.data.1st$ADDR_MAIL_SCORE, Elmt = as.character(analysis.data.1st$Elmt_Stat_1))
t_score_elmt_2 <- table(Score = analysis.data.1st$ADDR_MAIL_SCORE, Elmt = as.character(analysis.data.1st$Elmt_Stat_2))
t_score_elmt_3 <- table(Score = analysis.data.1st$ADDR_MAIL_SCORE, Elmt = as.character(analysis.data.1st$Elmt_Stat_3))
t_score_elmt_4 <- table(Score = analysis.data.1st$ADDR_MAIL_SCORE, Elmt = as.character(analysis.data.1st$Elmt_Stat_4))
t_score_elmt_5 <- table(Score = analysis.data.1st$ADDR_MAIL_SCORE, Elmt = as.character(analysis.data.1st$Elmt_Stat_5))
t_score_elmt_6 <- table(Score = analysis.data.1st$ADDR_MAIL_SCORE, Elmt = as.character(analysis.data.1st$Elmt_Stat_6))
t_score_elmt_7 <- table(Score = analysis.data.1st$ADDR_MAIL_SCORE, Elmt = as.character(analysis.data.1st$Elmt_Stat_7))
t_score_elmt_8 <- table(Score = analysis.data.1st$ADDR_MAIL_SCORE, Elmt = as.character(analysis.data.1st$Elmt_Stat_8))
t_score_elmt_9 <- table(Score = analysis.data.1st$ADDR_MAIL_SCORE, Elmt = as.character(analysis.data.1st$Elmt_Stat_9))
t_score_elmt_10 <- table(Score = analysis.data.1st$ADDR_MAIL_SCORE, Elmt = as.character(analysis.data.1st$Elmt_Stat_10))
t_score_elmt_11 <- table(Score = analysis.data.1st$ADDR_MAIL_SCORE, Elmt = as.character(analysis.data.1st$Elmt_Stat_11))
t_score_elmt_12 <- table(Score = analysis.data.1st$ADDR_MAIL_SCORE, Elmt = as.character(analysis.data.1st$Elmt_Stat_12))
t_score_elmt_13 <- table(Score = analysis.data.1st$ADDR_MAIL_SCORE, Elmt = as.character(analysis.data.1st$Elmt_Stat_13))
t_score_elmt_14 <- table(Score = analysis.data.1st$ADDR_MAIL_SCORE, Elmt = as.character(analysis.data.1st$Elmt_Stat_14))
t_score_elmt_15 <- table(Score = analysis.data.1st$ADDR_MAIL_SCORE, Elmt = as.character(analysis.data.1st$Elmt_Stat_15))
t_score_elmt_16 <- table(Score = analysis.data.1st$ADDR_MAIL_SCORE, Elmt = as.character(analysis.data.1st$Elmt_Stat_16))
t_score_elmt_17 <- table(Score = analysis.data.1st$ADDR_MAIL_SCORE, Elmt = as.character(analysis.data.1st$Elmt_Stat_17))
t_score_elmt_18 <- table(Score = analysis.data.1st$ADDR_MAIL_SCORE, Elmt = as.character(analysis.data.1st$Elmt_Stat_18))
t_score_elmt_19 <- table(Score = analysis.data.1st$ADDR_MAIL_SCORE, Elmt = as.character(analysis.data.1st$Elmt_Stat_19))
t_score_elmt_20 <- table(Score = analysis.data.1st$ADDR_MAIL_SCORE, Elmt = as.character(analysis.data.1st$Elmt_Stat_20))

chisq.test(t_score_elmt_1, correct = T)
chisq.test(t_score_elmt_2, correct = T)
chisq.test(t_score_elmt_3, correct = T)
chisq.test(t_score_elmt_4, correct = T)
chisq.test(t_score_elmt_5, correct = T)
chisq.test(t_score_elmt_6, correct = T)
chisq.test(t_score_elmt_7, correct = T)
chisq.test(t_score_elmt_8, correct = T)
chisq.test(t_score_elmt_9, correct = T)
chisq.test(t_score_elmt_10, correct = T)
chisq.test(t_score_elmt_11, correct = T)
chisq.test(t_score_elmt_12, correct = T)
chisq.test(t_score_elmt_13, correct = T)
chisq.test(t_score_elmt_14, correct = T)
chisq.test(t_score_elmt_15, correct = T)
chisq.test(t_score_elmt_16, correct = T)
chisq.test(t_score_elmt_17, correct = T)
chisq.test(t_score_elmt_18, correct = T)
chisq.test(t_score_elmt_19, correct = T)
chisq.test(t_score_elmt_20, correct = T)

#结论：其实根本不需要对score和elmt进行卡方检验，因为他们之间必然有关系
#现在需要找出了的是score与每个elmt的相关性强度


########################################################3.2 相关系数########################################################
########################################################3.2.1 Kendall相关系数########################################################
########################################################3.2.1.1 第一轮数据########################################################
#Kendall相关系数
#Kendall_1$tau
#Kendall_1$sl
#Kendall_1$S
#Kendall_1$D

Kendall_1 <- Kendall(analysis.data.1st$ADDR_MAIL_SCORE, analysis.data.1st$Elmt_Stat_1)
Kendall_2 <- Kendall(analysis.data.1st$ADDR_MAIL_SCORE, analysis.data.1st$Elmt_Stat_2)
Kendall_3 <- Kendall(analysis.data.1st$ADDR_MAIL_SCORE, analysis.data.1st$Elmt_Stat_3)
Kendall_4 <- Kendall(analysis.data.1st$ADDR_MAIL_SCORE, analysis.data.1st$Elmt_Stat_4)
Kendall_5 <- Kendall(analysis.data.1st$ADDR_MAIL_SCORE, analysis.data.1st$Elmt_Stat_5)
Kendall_6 <- Kendall(analysis.data.1st$ADDR_MAIL_SCORE, analysis.data.1st$Elmt_Stat_6)
Kendall_7 <- Kendall(analysis.data.1st$ADDR_MAIL_SCORE, analysis.data.1st$Elmt_Stat_7)
Kendall_8 <- Kendall(analysis.data.1st$ADDR_MAIL_SCORE, analysis.data.1st$Elmt_Stat_8)
Kendall_9 <- Kendall(analysis.data.1st$ADDR_MAIL_SCORE, analysis.data.1st$Elmt_Stat_9)
Kendall_10 <- Kendall(analysis.data.1st$ADDR_MAIL_SCORE, analysis.data.1st$Elmt_Stat_10)
Kendall_11 <- Kendall(analysis.data.1st$ADDR_MAIL_SCORE, analysis.data.1st$Elmt_Stat_11)
Kendall_12 <- Kendall(analysis.data.1st$ADDR_MAIL_SCORE, analysis.data.1st$Elmt_Stat_12)
Kendall_13 <- Kendall(analysis.data.1st$ADDR_MAIL_SCORE, analysis.data.1st$Elmt_Stat_13)
Kendall_14 <- Kendall(analysis.data.1st$ADDR_MAIL_SCORE, analysis.data.1st$Elmt_Stat_14)
Kendall_15 <- Kendall(analysis.data.1st$ADDR_MAIL_SCORE, analysis.data.1st$Elmt_Stat_15)
Kendall_16 <- Kendall(analysis.data.1st$ADDR_MAIL_SCORE, analysis.data.1st$Elmt_Stat_16)
Kendall_17 <- Kendall(analysis.data.1st$ADDR_MAIL_SCORE, analysis.data.1st$Elmt_Stat_17)
Kendall_18 <- Kendall(analysis.data.1st$ADDR_MAIL_SCORE, analysis.data.1st$Elmt_Stat_18)
Kendall_19 <- Kendall(analysis.data.1st$ADDR_MAIL_SCORE, analysis.data.1st$Elmt_Stat_19)
Kendall_20 <- Kendall(analysis.data.1st$ADDR_MAIL_SCORE, analysis.data.1st$Elmt_Stat_20)

Kendall.tau <- c(Kendall_1$tau,Kendall_2$tau,Kendall_3$tau,Kendall_4$tau,Kendall_5$tau,Kendall_6$tau,Kendall_7$tau,Kendall_8$tau,
                 Kendall_9$tau,Kendall_10$tau,Kendall_11$tau,Kendall_12$tau,Kendall_13$tau,Kendall_14$tau,Kendall_15$tau,Kendall_16$tau,
                 Kendall_17$tau,Kendall_18$tau,Kendall_19$tau,Kendall_20$tau)

elmt_order <- order(Kendall.tau,decreasing = T)[(length(which(Kendall.tau == 1)) + 1):20]
Kendall.tau[elmt_order]
table(analysis.data.1st$ADDR_MAIL_SCORE, analysis.data.1st$Elmt_Stat_7)
View(analysis.data.1st)


















