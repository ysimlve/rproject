###############################################Phase 1###############################################
#Requirement - Scrape company data from 51job, find the companies contact(Non-400) from their offical website
#1. Scrape companies recruitment url of 51Job - Company_Name, Url_51job, Industry, Sub_Industry, Recruit_City (11 Industry, 60 Sub_Industry, )
#2. Scrape companies basic information from 51Job - 

p_home <- gv_url_51job_jobSearch_home
p_jobArea <- gv_51job_jobAreaCode[1]
p_issuedate <- gv_51job_issuedateCode[5]
p_providesalary <- gv_51job_providesalaryCode[11]
p_cotype <- gv_51job_cotypeCode[12]
p_companysize <- gv_51job_companysizeCode[8]

for(i in 1:60){
  print(i)
  p_industry <- gv_51job_industries$sub_industry_code[i]
  df_companies_51urls <- uf_scrape_51job_input(p_home,p_jobArea,p_issuedate,p_providesalary,p_cotype,p_companysize,p_industry)
  uf_xlsxDataExport(df_companies_51urls,paste0("Companies_51urls_",attr(gv_51job_jobAreaCode,"city_en")[which(gv_51job_jobAreaCode == p_jobArea)[1]],".xlsx")
                    ,gv_51job_industries$sub_industry_en[which(gv_51job_industries$sub_industry_code == p_industry)[1]], TRUE)
  rm(df_companies_51urls)
}



p_output_temp <- data.frame(company = "", address = "", address1 = "", city = "", building = "", contact = "", coType = "", coSize = "", industry = "", sub_industry = "", 
                            is_local = "", headquarter = "", legal_person = "", registered_capital = "", establish_date = "", business_Sector_RA = "",op_status = "", 
                            official_website = "", web_record_code = "", coDesc = "", jobCount_all = "",
                            jobCount_cd = "", salaryRange_all = "", salaryRange_cd = "", job_issue_latest = "", scape_date = "", data_source = "", recruitPage_51job = "")[0]


each_max <- 200 #300
each_for <- c(1:each_max)
#j <- 1
for(j in 2:3){
  print(j)
  p_industry <- gv_51job_industries$sub_industry_code[j]
  p_sheet_import <- gv_51job_industries$sub_industry_en[which(gv_51job_industries$sub_industry_code == p_industry)[1]]
  df_companyUrls_import <- uf_xlsxDataImport(paste0("Companies_51urls_",attr(gv_51job_jobAreaCode,"city_en")[which(gv_51job_jobAreaCode == p_jobArea)[1]],".xlsx")
                                             ,p_sheet_import)
  
  count_total <- nrow(df_companyUrls_import)
  for_times <- ifelse((count_total / each_max) - round(count_total / each_max)>0, round(count_total / each_max) + 1, round(count_total / each_max))
  for(f in 1:for_times){ 
    df_output <- p_output_temp
    for(g in each_for + length(each_for) * (f - 1)){
      if(g <= count_total){
        print(paste0("序号",f,"-",g - (f - 1) * length(each_for)))
        print(Sys.time())
        df_op_1 <- uf_scrape_51job(df_companyUrls_import[g,],p_jobArea)
        df_output <- rbind(df_output,df_op_1)
      }
    }
    uf_xlsxDataExport(df_output,
                      paste0("Infor_Companies_",attr(gv_51job_jobAreaCode,"city_en")[which(gv_51job_jobAreaCode == p_jobArea)],"_"
                             ,gv_51job_industries$sub_industry_en[which(gv_51job_industries$sub_industry_code == p_industry)[1]],".xlsx")
                      ,paste0("list_",as.character(f)), 
                      TRUE)
    rm("df_output")
    rm("df_op_1")
  }
  rm("df_companyUrls_import")
  
}


###########################################Data Consolidate####################################################################
xlxs_name <- "Infor_Companies_Chengdu_ITHardware.xlsx"
sheet_num <- 2
op_sheet_name <- substr(xlxs_name,25,nchar(xlxs_name) - 5)


df_import_consolidated <- data.frame(company = "", address = "", address1 = "", city = "", building = "", contact = "", coType = "", coSize = "", industry = "", sub_industry = "", 
                            is_local = "", headquarter = "", legal_person = "", registered_capital = "", establish_date = "", business_Sector_RA = "",op_status = "", 
                            official_website = "", web_record_code = "", coDesc = "", jobCount_all = "",
                            jobCount_cd = "", salaryRange_all = "", salaryRange_cd = "", job_issue_latest = "", scape_date = "", data_source = "", recruitPage_51job = "")[0]

for(k in 1:sheet_num){
  print(k)
  sheet_name <- paste0("list_",as.character(k))
  df_list1 <- uf_xlsxDataImport(xlxs_name,sheet_name)
  df_import_consolidated <- rbind(df_import_consolidated,df_list1)
}


uf_xlsxDataExport(df_import_consolidated,
                  "Infor_Companies_Chengdu_Consolidated.xlsx", 
                  op_sheet_name,
                  TRUE)

rm("df_import_consolidated")












































































