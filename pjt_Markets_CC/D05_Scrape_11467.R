setwd("C:/YuanLe/R/RWkDir/06. Markets_CC")
#options(encoding = "utf-8")

#source("C05_Scrape_11467.R")

p_home <- gv_url_11467_home
p_city_en <- "Chengdu"
coID <- c(229536:294715) #294715

for(i in coID){
  print(paste0(as.character(i),"--",as.character(Sys.time())))
  rlt <- uf_scrape_11467(p_home,i,p_city_en)
}






