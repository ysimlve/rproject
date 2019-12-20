
p_city <- gv_cities[1]
df.BuildList <- uf_Retrieve_Build_List(p_city)

Query <- "SELECT TOP 1500 ID,address,city,building FROM CN_CD_CC.Companies_Infor_51Job  WHERE Is_Current = 1 AND GoBDMap IS NULL AND data_source = '51job'  ORDER BY ID DESC"
df.properties <- uf_execute_Query("DEFAULT",Query,20)


df.properties$address <- iconv(df.properties$address,"gb2312","UTF-8")
df.properties$city <- iconv(df.properties$city,"gb2312","UTF-8")
df.properties$building <- ""

count_pro <- nrow(df.properties)

for(j in 1195:count_pro){
  v_address <- df.properties$address[j]
  v_city <- df.properties$city[j]
  v_id <- df.properties$ID[j]
  print(paste0(as.character(j),"/",as.character(count_pro),"--",as.character(v_id),"--",v_address,"-",as.character(Sys.time())))
  
  op_property <- uf_Build_Indicator(v_address,v_city,df.BuildList)
  
  #update record in database
  query <- paste0("UPDATE CN_CD_CC.Companies_Infor_51Job SET building = N'",
                  iconv(op_property[1],"UTF-8","gb2312"),"',",
                  "Latitude = N'",iconv(op_property[2],"UTF-8","gb2312"),"',",
                  "Lontitude = N'",iconv(op_property[3],"UTF-8","gb2312"),"',",
                  "GoBDMap = 'YES',",
                  "GoBDMap_DATE = '",as.character(Sys.Date()),"'",
                  " WHERE ID = ", v_id)
  uf_execute_Query("DEFAULT",query,20)
  
}
