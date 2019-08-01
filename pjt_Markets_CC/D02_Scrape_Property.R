###############################################Data Collection:0ffice028###############################################
p_home <- gv_028office_link
p_city <- gv_city[1]
p_city_en <- attr(gv_city,"En")[which(gv_city == p_city)]
week_end <- gsub("-","",as.character(floor_date(Sys.Date(), "week") + 7))
sheet_name <- "Property_Basic"

df.Property_Basic <- uf_scrape_office028_input(p_home,p_city)

uf_xlsxDataExport(df.Property_Basic,paste0("./office028/","Properties_Basic_office028_",
                                             p_city_en,"_Week_",week_end,".xlsx"),
                  sheet_name)


df.Property_Basic <- uf_xlsxDataImport(paste0("./office028/","Properties_Basic_office028_",
                                               p_city_en,"_Week_",week_end,".xlsx"),
                                        sheet_name)


#insert/update data into database
p_property <- df.Property_Basic
p_city <- gv_city[1]
initial_load <- FALSE
uf_scrape_office028(p_property,p_city,initial_load)


###############################################Data Collection:Anjuke###############################################
p_home <- gv_anjuke_link
p_city <- gv_city[1]
p_city_en <- attr(gv_city,"En")[which(gv_city == p_city)]
week_end <- gsub("-","",as.character(floor_date(Sys.Date(), "week") + 7))
sheet_name <- "Property_Basic"

df.Property_Basic_Anjuke <- uf_scrape_anjuke_input(p_home,p_city)

uf_xlsxDataExport(df.Property_Basic_Anjuke,paste0("./office028/","Properties_Basic_Anjuke_",
                                                  p_city_en,"_Week_",week_end,".xlsx"),
                  sheet_name)


df.Property_Basic_Anjuke <- uf_xlsxDataImport(paste0("./office028/","Properties_Basic_Anjuke_",
                                              p_city_en,"_Week_",week_end,".xlsx"),
                                       sheet_name)


p_property <- df.Property_Basic_Anjuke
p_city <- gv_city[1]
initial_load <- TRUE
uf_scrape_Anjuke(p_property,p_city,initial_load)




df.db_return_p
count_for <- nrow(df.db_return_p)
Conn <NA
DBConn <- "DEFAULT"
if(DBConn == "DEFAULT"){
  Conn <- tryCatch({
    evalWithTimeout({ odbcConnect(gv_Defualt_DBServer,uid = gv_Defualt_DBServer_Login, pwd = gv_Defualt_DBServer_pwd) }, timeout = timeouts)
  },warnings = function(w){
    ;
    NA
  },error = function(e){
    ;
    NA})
}

if(!is.na(Conn)){
  for(i in 1:count_for){
    v_ID <- df.db_return_p$ID[i]
    v_address <- iconv(df.db_return_p$Address[i],"gb2312","UTF-8") 
    v_City <-  iconv(df.db_return_p$City[i],"gb2312","UTF-8") 
    #bd_result <- baidu.api.detailSearch(baidu.api.poiSearch(v_address,"成都",gv_BDAPI_Key),"成都",gv_BDAPI_Key)
    if(nchar(v_address) > 3){
      v_geocode <- baidu.api.geocoder(v_address,v_City,gv_BDAPI_Key)
    }
    
    if(!any(v_geocode == "")){
      sql_query <- paste0("UPDATE [CN_CD_CC].[Properties_Info]"," SET ",
                          "Latitude = '", v_geocode[1],"',",
                          "Lontitude = '",v_geocode[2],"' ",
                          "WHERE ID = ", as.integer(v_ID))
      
      df.db_return <- tryCatch({
        evalWithTimeout({ sqlQuery(Conn,sql_query)
        }, timeout = timeouts);
        TRUE
      },warnings = function(w){
        ;
        FALSE
      },error = function(e){
        ;
        FALSE})
    }
  }
}

odbcClose(Conn)



















