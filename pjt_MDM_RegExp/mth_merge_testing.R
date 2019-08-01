library(plyr)
install.packages("plyr")
rm(list=ls())

ds_org <- uf_xlsxDataImport('Match_Org.xlsx','Sheet1')
#View(ds_org)

ds_org_core <- ds_org
#View(ds_org_core)


rownum <- nrow(ds_org)
gv_method_strSim <- "lcs"
gv_accept_sim <- 0.8

ds_match_rlt <- data.frame(org_sourceID = '', core_sourceID = '', reason = '')[0]

for(i in 1:rownum){
  org_rcd <- ds_org[i,]
  org_rcd.sourceID <- org_rcd$src_sys_id
  org_rcd.addr <- org_rcd$addr_line_1
  org_rcd.prop <- org_rcd$prop_nm
  org_rcd.city <- org_rcd$city_nm
  org_rcd.district <- org_rcd$city_subdiv_nm
  
  print(paste0("i:",i))
  
  for(j in 1:rownum){
    mth_rcd <- ds_org_core[j,]
    mth_rcd.sourceID <- mth_rcd$src_sys_id
    mth_rcd.addr <- mth_rcd$addr_line_1
    mth_rcd.prop <- mth_rcd$prop_nm
    mth_rcd.city <- mth_rcd$city_nm
    mth_rcd.district <- mth_rcd$city_subdiv_nm
    
    iftrue <- mth_rcd.sourceID != org_rcd.sourceID & org_rcd.city == mth_rcd.city & org_rcd.district == mth_rcd.district
    if(iftrue){
      if(mth_rcd.addr == org_rcd.addr &
         mth_rcd.prop == org_rcd.prop){  ##match reason: Address_Line_1/City/District/Prop_Name is the same
        if(!(any(paste0(org_rcd.sourceID,mth_rcd.sourceID) == paste0(ds_match_rlt$org_sourceID,ds_match_rlt$core_sourceID)) |
           any(paste0(org_rcd.sourceID,mth_rcd.sourceID) == paste0(ds_match_rlt$core_sourceID,ds_match_rlt$org_sourceID)))){
          ds_match_rlt <- rbind(ds_match_rlt,data.frame(org_sourceID = org_rcd.sourceID, core_sourceID = mth_rcd.sourceID,
                                                        reaon = "AutoMatch"))
        }
        
      }else if(mth_rcd.addr == org_rcd.addr &
               mth_rcd.prop != org_rcd.prop){##potential match reason: Address_Line_1/City/District is the same
        if(!(any(paste0(org_rcd.sourceID,mth_rcd.sourceID) == paste0(ds_match_rlt$org_sourceID,ds_match_rlt$core_sourceID)) |
             any(paste0(org_rcd.sourceID,mth_rcd.sourceID) == paste0(ds_match_rlt$core_sourceID,ds_match_rlt$org_sourceID)))){
          ds_match_rlt <- rbind(ds_match_rlt,data.frame(org_sourceID = org_rcd.sourceID, core_sourceID = mth_rcd.sourceID,
                                                        reaon = "PMatch:Address_Line_1/City/District is the same"))
        }
        
      }else if(mth_rcd.addr != org_rcd.addr &
               mth_rcd.prop == org_rcd.prop){##potential match reason: Address_Line_1/City/District is the same;     new added match/merge rule
        if(!(any(paste0(org_rcd.sourceID,mth_rcd.sourceID) == paste0(ds_match_rlt$org_sourceID,ds_match_rlt$core_sourceID)) |
             any(paste0(org_rcd.sourceID,mth_rcd.sourceID) == paste0(ds_match_rlt$core_sourceID,ds_match_rlt$org_sourceID)))){
          ds_match_rlt <- rbind(ds_match_rlt,data.frame(org_sourceID = org_rcd.sourceID, core_sourceID = mth_rcd.sourceID,
                                                        reaon = "PMatch:Prop_Name/City/District is the same"))
        }
        
      }else if(stringsim(mth_rcd.addr,org_rcd.addr,method = gv_method_strSim) >= gv_accept_sim &
               stringsim(mth_rcd.prop,org_rcd.prop,method = gv_method_strSim) >= gv_accept_sim 
      ){##potential match reason: City/District is the same, Address_Line_1/Prop_Name fuzzy match;  
        if(!(any(paste0(org_rcd.sourceID,mth_rcd.sourceID) == paste0(ds_match_rlt$org_sourceID,ds_match_rlt$core_sourceID)) |
             any(paste0(org_rcd.sourceID,mth_rcd.sourceID) == paste0(ds_match_rlt$core_sourceID,ds_match_rlt$org_sourceID)))){
          ds_match_rlt <- rbind(ds_match_rlt,data.frame(org_sourceID = org_rcd.sourceID, core_sourceID = mth_rcd.sourceID,
                                                        reaon = "PMatch:Address_Line_1/Prop_Name fuzzy match"))
        }
        
      }
    }
    
  }
  
}

#View(ds_match_rlt)

ds_output <- merge(merge(ds_match_rlt,ds_org,by.x = "org_sourceID",by.y = "src_sys_id"),ds_org,by.x = "core_sourceID",by.y = "src_sys_id")

uf_xlsxDataExport(ds_output,"Match_Org.xlsx","Match_Rlt")

























































































