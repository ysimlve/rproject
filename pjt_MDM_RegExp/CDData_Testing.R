#######################################################################Pre-processing Done#######################################################################

ODS.Staging.CS <- uf_xlsxDataImport("CSData.xlsx","Original Data")

or_PN <- uf_sdProcess_PN(ODS.Staging.CS$PROPERTY_NAME)
or_ADDR1 <- ODS.Staging.CS$ADDRESS_1
or_ADDR2 <- uf_sdProcess_ADDR(ODS.Staging.CS$ADDRESS_2)

remark_ADDR2 <- uf_validityCheck_ADDR(or_ADDR2)
remark_ADDR1 <- uf_validityCheck_PN(or_ADDR1)


View(filter(data.frame(Property_Name = or_PN,
           ADDRESS_1 = or_ADDR1,
           remark_ADDR1 = remark_ADDR1,
           ADDRESS_2 = or_ADDR2,
           remark_ADDRESS_2 = remark_ADDR2),
           remark_ADDRESS_2 == "Partial-Valid" | remark_ADDRESS_2 == "Valid"))




wh_ADDR2 <- which(remark_ADDR2 == "Partial-Valid" | remark_ADDR2 == "Valid")


or_PN[wh_ADDR2] <- or_ADDR1[wh_ADDR2]
or_ADDR1[wh_ADDR2] <- or_ADDR2[wh_ADDR2]


ODS.Staging.CS$PROPERTY_NAME <- or_PN
ODS.Staging.CS$ADDRESS_1 <- or_ADDR1

View(ODS.Staging.CS)

unique(ODS.Staging.CS$SRC_SYS_ID)

v_Default = rep("", nrow(ODS.Staging.CS))

uf_xlsxDataExport(data.frame(SOURCE_PROPERTY_ID = ODS.Staging.CS$SRC_SYS_ID,
                             PROPERTY_NAME = ODS.Staging.CS$PROPERTY_NAME,
                             PROPERTY_NAME_LC = v_Default,
                             ADDRESS_1 = ODS.Staging.CS$ADDRESS_1,
                             ADDRESS_LC = v_Default,
                             DISTRICT_NAME = v_Default,
                             DISTRICT_NAME_LC = v_Default,
                             CITY_NAME = ODS.Staging.CS$CITY,
                             CITY_NAME_LC = v_Default,
                             PROVINCE_NAME = v_Default,
                             PROVINCE_NAME_LC = v_Default,
                             COUNTRY_NAME = ODS.Staging.CS$COUNTRY,
                             COUNTRY_NAME_LC = v_Default,
                             TIMEZONE = v_Default,
                             GEO_LAT = v_Default,
                             GEO_LONG = v_Default), "Test.xlsx","Test")

#######################################################################Pinying Split#######################################################################
gv_pinying_all <- c("a","ai","an","ang","ao","ba","bai","ban","bang","bao","bei","ben","beng","bi","bian","biao","bie","bin","bing","bo",
                    "bu","ca","cai","can","cang","cao","ce","cen","ceng","cha","chai","chan","chang","chao","che","chen","cheng","chi","chong","chou","chu","chua",
                    "chuai","chuan","chuang","chui","chun","chuo","ci","cong","cou","cu","cuan","cui","cun","cuo","da","dai","dan","dang","dao","de","dei",
                    "den","deng","di","dia","dian","diao","die","ding","diu","dong","dou","du","duan","dui","dun","duo","e","en","eng","er","fa","fan","fang",
                    "fei","fen","feng","fiao","fo","fou","fu","ga","gai","gan","gang","gao","ge","gei","gen","geng","gong","gou","gu","gua","guai","guan",
                    "guang","gui","gun","guo","ha","hai","han","hang","hao","he","hei","hen","heng","hong","hou","hu","hua","huai",
                    "huan","huang","hui","hun","huo","ji","jia","jian","jiang","jiao","jie","jin","jing","jiong","jiu","ju","juan","jue","ka","kai",
                    "kan","kang","kao","ke","ken","keng","kong","kou","ku","kua","kuai","kuan","kuang","kui","kun","kuo","la",
                    "lai","lan","lang","lao","le","lei","leng","li","lia","lian","liang","liao","lie","lin","ling","liu",
                    "long","lou","lu","luan","lun","luo","lv","ma","mai","man","mang","mao","me","mei","men","meng","mi","mian","miao","mie","min","ming","miu","mo","mou",
                    "mu","na","nai","nan","nang","nao","ne","nei","nen","neng","ni","nian","niang","niao","nie","nin","ning","niu","nong","nou","nu","nuan","nun","nuo","nv","ou",
                    "pa","pai","pan","pang","pao","pei","pen","peng","pi","pian","piao","pie","pin","ping","po","pou","pu","qi","qia","qian","qiang","qiao","qie","qin","qing","qiong",
                    "qiu","qu","quan","que","qun","ran","rang","rao","re","ren","reng","ri","rong","rou","ru","rua","ruan","rui","run","ruo","sa","sai","san","sang","sao","se","sen",
                    "seng","sha","shai","shan","shang","shao","she","shei","shen","sheng","shi","shou","shu","shua","shuai","shuan","shuang","shui","shun","shuo","si","song","sou","su","suan",
                    "sui","sun","suo","ta","tai","tan","tang","tao","te","tei","teng","ti","tian","tiao","tie","ting","tong","tou","tu","tuan","tui","tun","tuo","wa","wai","wan","wang","wei",
                    "wen","weng","wo","wu","xi","xia","xian","xiang","xiao","xie","xin","xing","xiong","xiu","xu","xuan","xue","xun","ya","yan","yang","yao","ye","yi","yin","ying","yo","yong",
                    "you","yu","yuan","yue","yun","za","zai","zan","zang","zao","ze","zei","zen","zeng","zha","zhai","zhan","zhang","zhao","zhe","zhei","zhen","zheng","zhi","zhong","zhou",
                    "zhu","zhua","zhuai","zhuan","zhuang","zhui","zhun","zhuo","zi","zong","zou","zu","zuan","zui","zun","zuo")


uf_PinyingSplit <- function(con_pinying){
  con_pinying <- gsub("\\s{1,}","",con_pinying)
  #special handle - anga, ange, ango, angu
  con_pinying <- gsub("anga","an@ga",con_pinying)
  con_pinying <- gsub("ange","an@ge",con_pinying)
  con_pinying <- gsub("ango","an@go",con_pinying)
  con_pinying <- gsub("angu","an@gu",con_pinying)
  
  con_pinying <- paste0(con_pinying,"z")
  len_py <- nchar(con_pinying)
  single_list <- rep("",len_py)
  for(i in 1:len_py){
    substr(con_pinying,i,i) <- tolower(substr(con_pinying,i,i))
    single_list[i] <- substr(con_pinying,i,i)
  }
  k = 1
  word <- single_list[k]
  words <- rep("",len_py)
  for(j in 1:len_py){
    word <- paste0(word,single_list[j+1])
    if(!any(gv_pinying_all == word) & any(gv_pinying_all == substr(word,1,nchar(word)-1))){
      k <- j + 1
      words[j] <- substr(word,1,nchar(word)-1)
      word <- single_list[k]
      word <- ifelse(word == "@", "", word)
    }
  }
  
  op_word <- ""
  for(n in 1:len_py){
    op_word <- paste0(op_word," ",words[n])
  }
  op_word <- gsub("\\s$","",gsub("^\\s","",gsub("\\s{2,}"," ",op_word)))
  
  #special handling yuan & yu an
  if(regexpr(" yu an ",op_word) > 0){
    op_word <- gsub(" yu an "," yuan ",op_word)
  }
  
  #translate first character of each word to UPPER case
  for(lc in letters){
    pattern_UC <- paste0('(\\s|^)(',lc,')')
    op_word <- gsub(pattern_UC, paste0(' ', toupper(lc)) , op_word)
  }
  op_word <- gsub("^\\s","",op_word)
  
  return(op_word)
}

#lu,dajie
con_pinying <- "Jianguomenbeidajie"
uf_PinyingSplit(con_pinying)

########################################################Programm Process Start########################################################
#########1. Load Source Raw Data into ODS Staging Area#########
ODS.Staging.CS <- uf_xlsxDataImport('CSData.xlsx','ODS.Staging.CS')

#########Select Geo Core columns for processing
ODS.Staging.CS <- select(ODS.Staging.CS,
                         SOURCE_PROPERTY_ID,PROPERTY_NAME,PROPERTY_NAME_LC,ADDRESS_1,ADDRESS_LC,DISTRICT_NAME,DISTRICT_NAME_LC,
                         CITY_NAME,CITY_NAME_LC,PROVINCE_NAME,PROVINCE_NAME_LC,COUNTRY_NAME,COUNTRY_NAME_LC,TIMEZONE,GEO_LAT,GEO_LONG)

ODS.Staging.CS.BK <- ODS.Staging.CS


#########2. Execute Standardization Rules on Source Raw Data#########
##########Additional Process on CITY_NAME
cs_city <- ODS.Staging.CS$CITY_NAME
cs_city <- gsub("shi$", "", cs_city)
cs_city <- uf_sdProcess_CITY(cs_city)

mt_source <- cs_city
#mt_source[9]

mt_target <- unique(gv_china_division_list$CITY)
#mt_target[6]
tar_index <- amatch(mt_source, mt_target, nomatch = "", maxDist = 3)

cs_city_fix <- ifelse(is.na(mt_target[tar_index]), "", mt_target[tar_index])

ODS.Staging.CS$CITY_NAME <- cs_city_fix

########Regular Standardization Rule
ODS.Standardized.CS <- uf_sdProcess_ALL(ODS.Staging.CS)
uf_xlsxDataExport(ODS.Standardized.CS, "Test.xlsx","Test")
ODS.Standardized.CS <- uf_xlsxDataImport("CSData.xlsx","ODS.Standardized.CS")
##########Additional Process on PROPERTY_NAME


#########3. Execute Validity Check on Standardized Source Data#########
ODS.Remarked.CS <- uf_validityCheck_ALL(ODS.Standardized.CS)
#uf_xlsxDataExport(ODS.Remarked.MarketLink, "Test.xlsx","Test")


#########4. Execute Pre-API Rejection Rule on Standardized Source Data#########
#ODS.PreAPI.CS <- uf_xlsxDataImport("CSData.xlsx","ODS.PreAPI.CS")
ODS.Temp.CS <- uf_rejectProcess_preAPI(ODS.Remarked.CS)
ODS.PreRej.CS <- filter(ODS.Temp.CS, Reject_Mark == "Rejected")
ODS.PreAPI.CS <- filter(ODS.Temp.CS, Reject_Mark != "Rejected")[,gv_geocore_fields]
uf_xlsxDataExport(ODS.PreAPI.CS, "Test.xlsx","Test")

########split address
pre_splt_address <- ODS.PreAPI.CS$ADDRESS_1
mark_aplt_address <- rep("",length(pre_splt_address))
post_spltaddressa <- rep("",length(pre_splt_address))
pattern_splt <- "([0-9]{0,5}-[0-9]{0,5})?\\s{0,1}[A-Z][a-z]{3,50}(lu|dajie|dadao|jie)"
for(i in 1:length(pre_splt_address)){
  if(regexpr(pattern_splt,pre_splt_address[i])>0){
    mark_aplt_address[i] <- "YES"
    pattern_splt_1 <- "[A-Z][a-z]{3,50}(lu|dajie|dadao|jie)"
    splt_str <- substr(pre_splt_address[i],regexpr(pattern_splt_1,pre_splt_address[i]),attr(regexpr(pattern_splt_1,pre_splt_address[i]),"match.length") + regexpr(pattern_splt_1,pre_splt_address[i]) - 1) 
    post_spltaddressa[i] <- gsub(splt_str,uf_PinyingSplit(splt_str),pre_splt_address[i])
  }else{
    mark_aplt_address[i] <- "NO"
    post_spltaddressa[i] <- pre_splt_address[i]
  }
}

ODS.PreAPI.CS$ADDRESS_1 <- post_spltaddressa
View(ODS.PreAPI.CS)
uf_xlsxDataExport(data.frame(pre_splt_address = pre_splt_address,
                             mark_aplt_address = mark_aplt_address,
                             post_spltaddressa = post_spltaddressa),
                  "test.xlsx","test")


#########5. Execute API Process on Standardized Source Data#########
DS_PreAPI <- ODS.PreAPI.CS[1:500,]
key <- keys[2:47]
language = "zh-CN"
DS_PostAPI_Google <- API_Search_Google(DS_PreAPI[1,],keys[1],language)
for(i in 2:length(key)){
  print(i)
  sn <- 2+10*(i-2)
  en <- 1+10*(i-1)
  ODS.PostAPI.Temp <- API_Search_Google(DS_PreAPI[sn:en,],key[i-1],language)
  DS_PostAPI_Google <- rbind(DS_PostAPI_Google, ODS.PostAPI.Temp)
}
uf_xlsxDataExport(DS_PostAPI_Google, "Test.xlsx","Test")







































