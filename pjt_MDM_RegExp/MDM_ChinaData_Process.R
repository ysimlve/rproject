########################################################Remark########################################################
#The program is a prototype of how to implement MDM process(including Standardization Process, Rejection Process, API Process) on China 
#specific Property Data
#The generic process is: Raw source data -> Standardization Process -> Rejection Process(Pre-API) -> API Process -> Rejection Process(Post-API)



########################################################Programm Start########################################################
setwd("C:/YuanLe/R/RWkDir/01. MDM_RegExp")
#ls()
#rm(list=ls())
#install.packages("geosphere")

options(stringsAsFactors = FALSE)
options(java.parameters = "-Xmx4g")

library(readxl)            ##import data from Excel
library(dplyr)             ##data manipulation, e.g. select, filter
library(stringdist)        ##string distance calculation and approximate string matching
library(RCurl)             ##use for processing Google Geocoding API and JSON file
library(RJSONIO)           ##use for processing Google Geocoding API and JSON file
library(xlsx)
library(geosphere)
version


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



#######################################################################Hanzi Translation to Pinyin#######################################################################
uf_Translate_Hz_Py <- function(hanzi,firstcapital = T){
  rlt_piny <- hanzi
  if(nchar(hanzi, type="bytes") > nchar(hanzi)){
    rlt_piny <- ""
    len <- nchar(hanzi)
    for(i in 1:len){
      word <- substr(hanzi,i,i)
      if(nchar(word,type = "bytes")>1){
        whch <- which(gv_hanzi_pinyin_list$hanzi == word)[1]
        whch <- ifelse(is.na(whch),0,whch)
        if(whch != 0){
          piny <- gv_hanzi_pinyin_list$pinyin1[whch] 
          if(firstcapital){
            for(lc in letters){
              pattern_UC <- paste0('(\\s|^)(',lc,')')
              piny <- gsub(pattern_UC, paste0(' ', toupper(lc)) , piny)
            }
          }
          
          word <- paste0(" ",piny)
        }
      }
      rlt_piny <- paste0(rlt_piny,word)
      rlt_piny <- gsub("\\s$","",gsub("^\\s","",gsub("\\s{1,}"," ",rlt_piny))) 
    }
  }
  
  return(rlt_piny)
}

#uf_Translate_Hz_Py(hanzi)



########################################################Global Variables########################################################
gv_toneSign <-       c("ā","á","ă","à","ō","ó","ŏ","ò","ē","é","ĕ","è","ī","í","ĭ","ì","ū","ú","ŭ","ù")
gv_toneSign_fixed <- c("a","a","a","a","o","o","o","o","e","e","e","e","i","i","i","i","u","u","u","u")


gv_address_type_abbr <- c("[Rr][Dd]", "S[Tt][Rr]?",  "A[Vv][Ee]", "D[Rr]", "L[Nn]", "N",     "S",     "E",    "W",    "E[Xx][Pp][Yy]", "H[Ww][Yy]", "P[Kk][Ww][Yy]", "C[Ii][Rr]", "W[Yy]", "B[Ll][Vv][Dd]", "P[Ll][Zz]", "C[Tt][Rr]", "C[Tt]")
gv_address_type_full <- c("Road",     "Street",      "Avenue",     "Drive", "Lane",  "North", "South", "East", "West", "Expressway",    "Highway",   "Parkway",       "Circle",    "Way",   "Boulevard",     "Plaza",     "Center",    "Court")

gv_property_type_abbr <- c('BLDG',     'TWR',   'HQ',           'C[Tt][Rr]', 'CT',    'APT',       'COMM’l',     'INT’L')
gv_property_type_full <- c('Building', 'Tower', 'Headquarters', 'Center',    'Court', 'Apartment', 'Commercial', 'International')

gv_district_old <- c("Jinqiao", "Luwan",   "Lujiazui", "Nanhui", "Pudongothers", "Tangqiao", "Zhabei",  "Zhangjiang", "Zhuyuan")
gv_district_sub <- c("Pudong",  "Huangpu", "Pudong",   "Pudong", "Pudong",       "Pudong",   "Jing'an", "Pudong",     "Pudong")

gv_district_lc_old <- c("金桥","卢湾","陆家嘴","南汇","浦东新","浦东新区其他","塘桥","闸北","张江","竹园")
gv_district_lc_sub <- c("浦东","黄浦","浦东"  ,"浦东","浦东"  ,"浦东"        ,"浦东","静安","浦东","浦东")

gv_geocore_fields <- c("SOURCE_PROPERTY_ID", "PROPERTY_NAME",      "PROPERTY_NAME_LC",   "ADDRESS_1" ,         "ADDRESS_LC",         "DISTRICT_NAME" ,     "DISTRICT_NAME_LC" ,  "CITY_NAME"         
                       ,"CITY_NAME_LC"    ,   "PROVINCE_NAME"   ,   "PROVINCE_NAME_LC"  , "COUNTRY_NAME"    ,   "COUNTRY_NAME_LC"   , "TIMEZONE"         ,  "GEO_LAT"      ,      "GEO_LONG" )

gv_method_strSim <- "lcs"
gv_accept_sim <- 0.5



########################################################Google Keys########################################################
keys <- c("AIzaSyA8Dmo33Ao1BwqTYthLhzmBRsv0Y9EtcBY"
          ,"AIzaSyDWDVo9Sy-Yk2eqjF5nS88hOPuiZpTOmD0"
          ,"AIzaSyBcqmigDl8Tuo06kWc7Ib-SrddUPRfLa5E"
          ,"AIzaSyA4-cljqh4KkfNWtziQgaAKPUG2XudfLfI"
          ,"AIzaSyClo0crDJDNwvEn5iMohXx_G6fInbbApXw"
          ,"AIzaSyD8ylAoVNwWJ8M4Xz8wKjXgoK-eyYLI4ew"
          ,"AIzaSyBqfG4uVkgVsqm-KGhsFXi_Bm87Ozg5Tbw"   
          ,"AIzaSyAOlyJl-HvmCgKIWtsRi0xBAjjPpO3vqXg"   
          ,"AIzaSyCLd9r79jmVTzO-rTot1vHsQ92-8xIYb1Y"   
          ,"AIzaSyDUGcoXWCXEEiAPcC-HzJl7GOS3xXZe6Xs"   
          ,"AIzaSyADoa11-g4vkv6Vf0QDaVYRHheSmkuJ1tg"   
          ,"AIzaSyBEMDfkNhwlQusKIQifo_OjrYl9gRq1ozs"   
          ,"AIzaSyAPXKtsC7bnUwQhIwGqK_Xs34S91TnKWeg"
          ,"AIzaSyBW97apAnfAxJRXAGCfztwBqV4uErV_llQ"
          ,"AIzaSyA_C99YFdu8tdZHO2XrTvCwswck3xPAviA"
          ,"AIzaSyApru4GSxXTt-HHEXSg2dnR-R9yoPxCM68"
          ,"AIzaSyAjsIquTPJph_8kBUjFsAdPceQLw0-dNbA"
          ,"AIzaSyCA5qZD8c68C9Lzj7JBEgXulqA9shRhq8s"
          ,"AIzaSyAX4IM8_H7sEAWVNvgZj2UmYohpKGc12Bo"
          ,"AIzaSyDfm86KYWGutSe5qPRcR8B22yD9EkNXD0Y"
          ,"AIzaSyCrd3k-ZG9ba7S9qNiO40F-DJmXVV209hg"
          ,"AIzaSyBFgq1V4WBORn_Guw2989IFc4biqahMIiY"
          ,"AIzaSyDIyk3QGB2TWO7tQp912pd3E04-gEZXb9A"
          ,"AIzaSyAkZ_5XMfguMICHGenTYDbT7vpdtYlYo0s"
          ,"AIzaSyASmGO6SgW7snetVl7B81UjT27aBZVdUvw"
          ,"AIzaSyDH8iGmWWSkYb7dfAJQdhx2YXS2N72F9GY"
          ,"AIzaSyDIhOhId0EVpyebvnSBvmkNQqFRAT335X8"
          ,"AIzaSyAOuD9IZPIOehoeV6Yb9E_zqNKAxmG4trM"
          ,"AIzaSyDWJt73kRLVrhkpB4y3dDsWjFqTDcV6ZPY"
          ,"AIzaSyBzgdu1JGlZ3eT2zAYnvnM0ZP8kKAyH5Ec"
          ,"AIzaSyDPPcLO6hIyQUS9AOTBQ861D-VO9kxbK0A"
          ,"AIzaSyCRkj0P5n2eHnOLmUDCgK4QZP2nnDt3IC0"
          ,"AIzaSyDXdk5X4ldrEcrD5zxiSb_FTWnu2Ptab_c"
          ,"AIzaSyBHLusUqoqkFnKoHsu-Yuz8OaizfTqvKWA"
          ,"AIzaSyDfIU6Hbi_Z5e86HIiIVJCeL81AX-ddUzM"
          ,"AIzaSyD_jphifEC1p1jvxv5e5sLYOc_QsZPFnTU"
          ,"AIzaSyC7k6p4UMHRVxWlnbyprpGWtg72BL7zGyk"
          ,"AIzaSyDPdMOZNWGkCpSVcU9AKsPB7Rqrw5sxNRQ"
          ,"AIzaSyCdeheCQP7K4abGbnXduKjtxocQEfhX3s4"
          ,"AIzaSyBr9KLDJbE7YiAqcwrrkTsenrZJusD9S44"
          ,"AIzaSyArYyiHgQJDIZYWPaWMAGCeTn4MTMnNeME"
          ,"AIzaSyCHGUo37HTZ-mORyi78cPgCp-MeDXBH6OM"
          ,"AIzaSyD6AJi4tjV0xK1tPW7hO-__00rJ6WfqXrE"
          ,"AIzaSyDkinkKUqsQ29o4nkxduHp1BKOnx5wPzFk"
          ,"AIzaSyD6-YuO_vMYns9U3JVBCY3zobtKxFbL-WY"
          ,"AIzaSyDHWrZLRpzUe1RDGXyVAtJRzDKPNcUYRi0"
          ,"AIzaSyCIbOPps5BBU1rjsBRM2SMW86jyO5WpKog"
          ,"AIzaSyDe4TVXfbYIaoIKsqNg1cGiefzJllpCK_8"
          ,"AIzaSyA11qa9JWv0ueIXeblI828Q7FCOuYGXSPI" #ChrisHuang2727
          ,"AIzaSyBNxVNb4ktkWXysMFdl_LBbUevPKWuhXVs"
          ,"AIzaSyC5asx7Rvcb_tTdOC0XWacpw5zyrbAN-jQ"
          ,"AIzaSyA-H38lXu3-C2ReM9utCjyOj4MQjbsu_S0"
          ,"AIzaSyDd-9IJUuQB1j98XObuJFhjhatwDjSmFIY"
          ,"AIzaSyDy4TfKU02TRNkBMyzXPjISU6k_D-62nKI"
          ,"AIzaSyBmBxm8PAoC9oRFPdzzo6wfjAoenRJ-e6s"
          ,"AIzaSyCD73DzYXJINTjfCJfge6GA41IqQY8zgns"
          ,"AIzaSyDFaK-RDT8ridXsx9SCTxpCR8PYI3b0MU0"
          ,"AIzaSyCg3iI4JkZjzwaXfGz9Z2NJnqVRfSg0Wes"
          ,"AIzaSyB-g9PLdZspsxMANIZr0OBZ7ODOrX1vqps"
          ,"AIzaSyCX85Krz6_TAQvFuEfOc5VL5x2D1djMP_A"
          ,"AIzaSyAXeKw4HY6dRRgCrqgzyDD7mYvhfcszCo8" #NeilJin2727
          ,"AIzaSyBc5wAhMhQwUj_lcGyYCjIVJ9PEV0LwDIk"
          ,"AIzaSyCtkmGVIcXsNk9JJxt3FFw5xTatUSkhLK4"
          ,"AIzaSyDbfWT2kLDinwxWBzxSSEGog-9mrnzZff4"
          ,"AIzaSyABj2gtLQrSLt6RWFBfO-ek4NVWSj2NHRQ"
          ,"AIzaSyDy_xdhoI9bBNs99dWAjj1IOEnpfpVImQk"
          ,"AIzaSyDwzw_H_fQidBOqwNIRM3st5iw4lZyICyo"
          ,"AIzaSyDFJyzURabjpTGq5b3pM6K56QYDwfNqPdo"
          ,"AIzaSyAyRxHnurdyEGTlvhdwlJA8AgbxZRtWULg"
          ,"AIzaSyCCGC1mpUImCYY37FXnSeP-a9GUsjE708w"
          ,"AIzaSyASD-dMT5knHfO6cNZRFID3NvGoUt7JkkU"
          ,"AIzaSyBqbX6egM-SuVrB3e5lb84MXZhtGtTGq-I"
          ,"AIzaSyBseP9kM-mYSpz7T5Anmb3h2uxtPpc9UtE" #TerryTang2727
          ,"AIzaSyAsUAwSONushGaAfekiPZ6lP8QZmnX5Hf0"
          ,"AIzaSyA1fPBr3uLSMMgIwVsxCkb4UKK0kWI0Dm4"
          ,"AIzaSyCVRRzkFs-tG4k17AQ5A1wCQR83UA8cciE"
          ,"AIzaSyBthiLYOYPvsYhx96SBl2BUUHKxfZKmfew"
          ,"AIzaSyAFGjl-c7q5tYpXTSQnst_v_YXlWckGXyI"
          ,"AIzaSyCFUYV9WgquJccwh05eWo9KsAug7xj_iTE"
          ,"AIzaSyAoKirzJQ8u57e5L0zNRVVwbIREeoxP5f4"
          ,"AIzaSyDwka2VcE0OTfwtaY3p_jXXyQJM2FdY43c"
          ,"AIzaSyAXdQgqYejW-KXPv4uaIobu5dS6NJEOgdE"
          ,"AIzaSyAozR75Hz4Irr-l7zLYZu6u9PK5tEmil6I"
          ,"AIzaSyAGimiDR3pewa85FEt6y6-Icz1kMH5H7JE"
          ,"AIzaSyDsOMvp7aBMKgFIH8jIJXxa1ZyeEVzQKu8" #TomXie2727
          ,"AIzaSyA6x2RCsG0hgYmzhwqQPaPMM2Dsse62JNs"
          ,"AIzaSyCLI-SM52Z5-AOCwesweq6GxFtVD8tfmtE"
          ,"AIzaSyDrygN6ZzamQ8qXuiQqYhiMOkso8XhW_xc"
          ,"AIzaSyBqPNftbSBsxz5_wNzNf7aQ3vPBmYOqUpM"
          ,"AIzaSyC24SxN5KiENZ_zHhd-BCljPMnoapjccV8"
          ,"AIzaSyBCl2NnZCq2KqPUaz5qa9cqScx6h9BBoe8"
          ,"AIzaSyDUfVIrp_UBIh9SFfTWcT2KpB10pxGvD8Y"
          ,"AIzaSyDRlOHkp2GWBXeNqBceZ9PieJ_9b3sqaEc"
          ,"AIzaSyDIfNTTtGRUBdtCILyXEI9rUJE-S_pSqyo"
          ,"AIzaSyBNtReKGZqAYgH-VTs1Mwvyr2xE9FUR64g"
          ,"AIzaSyDBlyO0cZF2--Vd38RSB13OOVTkmPAE0ik"
          ,"AIzaSyArOluo4CLn2kZ7APdNXdrFcN52tHKQP94" #JielingLiang2727
          ,"AIzaSyAMj3OQSflF2SmOd23LHQpsZrodfC6QD7Y"
          ,"AIzaSyCJxiFVD5jMHxrVxlvHmoqcJ8kQB6VsX3Y"
          ,"AIzaSyBVZNxrwIEZ9yQDMqUsQiS_VOGXBVa52q4"
          ,"AIzaSyAgikjUeGX-zc_i2rIFagabb7oElphOuQU"
          ,"AIzaSyAe9gd93BFnA5w8lpW1mbw0gmeWAZcytqI"
          ,"AIzaSyBPfy1ksMOFxxWQzhKmu9i2RfHcWrN4okE"
          ,"AIzaSyDgDHtKzeusG679F_Qqld11GvIsyGX6PcA"
          ,"AIzaSyCFzPfikEqkm8ga7d97jokoHa_T5IvHIp8"
          ,"AIzaSyDYCXNuoriMi71prW_IvjLmJQOS3YZwktc"
          ,"AIzaSyCoZfZjtfw9pSeY23gM1A1IZ-tEH8TtTXo")



########################################################Functions - Common########################################################
########################################################uf_xlsxDataImport - import data into a data frame from excel file
uf_xlsxDataImport <- function(xlsx_file,spreedsheet){
  df_xlsx <- read.xlsx(xlsx_file,sheetName = spreedsheet,header=TRUE,encoding = "UTF-8")
  return(df_xlsx)
}

gv_china_division_list <- uf_xlsxDataImport('AndyData_1.xlsx','DivisionList_China')
gv_hanzi_pinyin_list <- uf_xlsxDataImport('AndyData_1.xlsx','List_Hanzi_Pinyin')

########################################################uf_xlsxDataExport - export data to excel file from a data frame
uf_xlsxDataExport <- function(p_df, xlsx_file, spreedsheet){
  xlsx::write.xlsx(p_df,xlsx_file,sheetName = spreedsheet)
}

########################################################uf_null_fix - replace NULL, NA to Blank
uf_null_fix <- function(p_input){
  v_input <- p_input
  
  in_Count <- length(v_input)
  v_input <- ifelse(is.na(v_input), "", v_input)
  v_input <- ifelse(v_input == "NULL" | v_input == "NA" | v_input == " " | v_input == "N/A" | v_input == "Null", "", v_input)
  
  out_input <- v_input
  return(out_input)
}




########################################################Functions - Standadization Process########################################################



########################################################uf_sdProcess_Common_Eng - process common standardization rules for English fields
uf_sdProcess_Common_Eng <- function(p_input){
  v_input <- p_input
  
  #Common Rule No.1 - The first character of any separate word should be in UPPER case;
  for(lc in letters){
    pattern_UC <- paste0('(\\s|^)(',lc,')')
    v_input <- gsub(pattern_UC, paste0(' ', toupper(lc)) , v_input)
  }
  
  #Common Rule No.2 - Remove all tone sign;
  for(i in 1:length(gv_toneSign_fixed)){
    pattern_ts <- gv_toneSign[i]
    v_input <- gsub(pattern_ts,gv_toneSign_fixed[i],v_input)
  }
  
  #Common Rule No.3 -  Two or more space(‘ ’) should be cut to one;
  pattern_tp <- "\\s{2,}"
  v_input <- gsub(pattern_tp, " " , v_input)
  
  #Common Rule No.4 - Remove any space(‘ ‘) at the end or in front;
  v_input <- gsub("^\\s{1,}", "", v_input)
  v_input <- gsub("\\s{1,}$", "", v_input)
  
  
  out_input <- v_input
  return(out_input)
}

########################################################uf_sdProcess_StreetNum - process standardization rule of street number, e.g. standardize 19-10 Zonghu Load -> 10-19 Zonghu Load
uf_sdProcess_StreetNum <- function(p_address,language = "zh-CN"){
  v_address <- p_address
  
  pattern_NoRng <- '[0-9]{1,}(-)[0-9]{1,}'
  if(language == "zh-CN"){
    pattern_NoRng <- paste0(pattern_NoRng,"号")
  }else if(language == "en"){
    pattern_NoRng <- paste0("^",pattern_NoRng,"\\s")
  }else{
    print("Please input correct language code, en or zh-CN!")
    return()
  }
  
  vec_Addr <- v_address
  match_start <- regexpr(pattern_NoRng, vec_Addr)
  match_len <- attr(match_start, "match.length")
  vec_Addr_rng <- gsub(' ', '', substr(vec_Addr, match_start, match_len + match_start - 1))
  if(language == "zh-CN"){
    vec_Addr_rng <- substr(vec_Addr, match_start, match_len + match_start - 2)
  }
  
  vec_Addr_rng_1 <- as.numeric(substr(vec_Addr_rng, 1 ,regexpr('-', vec_Addr_rng) - 1))
  vec_Addr_rng_2 <- as.numeric(substr(vec_Addr_rng, regexpr('-', vec_Addr_rng) + 1, 10000L))
  vec_Addr_rng_dif <- vec_Addr_rng_2 - vec_Addr_rng_1
  
  vec_Addr_rng_fixed <- vec_Addr_rng
  vec_Addr_fixed <- vec_Addr
  for(i in 1:length(vec_Addr)){
    if(is.na(vec_Addr_rng_dif[i])){
      vec_Addr_rng_fixed[i] <- ""
    } else if(vec_Addr_rng_1[i] == 0 | vec_Addr_rng_2[i] == 0){
      vec_Addr_rng_fixed[i] <- vec_Addr_rng[i]
    }else if(vec_Addr_rng_dif[i] == 0 & vec_Addr_rng_1[i] != 0 & vec_Addr_rng_2[i] != 0){
      vec_Addr_rng_fixed[i] <- as.character(vec_Addr_rng_1[i])  
    }else if(vec_Addr_rng_dif[i] < 0 & vec_Addr_rng_1[i] != 0 & vec_Addr_rng_2[i] != 0){
      vec_Addr_rng_fixed[i] <- paste0(as.character(vec_Addr_rng_2[i]), "-", as.character(vec_Addr_rng_1[i]))
    }else {
      vec_Addr_rng_fixed[i] <- vec_Addr_rng[i]
    }
    
    if(vec_Addr_rng_fixed[i] != vec_Addr_rng[i]){
      vec_Addr_fixed[i] <- gsub(vec_Addr_rng[i], vec_Addr_rng_fixed[i], vec_Addr[i])
    }
  }
  
  v_address <- vec_Addr_fixed
  
  out_address <- v_address
  return(out_address)
}

########################################################Format Standardization Rule - PROPERTY_NAME
uf_sdProcess_PN <- function(p_propertyName){
  v_propertyName <- p_propertyName
  v_propertyName <- uf_null_fix(v_propertyName)
  
  #Rule No.1 - Replace any comma(,) by space(‘ ‘);
  pattern_comma <- ','
  v_propertyName <- gsub(pattern_comma, " " , v_propertyName)
  
  #Rule No.2 - Abbreviation translation;
  for(i in 1:length(gv_property_type_abbr)){
    pattern_Abbr <- paste0("(\\s|^)(",gv_property_type_abbr[i],")(\\s|$)")
    v_propertyName <- gsub(pattern_Abbr, paste0(" ", gv_property_type_full[i], ' ') , v_propertyName)
  }
  
  #Common Rules
  v_propertyName <- uf_sdProcess_Common_Eng(v_propertyName)
  
  out_propertyName <- v_propertyName
  return(out_propertyName)
}

########################################################Format Standardization Rule - ADDRESS_1
uf_sdProcess_ADDR <- function(p_address){
  v_address <- p_address
  v_address <- uf_null_fix(v_address)
  
  #Rule No1. - Replace any comma(, and .) by space(‘ ‘);
  pattern_comma <- "[,\\.]"
  v_address <- gsub(pattern_comma, ' ' , v_address)
  
  #Rule No.2 - District, City, Province and Country should be removed;
  pattern_cntr <- '\\s[Cc](hina)$'
  v_address <- gsub(pattern_cntr, '' , v_address)
  
  provList <- unique(gv_china_division_list$PROVIENCE)
  for(prn in provList){
    pattern_prn <- paste0('\\s(', prn, ')(\\s([Pp][Rr][Oo][Vv][Ii][Nn][Cc][Ee]|[Ss][Hh][Ee][Nn][Gg]))?$')
    v_address <- gsub(pattern_prn, '' , v_address)
  }
  
  cityList <- unique(gv_china_division_list$CITY)
  for(ct in cityList){
    pattern_city <- paste0('\\s(', ct, ')(\\s([Cc][Ii][Tt][Yy]|[Ss][Hh][Ii]))?$')
    v_address <- gsub(pattern_city, '' , v_address)
  }
  
  districtList <- unique(gv_china_division_list$DISTRICT)
  for(ds in districtList){
    pattern_dist <- paste0('\\s(', ds, ')\\s?([Dd]istrict|DISTRICT|[Qq][Uu])?$')
    v_address <- gsub(pattern_dist, '' , v_address)
  }
  
  #Rule No.3 - 'NO '(case-insensitive) in front should be removed;
  v_address <- gsub("^\\s{1,}", "", v_address)
  pattern_NO <- '^N[Oo][^A-Za-z0-9]'
  v_address <- gsub(pattern_NO, "" ,  v_address)
  
  #Rule No.4 - Correct street number, e.g. 17-14 to 14-17; 17-17 to 17;
  v_address <- uf_sdProcess_StreetNum(v_address)
  
  #Rule No.5 - Abbreviation translation;
  for(i in 1:length(gv_address_type_full)){
    pattern_abbr <- paste0("(^|\\s)", gv_address_type_abbr[i], "(\\s|$|\\.?)")
    v_address <- gsub(pattern_abbr,paste0(" ", gv_address_type_full[i], " "),v_address)
  }
  
  #Rule No.- Remove charactor '号'
  v_address <- gsub("号", "", v_address)
  
  #Common Rules
  v_address <- uf_sdProcess_Common_Eng(v_address)
  
  out_address <- v_address
  return(out_address)
}

########################################################Format Standardization Rule - DISTRICT_NAME
uf_sdProcess_DISTRICT <- function(p_district){
  v_district <- p_district
  v_district <- uf_null_fix(v_district)
  
  #Rule No.1 - 'District'(case-insensitive) should be removed;
  pattern_disrm <- "\\s(DISTRICT|[Dd]istrict|[Qq][Uu])"
  v_district <- gsub(pattern_disrm, "", v_district)
  
  #Rule No.2 - District Name conversion;
  for(i in 1:length(gv_district_sub)){
    pattern_disfrom <- gv_district_old[i]
    v_district <- gsub(pattern_disfrom, gv_district_sub[i], v_district, ignore.case = TRUE)
  }
  
  #Rule No.3 - The first character should be in UPPER case;
  for(lc in letters){
    pattern_UC <- paste0('(\\s|^)(',lc,')')
    v_district <- gsub(pattern_UC, paste0(' ', toupper(lc)) , v_district)
  }
  
  #Rule No.4 - Remove all tone sign;
  for(i in 1:length(gv_toneSign_fixed)){
    pattern_ts <- gv_toneSign[i]
    v_district <- gsub(pattern_ts,gv_toneSign_fixed[i],v_district)
  }
  for(i in 1:length(gv_toneSign_fixed)){
    #print(gv_toneSign[i])
  }
  
  
  #Rule No.5 - Remove all space character(‘ ‘);
  pattern_tp <- "\\s{1,}"
  v_district <- gsub(pattern_tp, "", v_district)
  
  #Rule No.6 - Approximate matching with standardized district list; 
  #v_sd_district <- unique(gv_china_division_list$DISTRICT)
  #index_district <- amatch(v_district,v_sd_district,maxDist = 1, nomatch = 0)
  #v_district <- ifelse(index_district == 0, "", v_sd_district[index_district]) 
  
  out_district <-  v_district
  return(out_district)
}

########################################################Format Standardization Rule - CITY_NAME
uf_sdProcess_CITY <- function(p_city){
  v_city <- p_city
  v_city <- uf_null_fix(v_city)
  
  #Rule No.1 'Shi’ or ‘City’(case-insensitive) should be removed;
  pattern_cityrm <- "\\s([Cc][Ii][Tt][Yy]|[Ss][Hh][Ii])"
  v_city <- gsub(pattern_cityrm, "", v_city)
  
  #Rule No.2 - The first character should be in UPPER case;
  for(lc in letters){
    pattern_UC <- paste0('(\\s|^)(',lc,')')
    v_city <- gsub(pattern_UC, paste0(' ', toupper(lc)) , v_city)
  }
  
  #Rule No.3 - Remove all tone sign;
  for(i in 1:length(gv_toneSign_fixed)){
    pattern_ts <- gv_toneSign[i]
    v_city <- gsub(pattern_ts,gv_toneSign_fixed[i],v_city)
  }
  
  #Rule No.4 - Remove all space character(‘ ‘);
  pattern_tp <- "\\s{1,}"
  v_city <- gsub(pattern_tp, "", v_city)
  
  out_city <- v_city
  return(out_city)
}

########################################################Format Standardization Rule - PROVINCE_NAME
uf_sdProcess_PROVINCE <- function(p_province, p_city_sd){
  v_province <- p_province
  v_province <- uf_null_fix(v_province)
  
  #Rule No.1 'Sheng’ or ‘Province’(case-insensitive) should be removed;
  pattern_provincerm <- "\\s([Pp][Rr][Oo][Vv][Ii][Nn][Cc][Ee]|[Ss][Hh][Ee][Nn][Gg])"
  v_province <- gsub(pattern_provincerm, "", v_province)
  
  #Rule No.2 - The first character should be in UPPER case;
  for(lc in letters){
    pattern_UC <- paste0('(\\s|^)(',lc,')')
    v_province <- gsub(pattern_UC, paste0(' ', toupper(lc)) , v_province)
  }
  
  #Rule No.3 - Remove all tone sign;
  for(i in 1:length(gv_toneSign_fixed)){
    pattern_ts <- gv_toneSign[i]
    v_province <- gsub(pattern_ts,gv_toneSign_fixed[i],v_province)
  }
  
  #Rule No.4 - Remove all space character(‘ ‘);
  pattern_tp <- "\\s{1,}"
  v_province <- gsub(pattern_tp, "", v_province)
  
  #Rule No.5 - If PROVINCE_NAME is NULL or invalid, fix it according to CITY_NAME
  df_province_sc <- data.frame(PROVINCE_SC = v_province,CITY_SC = p_city_sd, stringsAsFactors = FALSE)
  ds_province_sd <- unique(gv_china_division_list[,c("CITY","PROVINCE")]) 
  
  ind_mch <- match(df_province_sc$CITY_SC, ds_province_sd$CITY)
  df_province_sc[ , "PROVINCE_SC"] <- ds_province_sd[ind_mch, "PROVINCE"]
  v_province <- df_province_sc[ , "PROVINCE_SC"]
  
  out_province <- ifelse(is.na(v_province), "", v_province)
  return(out_province)
}

########################################################Format Standardization Rule - PROPERTY_NAME_CN
uf_sdProcess_PN_CN <- function(p_property_name_cn){
  v_property_name_cn <- p_property_name_cn
  v_property_name_cn <- uf_null_fix(v_property_name_cn)
  
  #Rule No.1 - Remove all space(' ')
  v_property_name_cn <- gsub("\\s{1,}", "", v_property_name_cn)
  
  out_property_name_cn <- v_property_name_cn
  return(out_property_name_cn)
}

########################################################Format Standardization Rule - ADDRESS_LC
uf_sdProcess_ADDR_CN <- function(p_address_cn){
  v_address_cn <- p_address_cn
  v_address_cn <- uf_null_fix(v_address_cn)
  
  #Rule No.1 - Remove all space(' ')
  v_address_cn <- gsub("\\s{1,}", "", v_address_cn)
  
  #Rule No.2 - Remove Country, Province, City, District
  v_address_cn <- gsub("^中国", "", v_address_cn)
  
  provList <- unique(gv_china_division_list$PROVINCE_LC)
  for(prn in provList){
    pattern_prn <- paste0('^', prn, '[省|市]')
    v_address_cn <- gsub(pattern_prn, '' , v_address_cn)
  }
  
  cityList <- unique(gv_china_division_list$CITY_LC)
  for(ct in cityList){
    pattern_city <- paste0('^', ct, '(市)')
    v_address_cn <- gsub(pattern_city, '' , v_address_cn)
  }
  
  districtList <- unique(gv_china_division_list$DISTRICT_LC)
  for(ds in districtList){
    pattern_dist <- paste0('^', ds, '(区)')
    v_address_cn <- gsub(pattern_dist, '' , v_address_cn)
  }
  
  #Rule No.3 - Add ‘号’at the end if last character is digital number;
  pattern_hao <- '[0-9]$'   
  for (i in 1:length(v_address_cn)) {
    if(regexpr(pattern_hao, v_address_cn[i]) > 0){
      v_address_cn[i] <- paste0(v_address_cn[i], "号")
    }
  }
  
  #Rule No.4 - Correct street number, e.g. 17-14 to 14-17; 17-17 to 17;
  v_address_cn <- uf_sdProcess_StreetNum(v_address_cn, "zh-CN")
  
  #Rule No.5 - Remove words after pattern '[0-9]号'
  pattern_sufix <- "[0-9]号.*"
  num_1 <- regexpr(pattern_sufix,v_address_cn)
  v_address_cn <- ifelse(num_1 > 0, substr(v_address_cn,1, num_1 + 1), v_address_cn) 
  
  out_address_cn <- v_address_cn
  return(out_address_cn)
}

########################################################Format Standardization Rule - DISTRICT_NAME_LC
uf_sdProcess_DISTRICT_CN <- function(p_district_cn){
  v_district_cn <- p_district_cn
  v_district_cn <- uf_null_fix(v_district_cn)
  
  #Rule No.1 - Remove all space(‘ ’)
  v_district_cn <- gsub("\\s{1,}", "",v_district_cn)
  
  #Rule No.2 - '区' at the end should be removed;
  v_district_cn <- gsub("区$", "", v_district_cn)
  
  #Rule No.3 - District Name conversion;
  for(i in 1:length(gv_district_lc_sub)){
    pattern_disfrom <- gv_district_lc_old[i]
    v_district_cn <- gsub(pattern_disfrom, gv_district_lc_sub[i], v_district_cn, ignore.case = TRUE)
  }
  
  out_district_cn <- v_district_cn 
  return(out_district_cn)
}

########################################################Format Standardization Rule - CITY_NAME_LC
uf_sdProcess_CITY_CN <- function(p_city_cn){
  v_city_cn <- p_city_cn
  v_city_cn <- uf_null_fix(v_city_cn)
  
  #Rule No.1 - Remove all space(‘ ’)
  v_city_cn <- gsub("\\s{1,}", "", v_city_cn)
  
  #Rule No.2 - ‘市’at the end should be removed;
  v_city_cn <- gsub("市$", "", v_city_cn)
  
  out_city_cn <- v_city_cn 
  return(out_city_cn)
}

########################################################Format Standardization Rule - PROVINCE_NAME_LC
uf_sdProcess_PROVINCE_CN <- function(p_province_cn, p_city_cn_sd){
  v_province_cn <- p_province_cn
  v_province_cn <- uf_null_fix(v_province_cn)
  
  #Rule No.1 Remove all space(‘ ’)
  v_province_cn <- gsub("\\s{1,}", "", v_province_cn)
  
  #Rule No.2 - '省’or ‘市’at the end should be removed;
  v_province_cn <- gsub("[省|市]$", "", v_province_cn)
  
  #Rule No.3 - If PROVINCE_NAME_LC is NULL or invalid, fix it according to CITY_NAME_LC
  df_province_sc <- data.frame(PROVINCE_SC = v_province_cn,CITY_SC = p_city_cn_sd, stringsAsFactors = FALSE)
  ds_province_sd <- unique(gv_china_division_list[,c("CITY_LC","PROVINCE_LC")]) 
  
  ind_mch <- match(df_province_sc$CITY_SC, ds_province_sd$CITY_LC)
  df_province_sc[ , "PROVINCE_SC"] <- ds_province_sd[ind_mch, "PROVINCE_LC"]
  v_province_cn <- df_province_sc[ , "PROVINCE_SC"]
  
  out_province_cn <- ifelse(is.na(v_province_cn),"",v_province_cn)
  return(out_province_cn)
}

########################################################Format Standardization Rule - ALL
uf_sdProcess_ALL <- function(p_df){
  df_raw <- p_df
  
  d_count <- nrow(df_raw)
  
  if(any((colnames(df_raw) == gv_geocore_fields) == FALSE)){
    print("Please check the Geo Core fields of your input!")
    return()
  }else {
    PROPERTY_NAME <- uf_sdProcess_PN(df_raw$PROPERTY_NAME)
    ADDRESS_1 <- uf_sdProcess_ADDR(df_raw$ADDRESS_1)
    DISTRICT_NAME <- uf_sdProcess_DISTRICT(df_raw$DISTRICT_NAME)
    CITY_NAME <- uf_sdProcess_CITY(df_raw$CITY_NAME)
    PROVINCE_NAME <- uf_sdProcess_PROVINCE(df_raw$PROVINCE_NAME,uf_sdProcess_CITY(df_raw$CITY_NAME))
    
    PROPERTY_NAME_LC <- uf_sdProcess_PN_CN(df_raw$PROPERTY_NAME_LC)
    ADDRESS_LC <- uf_sdProcess_ADDR_CN(df_raw$ADDRESS_LC)
    DISTRICT_NAME_LC <- uf_sdProcess_DISTRICT_CN(df_raw$DISTRICT_NAME_LC)
    CITY_NAME_LC <- uf_sdProcess_CITY_CN(df_raw$CITY_NAME_LC)
    PROVINCE_NAME_LC <- uf_sdProcess_PROVINCE_CN(df_raw$PROVINCE_NAME_LC,uf_sdProcess_CITY_CN(df_raw$CITY_NAME_LC))
    
    df_fixed <- data.frame(SOURCE_PROPERTY_ID = df_raw$SOURCE_PROPERTY_ID,
                           PROPERTY_NAME = PROPERTY_NAME,
                           PROPERTY_NAME_LC = PROPERTY_NAME_LC,
                           ADDRESS_1 = ADDRESS_1,
                           ADDRESS_LC = ADDRESS_LC,
                           DISTRICT_NAME = DISTRICT_NAME,
                           DISTRICT_NAME_LC = DISTRICT_NAME_LC,
                           CITY_NAME = CITY_NAME,
                           CITY_NAME_LC = CITY_NAME_LC,
                           PROVINCE_NAME = PROVINCE_NAME,
                           PROVINCE_NAME_LC = PROVINCE_NAME_LC,
                           COUNTRY_NAME = rep("China",d_count),
                           COUNTRY_NAME_LC = rep("中国",d_count),
                           TIMEZONE = rep("UTC/GMT+08:00",d_count),
                           GEO_LAT = df_raw$GEO_LAT,
                           GEO_LONG = df_raw$GEO_LONG)
    
    return(df_fixed)
  }
  
}




########################################################Functions - Validity Check########################################################




########################################################Validity Check - ADDRESS_1 
uf_validityCheck_ADDR <- function(p_address_sd){
  v_address <- p_address_sd
  v_address <- uf_null_fix(v_address)
  a_count <- length(v_address)
  
  v_Isvalid <- rep("TBC", a_count)
  
  for(i in 1:a_count){
    if(v_address[i] == ""){
      v_Isvalid[i] <- "NULL"
    }else if(v_address[i] == "TBC" |
             v_address[i] == "TBD"){
      v_Isvalid[i] <- "Invalid"
    }else if(nchar(v_address[i]) < 7 | 
             regexpr("(Road|Avenue|Street|[Ll]u$|Hutong|Alley|[Jj]ie$|[Yy]uan$|Xiang|[Tt]iao$|[Dd]ao$|[Dd]a[Dd]ao$|Boulevard|Lane)", v_address[i]) < 0){
      v_Isvalid[i] <- "Invalid"
    }else if(nchar(v_address[i],type = "chars") < nchar(v_address[i],type = "bytes")){
      v_Isvalid[i] <- "Partial-Valid"
    }else if(regexpr("^[0-9]",v_address[i]) < 0){
      v_Isvalid[i] <- "Partial-Valid"
    }else {
      v_Isvalid[i] <- "Valid"
    }
  }
  
  return(v_Isvalid)
}

########################################################Validity Check - PROPERTY_NAME
uf_validityCheck_PN <- function(p_propertyName_sd){
  v_propertyName <- p_propertyName_sd
  v_propertyName <- uf_null_fix(v_propertyName)
  p_count <- length(v_propertyName)
  
  v_Isvalid <- rep("TBC", p_count)

  for(i in 1:p_count){
    if(v_propertyName[i] == ""){
      v_Isvalid[i] <- "NULL"
    }else if(v_propertyName[i] == "TBC" |
             v_propertyName[i] == "TBD"){
      v_Isvalid[i] <- "Invalid"
    }else if(nchar(v_propertyName[i]) < 3 | 
             as.character(uf_validityCheck_ADDR(v_propertyName[i])) == "Valid"){
      v_Isvalid[i] <- "Invalid"
    }else if(nchar(v_propertyName[i],type = "chars") < nchar(v_propertyName[i],type = "bytes")){
      v_Isvalid[i] <- "Partial-Valid"
    }else {
      v_Isvalid[i] <- "Valid"
    }
  }
  
  return(v_Isvalid)
}

########################################################Validity Check - CITY_NAME
uf_validityCheck_CITY <- function(p_city_sd){
  v_city <- p_city_sd
  v_city <- uf_null_fix(v_city)
  c_count <- length(v_city)
  
  v_Isvalid <- rep("TBC", c_count)
  
  for(i in 1:c_count){
    if(v_city[i] == ""){
      v_Isvalid[i] <- "NULL"
    }else if(!any(unique(gv_china_division_list$CITY) == v_city[i])){
      v_Isvalid[i] <- "Invalid"
    }else{
      v_Isvalid[i] <- "Valid"
    }
  }
  
  return(v_Isvalid)
}


########################################################Validity Check - ADDRESS_LC
uf_validityCheck_ADDR_CN <- function(p_address_lc_sd){
  v_address_lc <- p_address_lc_sd
  v_address_lc <- uf_null_fix(v_address_lc)
  a_count <- length(v_address_lc)
  
  v_Isvalid <- rep("TBC", a_count)
  for(i in 1:a_count){
    if(v_address_lc[i] == ""){
      v_Isvalid[i] <- "NULL"
    } else if(nchar(v_address_lc[i]) < 3){
      v_Isvalid[i] <- "Invalid"
    }else if(regexpr("(街|路|道|巷|院|条|里|胡同)",v_address_lc[i]) < 0){
      v_Isvalid[i] <- "Invalid"
    }else if(regexpr("([0-9]号|[一二三四五六七八九十零]号)",v_address_lc[i]) < 0){
      v_Isvalid[i] <- "Partial-Valid"
    }else {
      v_Isvalid[i] <- "Valid"
    }
  }
  return(v_Isvalid)
}

########################################################Validity Check - PROPERTY_NAME_LC
uf_validityCheck_PN_CN <- function(p_propertyName_cn_sd){
  v_propertyName_cn <- p_propertyName_cn_sd
  v_propertyName_cn <- uf_null_fix(v_propertyName_cn)
  p_count <- length(v_propertyName_cn)
  
  v_Isvalid <- rep("TBC", p_count)
  
  for(i in 1:p_count){
    if(v_propertyName_cn[i] == ""){
      v_Isvalid[i] <- "NULL"
    }else if(nchar(v_propertyName_cn[i]) < 3 |
             as.character(uf_validityCheck_ADDR_CN(v_propertyName_cn[i])) == "Valid" |
             as.character(uf_validityCheck_ADDR(v_propertyName_cn[i])) == "Valid"){
      v_Isvalid[i] <- "Invalid"
    }else if(nchar(v_propertyName_cn[i], type = "chars") == nchar(v_propertyName_cn[i], type = "bytes")){
      v_Isvalid[i] <- "Partial-Valid"
    }else {
      v_Isvalid[i] <- "Valid"
    }
  }
  
  return(v_Isvalid)
}

########################################################Validity Check - CITY_NAME_LC
uf_validityCheck_CITY_LC <- function(p_city_cn_sd){
  v_city_cn <- p_city_cn_sd
  v_city_cn <- uf_null_fix(v_city_cn)
  c_count <- length(v_city_cn)
  
  v_Isvalid <- rep("TBC", c_count)
  
  for(i in 1:c_count){
    if(v_city_cn[i] == ""){
      v_Isvalid[i] <- "NULL"
    }else if(!any(unique(gv_china_division_list$CITY_LC) == v_city_cn[i])){
      v_Isvalid[i] <- "Invalid"
    }else{
      v_Isvalid[i] <- "Valid"
    }
  }
  
  return(v_Isvalid)
}

########################################################Validity Check - ALL
uf_validityCheck_ALL <- function(p_df_sd){
  df_sd <- p_df_sd
  
  if(any((colnames(df_sd) == gv_geocore_fields) == FALSE)){
    print("Please check the Geo Core fields of your input!")
    return()
  }else {
    Remark_PROPERTY_NAME <- uf_validityCheck_PN(df_sd$PROPERTY_NAME)
    Remark_ADDRESS_1 <- uf_validityCheck_ADDR(df_sd$ADDRESS_1)
    Remark_CITY_NAME <- uf_validityCheck_CITY(df_sd$CITY_NAME)
    
    Remark_PROPERTY_NAME_LC <- uf_validityCheck_PN_CN(df_sd$PROPERTY_NAME_LC)
    Remark_ADDRESS_LC <- uf_validityCheck_ADDR_CN(df_sd$ADDRESS_LC)
    Remark_CITY_NAME_LC <- uf_validityCheck_CITY_LC(df_sd$CITY_NAME_LC)
    
    df_remarked <- data.frame(SOURCE_PROPERTY_ID = df_sd$SOURCE_PROPERTY_ID,
                           PROPERTY_NAME = df_sd$PROPERTY_NAME,
                           Remark_PROPERTY_NAME = Remark_PROPERTY_NAME,
                           PROPERTY_NAME_LC = df_sd$PROPERTY_NAME_LC,
                           Remark_PROPERTY_NAME_LC = Remark_PROPERTY_NAME_LC,
                           ADDRESS_1 = df_sd$ADDRESS_1,
                           Remark_ADDRESS_1 = Remark_ADDRESS_1,
                           ADDRESS_LC = df_sd$ADDRESS_LC,
                           Remark_ADDRESS_LC = Remark_ADDRESS_LC,
                           DISTRICT_NAME = df_sd$DISTRICT_NAME,
                           DISTRICT_NAME_LC = df_sd$DISTRICT_NAME_LC,
                           CITY_NAME = df_sd$CITY_NAME,
                           Remark_CITY_NAME = Remark_CITY_NAME,
                           CITY_NAME_LC = df_sd$CITY_NAME_LC,
                           Remark_CITY_NAME_LC = Remark_CITY_NAME_LC,
                           PROVINCE_NAME = df_sd$PROVINCE_NAME,
                           PROVINCE_NAME_LC = df_sd$PROVINCE_NAME_LC,
                           COUNTRY_NAME = df_sd$COUNTRY_NAME,
                           COUNTRY_NAME_LC = df_sd$COUNTRY_NAME_LC,
                           TIMEZONE = df_sd$TIMEZONE,
                           GEO_LAT = df_sd$GEO_LAT,
                           GEO_LONG = df_sd$GEO_LONG)
    
    return(df_remarked)
  }
}




########################################################Functions - Rejection Process########################################################


########################################################Pre-API Rejection Process
uf_rejectProcess_preAPI <-function(p_df_remarked){
  df_remarked <- p_df_remarked
  d_count <- nrow(df_remarked)
  
  v_rejectMark <- rep("TBC",d_count)
  v_rejectReason <- rep("", d_count)
  
  for(i in 1:d_count){
    rm_CITY <- df_remarked$Remark_CITY_NAME[i]
    re_CITY_cn <- df_remarked$Remark_CITY_NAME_LC[i]
    rm_PN <- df_remarked$Remark_PROPERTY_NAME[i]
    rm_PN_cn <- df_remarked$Remark_PROPERTY_NAME_LC[i]
    rm_ADDR <- df_remarked$Remark_ADDRESS_1[i]
    rm_ADDR_cn <- df_remarked$Remark_ADDRESS_LC[i]
    
    rm_core_reject <- c((rm_PN == "NULL" | rm_PN == "Invalid"), 
                        (rm_PN_cn == "NULL" | rm_PN_cn == "Invalid"), 
                        (rm_ADDR == "NULL" | rm_ADDR == "Invalid"), 
                        (rm_ADDR_cn == "NULL" | rm_ADDR_cn == "Invalid") )
    
    if(rm_CITY != "Valid" & re_CITY_cn != "Valid"){
      v_rejectMark[i] <- "Rejected"
      v_rejectReason[i] <- "Both CITY_NAME and CITY_NAME_LC is Invalid or NULL."
    }else if(length(which(rm_core_reject == TRUE)) >= 3){
      v_rejectMark[i] <- "Rejected"
      v_rejectReason[i] <- "Any 3 out 4 core fileds(PROPERTY_NAME,ADDRESS_1,PROPERTY_NAME_LC and ADDRESS_LC) are Invalid or NULL."
    }else if(rm_core_reject[1] & rm_core_reject[2]){
      v_rejectMark[i] <- "Rejected"
      v_rejectReason[i] <- "Both PROPERTY_NAME and PROPERTY_NAME_LC is NULL or Invalid."
    }else if(rm_core_reject[3] & rm_core_reject[4]){
      v_rejectMark[i] <- "Rejected"
      v_rejectReason[i] <- "Both ADDRESS_1 and ADDRESS_LC  is NULL or Invalid."
    }else{
      v_rejectMark[i] <- "Qualified For API Process"
    }
  }
  
  df_remarked$Reject_Mark <- v_rejectMark
  df_remarked$Reject_Reason <- v_rejectReason
  
  return(df_remarked)
  
}


  



###############################################GOOGLE API Search###############################################
################textSearch - Google Places API
################input - query e.g. c("淞沪路234号", "杨浦",             "上海"      ,       "中国" )
################output - PlaceID e.g. ChIJZ-hP579zsjURalSz06B0qOE
google.api.textSearch <- function(query, key, language){
  placeid = ""
  
  query_1 <- paste0(query[1],",",query[2],",",query[3],",",query[4])
  root <- "https://maps.googleapis.com/maps/api/place/textsearch/json"
  u <- paste0(root, "?query=", query_1, "&language=", language, "&key=", key)
  doc <- getURL(URLencode(u))
  x <- fromJSON(doc,simplify = FALSE)
  
  if(x$status == "OK"){
    #select the best matched result
    result <- x$results[[1]]   
    rsl_placeid <- result$place_id
    placeid <- rsl_placeid
  }else{
    print("Google can not find anything according to your input!")
  }
  return(placeid)
}


################textSearch - Google Places API
################input - PlaceID e.g. ChIJZ-hP579zsjURalSz06B0qOE
################output - v_output
################potential types:
################types : [ "point_of_interest", "establishment" ]  
################types : [ "street_address" ]
################types : [ "route" ]
################types : [ "premise" ]
google.api.detailSearch <- function(placeid, key, language, in_pty_name){
  #####Define output
  v_output <- rep("",12)
  attr(v_output, "Elements") <- c("PROPERTY_NAME", "ADDRESS", "DISTRICT", "CITY", "GEO_LAT", "GEO_LONG", "postal_code", "phone_number","place_id","gmap_url","website","remark")
  
  root <- "https://maps.googleapis.com/maps/api/place/details/json?placeid="
  u <- paste0(root,placeid,"&language=", language, "&key=", key)
  doc <- getURL(URLencode(u))
  x <- fromJSON(doc,simplify = FALSE)
  
  if(x$status == "OK"){
    result <- x$result
    rsl_types <- result$types
    
    if(any(rsl_types == "establishment")){
      #print("The found place is an establishment!")
      v_output[which(attr(v_output,"Elements") == "PROPERTY_NAME")] <- result$name
      v_output[which(attr(v_output,"Elements") == "GEO_LAT")] <- result$geometry$location$lat  
      v_output[which(attr(v_output,"Elements") == "GEO_LONG")] <- result$geometry$location$lng  
      v_output[which(attr(v_output,"Elements") == "phone_number")] <- ifelse(is.null(result$formatted_phone_number), "", result$formatted_phone_number)
      v_output[which(attr(v_output,"Elements") == "place_id")] <- ifelse(is.null(result$place_id), "", result$place_id)
      v_output[which(attr(v_output,"Elements") == "gmap_url")] <- ifelse(is.null(result$url), "", result$url)
      v_output[which(attr(v_output,"Elements") == "website")] <- ifelse(is.null(result$website), "", result$website)
      
      street_number <- ""
      route <- ""
      district <- ""
      city <- ""
      postal_code <- ""
      address_components <- result$address_components
      for(i in 1:length(address_components)){
        if(any(address_components[[i]]$types == "street_number")){
          street_number <-  address_components[[i]]$long_name
        }else if(any(address_components[[i]]$types == "route")){
          route <- address_components[[i]]$long_name
        }else if(any(address_components[[i]]$types == "sublocality")){
          district <- address_components[[i]]$long_name
          district <- gsub("(区$|\\sQu)","",district)
        }else if(any(address_components[[i]]$types == "locality")){
          city <- address_components[[i]]$long_name
          city <- gsub("(市$|\\s{0,1}[Ss]hi)","",city)
        }else if(any(address_components[[i]]$types == "postal_code")){
          postal_code <- address_components[[i]]$long_name
        }
      }
      if(language == "zh-CN"){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- paste0(route,street_number)
      }else if(language == "en"){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- paste0(street_number," ", route)
      }
      
      v_output[which(attr(v_output,"Elements") == "DISTRICT")] <- district
      v_output[which(attr(v_output,"Elements") == "CITY")] <- city
      v_output[which(attr(v_output,"Elements") == "postal_code")] <- postal_code
      
      if(gsub("\\s{1,}","",v_output[which(attr(v_output,"Elements") == "ADDRESS")]) == ""){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- result$vicinity
      }
      v_output[12] <- "establishment"
      
    }else if(any(rsl_types == "street_address")){
      #print("The found place is a street_address")
      
      v_output[which(attr(v_output,"Elements") == "PROPERTY_NAME")] <- ""
      v_output[which(attr(v_output,"Elements") == "GEO_LAT")] <- result$geometry$location$lat  
      v_output[which(attr(v_output,"Elements") == "GEO_LONG")] <- result$geometry$location$lng  
      v_output[which(attr(v_output,"Elements") == "phone_number")] <- ifelse(is.null(result$formatted_phone_number), "", result$formatted_phone_number)
      v_output[which(attr(v_output,"Elements") == "place_id")] <- ifelse(is.null(result$place_id), "", result$place_id)
      v_output[which(attr(v_output,"Elements") == "gmap_url")] <- ifelse(is.null(result$url), "", result$url)
      v_output[which(attr(v_output,"Elements") == "website")] <- ifelse(is.null(result$website), "", result$website)
      
      street_number <- ""
      route <- ""
      district <- ""
      city <- ""
      postal_code <- ""
      address_components <- result$address_components
      for(i in 1:length(address_components)){
        if(any(address_components[[i]]$types == "street_number")){
          street_number <-  address_components[[i]]$long_name
        }else if(any(address_components[[i]]$types == "route")){
          route <- address_components[[i]]$long_name
        }else if(any(address_components[[i]]$types == "sublocality")){
          district <- address_components[[i]]$long_name
          district <- gsub("(区$|\\sQu)","",district)
        }else if(any(address_components[[i]]$types == "locality")){
          city <- address_components[[i]]$long_name
          city <- gsub("(市$|\\s{0,1}[Ss]hi)","",city)
        }else if(any(address_components[[i]]$types == "postal_code")){
          postal_code <- address_components[[i]]$long_name
        }
      }
      if(language == "zh-CN"){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- paste0(route,ifelse(substr(street_number,nchar(street_number),nchar(street_number)) == "号",street_number,paste0(street_number,"号")))
      }else if(language == "en"){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- paste0(street_number," ", route)
      }
      v_output[which(attr(v_output,"Elements") == "DISTRICT")] <- district
      v_output[which(attr(v_output,"Elements") == "CITY")] <- city
      v_output[which(attr(v_output,"Elements") == "postal_code")] <- postal_code
      
      latlng = c(v_output[which(attr(v_output,"Elements") == "GEO_LAT")],
                 v_output[which(attr(v_output,"Elements") == "GEO_LONG")])
      property_name <- google.api.reverse.geocode(latlng,language = language, key = key)
      if(gsub("\\s{1,}","",property_name) == ""){  #if property_name is null, try nearbysearch
        property_name = google.api.nearbysearch(latlng,language = language, key = key,placetype = "street_address",in_pty_name = in_pty_name)
        v_output[which(attr(v_output,"Elements") == "PROPERTY_NAME")] <- property_name
        v_output[12] <- "street_address.nearbysearch"
      }else{
        v_output[which(attr(v_output,"Elements") == "PROPERTY_NAME")] <- property_name
        v_output[12] <- "street_address.reverse.geocode"
      }
    }else if(any(rsl_types == "route")){
      #print("The found place is a route")
      
      v_output[which(attr(v_output,"Elements") == "GEO_LAT")] <- result$geometry$location$lat  
      v_output[which(attr(v_output,"Elements") == "GEO_LONG")] <- result$geometry$location$lng  
      v_output[which(attr(v_output,"Elements") == "phone_number")] <- ifelse(is.null(result$formatted_phone_number), "", result$formatted_phone_number)
      v_output[which(attr(v_output,"Elements") == "place_id")] <- ifelse(is.null(result$place_id), "", result$place_id)
      v_output[which(attr(v_output,"Elements") == "gmap_url")] <- ifelse(is.null(result$url), "", result$url)
      v_output[which(attr(v_output,"Elements") == "website")] <- ifelse(is.null(result$website), "", result$website)
      
      street_number <- ""
      route <- ""
      district <- ""
      city <- ""
      postal_code <- ""
      address_components <- result$address_components
      for(i in 1:length(address_components)){
        if(any(address_components[[i]]$types == "street_number")){
          street_number <-  address_components[[i]]$long_name
        }else if(any(address_components[[i]]$types == "route")){
          route <- address_components[[i]]$long_name
        }else if(any(address_components[[i]]$types == "sublocality")){
          district <- address_components[[i]]$long_name
          district <- gsub("(区$|\\sQu)","",district)
        }else if(any(address_components[[i]]$types == "locality")){
          city <- address_components[[i]]$long_name
          city <- gsub("(市$|\\s{0,1}[Ss]hi)","",city)
        }else if(any(address_components[[i]]$types == "postal_code")){
          postal_code <- address_components[[i]]$long_name
        }
      }
      if(language == "zh-CN"){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- paste0(route,street_number)
      }else if(language == "en"){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- paste0(street_number," ", route)
      }
      v_output[which(attr(v_output,"Elements") == "DISTRICT")] <- district
      v_output[which(attr(v_output,"Elements") == "CITY")] <- city
      v_output[which(attr(v_output,"Elements") == "postal_code")] <- postal_code
      
      latlng = c(v_output[which(attr(v_output,"Elements") == "GEO_LAT")],
                 v_output[which(attr(v_output,"Elements") == "GEO_LONG")])
      
      property_name = google.api.nearbysearch(latlng,language = language, key = key,placetype = "route",in_pty_name = in_pty_name)
      v_output[which(attr(v_output,"Elements") == "PROPERTY_NAME")] <- property_name
      v_output[12] <- "street_address.nearbysearch"
    }else if(any(rsl_types == "premise")){
      #print("The found place is a premise!")
      v_output[which(attr(v_output,"Elements") == "PROPERTY_NAME")] <- result$name
      v_output[which(attr(v_output,"Elements") == "GEO_LAT")] <- result$geometry$location$lat  
      v_output[which(attr(v_output,"Elements") == "GEO_LONG")] <- result$geometry$location$lng  
      v_output[which(attr(v_output,"Elements") == "phone_number")] <- ifelse(is.null(result$formatted_phone_number), "", result$formatted_phone_number)
      v_output[which(attr(v_output,"Elements") == "place_id")] <- ifelse(is.null(result$place_id), "", result$place_id)
      v_output[which(attr(v_output,"Elements") == "gmap_url")] <- ifelse(is.null(result$url), "", result$url)
      v_output[which(attr(v_output,"Elements") == "website")] <- ifelse(is.null(result$website), "", result$website)
      
      street_number <- ""
      route <- ""
      district <- ""
      city <- ""
      postal_code <- ""
      address_components <- result$address_components
      for(i in 1:length(address_components)){
        if(any(address_components[[i]]$types == "street_number")){
          street_number <-  address_components[[i]]$long_name
        }else if(any(address_components[[i]]$types == "route")){
          route <- address_components[[i]]$long_name
        }else if(any(address_components[[i]]$types == "sublocality")){
          district <- address_components[[i]]$long_name
          district <- gsub("(区$|\\sQu)","",district)
        }else if(any(address_components[[i]]$types == "locality")){
          city <- address_components[[i]]$long_name
          city <- gsub("(市$|\\s{0,1}[Ss]hi)","",city)
        }else if(any(address_components[[i]]$types == "postal_code")){
          postal_code <- address_components[[i]]$long_name
        }
      }
      if(language == "zh-CN"){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- paste0(route,street_number)
      }else if(language == "en"){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- paste0(street_number," ", route)
      }
      v_output[which(attr(v_output,"Elements") == "DISTRICT")] <- district
      v_output[which(attr(v_output,"Elements") == "CITY")] <- city
      v_output[which(attr(v_output,"Elements") == "postal_code")] <- postal_code
      
      if(gsub("\\s{1,}","",v_output[which(attr(v_output,"Elements") == "ADDRESS")]) == ""){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- ""
      }
      v_output[12] <- "premise"
      
      #if(v_output[which(attr(v_output,"Elements") == "ADDRESS")] == ""){
      #  latlng = c(v_output[which(attr(v_output,"Elements") == "GEO_LAT")],
      #             v_output[which(attr(v_output,"Elements") == "GEO_LONG")])
      #  v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- google.api.nearbysearch(latlng, language = language, key = key,placetype = "premise", in_pty_name = in_pty_name)
      #}
      
    }
  }else{
    print("Placeid you inputed is incorrect!")
  }
  return(v_output)
}


################reverse.geocode - Google GeoCoding API
################input - latlng e.g. c(31.30482 121.51377)
################output - property_name
################potential types: https://developers.google.com/maps/documentation/geocoding/intro?hl=zh-tw#Types
################types : [ "premise"]  
################types : ["point_of_interest"]
google.api.reverse.geocode <- function(latlng,language,key,result_type="premise|point_of_interest|establishment"){
  #####Define output
  property_name = ""
  
  if(length(latlng) != 2){
    print("Please input correct Lat and Long value!")
    return()
  }else{
    root = "https://maps.googleapis.com/maps/api/geocode/json?latlng="
    u <- paste0(root,paste0(latlng[1],",",latlng[2]),"&result_type=",result_type,"&language=",language,"&key=",key)
    doc <- getURL(URLencode(u))
    x <- fromJSON(doc,simplify = FALSE)
    if(x$status == "OK"){
      result <- x$results[[1]]
      
      if(length(result$types)<=2 & (any(result$types == "premise") | any(result$types == "point_of_interest") | any(result$types == "establishment"))){
        for(i in 1:length(result$address_components)){
          type_ac <- result$address_components[[i]]$types
          if(any(type_ac == "premise")){
            property_name <- result$address_components[[i]]$long_name
            break
          }
        }
      }
      
    }else{
      print("Google can not find anything according to your input!")
    }
  }
  
  return(property_name)
}


################nearbysearch - Google Place API
################input - location = c("31.2596854","121.5127786")
################input - radius e.g. 100
################output - property_name
google.api.nearbysearch <- function(location,language, key, placetype = "route", radius = 100, types="establishment|point_of_interest", in_pty_name){
  #placetype - route,street_address,premise
  
  #####Define output
  s_output = ""
  
  if(length(location) != 2){
    print("Please input correct Lat and Long value!")
    return()
  }else{
    root = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location="
    u <- paste0(root,paste0(location[1],",",location[2]),"&radius=",radius,"&types=",types,"&language=",language,"&key=",key)
    doc <- getURL(URLencode(u))
    x <- fromJSON(doc,simplify = FALSE)
    if(x$status == "OK"){
      results_pre <- x$results
      n_result <- length(results_pre)
      list_num <- rep(0,n_result)
      for(i in 1:n_result){
        rlt_type <- results_pre[[i]]$types
        if(length(rlt_type)<=2 & (any(rlt_type=="establishment") | any(rlt_type=="point_of_interest"))){
          list_num[i] <- 1
        }
      }
      results <- results_pre[which(list_num==1)]
      if(length(results)>0){
        result <- results[[1]]
      }else{
        return(s_output)
      }
      
      
      if(placetype == "street_address"){
        rlt_pty_name <- rep("",length(results))
        str_dist_pty_name <- rep(1,length(results))
        dst_diff <- rep(1,length(results))
        for(i in 1:length(results)){
          rlt_pty_name[i] <- results[[i]]$name
          #str_dist_pty_name[i] <- stringdist(rlt_pty_name[i],in_pty_name)/max(nchar(rlt_pty_name[i]),nchar(in_pty_name))
          str_dist_pty_name[i] <- stringsim(rlt_pty_name[i],in_pty_name,method = gv_method_strSim)
          
          r_lat <- results[[i]]$geometry$location$lat
          r_lat <- ifelse(is.null(r_lat), 180, r_lat)
          r_long <- results[[i]]$geometry$location$lng
          r_long <- ifelse(is.null(r_long), 180, r_long)
          #diff_lat <- abs(r_lat - as.numeric(location[1]))
          #diff_long <- abs(r_long - as.numeric(location[2]))
          #diff_3 <- sqrt(diff_lat^2 + diff_long^2)
          
          point_dist <- distm(c(r_long, r_lat), c(as.numeric(location[2]), as.numeric(location[1])), fun = distHaversine)
          
          dst_diff[i] <- point_dist
        }
        
        rlt_num <- 1
        if(any(str_dist_pty_name >= gv_accept_sim - 0.25)){
          rlt_num <- which(str_dist_pty_name == max(str_dist_pty_name))[1]
        }else{
          rlt_num <- which(dst_diff == min(dst_diff))[1]
        }
        
        result <- results[[rlt_num]]
        s_output = result$name
      }
      
    }else{
      print("Google can not find anything according to your input!")
    }
  }
  return(s_output)
}



###############################################BAIDU API Search###############################################
baidu.api.poiSearch <- function(query,region,key,tag="房地产,酒店,写字楼",scope=2,page_size=1,output="json"){
  uid = ""
  
  root <- "http://api.map.baidu.com/place/v2/search"
  u <- paste0(root, "?q=", query, "&region=",region,"&city_limit=true","&tag=", tag, "&scope=",scope,"&page_size=",page_size,"&output=",output,"&ak=", key)
  doc <- getURL(u)
  
  if(doc != "" & !is.null(doc)){
    x <- fromJSON(doc,simplify = FALSE)
    if(length(x$results) > 0){
      if(x$status==0 & !is.null(x$results[[1]]$uid)){
        results <- x$results
        result <- results[[1]]
        uid <- result$uid
      }
    }
  }else{
    print("Baidu can not find anything according to your input!")
  }
  return(uid)
}

#uid <- "7d5f3a0a60d160352cb90873"
#region <- "北京"
#key <- "NdgFqhKyiqPksC2pLfsGsWps15wzRsES"
baidu.api.detailSearch <- function(uid,region,key,output="json",scope=2){
  #####Define output
  v_output <- rep("",11)
  attr(v_output, "Elements") <- c("PROPERTY_NAME", "ADDRESS", "DISTRICT", "CITY", "GEO_LAT", "GEO_LONG", "postal_code", "phone_number","place_id","gmap_url","website")
  
  if(uid != ""){
    root <- "http://api.map.baidu.com/place/v2/detail?"
    u <- paste0(root, "uid=", uid, "&output=",output,"&scope=",scope,"&ak=", key)
    doc <- getURL(URLencode(u))
    
    if(doc != "" & !is.null(doc)){
      x <- fromJSON(doc,simplify = FALSE)
      if(x$status==0 & x$result$uid == uid){
        result <- x$result
        place_id <- result$uid
        property_name <- ifelse(is.null(result$name), "", result$name) 
        address <- ifelse(is.null(result$address), "", result$address) 
        website <- ifelse(is.null(result$detail_info$detail_url), "", result$detail_info$detail_url)  
        phone_number <- ifelse(is.null(result$telephone), "", result$telephone) 
        geo_lat <- ifelse(is.null(result$location$lat), "", result$location$lat)
        geo_long <- ifelse(is.null(result$location$lng), "", result$location$lng)
        gmap_url <- ""
        district <- ""
        postal_code <- ""
        
        v_output[which(attr(v_output,"Elements") == "PROPERTY_NAME")] <- property_name
        v_output[which(attr(v_output,"Elements") == "GEO_LAT")] <- geo_lat
        v_output[which(attr(v_output,"Elements") == "GEO_LONG")] <- geo_long
        v_output[which(attr(v_output,"Elements") == "phone_number")] <- phone_number
        v_output[which(attr(v_output,"Elements") == "place_id")] <- place_id
        v_output[which(attr(v_output,"Elements") == "gmap_url")] <- gmap_url
        v_output[which(attr(v_output,"Elements") == "website")] <- website
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- address
        v_output[which(attr(v_output,"Elements") == "DISTRICT")] <- district
        v_output[which(attr(v_output,"Elements") == "CITY")] <- region
        v_output[which(attr(v_output,"Elements") == "postal_code")] <- postal_code
        
      }
    }else{
      print("Google can not find anything according to your input!")
    }
  }
  return(v_output)
  
}


baidu.api.geocoder <- function(address,city,key,output="json"){
  location = ""
  
  root <- "http://api.map.baidu.com/geocoder/v2/?"
  u <- paste0(root, "&address=", address, "&city=",city,"&output=",output,"&ak=", key)
  doc <- getURL(u)
  
  if(doc != "" & !is.null(doc)){
    x <- fromJSON(doc,simplify = FALSE)
    if(length(x$result) > 0){
      if(x$status==0){
        geo_lat <- x$result$location$lat
        geo_long <- x$result$location$lng
        
        location <- c(as.character(geo_lat),as.character(geo_long))
      }
    }
  }else{
    print("Google can not find anything according to your input!")
  }
  return(location)
}


baidu.api.nearbySearch <- function(location,city,key,type="写字楼$房地产$酒店",scope=2,radius=500,output="json",in_pty_name){
  #####Define output
  v_output <- rep("",11)
  attr(v_output, "Elements") <- c("PROPERTY_NAME", "ADDRESS", "DISTRICT", "CITY", "GEO_LAT", "GEO_LONG", "postal_code", "phone_number","place_id","gmap_url","website")
  
  root <- "http://api.map.baidu.com/place/v2/search?"
  u <- paste0(root, "query=", type, "&scope=",scope,"&location=",paste0(location[1],",",location[2]),"&radius=",radius,"&output=",output,"&ak=", key)
  doc <- getURL(URLencode(u))
  
  if(doc != "" & !is.null(doc)){
    x <- fromJSON(doc,simplify = FALSE)
    if(x$status==0 & length(x$results) > 0){
      results <- x$results
      n_rlt <- length(results)
      result <- results[[1]]
      
      
      rlt_pty_name <- rep("",n_rlt)
      str_dist_pty_name <- rep(1,n_rlt)
      dst_diff <- rep(1,n_rlt)
      for(i in 1:n_rlt){
        rlt_pty_name[i] <- results[[i]]$name
        #str_dist_pty_name[i] <- stringdist(rlt_pty_name[i],in_pty_name)/max(nchar(rlt_pty_name[i]),nchar(in_pty_name))
        str_dist_pty_name[i] <- stringsim(rlt_pty_name[i],in_pty_name,method = gv_method_strSim)
        
        r_lat <- results[[i]]$location$lat
        r_lat <- ifelse(is.null(r_lat), 180, r_lat)
        r_long <- results[[i]]$location$lng
        r_long <- ifelse(is.null(r_long), 180, r_long)
        
        point_dist <- distm(c(r_long, r_lat), c(as.numeric(location[2]), as.numeric(location[1])), fun = distHaversine)
        
        dst_diff[i] <- point_dist
      }
      
      rlt_num <- 1
      if(any(str_dist_pty_name >= gv_accept_sim - 0.25)){
        rlt_num <- which(str_dist_pty_name == max(str_dist_pty_name))[1]
      }else{
        rlt_num <- which(dst_diff == min(dst_diff))[1]
      }
      
      result <- results[[rlt_num]]
      
      place_id <- result$uid
      property_name <- ifelse(is.null(result$name), "", result$name) 
      address <- ifelse(is.null(result$address), "", result$address) 
      website <- ifelse(is.null(result$detail_info$detail_url), "", result$detail_info$detail_url)  
      phone_number <- ifelse(is.null(result$telephone), "", result$telephone) 
      geo_lat <- ifelse(is.null(result$location$lat), "", result$location$lat)
      geo_long <- ifelse(is.null(result$location$lng), "", result$location$lng)
      gmap_url <- ""
      district <- ""
      postal_code <- ""
      
      v_output[which(attr(v_output,"Elements") == "PROPERTY_NAME")] <- property_name
      v_output[which(attr(v_output,"Elements") == "GEO_LAT")] <- geo_lat
      v_output[which(attr(v_output,"Elements") == "GEO_LONG")] <- geo_long
      v_output[which(attr(v_output,"Elements") == "phone_number")] <- phone_number
      v_output[which(attr(v_output,"Elements") == "place_id")] <- place_id
      v_output[which(attr(v_output,"Elements") == "gmap_url")] <- gmap_url
      v_output[which(attr(v_output,"Elements") == "website")] <- website
      v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- address
      v_output[which(attr(v_output,"Elements") == "DISTRICT")] <- district
      v_output[which(attr(v_output,"Elements") == "CITY")] <- city
      v_output[which(attr(v_output,"Elements") == "postal_code")] <- postal_code
      
    }
  }else{
    print("Google can not find anything according to your input!")
  }
  return(v_output)
}



###############################################API Search###############################################
API_Search <- function(DS_PreAPI,baidu_key,google_key){
  if(!any(colnames(DS_PreAPI) == gv_geocore_fields)) {
    print("Please check the fileds of your input.")
    return()
  }
  
  DS_PostAPI <- cbind(DS_PreAPI[1,],postal_code="",phone_number="",map_url="",website="",Remark="")[0,]
  
  rownum <- nrow(DS_PreAPI)
  DS_PreAPI_Remarked <- uf_validityCheck_ALL(DS_PreAPI)
  
  for(i in 1:rownum){
    #print(as.character(DS_PreAPI[i,1]))
    print(DS_PreAPI$PROPERTY_NAME[i])
    #define best output
    v_best <- rep("",15)
    attr(v_best, "Elements") <- c("PROPERTY_NAME","PROPERTY_NAME_LC","ADDRESS_1","ADDRESS_LC","DISTRICT_NAME","DISTRICT_NAME_LC", 
                                  "CITY_NAME","CITY_NAME_LC", "GEO_LAT", "GEO_LONG","postal_code","phone_number","map_url","website","Remark")
    
    p_country <- "中国"
    p_country_en <- "China"
    p_city <- ifelse(DS_PreAPI$CITY_NAME_LC[i] == "", DS_PreAPI$CITY_NAME[i],DS_PreAPI$CITY_NAME_LC[i])
    p_district <- ifelse(DS_PreAPI$DISTRICT_NAME_LC[i] == "", DS_PreAPI$DISTRICT_NAME[i],DS_PreAPI$DISTRICT_NAME_LC[i])
    org_pty_cn <- DS_PreAPI$PROPERTY_NAME_LC[i]
    org_addr_cn <- DS_PreAPI$ADDRESS_LC[i]
    org_pty_en <- DS_PreAPI$PROPERTY_NAME[i]
    org_addr_en <- DS_PreAPI$ADDRESS_1[i]
    
    #Both Chinese Property & Address are Valid or Partial-Valid
    if(DS_PreAPI_Remarked$Remark_PROPERTY_NAME_LC[i] %in% c("Partial-Valid","Valid") & DS_PreAPI_Remarked$Remark_ADDRESS_LC[i] %in% c("Partial-Valid","Valid")){
      #search by property_name by google&baidu API
      if(1==1){
        #Baidu.DetailSearch(PtyName_CN)
        CN_Output_B1_Pty <- baidu.api.detailSearch(baidu.api.poiSearch(org_pty_cn,p_city,baidu_key,tag = "",page_size = 1),p_city,baidu_key)
        #Google.DetailSearch(PtyName_CN)
        CN_Output_G1_Pty <- google.api.detailSearch(google.api.textSearch(c(org_pty_cn,p_district,p_city,p_country),google_key,"zh-CN"), google_key, "zh-CN",DS_PreAPI$PROPERTY_NAME_LC[i]) 
        if(any(CN_Output_G1_Pty[5:6] == "")){
          CN_Output_G1_Pty <- google.api.detailSearch(google.api.textSearch(c(org_pty_cn,"",p_city,p_country),google_key,"zh-CN"), google_key, "zh-CN",DS_PreAPI$PROPERTY_NAME_LC[i]) 
        }
        
        #Result Selection
        CN_Output_B1_Pty[1] <- CN_Output_B1_Pty.PtyName <- uf_sdProcess_PN_CN(CN_Output_B1_Pty[1])
        CN_Output_B1_Pty[2] <- CN_Output_B1_Pty.Addr <- uf_sdProcess_ADDR_CN(CN_Output_B1_Pty[2])
        CN_Output_G1_Pty[1] <- CN_Output_G1_Pty.PtyName <- uf_sdProcess_PN_CN(CN_Output_G1_Pty[1])
        CN_Output_G1_Pty[2] <- CN_Output_G1_Pty.Addr <- uf_sdProcess_ADDR_CN(CN_Output_G1_Pty[2])
        
        CN_Output_B1_Pty.PtyName
        CN_Output_B1_Pty.Addr
        CN_Output_G1_Pty.PtyName
        CN_Output_G1_Pty.Addr
        
        #strDist.Pty.B1G1 <- stringdist(c(CN_Output_B1_Pty.PtyName,CN_Output_G1_Pty.PtyName),org_pty_cn)/
        #  c(max(nchar(CN_Output_B1_Pty.PtyName),nchar(org_pty_cn)),max(nchar(CN_Output_G1_Pty.PtyName),nchar(org_pty_cn)))
        strDist.Pty.B1G1 <- stringsim(c(CN_Output_B1_Pty.PtyName,CN_Output_G1_Pty.PtyName),org_pty_cn,method = gv_method_strSim)
        
        #strDist.Addr.B1G1 <- stringdist(c(CN_Output_B1_Pty.Addr,CN_Output_G1_Pty.Addr),org_addr_cn)/
        #  c(max(nchar(CN_Output_B1_Pty.Addr),nchar(org_addr_cn)),max(nchar(CN_Output_G1_Pty.Addr),nchar(org_addr_cn)))
        strDist.Addr.B1G1 <- stringsim(c(CN_Output_B1_Pty.Addr,CN_Output_G1_Pty.Addr),org_addr_cn,method = gv_method_strSim)
        
        
        if(strDist.Pty.B1G1[1] == 1 & strDist.Addr.B1G1[1] >= gv_accept_sim - 0.15){
          v_best[c(2,4,6,8,9:14)] <- CN_Output_B1_Pty[-9]
          v_best[15] <- "API output generated by BaiduAPI.DetailSearch(Property_CN)"
        }else if(strDist.Pty.B1G1[2] == 1 & strDist.Addr.B1G1[2] >= gv_accept_sim - 0.15){
          v_best[c(2,4,6,8,9:14)] <- CN_Output_G1_Pty[c(-9,-12)]
          v_best[15] <- "API output generated by GoogleAPI.DetailSearch(Property_CN)"
        }else if(any(strDist.Pty.B1G1 >= gv_accept_sim) & any(strDist.Addr.B1G1[which(strDist.Pty.B1G1 >= gv_accept_sim)] > gv_accept_sim - 0.15)){
          strDist.B1G1 <- strDist.Pty.B1G1 * 0.65 + strDist.Addr.B1G1 * 0.35
          if(any(strDist.B1G1 >= (0.65 * 0.5 + 0.35 * 0.35))){
            if(strDist.B1G1[1] >= strDist.B1G1[2]){
              v_best[c(2,4,6,8,9:14)] <- CN_Output_B1_Pty[-9]
              v_best[15] <- "API output generated by BaiduAPI.DetailSearch(Property_CN)"
            }else {
              v_best[c(2,4,6,8,9:14)] <- CN_Output_G1_Pty[c(-9,-12)]
              v_best[15] <- "API output generated by GoogleAPI.DetailSearch(Property_CN)"
            }
          }
        }
      }
      
      #if no Valid Result Returned, try google.reverse search and google/baidu nearby search
      if(v_best[15] == ""){
        #Google.ReverseGeoCode(AddName_CN)
        CN_Output_G2_Addr <- google.api.detailSearch(google.api.textSearch(c(org_addr_cn,p_district,p_city,p_country),google_key,"zh-CN"), google_key, "zh-CN",org_pty_cn) 
        if(CN_Output_G2_Addr[12] == "street_address.reverse.geocode"){
          v_best[c(2,4,6,8,9:14)] <- CN_Output_G2_Addr[c(-9,-12)]
          v_best[15] <- "API output generated by GoogleAPI.ReverseGeoCodeSearch(Address_CN)"
        }else{
          CN_Output_B2_Addr <- baidu.api.nearbySearch(baidu.api.geocoder(org_addr_cn,p_city,baidu_key),p_city,baidu_key,in_pty_name = org_pty_cn)
          CN_Output_B2_Addr[2] <- org_addr_cn
          
          #Result Selection
          CN_Output_B2_Addr[1] <- CN_Output_B2_Addr.PtyName <- uf_sdProcess_PN_CN(CN_Output_B2_Addr[1])
          CN_Output_G2_Addr[1] <- CN_Output_G2_Addr.PtyName <- uf_sdProcess_PN_CN(CN_Output_G2_Addr[1])
          CN_Output_B2_Addr[2] <- CN_Output_B2_Addr.Addr <- uf_sdProcess_ADDR_CN(CN_Output_B2_Addr[2])
          
          CN_Output_B2_Addr.PtyName
          CN_Output_G2_Addr.PtyName
          CN_Output_B2_Addr.Addr
          #strDist.Pty.B2G2 <- stringdist(c(CN_Output_B2_Addr.PtyName,CN_Output_G2_Addr.PtyName),org_pty_cn)/
          #  c(max(nchar(CN_Output_B2_Addr.PtyName),nchar(org_pty_cn)),max(nchar(CN_Output_G2_Addr.PtyName),nchar(org_pty_cn)))
          strDist.Pty.B2G2 <- stringsim(c(CN_Output_B2_Addr.PtyName,CN_Output_G2_Addr.PtyName),org_pty_cn,method = gv_method_strSim)
          
          #strDist.Addr.B2G2 <- stringdist(CN_Output_B2_Addr.Addr,org_addr_cn)/ max(nchar(CN_Output_B2_Addr.Addr),nchar(org_addr_cn))
          strDist.Addr.B2G2 <- stringsim(CN_Output_B2_Addr.Addr,org_addr_cn,method = gv_method_strSim)
          
          rlt_num <- which(strDist.Pty.B2G2 == max(strDist.Pty.B2G2))[1]
          if(strDist.Addr.B2G2 >= (gv_accept_sim + 0.15) & any(strDist.Pty.B2G2 >= gv_accept_sim - 0.25)){
            if(rlt_num == 1){
              v_best[c(2,4,6,8,9:14)] <- CN_Output_B2_Addr[-9]
              v_best[15] <- "API output generated by BaiduAPI.NearbySearch(Address_CN)"
            }else{
              v_best[c(2,4,6,8,9:14)] <- CN_Output_G2_Addr[c(-9,-12)]
              v_best[15] <- "API output generated by GoogleAPI.NearbySearch(Address_CN)"
            }
          }
        }
      }
      
    }
    
    #only Chinese Property is Valid
    if(DS_PreAPI_Remarked$Remark_PROPERTY_NAME_LC[i] %in% c("Valid") & v_best[15] == ""){
      #Baidu.DetailSearch(PtyName_CN)
      CN_Output_B1_1_Pty <- baidu.api.detailSearch(baidu.api.poiSearch(org_pty_cn,p_city,baidu_key,tag = "",page_size = 1),p_city,baidu_key)
      if(stringsim(CN_Output_B1_1_Pty[1],org_pty_cn) > 0.6 & uf_validityCheck_ADDR_CN(uf_sdProcess_ADDR_CN(CN_Output_B1_1_Pty[2])) %in% c("Partial-Valid","Valid")){
        v_best[c(2,4,6,8,9:14)] <- CN_Output_B1_1_Pty[-9]
        v_best[15] <- "API output generated by BaiduAPI.DetailSearch(Property_CN)"
      }else{
        CN_Output_G1_1_Pty <- google.api.detailSearch(google.api.textSearch(c(org_pty_cn,p_district,p_city,p_country),google_key,"zh-CN"), google_key, "zh-CN",org_pty_cn) 
        if(any(CN_Output_G1_1_Pty[5:6] == "")){
          CN_Output_G1_1_Pty <- google.api.detailSearch(google.api.textSearch(c(org_pty_cn,"",p_city,p_country),google_key,"zh-CN"), google_key, "zh-CN",org_pty_cn) 
        }
        if(stringsim(CN_Output_G1_1_Pty[1],org_pty_cn) > 0.6 & uf_validityCheck_ADDR_CN(uf_sdProcess_ADDR_CN(CN_Output_G1_1_Pty[2])) %in% c("Partial-Valid","Valid")){
          v_best[c(2,4,6,8,9:14)] <- CN_Output_G1_1_Pty[c(-9,-12)]
          v_best[15] <- "API output generated by GoogleAPI.DetailSearch(Property_CN)"
      }
      }
    }
    
    #Both English Property & Address are Valid and haven't get valid Chinese output yet 
    if(DS_PreAPI_Remarked$Remark_PROPERTY_NAME[i] %in% c("Partial-Valid","Valid") & DS_PreAPI_Remarked$Remark_ADDRESS_1[i] %in% c("Partial-Valid","Valid") & v_best[15] == ""){
      CN_Output_G4_Pty <- google.api.detailSearch(google.api.textSearch(c(org_pty_en,"",p_city,p_country_en),google_key,"zh-CN"), google_key, "zh-CN",org_pty_cn) 
      
      #if google API get Chinese Property Name, but no valid address returned,then try to use baidu to get address
      if(uf_validityCheck_PN_CN(uf_sdProcess_PN_CN(CN_Output_G4_Pty[1])) %in% (c("Valid","Patial-Valid")) & 
         uf_validityCheck_ADDR_CN(uf_sdProcess_ADDR_CN(CN_Output_G4_Pty[2])) %in% (c("NULL","Invalid"))){
        CN_Output_BG4_Pty <- baidu.api.detailSearch(baidu.api.poiSearch(CN_Output_G4_Pty[1],CN_Output_G4_Pty[4],baidu_key,tag = "",page_size = 1),CN_Output_G4_Pty[4],baidu_key)
        if(stringsim(CN_Output_G4_Pty[1],CN_Output_BG4_Pty[1]) > gv_accept_sim &
           uf_validityCheck_ADDR_CN(uf_sdProcess_ADDR_CN(CN_Output_BG4_Pty[2])) %in% (c("Valid","Partial-Valid")) ){
          CN_Output_G4_Pty[2] <- CN_Output_BG4_Pty[2]
        }
      }
      
      CN_Output_G5_Addr <- google.api.detailSearch(google.api.textSearch(c(org_addr_en,"",p_city,p_country_en),google_key,"zh-CN"),google_key,"zh-CN",org_pty_cn) 
      
      CN_Output_G4_Pty[c(1:6,12)]
      CN_Output_G5_Addr[c(1:6,12)]
      
      CN_Output_G4_Pty.Lat <- as.numeric(ifelse(CN_Output_G4_Pty[5] == "", "0.0",CN_Output_G4_Pty[5])) 
      CN_Output_G4_Pty.Long <- as.numeric(ifelse(CN_Output_G4_Pty[6] == "", "0.0",CN_Output_G4_Pty[6])) 
      CN_Output_G5_Addr.Lat <- as.numeric(ifelse(CN_Output_G5_Addr[5] == "", "0.0",CN_Output_G5_Addr[5]))
      CN_Output_G5_Addr.Long <- as.numeric(ifelse(CN_Output_G5_Addr[6] == "", "0.0",CN_Output_G5_Addr[6]))
      
      max_point_dist <- 2000
      point_dist <- max_point_dist + 1
      if(any(c(CN_Output_G5_Addr.Lat,CN_Output_G5_Addr.Long)==0)){
        point_dist <- 0
      }else{
        if(any(c(CN_Output_G4_Pty.Lat,CN_Output_G4_Pty.Long)==0)){
          point_dist <- max_point_dist + 1
        }else{
          point_dist <- distm(c(CN_Output_G4_Pty.Long, CN_Output_G4_Pty.Lat), c(CN_Output_G5_Addr.Long, CN_Output_G5_Addr.Lat), fun = distHaversine)[1,1]
        }
      }
      
      if(point_dist <= max_point_dist){
        v_best[c(2,4,6,8,9:14)] <- CN_Output_G4_Pty[c(-9,-12)]
        v_best[15] <- "API output generated by GoogleAPI.DetailSearch(Property_EN)"
      }else{
        v_best[c(2,4,6,8,9:14)] <- CN_Output_G5_Addr[c(-9,-12)]
        v_best[15] <- ifelse(CN_Output_G5_Addr[12] != "street_address.nearbysearch",
                             "API output generated by GoogleAPI.ReverseGeoCodeSearch(Address_EN)",
                             "API output generated by GoogleAPI.NearbySearch(Address_EN)") 
      }
      
      if(2==1){
        if(uf_validityCheck_ADDR_CN(uf_sdProcess_ADDR_CN(CN_Output_G5_Addr[2])) == "Valid" &  point_dist > max_point_dist){
          v_best[c(2,4,6,8,9:14)] <- CN_Output_G5_Addr[c(-9,-12)]
          v_best[15] <- ifelse(CN_Output_G5_Addr[12] != "street_address.nearbysearch",
                               "API output generated by GoogleAPI.ReverseGeoCodeSearch(Address_EN)",
                               "API output generated by GoogleAPI.NearbySearch(Address_EN)") 
        }else if(uf_validityCheck_ADDR_CN(uf_sdProcess_ADDR_CN(CN_Output_G4_Pty[2])) %in% (c("Valid","Partial-Valid"))){
          v_best[c(2,4,6,8,9:14)] <- CN_Output_G4_Pty[c(-9,-12)]
          v_best[15] <- "API output generated by GoogleAPI.DetailSearch(Property_EN)"
        }else{
          v_best[c(2,4,6,8,9:14)] <- CN_Output_G5_Addr[c(-9,-12)]
          v_best[15] <- ifelse(CN_Output_G5_Addr[12] != "street_address.nearbysearch",
                               "API output generated by GoogleAPI.ReverseGeoCodeSearch(Address_EN)",
                               "API output generated by GoogleAPI.NearbySearch(Address_EN)") 
        }
      }
      
      
    } 
    
    if(DS_PreAPI_Remarked$Remark_PROPERTY_NAME[i] %in% c("Partial-Valid","Valid") & DS_PreAPI_Remarked$Remark_ADDRESS_LC[i] %in% c("Partial-Valid","Valid") & v_best[15] == ""){
      CN_Output_G99_Pty <- google.api.detailSearch(google.api.textSearch(c(org_pty_en,"",p_city,p_country_en),google_key,"zh-CN"), google_key, "zh-CN",org_pty_cn) 
      
      #if google API get Chinese Property Name, but no valid address returned,then try to use baidu to get address
      if(uf_validityCheck_PN_CN(uf_sdProcess_PN_CN(CN_Output_G99_Pty[1])) %in% (c("Valid","Patial-Valid")) & 
         uf_validityCheck_ADDR_CN(uf_sdProcess_ADDR_CN(CN_Output_G99_Pty[2])) %in% (c("NULL","Invalid"))){
        CN_Output_BG99_Pty <- baidu.api.detailSearch(baidu.api.poiSearch(CN_Output_G99_Pty[1],CN_Output_G99_Pty[4],baidu_key,tag = "",page_size = 1),CN_Output_G99_Pty[4],baidu_key)
        if(stringsim(CN_Output_G99_Pty[1],CN_Output_BG99_Pty[1]) > gv_accept_sim &
           uf_validityCheck_ADDR_CN(uf_sdProcess_ADDR_CN(CN_Output_BG99_Pty[2])) %in% (c("Valid","Partial-Valid")) ){
          CN_Output_G99_Pty[2] <- CN_Output_BG99_Pty[2]
        }
      }
      
      v_best[c(2,4,6,8,9:14)] <- CN_Output_G99_Pty[c(-9,-12)]
      v_best[15] <- "API output generated by GoogleAPI.DetailSearch(Property_EN)"
    }
    if(DS_PreAPI_Remarked$Remark_PROPERTY_NAME_LC[i] %in% c("Partial-Valid","Valid") & DS_PreAPI_Remarked$Remark_ADDRESS_1[i] %in% c("Partial-Valid","Valid") & v_best[15] == ""){
      CN_Output_B99_Pty <- baidu.api.detailSearch(baidu.api.poiSearch(org_pty_cn,p_city,baidu_key,tag = "",page_size = 1),p_city,baidu_key)
      v_best[c(2,4,6,8,9:14)] <- CN_Output_B99_Pty[-9]
      v_best[15] <- "API output generated by BaiduAPI.DetailSearch(Property_CN)"
    }
    
    #Get English output
    v_15 <- c("Only English Property Name and Chinese Address are Valid; No API Action.","Only Chinese Property Name and English Address are Valid; No API Action.","")
    if(!any(v_15 == v_best[15])){
      p_city_en <- ifelse(v_best[8] == "", p_city, v_best[8])
      
      EN_Output_G7_Pty <- google.api.detailSearch(google.api.textSearch(c(v_best[2],"",p_city_en,p_country_en),google_key,"en"), google_key, "en",org_pty_en)
      EN_Output_G8_Addr <- google.api.detailSearch(google.api.textSearch(c(v_best[4],"",p_city_en,p_country_en),google_key,"en"), google_key, "en",org_addr_en) 
      
      EN_Output_G7_Pty[c(1:6,12)]
      EN_Output_G8_Addr[c(1:6,12)]
      
      v_best[1] <- ifelse(EN_Output_G7_Pty[1] != "", EN_Output_G7_Pty[1], EN_Output_G8_Addr[1])
      v_best[3] <- ifelse(uf_validityCheck_ADDR(uf_sdProcess_ADDR(EN_Output_G7_Pty[2])) %in% (c("Valid","Partial-Valid")), EN_Output_G7_Pty[2], 
                          ifelse(uf_validityCheck_ADDR(uf_sdProcess_ADDR(EN_Output_G8_Addr[2])) %in% (c("Valid","Partial-Valid")),EN_Output_G8_Addr[2], ""))
      
      unq_district <- unique(gv_china_division_list[,1:2])
      op_district_en <- unq_district$DISTRICT[which(unq_district$DISTRICT_LC == v_best[6])[1]] 
      v_best[5] <- ifelse(is.na(op_district_en), EN_Output_G8_Addr[3],op_district_en)
      unq_city <- unique(gv_china_division_list[,3:4])
      op_city_en <- unq_city$CITY[which(unq_city$CITY_LC == v_best[8])[1]] 
      v_best[7] <- ifelse(is.na(op_city_en), EN_Output_G8_Addr[4],op_city_en)
      
      #translate Chinese Property_Name to Pinyin
      rate_hanzi <- nchar(v_best[1], type = "bytes") / nchar(v_best[1])
      rate_hanzi <- ifelse(is.na(rate_hanzi),0,rate_hanzi)
      if(rate_hanzi > 1.6){
        v_best[1] <- uf_Translate_Hz_Py(v_best[1],TRUE)
      }
      
    }
    
    if(v_best[15] == "API output generated by GoogleAPI.NearbySearch(Address_EN)"){
      if(stringsim(v_best[3],org_addr_en) < 0.3){
        v_best[2] <- ""
        v_best[3] <- EN_Output_G8_Addr[2]
        v_best[1] <- ifelse(EN_Output_G8_Addr[12] == "API output generated by GoogleAPI.ReverseGeoCodeSearch(Address_EN)",EN_Output_G8_Addr[1],"")  
      }
    }
    if(all(c(v_best[1:4]) == "")){
      v_best[15] <- "API is not able to find a result."
      v_best[1:14] <- rep("",14)
    }
    
    DS_PostAPI[i,1] <- DS_PreAPI$SOURCE_PROPERTY_ID[i]
    DS_PostAPI[i,2] <- v_best[1]
    DS_PostAPI[i,3] <- v_best[2]
    DS_PostAPI[i,4] <- v_best[3]
    DS_PostAPI[i,5] <- v_best[4]
    DS_PostAPI[i,6] <- v_best[5]
    DS_PostAPI[i,7] <- v_best[6]
    DS_PostAPI[i,8] <- v_best[7]
    DS_PostAPI[i,9] <- v_best[8]
    op_city <- ifelse(v_best[8] == "", v_best[7], v_best[8])
    DS_PostAPI[i,10] <- gv_china_division_list[which(gv_china_division_list$CITY == op_city | gv_china_division_list$CITY_LC == op_city)[1],5]
    DS_PostAPI[i,11] <- gv_china_division_list[which(gv_china_division_list$CITY == op_city | gv_china_division_list$CITY_LC == op_city)[1],6]
    DS_PostAPI[i,12] <- "China"
    DS_PostAPI[i,13] <- "中国"
    DS_PostAPI[i,14] <- "UTC/GMT+08:00"
    DS_PostAPI[i,15] <- v_best[9]
    DS_PostAPI[i,16] <- v_best[10]
    DS_PostAPI[i,17] <- v_best[11]
    DS_PostAPI[i,18] <- v_best[12]
    DS_PostAPI[i,19] <- v_best[13]
    DS_PostAPI[i,20] <- v_best[14]
    DS_PostAPI[i,21] <- v_best[15]
  }
  
  return(cbind(uf_sdProcess_ALL(DS_PostAPI[,1:16]),DS_PostAPI[17:21]))
}



########################################################Programm Process Start########################################################
#########1. Load Source Raw Data into ODS Staging Area#########
ODS.Staging.MarketLink <- uf_xlsxDataImport('AndyData_1.xlsx','ODS.Staging.MarketLink')


#########Select Geo Core columns for processing
ODS.Staging.MarketLink <- select(ODS.Staging.MarketLink,
                                        SOURCE_PROPERTY_ID,PROPERTY_NAME,PROPERTY_NAME_LC,ADDRESS_1,ADDRESS_LC,DISTRICT_NAME,DISTRICT_NAME_LC,
                                        CITY_NAME,CITY_NAME_LC,PROVINCE_NAME,PROVINCE_NAME_LC,COUNTRY_NAME,COUNTRY_NAME_LC,TIMEZONE,GEO_LAT,GEO_LONG)

ODS.Staging.MarketLink <- ODS.Staging.MarketLink[1:700,]


#########2. Execute Standardization Rules on Source Raw Data#########
ODS.Standardized.MarketLink <- uf_sdProcess_ALL(ODS.Staging.MarketLink)
#uf_xlsxDataExport(ODS.Standardized.MarketLink, "Test.xlsx","Test")


#########3. Execute Validity Check on Standardized Source Data#########
ODS.Remarked.MarketLink <- uf_validityCheck_ALL(ODS.Standardized.MarketLink)
#uf_xlsxDataExport(ODS.Remarked.MarketLink, "Test.xlsx","Test")


  


#########4. Execute Pre-API Rejection Rule on Standardized Source Data#########
ODS.Temp.MarketLink <- uf_rejectProcess_preAPI(ODS.Remarked.MarketLink)
ODS.PreRej.MarketLink <- filter(ODS.Temp.MarketLink, Reject_Mark == "Rejected")
ODS.PreAPI.MarketLink <- filter(ODS.Temp.MarketLink, Reject_Mark != "Rejected")[,gv_geocore_fields]
#uf_xlsxDataExport(ODS.PreAPI.MarketLink, "Test.xlsx","Test")



#########5. Execute API Process on Standardized Source Data#########
DS_PreAPI <- ODS.PreAPI.MarketLink[401:669,]
key_Google = keys[19:27]
key_Baidu = "NdgFqhKyiqPksC2pLfsGsWps15wzRsES"

ODS.PostAPI.MarketLink <- DS_PreAPI[0,]

for(i in 1:length(key_Google)){
  sn <- 401+30*(i-19)
  en <- 400+30*(i-18)
  ODS.PostAPI.Temp <- API_Search(DS_PreAPI[sn:en,],key_Google[i],key_Baidu)
  ODS.PostAPI.MarketLink <- rbind(ODS.PostAPI.MarketLink, ODS.PostAPI.Temp)
}
#uf_xlsxDataExport(ODS.PostAPI.MarketLink, "Test.xlsx","Test")
#View(ODS.PreAPI.MarketLink)






########################################################Programm Process End########################################################

























































