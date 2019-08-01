
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

  

