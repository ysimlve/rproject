#############################################R0     -- Start#############################################
getwd()
options(stringsAsFactors = FALSE)
options(digits = 5)
#rm(list=ls())
#http://www.manning.com/RinAction
#Quick list of useful R packages
#https://support.rstudio.com/hc/en-us/articles/201057987-Quick-list-of-useful-R-packages

#############################################R1     -- 基础介绍#############################################
#CRAN: Comprehensive R Archive Network - is a network of ftp and web servers around the world that store identical, up-to-date, versions of code and 
#########################################documentation for R.
#############################################R1.1   -- 升级R版本#############################################
version

library(installr)

updateR()


#############################################R1.2   -- 会话和工作空间#############################################
#R session
#workspace(工作空间) - 一个workspace对应一个R session, 是当前R的工作环境，它储存着所有用户定义的对象（向量、矩阵、函数、数据框、列表）。
#                      在一个R会话结束时，你可以将当前工作空间保存到一个镜像中(格式后缀.RData)，并在下次启动R时自动载入它。
#工作目录（working directory）是R用来读取文件和保存结果的默认目录。
options("encoding" = "utf-8")
options("encoding")
#工作空间管理相关的命令
getwd()
setwd("")
ls()                      #列出当前工作空间中的对象
rm("tryCatch.W.E", "x")   #移除（删除）一个或多个对象
options("stringsAsFactors" = FALSE)       #显示或设置当前选项
options("stringsAsFactors")
history(2)                #显示最近使用过的#个命令（默认值为25）
savehistory("myfile")     #保存命令历史到文件myfile中（默认值为.Rhistory）
loadhistory("myfile")     #载入一个命令历史文件（默认值为.Rhistory）
save.image("myfile")      #保存工作空间到文件myfile中（默认值为.RData）
load("myfile")            #读取一个工作空间到当前会话中（默认值为.RData）
save("objectlist", file = "myfile")       #保存指定对象到一个文件中
q()                       #退出R。将会询问你是否保存工作空间



#############################################R1.3   -- 输入输出#############################################
sink("file.txt",append = TRUE, split = TRUE)
pdf("file.pdf")
source("script1.R")
###执行以上的3条语句，script1 脚本被执行，执行结果将显示在屏幕上，同时执行结果的文本信息将输出到file.txt中,图形信息将输出到file.pdf中.

sink()
dev.off()
source("script2.R")
###执行以上的3条语句，script2 脚本被执行，执行结果将显示在屏幕上. 输出接口被关闭.


#############################################R1.4   -- 包#############################################
#包是R函数、数据、预编译代码以一种定义完善的格式组成的集合。
library()                   #显示库中有哪些包
.libPaths()                 #显示库所在的路径
search()                    #显示哪些包已加载并可使用

install.packages("ggplot2")           #安装包
update.packages("ggplot2")            #更新包
installed.packages("ggplot2")         #显示版本号、依赖关系等信息
library(ggplot2)                      #载入一个包到R session中
help(package = "ggplot2")


#############################################R1.5   -- 批处理#############################################
#通过操作系统命令行来执行指定的R文件代码
#Linux上:
#R CMD BATCH [options] [infile] [outfile]

#windows上:
#"C:\YuanLe\R\RforWin\R\R-3.3.1\bin" CMD BATCH --vanilla --slave [Rfile]



#############################################R1.6   --语法基础#############################################
#############################################R1.6.1 -- 算术运算符#############################################
#+ 加
#- 减
#* 乘
#/ 除
#^或** 求幂
#x%%y 求余（x mod y）。5%%2的结果为1
#x%/%y 整数除法。5%/%2的结果为2

#############################################R1.6.2 -- 逻辑运算符#############################################
#< 小于
#<= 小于或等于
#> 大于
#>= 大于或等于
#== 严格等于*
#  != 不等于
#!x 非x
#x | y x或y
#x & y X和y
#isTRUE(x) 测试x是否为TRUE




#############################################R1.6.3 -- 控制流与自定义函数#############################################
#############################################R2     -- 数据结构(对象类型)简介#############################################
#R中用于存储数据的对象类型，包括标量、向量、矩阵、数组、数据框和列表,以及一个特殊的类型因子(Factor)。

#数据框的行称为观测，列称为变量
#变量可归结为名义型、有序型或连续型变量。
#1. 名义型变量是没有顺序之分的类别变量。如公司类型变量
#2. 有序型变量表示一种顺序关系，而非数量关系。 如公司规模(观测值可能是1,2,3,4)
#3. 连续型变量可以呈现为某个范围内的任意值，并同时表示了顺序和数量。 如公司年营业额


#############################################R2.1   -- 矩阵#############################################
mymatrix_byrow <- matrix(c(1:20),nrow = 4, ncol = 5, byrow = TRUE, dimnames = list(c("A","B","C","D"),c("C1","C2","C3","C4","C5")))
mymatrix_bycolumn <- matrix(c(1:20),nrow = 4, ncol = 5, byrow = FALSE, dimnames = list(c("A","B","C","D"),c("C1","C2","C3","C4","C5")))

mymatrix_byrow[1,c(2,4)]


#############################################R2.2   -- 数据框#############################################
#注意attach(),detach()和with()的用法


#############################################R2.3   -- 因子#############################################
#名义型变量和有序型变量在R中称为因子
#因子在R中非常重要，因为它决定了数据的分析方式以及如何进行视觉呈现。
#函数factor()以一个整数向量的形式存储类别值，整数的取值范围是[1... k ]（其中k 是名义型变量中唯一值的个数），同时一个由字符串（原始值）组成的内部向量将映射到这些整数上。
#针对因子向量进行的任何分析都会将其作为名义型变量对待，并自动选择适合这一测量尺度①的统计方法。
status <- c("Bad","Good","Excellent","Bad")
status_1 <- factor(status)
status_2 <- factor(status,ordered = TRUE)  #表示status_2为序型变量,因子的水平默认依字母顺序创建。
status_3 <- factor(status,ordered = TRUE,
                   levels = c("Excellent","Good","Bad"))


#############################################R2.4   -- 列表#############################################
#列表（list）是一些对象（或成分，component）的有序集合,它可能是若干向量、矩阵、数据框，甚至其他列表的组合

#############################################R2.5   -- 数据对象处理函数#############################################
#length(object) 显示对象中元素/成分的数量
#dim(object) 显示某个对象的维度
#str(object) 显示某个对象的结构
#class(object) 显示某个对象的类或类型
#mode(object) 显示某个对象的模式
#names(object) 显示某对象中各成分的名称
#c(object, object,…) 将对象合并入一个向量
#cbind(object, object, …) 按列合并对象
#rbind(object, object, …) 按行合并对象
#Object 输出某个对象
#head(object) 列出某个对象的开始部分
#tail(object) 列出某个对象的最后部分
#ls() 显示当前的对象列表
#rm(object, object, …) 删除一个或更多个对象。语句rm(list = ls())将删除当前工作环境中的几乎所有对象*
#newobject <- edit(object) 编辑对象并另存为newobject
#fix(object) 直接编辑对象




#############################################R3     -- 数据输入(收集) in R#############################################
#R Data Import/Export Manul
#http://cran.r-project.org/doc/manuals/R-data.pdf

#############################################input from keyboard
mydata <- data.frame(age = numeric(0),gender = character(0),weight = numeric(0))  #类似于age=numeric(0)的赋值语句将创建一个指定模式但不含实际数据的变量。
mydata <- edit(mydata) #equal to fix(mydata)
fix(mydata)

#############################################从带分隔符的文本文件导入数据
mydata1 <- read.table(file = "dataimport_csv.csv", header = TRUE, sep = "\t",
                      colClasses = c("character","numeric","factor"),
                      stringsAsFactors = FALSE,
                      encoding = "utf-8")
summary(mydata1)
class(mydata1$Gender)
#sep - delimiter
#\t - 以制表符分隔

#############################################从Excel读取数据或导出数据到Excel
library(readxl) #readxl::read_excel()
library(xlsx)   #xlsx::write.xlsx()


#############################################访问关系数据库
install.packages("RODBC")
library(RODBC)

myconn <- odbcConnect("Marketlink_Prod",uid = "EDWUser_RO", pwd = "EdW@$iAr0")
df_Province <- sqlFetch(myconn,"EDWStaging.ProvinceName")
df_StateProvinceLongName <- sqlQuery(myconn,"select [StateProvinceLongName] from [EDWStaging].[ProvinceName]")
class(df_StateProvinceLongName)
close.connection(myconn)
close(myconn)

names(df_Province)
colnames(df_Province)

#############################################从网页抓取数据
#package - rvest

#############################################R4     -- 数据处理#############################################
#useful data manipulation packages: https://www.analyticsvidhya.com/blog/2015/12/faster-data-manipulation-7-packages/
#1. dplyr - A fast, consistent tool for working with data frame like objects, both in memory and out of memory
###filter – It filters the data based on a condition
###select – It is used to select columns of interest from a data set
###arrange – It is used to arrange data set values on ascending or descending order
###mutate – It is used to create new variables from existing variables
###summarise (with group_by) – It is used to perform analysis by commonly used operations such as min, max, mean count etc
#2. data.table
###this package help to perform faster manipulation in a data set.
###Using data.table helps in reducing computing time as compared to data.frame. 
#3. reshape2
###this package is useful in reshaping data
#4. tidyr 
###This package can make your data look ‘tidy’. 
###gather() – it ‘gathers’ multiple columns. Then, it converts them into key:value pairs. This function will transform wide from of data to long form. You can use it as in alternative to ‘melt’ in reshape package.
###spread() – It does reverse of gather. It takes a key:value pair and converts it into separate columns.
###separate() – It splits a column into multiple columns.
###unite() – It does reverse of separate. It unites multiple columns into single column
#5. Lubridate 
###Lubridate package reduces the pain of working of data time variable in R. 
#6. stringr
###Easy to learn tools for regular expressions and character strings.
#7. sqldf
###for selecting from data frames using SQL

#############################################R4.1   -- 基础数学函数###############################################
#abs(x)                           绝对值
#sqrt(x)                          平方根sqrt(25)返回值为5和25^(0.5)等价, 有的数学函数可以用算术运算符替换得到相同的结果
#ceiling(x)                       不小于x的最小整数 - ceiling(3.475)返回值为4
#floor(x)                         不大于x的最大整数 - floor(3.475)返回值为3
#trunc(x)                         向 0 的方向截取的x中的整数部分 - trunc(5.99)返回值为5
#round(x, digits=n)               将x舍入为指定位的小数 - round(3.475, digits=2)返回值为3.48
#signif(x, digits=n)              将x舍入为指定的有效数字位数 - signif(3.475, digits=2)返回值为3.5
#cos(x)、sin(x) 、tan(x)          余弦、正弦和正切 - cos(2)返回值为–0.416
#acos(x) 、asin(x) 、atan(x)      反余弦、反正弦和反正切 - acos(-0.416)返回值为2
#cosh(x) 、sinh(x) 、tanh(x)      双曲余弦、双曲正弦和双曲正切 sinh(2)返回值为3.627
#acosh(x) 、asinh(x) 、atanh(x)   反双曲余弦、反双曲正弦和反双曲正切 - asinh(3.627)返回值为2
#log(x,base=n)                    对x取以n为底的对数
#log(x)                           自然对数 - log(10)返回值为2.3026   e = 2.718282….
#log10(x)                         常用对数
#exp(x)                           指数函数 - exp(2.3026)即((1+1/100000)^100000)^2.3026返回值为10 

#神奇的无理数e, 当对存在明显偏倚的变量进行分析前，对这个变量取对数操作是很常见的手段。
#e = (1 + 1/100000000) ^ 100000000 = 2.718282
log(c(10,100,1000,10000,100000,1000000,10000000))

#############################################R4.2   -- 基础统计函数###############################################
#mean(x)              平均数 - mean(c(1,2,3,4))返回值为2.5
#median(x)            中位数 - median(c(1,2,3,4))返回值为2.5
#sd(x)                标准差 - sd(c(1,2,3,4))返回值为1.29
#var(x)               方差 - var(c(1,2,3,4))返回值为1.67
#mad(x)               绝对中位差（median absolute deviation）- x <- c(2, 3, 8, 7, 9, 6, 4) median(x) median(abs(x - median(x))) mad(x,constant = 1)
#quantile(x,probs)    求分位数。其中x为待求分位数的数值型向量，probs为一个由[0,1]之间的概率值组成的数值向量
                     #求x的30%和84%分位点 quantile(c(1:100,1:10)) quantile(c(1:100,1:10), c(.25,.50)) 
#range(x)             求值域 - x <- c(1,2,3,4) range(x)返回值为c(1,4)  -  diff(range(x))返回值为3
#sum(x) 求和          sum(c(1,2,3,4))返回值为10
#diff(x, lag=n)       滞后差分，lag用以指定滞后几项。默认的lag值为1  x<- c(1, 5, 23, 29) diff(x)返回值为c(4, 18, 6)
#min(x)               求最小值
#min(c(1,2,3,4))      返回值为1
#max(x)               求最大值
#max(c(1,2,3,4))      返回值为4
#scale(x,center=TRUE, scale=TRUE)   为数据对象x按列进行中心化（center=TRUE）或标准化（center=TRUE,scale=TRUE）；

#scale sample
#默认情况下，函数scale()对矩阵或数据框的指定列进行均值为0、标准差为1的标准化/规范化
x <- c(1,200,30,20,99)
scale(x,center = TRUE,scale = FALSE)
scale(x, center = TRUE, scale = TRUE)
#要对每一列进行任意均值和标准差的标准化，可以使用如下的代码
sd <- 2
m <- 3
scale(x, center = TRUE, scale = TRUE) * sd + m



#############################################R4.3   -- 基础概率函数###############################################
#在R语言中，概率函数形式如： [dpqr]distribution_abbreviation, 各种分布函数见RAction P112
#d - density function - 密度函数
#p - distribution function - 分布函数
#q - quantile function - 分位数函数
#r - random function - 生成随机数(随机偏差)

#############################################R4.3.1 -- 正太分布###############################################
set.seed(100)                             #显示地设置种子，以保证每次随机生成的值一样
x <- rnorm(100,mean = 0, sd = 1)          #随机生成100个服从标准正太分布的随机变量
y <- dnorm(x,mean = 0, sd = 1)            #100个随机变量对应的概率密度值f(x)
pnorm(0,mean = 0, sd = 1)                 #服从标准正太分布的随机变量X(X < 0)的概率值F(0) = P(X < 0)（分布函数）
pnorm(1,mean = 0, sd = 1) - pnorm(0,mean = 0, sd = 1)     #服从标准正太分布的随机变量X (0 < X < 1) 的概率值F(1) - F(0) = P(0<X<1)（分布函数）

#c(min(x),max(x))
plot(pretty(c(-5,5),n = 30),dnorm(pretty(c(-5,5),n = 30)),
     type = "l",
     xlab = "NormalDeviate",
     ylab = "Density",
     yaxs = "i",
     xaxs = "i")

#############################################R4.3.2 -- 均匀分布###############################################
set.seed(101)
#set.seed()用于设定随机数种子，一个特定的种子可以产生一个特定的伪随机序列，这个函数的主要目的，是让你的模拟能够可重复出现，因为很多时候我们需要取随机数，
#但这段代码再跑一次的时候，结果就不一样了，如果需要重复出现同样的模拟结果的话，就可以用set.seed()。
x1 <- runif(100, min = 0, max = 1)
y1 <- dunif(x1,min = 0, max = 1)               #均匀分布的密度函数为: f(x) = 1/(max-min)
punif(0.5,min = 0, max = 1)              #F(0.5) = P(X < 0.5) = 0.5
punif(0.6,min = 0, max = 1)

plot(x1,y1,
     type = "l",
     xlab = "X",
     ylab = "Density",
     yaxs = "i",
     xaxs = "i")










#############################################R4.4   -- 字符处理函数###############################################
#常用的字符处理函数有:
nchar("abc",type = "chars")
substr("abc",1,2)
regexpr("[a-b]","123b5")
gsub("[a-b]","9","123b5")
unlist(strsplit("a.b.c",".",fixed = T))
paste("a","b","c",sep = ".")
toupper("abc")
tolower("ABC")

#############################################R4.5   -- 其他有用的函数###############################################
length(c(1,2,3))
seq(1,10,by = 2)
rep(1,20)
cut(runif(100,min = 1, max = 100),breaks = c(1,20,40,60,80,100),n = 6)
pretty(rnorm(100,mean = 0, sd = 1),20)
cat("Hello","Andy,", "\n", "\b","How are you?","\t","\"See\" 'you'!")
?Quotes


#############################################R4.6   -- apply###############################################
#apply: 将函数FUN应用到数据框，矩阵或数组的任何维度上
mymax <- matrix(rnorm(12),nrow = 3,byrow = T)
apply(mymax,MARGIN = 1, FUN = sum, na.rm = T)
apply(mymax,MARGIN = 2, FUN = sum, na.rm = T)
apply(mymax,MARGIN = 1, FUN = mean, trim = 0.1)  #FUN 可以是用户自定义函数

#############################################R4.7   -- 一个例子###############################################
Student <- c("Andy Yuan","Ling Liang","Chris Huang","Lillian Li","Tom Xie","Terry Tang","Tim Wang")
Maths <- sample(c(400:600),size = 7)
English <- sample(c(60:100), size = 7)
Chinese <- sample(c(1:20),size = 7)
roster <- data.frame(Student,Maths,English,Chinese)

z <- scale(roster[,c(2:4)])                     #标准化各科成绩，使每科成绩用单位标准来表示。也可以纵向比较。

avg_score <- apply(z,1,mean)                    #求每个学生的综合平均分
roster <- cbind(roster, avg_score)
 
y <- quantile(roster$avg_score,c(.8,.6,.4,.2))  #学生综合平均得分的百分位数，由此可将成绩划分为A,B,C,D, 且可到每个成绩level的临界值.
roster$grade[avg_score >= y[1]] <- "A"
roster$grade[avg_score < y[1] & avg_score >= y[2]] <- "B"
roster$grade[avg_score < y[2] & avg_score >= y[3]] <- "C"
roster$grade[avg_score < y[3] & avg_score >= y[4]] <- "D"
roster$grade[avg_score < y[4]] <- "E"

name <- strsplit(roster$Student, " ")    
class(name)
unlist(name)
FirstName <- sapply(name,"[",1)
LastName <- sapply(name,"[",2)
sapply(name,substr,1,1)
roster <- cbind(FirstName,LastName,roster)

roster[order(avg_score, decreasing = T),]




#############################################R4.8   -- 整合与重构###############################################
mtcars
str(mtcars)
car <- mtcars[1:5,1:4]
class(car)
car_t <- t(car)                     #转置数据
class(car_t)

aggregate(mtcars,by = list(mtcars$cyl,mtcars$gear),FUN = mean, na.rm = T)   #聚合

#############################################R4.8.1 -- reshape包###############################################
library(reshape2)

#############################################R4.9   -- dplyr包#############################################
#dplyr - A fast, consistent tool for working with data frame like objects, both in memory and out of memory
#Manual - https://cran.r-project.org/web/packages/dplyr/dplyr.pdf
mydf <- data.frame(ID = numeric(0), Name = character(0), Gender = character(0))
fix(mydf)
summary(mydf)



#############################################R6     -- 基本统计分析#############################################
#############################################R6.1   -- 描述性统计分析#############################################
#描述性统计分析可以利用基础统计函数中介绍的函数来实现
varss <- c("mpg","hp","wt")  #hp:马力, wt:车重, am:变速箱类型, cyl:汽缸数
head(mtcars[varss])

summary(mtcars[varss])

sapply(mtcars[varss],mean)
aggregate(mtcars[varss],by = list(am = mtcars$am), mean)   #aggregate 只能使用少数内置的单返回值函数, e.g. 

dstats <- function(x){
  return(c(mean = mean(x), sd = sd(x)))
}
dstats(mtcars[varss])
by(mtcars$mpg,mtcars$am,dstats)


#############################################R6.2   -- 频数表和列联表#############################################
library(vcd)
head(Arthritis)

#一维列联表
mytable <- with(Arthritis,table(Improved))

#将频数转化为比例值或百分比
prop.table(mytable) * 100

mytable2 <- with(Arthritis, table(Treatment,Improved))
mytable2 <- xtabs(~Treatment+Improved, data = Arthritis)

margin.table(mytable2,1)
margin.table(mytable2,2)

prop.table(mytable2,1)
prop.table(mytable2,2)
prop.table(mytable2)

addmargins(mytable2)
addmargins(mytable2,1)
addmargins(mytable2,2)

#多元列联表
mytable <- xtabs(~Treatment+Improved+Sex, data = Arthritis)
ftable(mytable)

ftable(addmargins(mytable))
ftable(addmargins(prop.table(mytable)))

#############################################R6.3   -- 分类型变量的独立性检验#############################################
#根据列联表可以对两个或多个离散型变量之间的相关关系进行检验
#############################################R6.3.1 -- 卡方独立性检验#############################################
#卡方检验(chi-square test)，用来确定两个分类（离散型）变量之间是否存在显著的相关性
#卡方检验的原假设是 - 两个变量相互独立

library(vcd)
mytable <- xtabs(~Treatment+Improved, data = Arthritis)

#计算自由度 - 10
#df=(行个数-1)（列个数-1）
df <- (nrow(mytable) - 1) * (ncol(mytable) - 1)

#计算卡方值
chisq.test(mytable, correct = T)
#结果解释
#X-squared = 13.055   -- 卡方值,用计算出来的这个值与卡方分布表里的值（注意要选对应的自由度和显著水平）进行比较
#                        如果算出的值大于对应的分布表里面的值，则说明两个变量之间有显著的关系，反之则表示没有显著关系
#df = 2               -- 自由度，跟上面的公式得到的结果一样
#p-value = 0.001463   -- p值表示从总体中抽取的样本行变量与列变量是相互独立的概率。,p值越小，则拒绝原假设的概率越大。
#                        可以将p值与0.05,0.01,0.001等常用的显著性水平进行比较，一般如果小于0.05，就可以回答“在5%显著性水平下拒绝原假设”了。
#                        也就是说拒绝原假设错误的概率是5%, 如果p>0.05,则说明两个变量是相互独立的。
#总结， 如果p值<0.05, 不需要看X-squred值，便可拒绝原假设。

mytable1 <- xtabs(~Improved + Sex, data = Arthritis)  #警告信息的原因是: 表中的6个单元格之一（男性 - 一定程度上的改善）是一个小于5的值，这可能会使卡方近似无效。
chisq.test(mytable1) 


#############################################R6.3.2 -- Fisher精准检验#############################################
#Fisher精确检验的原假设是：边界固定的列联表中行和列是相互独立的
#最后还是看p值来判断
fisher.test(mytable)      #注意: Fisher精准检验不能用于2×2的列联表, 只适用于大于2×2的列联表
fisher.test(mytable1)


#############################################R6.3.3 -- Cochran—Mantel—Haenszel卡方检验#############################################
#其原假设是，两个名义变量在第三个变量的每一层中都是条件独立的
#下列代码可以检验治疗情况和改善情况在性别的每一水平下是否独立。
mytable2 <- xtabs(~Treatment+Improved+Sex, data = Arthritis)
mantelhaen.test(mytable2)

#结果表明，患者接受的治疗与得到的改善在性别的每一水平下并不独立（即，分性别来看，用药治疗的患者较接受安慰剂的患者有了更多的改善）。


#############################################R6.4   -- 分类型变量的相关性度量#############################################
library(vcd)
mytable <- xtabs(~Treatment+Improved,data = Arthritis)
assocstats(mytable)

#############################################R6.4.1 -- 马赛克图 - 分类型变量的相关性可视化#############################################
library(vcd)   #vcd: Visualizing Categorical Data 可视化分类数据

str(Titanic)
View(Titanic)
ftable(Titanic)

mosaic(Titanic,shade = T, legend = T)  #shade = TRUE将根据拟合模型的皮尔逊残差值对图形上色
#马赛克图隐含着大量的数据信息,e.g.
#1. 从船员舱到头等舱，存活率陡然提高；尤其是女性
#2. 无论是哪个等级的舱,女性的存活率都显著高于男性
#3. 大部分孩子都处于二等和三等舱中
#4. 1,2等舱中的小孩基本都存活了

#扩展的马赛克图添加了颜色和阴影来表示拟合模型的残差值

mosaic(~Class+Sex+Survived, data = Titanic, shade = T, legend = T)

#############################################R6.5   -- 列联表的扁平化#############################################
mytable <- xtabs(~Treatment+Improved+Sex,data = Arthritis)

table2flat <- function(mytable){
  df <- as.data.frame(mytable)
  rows <- dim(df)[1]
  cols <- dim(df)[2]
  x <- NULL
  for(i in 1:rows){
    for(j in 1:df$Freq[i]){
      row <- df[i,c(1:(cols-1))]
      x <- rbind(x,row)
    }
  }
  rownames(x) <- c(1:dim(x)[1])
  return(x)
}

table2flat(mytable)


#############################################R6.6   -- 相关-各种类型的变量#############################################
str(state.x77)
View(state.x77)
class(state.x77)
states <- state.x77[,1:6]

#cov: 计算协方差
cov(state.x77)

#cor(x = data.frame/matrix,use = all.obs/everything/complete.obs.., method = pearson/spearman/kendall)
cor(states, use = "everything", method = "pearson")
cor(states, use = "everything", method = "spearman")
cor(states, use = "everything", method = "kendall")
cor(states[,1:3],states[,4:6])

#############################################R6.6.1 -- 相关性的显著性检验#############################################
##cor.test(
##x,y,
##alternative = two.side(原假设为变量间不相关的双侧检验)/
##              less(原假设为总体的相关系数小于0的单侧检验)/
##              greater(原假设为总体的相关系数大于0的单侧检验),
##method = pearson, spearman, kendall
##)
cor(states[,"Life Exp"],states[,"Murder"], method = "pearson")
cor.test(states[,"Life Exp"],states[,"Murder"], method = "pearson")

#这段代码检验了预期寿命和谋杀率的Pearson相关系数为0的原假设。假设总体的相关度为0，
#则预计在一千万次中只会有少于一次的机会见到0.703这样大的样本相关度（即p = 1.258e08）。
#由于这种情况几乎不可能发生，所以你可以拒绝原假设，从而支持了要研究的猜想，即预期寿命
#和谋杀率之间的总体相关度不为0。

#############################################R6.7   -- t检验#############################################
#在研究中最常见的行为就是对两个组进行比较。接受某种新药治疗的患者是否较使用某种现有药物的患者表现出了更大程度的改善？
#某种制造工艺是否较另外一种工艺制造出的不合格品更少？两种教学方法中哪一种更有效？
#如果你的结果变量是类别型的，那么可以直接使用前面介绍的方法来进行相关性分析和相关的显著性检验

#如果我们研究的变量是连续型变量，且关注结果变量为连续型的组间比较，并假设其呈正太分布，就需要用到t检验。
library(MASS)
UScrime
str(UScrime)

#############################################R6.7.1   -- 独立样本的t检验#############################################
#以数据集UScrime为例，我们需要研究的主题是：如果你在南方犯罪(变量So是一个二分变量,1表示南方,0表示非南方.该变量为自变量),是否更有可能被判监禁
#(监禁的概率Prop为因变量). 我们假设两组数据是独立的，并且从正态分布中抽样得到。t检验的调用格式为:
t.test(y ~ x, data,...)
#y:因变量
#x:自变量

#假设方差不等的双侧检验
#原假设为：南方各州和非南方各州拥有相同的监禁概率
t.test(Prob~So,data = UScrime)
#p的结果值小于0.001,则可拒绝原假设

#############################################R6.7.1   -- 非独立样本的t检验#############################################
UScrime[c("U1","U2")]
P176
#############################################R6.8   -- 组间差异的非参数检验#############################################
#如果数据无法满足t检验或ANOVA的参数假设，可以转而使用非参数方法。P176

#############################################R7   -- 回归#############################################
#回归广义的定义:通指那些用一个或多个预测变量（也称自变量或解释变量）来预测响应变量（也称因变量、效标变量或结果变量）的方法。

#回归类型       #用途
#简单线性       用一个量化的解释变量预测一个量化的响应变量
#多项式         用一个量化的解释变量预测一个量化的响应变量，模型的关系是n阶多项式
#多元线性       用两个或多个量化的解释变量预测一个量化的响应变量
#多变量         用一个或多个解释变量预测多个响应变量
#Logistic       用一个或多个解释变量预测一个类别型响应变量
#泊松           用一个或多个解释变量预测一个代表频数的响应变量
#Cox比例风险    用一个或多个解释变量预测一个事件（死亡、失败或旧病复发）发生的时间
#时间序列       对误差项相关的时间序列数据建模
#非线性         用一个或多个量化的解释变量预测一个量化的响应变量，不过模型是非线性的
#非参数         用一个或多个量化的解释变量预测一个量化的响应变量，模型的形式源自数据形式，不事先设定
#稳健           用一个或多个量化的解释变量预测一个量化的响应变量，能抵御强影响点的干扰

#一个例子(该例子同时也暗含了进行回归分析的基本步骤与方法)
#一个工程师想找出跟桥梁退化有关的最重要的因素，比如使用年限、交通流量、桥梁设计、建造材料和建造方法、建造质量以及天气情况，并确定它们之间的数学关系。
#他从一个有代表性的桥梁样本中收集了这些变量的相关数据，然后使用OLS回归对数据进行建模。
#他准备拟合了一系列模型，检验它们是否符合相应的统计假设，探索了所有异常的发现，最终从许多可能的模型中选择了“最佳”的模型。如果成功，那么结果将会帮
#助他完成以下任务:
#1. 在众多变量中判断哪些对预测桥梁退化是有用的，得到它们的相对重要性，从而关注重要的变量。
#2. 根据回归所得的等式预测新的桥梁的退化情况（预测变量的值已知，但是桥梁退化程度未知），找出那些可能会有麻烦的桥梁。
#3. 利用对异常桥梁的分析，获得一些意外的信息。比如他发现某些桥梁的退化速度比预测的更快或更慢，那么研究这些“离群点”可能会有重大的发现，
#   能够帮助理解桥梁退化的机制。

#回归分析主要解决的问题：
#1.	从一组样本数据出发，确定变量之间的数学关系式；
#2.	对这些数学关系式的可信程度进行各种统计检验，并从影响某一特定变量的诸多变量中找出那些变量的影响是显著的，那些是不显著的；
#3.	利用所求的关系式，根据一个或多个变量的取值来估计或预测另一个特定变量的取值，并给出这种估计或预测的可靠程度。

#############################################R7.1   -- 最小二乘法(OLS)#############################################
#可以想象，对于一个样本中x，y的n对观察值，用于描述它们关系的直线可以有很多条，那么究竟选定哪条直线来代表两个变量的关系啦？
#即如何计算估计回归方程的参数p0,p1
#最小二乘法的原理是：最小化散点图中垂直方向的离差(y的实际观测值-y的期望值(由估计的回归方程计算得到))平方和来估计参数p0,p1.
#最小二乘法的一个重要特性：拟合的回归直线通过点(x的平均值,y的平均值)




#############################################R7.2 -- 一个例子#############################################
library(ggplot2)

#某商业银行2002年的主要业务数据
Branch_ID <- c(1:25)
#不良贷款
Bad_Loan <- c(0.9,1.1,4.8,3.2,7.8,2.7,1.6,12.5,1.0,2.6,0.3,4.0,0.8,3.5,10.2,3.0,0.2,0.4,1.0,6.8,11.6,1.6,1.2,7.2,3.2)
#贷款余额
Loan_Balance <- c(67.3,111.3,173.0,80.8,199.7,16.2,107.4,185.4,96.1,72.8,64.2,132.2,58.6,174.6,263.5,79.3,14.8,73.5,24.7,139.4,368.2,95.7,109.6,196.2,102.2)
#本年应收贷款余额
Loan_AR_CY <- c(6.8,19.8,7.7,7.2,16.5,2.2,10.7,27.1,1.7,9.1,2.1,11.2,6.0,12.7,15.6,8.9,0.6,5.9,5.0,7.2,16.8,3.8,10.3,15.8,12.0)
#贷款项目数
Loan_Project <- c(5,16,17,10,19,1,17,18,10,14,11,23,14,26,34,15,2,11,4,28,32,10,14,16,10)
#固定投资额
FAI_Amount <- c(51.9,90.9,73.7,14.5,63.2,2.2,20.2,43.8,55.9,64.3,42.7,76.7,22.8,117.1,146.7,29.9,42.1,25.3,13.4,64.3,163.9,44.5,67.9,39.7,97.1) #Investment in Fixed Assets

Bank_Business <- data.frame(Branch_ID,Bad_Loan,Loan_Balance,Loan_AR_CY,Loan_Project,FAI_Amount)

#############################################R7.2.1 -- 通过散点图看相关关系#############################################
#通过散点图来直观的参看两个变量间是否存在相关关系(或函数关系),并可以拟合回归曲线((正/负)线性或非线性)
spot1 <- ggplot(Bank_Business,aes(x = Loan_Balance, y = Bad_Loan
                                  #colour = Branch_ID, 
                                  #size = FAI_Amount
                                  )) + 
            geom_point(alpha = .5)

spot1 + geom_smooth(method = lm)


#############################################R7.2.2 -- 计算相关系数#############################################
#通过散点图可以大致确定两个变量之间是否存在相关关系，但其不能准确地反映变量之间的关系强度。
#相关系数(correlation coefficient) – 是根据样本数据计算的度量两个变量之间线性关系强度的统计量。
#线性相关系数(linear correlation coefficient), 或称为Pearson相关系数(Pearson’s correlation coefficient)

#在假设变量间存在的是线性相关关系的前提下，用下面的公式得到变量间的相关矩阵(样本数据)
cor(Bank_Business[,-1])
cor(Bank_Business[,-1],method = "pearson")

#############################################R7.2.3 -- 相关关系r的显著性检验#############################################
#以上得到的是样本的相关系数r,总体的相关系数用p表示
#由于抽取的样本不同，计算得到的r的取值也就不同，因此r是一个随机变量。显著性检验就是为了判断，能否根据样本相关系数说明总体的相关强弱程度
#假设r服从正太分布是具有很大风险的，所以对r的显著性检验一般采用t分布检验

#例：检验不良贷款(Bad_Loan)与贷款余额(Loan_Balance)之间的相关系数是否显著(显著性水平默认为0.05)
r <- 0.8436
ziyoudu <- 25 - 2 #自由度=数据量-2
#计算检验统计量
t = abs(r) * sqrt(ziyoudu/(1-r^2))
#根据显著性水平=0.05,自由度=23,查t分布表得:2.0687, t=7.5344 > 2.0687, 所有可以拒绝原假设(H0:p=0)

#下面的R函数可方便直接地用于相关性系数的显著性检验
cor.test(Bank_Business$Bad_Loan,Bank_Business$Loan_Balance,alternative = "two.sided",method = "pearson",conf.level = 0.95)



#############################################R7.2.4 -- 利用最小二乘法计算回归方程的参数值#############################################
fit <- lm(formula = Bad_Loan~Loan_Balance,data = Bank_Business)
summary(fit) #结果中的Multiple R-squared 即为判定系数
             #结果中的Residual standard error 即为残差标准误
coefficients(fit) #列出拟合模型的模型参数(截距p0和斜率(或叫回归系数)p1)
confint(fit)      #提供模型参数的置信区间（默认95%）
fitted(fit)       #列出拟合模型的预测值 可与实际值对比Bank_Business$Bad_Loan
residuals(fit)    #列出拟合模型的残差值
plot(fit)         #生成评价拟合模型的诊断图

#由结果得到的估计的回归方程式为:
#y = -0.8295 + 0.037895*x

#############################################R7.2.5   -- 回归直线的拟合优度#############################################
#当我们根据样本数据得到一条回归直线后，我们需要一个统计量来描述这条回归直线对数据的拟合程度的好坏。
#回归直线对数据的拟合优度(goodness of fit)就是回归直线与各观测点的接近程度。
#判定系数就是计算拟合优度的统计量。

#总变差(SST) = (y的实际观测值-y的平均值)的平方和
SST <- sum((Bank_Business$Bad_Loan - mean(Bank_Business$Bad_Loan)) ^ 2)

#SST可拆分成两个部分: SSR 和 SSE
#SSR(回归平方和) = (y的回归值-y的平均值)的平方和， 它反应了y的总表差中由于x与y之间的线性关系引起的y的变化的部分。
SSR = sum((fitted(fit) - mean(Bank_Business$Bad_Loan)) ^ 2)
#SSE(残差平方和) = (y的实际观察值 - y的回归值)的平方和，它反应的是不能由回归直线来解释的其他因素对y变差的作用。
SSE = sum((Bank_Business$Bad_Loan - fitted(fit)) ^ 2)

SSE + SSR == SST

#判定系数(R2 2表示平方) = SSR/SST, 判定系数越大表示直线拟合得越好, R2 = r^2(仅限于一元线性回归模型中)
R2 <- SSR/SST
r ^ 2 == R2

#此例中不良贷款与贷款余额拟合的回归方程的判定系数为71.16%, 这表示在不良贷款的变差中有71.16%可以由不良贷款与贷款余额之间的线性关系来解释，
#或者说，在不良贷款的变动中，有71.16%是由贷款余额决定的。

#############################################R7.2.6   -- 回归方程的显著性检验#############################################
#显著性检验的目的是：检验根据样本数据计算得到的估计回归方程是否真实地反映了变量x与y之间的关系。检验通过后才能将这个回归方程用于预测。
#回归分析中的显著性检验包含两方面内容：
#•	线性关系检验
#•	回归系数检验
#############################################R7.2.6.1   -- 线性关系检验#############################################
#用于线性关系检验的统计量F=(SSR/1)/(SSE/(n-2)) 

#############################################R7.2.6.2   -- 回归系数检验#############################################
#############################################R7.2.7   -- 利用回归方程进行预测#############################################
#回归方程:y = -0.8295 + 0.037895*x
#############################################R7.2.7.1   -- 点估计#############################################
#平均值点估计: 估计贷款平均余额为100时，所有分行不良贷款的平均值: (适用于总行的一个估计)
Ey0 <- -0.8295 + 0.037895*100

#个别值的点估计：估计贷款余额为72.8的编号为10的分行的不良贷款是多少
y10 <- -0.8295 + 0.037895*72.8


#############################################R7.2.7.2   -- 区间估计#############################################
#区间估计: 利用估计的回归方程，对给定的x的一个特定值x0，求出y的一个估计值的区间。
#区间估计分两种：
#置信区间(confidence interval)估计 – 求y的平均值的估计区间; 设贷款余额x0 = 100, 建立不良贷款的95%的置信区间？
#Eyo +(-) 0.8459 
#预测区间(prediction interval)估计 – 求y的个别值的估计区间







#############################################R7.2.8   -- 多项式回归#############################################
#通过添加一个二次项（即X 2）来提高回归的预测精度
fit2 <- lm(formula = Bad_Loan~Loan_Balance+I(Loan_Balance^2),data = Bank_Business)
summary(fit2)
plot(Bank_Business$Loan_Balance,Bank_Business$Bad_Loan,
     xlab = "Loan Balance",
     ylab = "Bad Load")
lines(Bank_Business$Loan_Balance,fitted(fit2))
#############################################R7.3 -- 又一个例子#############################################
#数据集
women


#############################################R8 -- Distribution Analysis#############################################
library(ggplot2)

set.seed(1234)
dat <- data.frame(cond = factor(rep(c("A","B"),each = 200)),
                  rating = c(rnorm(200),rnorm(200,mean = .8)))

#histogram
ggplot(dat, aes(x = rating)) + 
  geom_histogram(binwidth = .5,colour = "black", fill = "white")

#density
ggplot(dat, aes(x = rating)) +
  geom_density()

#histogram with density
ggplot(dat, aes(x=rating)) +
  geom_histogram(aes(y=..density..),
                 binwidth = .5,colour = "black", fill = "white") +
  geom_density(alpha = .2,fill = "#FF6666")

#add a line for mean in histogram
ggplot(dat, aes(x = rating)) + 
  geom_histogram(binwidth = .5,colour = "black", fill = "white") +
  geom_vline(aes(xintercept=mean(rating, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)

#histogram plot with multiple groups
ggplot(dat, aes(x = rating, fill = cond)) +
  geom_histogram(binwidth = .5,alpha = .5,position = "identity")
ggplot(dat, aes(x = rating, fill = cond)) +
  geom_histogram(binwidth = .5,alpha = .5,position = "dodge")

#density plot with multiple groups
ggplot(dat, aes(x=rating, colour=cond)) + geom_density()
ggplot(dat, aes(x=rating, fill=cond)) + geom_density(alpha=.3)


library(plyr)
library(ggplot2)
cdat <- ddply(dat, "cond", summarise, rating.mean = mean(rating))

#histogram plot with multiple groups and a mean line
ggplot(dat, aes(x=rating, fill=cond)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity") +
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=cond),
             linetype="dashed", size=1)

#density plot with multiple groups and a mean line
ggplot(dat, aes(x=rating, colour=cond)) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=cond),
             linetype="dashed", size=1)

#boxplot
ggplot(dat,aes(x=cond,y=rating)) + geom_boxplot()
ggplot(dat,aes(x=cond,y=rating,fill=cond)) + geom_boxplot()

# With flipped axes
ggplot(dat, aes(x=cond, y=rating, fill=cond)) + geom_boxplot() + 
  guides(fill=FALSE) + coord_flip()


#densityplot(rnorm(200))
#############################################Rx     -- 基础作图#############################################
########一个例子
pdf("./Files/plot1.pdf")
  attach(mtcars)
    plot(wt,mpg)
    abline(lm(mpg~wt))
    title(main = "Regression of MPG on weight")
  detach(mtcars)
dev.off()

########另一个例子
dose <- c(20,30,40,45,60)
drugA <- c(16,20,27,40,60)
drugB <- c(15,18,25,31,40)

plot(dose,drugA,type = "b")

########一个图形的全局图形参数
opar <- par(no.readonly = T)
par(lty = 2, pch = 17,
    mar = c(5,4,4,2) + 0.1)            ####图形尺寸和边界大小
plot(dose,drugA,type = "b")
par(opar)

plot(dose,drugA,type = "b", lty = 3, lwd = 3, pch = 19, cex = 2)

########颜色
n <- 10
rainbow(n)

plot(dose,drugA,type = "b", lty = 3, lwd = 3, pch = 19, cex = 2, col = rainbow(100)[sample(c(1:100),1)])

########title
plot(dose,drugA,type = "b", lty = 3, lwd = 3, pch = 19, cex = 2, col = rainbow(100)[sample(c(1:100),1)],
     main = "TEST - DOSE VS. DUGA", sub = "Sub Title", xlab = "DRUG_A", ylab = "DOSE")

####文本属性:大小，颜色，字体
plot(dose,drugA,type = "b", lty = 3, lwd = 3, pch = 19, cex = 2, col = rainbow(100)[sample(c(1:100),1)],
     main = "TEST - DOSE VS. DUGA", sub = "Sub Title", xlab = "DRUG_A", ylab = "DOSE",
     cex.sub = 0.97, font.main = 2, col.lab = "blue")

########自定义坐标轴范围
plot(dose,drugA,type = "b", lty = 3, lwd = 3, pch = 19, cex = 2, col = rainbow(100)[sample(c(1:100),1)],
     main = "TEST - DOSE VS. DUGA", sub = "Sub Title", xlab = "DRUG_A", ylab = "DOSE",
     cex.sub = 0.97, font.main = 2, col.lab = "blue",
     ylim = c(0,70),xlim = c(0,60))

########自定义坐标轴
x <- c(1:10)
y <- x
z <- 10/x

opar <- par(no.readonly = T)
par(mar = c(5,4,4,8) + 0.1)
plot(x,y,type = "b",pch = 21, col = "red", lty = 3,
     yaxt = "n",
     ann = F)
lines(x,z,type = "b",pch = 22,col = "blue", lty = 2)
axis(2, at = x, labels = x, col.axis = "red", las = 2)

axis(4, at = z, labels = round(z,digits = 2), col.axis = "blue",las = 2, cex.axis = 0.7)
mtext("Y=1/X",side = 4, line = 3, cex.lab = 1, las = 2, col = "blue")

title(main = "An example", xlab = "X values", ylab = "Y = X", col.lab = "red")
par(opar)

########图例, 一图双线, 次要刻度
opar <- par(no.readonly = T)
par(lwd = 2, cex = 1.5, font.lab = 2, mar = c(5,4,4,2) + 0.1)
plot(dose, drugA, type = "b", pch = 15, lty = 1, col = "red", 
     ylim = c(0,60),
     main = "Durg A vs. Drug B", xlab = "Drug Dosages", ylab = "Drug Response")

text(dose, drugA,c("a","b","c","d","e"),cex = 0.8, pos = 3)

lines(dose,drugB,type = "b", pch = 17, lty = 2, col = "blue")

text(dose, drugB,c("k","j","p","q","r"),cex = 0.8, pos = 1)

abline(h=c(30),lwd = 1.5, lty = 2, col = "black")
text(20, 30,"Standard Line",cex = 0.7, pos = 3, col = "red")

#install.packages("Hmisc")
#library(Hmisc)
minor.tick(nx = 3, ny = 3, tick.ratio = 0.5)

legend("topleft",inset = .05, title = "Drug Type", c("A","B"), lty = c(1,2), pch = c(15,17), col = c("red","blue"))

par(opar)

########文本标注
#text()可向绘图区域内部添加文本，
#mtext()则向图形的四个边界之一添加文本
attach(mtcars)
  plot(wt,mpg)
  text(wt,mpg,row.names(mtcars),cex = 0.6,pos = 4,col = "red")
detach()
  

########一屏多图,图形布局
attach(mtcars)
  opar <- par(no.readonly = T)
  par(mfrow = c(2,2))
  plot(wt,mpg,main = "First Plot")
  plot(wt,disp,main = "Second Plot")
  hist(wt,main = "Trird Plot")
  boxplot(wt, main = "Fourth Plot")
  par(opar)
detach(mtcars)
  
attach(mtcars)
  layout(matrix(c(1,1,2,3),2,2,byrow = T),widths = c(3,1),heights = c(1,2))
  hist(wt)
  hist(mpg)
  hist(disp)
detach(mtcars)
  
opar <- par(no.readonly = T)
par(fig = c(0,0.7,0,0.7))  #左下角坐标为(0,0)，而右上角坐标为(1,1), fig=c(x1, x2, y1, y2)
plot(mtcars$wt,mtcars$mpg,xlab = "Mile Per Gallon", ylab = "Car Weight")
par(fig = c(0,0.8,0.75,1), new = T)
boxplot(mtcars$wt, horizontal = T, axes = F, main = "111")
par(fig = c(0.75,0.8,0,0.7),new = T)
boxplot(mtcars$mpg,axes = F,horizontal = F)
mtext("Enhanced Scatterplot", side = 3, outer = T, line = -3)
par(opar)



#############################################Rx.1   -- 基本图形模块#############################################
#######基本的图形模块有: title, Text Annotation, Axis, Legend
#######基本的图形元素有: color,font,cex
install.packages("vcd")
library(vcd)

#######A. 用于展示类别型变量分布的图形：
#########A.1. 条形图 (一元条形图,堆砌条形图,分组条形图,棘状图)
#########A.2. 饼图

#######B. 用于展示连续型变量分布的图形：
#########B.1. 直方图 
#########B.2. 核密度图 
#########B.3. 箱线图

#############################################Rx.2   -- 条形图#############################################
t_1 <- table(Arthritis$Improved)
barplot(t_1,main = "Bar Plot Sample 2",xlab = "Improvement", ylab = "Count", ylim = c(0,50),horiz = F,
        names.arg = c("AAAA","BBBB","CCCC"))
text(t_1, labels = c(42,14,28),cex = 1, col = "blue", font = 4, pos = 1, offset = 1)


#############################################Rx.3   -- 堆砌/分组条形图/棘状图#############################################
r2 <- table(Arthritis$Improved,Arthritis$Treatment)

barplot(r2,ylim = c(0,30),col = c("red","black","blue"),beside = T)
barplot(r2,ylim = c(0,50),col = c("red","black","blue"),beside = F, legend = row.names(r2))

spine(r2,main = "Spinogram Example")

#############################################Rx.4   -- 饼图#############################################
par("mfrow")
par(mfrow = c(1,2))
slices <- c(10,12,4,16,8)
lbls <- c("US","UK","IND","CHN","AUS")

pie(slices,lbls,main = "Simple Par Plot")

pct <- round(slices/sum(slices)*100)
lbls2 <- paste(lbls," ", pct, "%", sep = " ")
pie(slices,lbls2,col = rainbow(length(lbls2)),main = "Pie Chart with Percentage")

#install.packages("plotrix")
#library(plotrix)
fan.plot(slices,lbls,main = "Fan Plot")

#############################################Rx.5   -- 直方图#############################################
par(mfrow = c(2,2))
hist(mtcars$mpg)

hist(mtcars$mpg,
     breaks = 12,
     col = "red",
     xlab = "Miles Per Gallon",
     main = "Colored Hist")

hist(mtcars$mpg, freq = F,
     breaks = 12,col = "red",
     xlab = "Miles Per Gallon",main = "The Third Hist")
rug(jitter(mtcars$mpg))
lines(density(mtcars$mpg),col = "blue", lwd = 2)        #添加概率密度线

x <- mtcars$mpg
h <- hist(x, breaks = 12, col = "red", xlab = "Miles Per Gallon",main = "The Fourth Hist")
xfit <- seq(min(x),max(x),length.out = 40)
yfit <- dnorm(x,mean = mean(x),sd = sd(x))
lines(yfit,x,col = "blue",lwd = 2)
box()

#############################################Rx.6   -- 核(概念)密度图#############################################
par(mfrow = c(2,1))
d <- density(mtcars$mpg)
plot(d)

plot(d,main = "Kernel Density of MPG")
polygon(d, col = "red",border = "blue")
rug(mtcars$mpg, col = "brown")


#############################################Rx.7   -- 箱线图#############################################
par(mfrow = c(1,1))
boxplot(mpg~cyl, data = mtcars, main = "Car", xlab = "X",ylab = "Y")

#############################################Rx.8   -- 小提琴图#############################################
#小提琴图是箱线图与核密度图的结合。
#install.packages("vioplot")
#library(vioplot)
x1 <- mtcars$mpg[mtcars$cyl == 4]
x2 <- mtcars$mpg[mtcars$cyl == 6]
x3 <- mtcars$mpg[mtcars$cyl == 8]
vioplot(x,x2,x3, names = c("4 cyl","6 cyl","8 cyl"), col = "gold")

#############################################Rx.9   -- 点图#############################################
dotchart(mtcars$mpg, labels = row.names(mtcars), cex = .7, main = " Ttiledf ", xlab =  "Mile Per Gallon")

x <- mtcars[order(mtcars$mpg),]
x$cyl <- factor(x$cyl)
x$color[x$cyl == 4] <- "red"
x$color[x$cyl == 6] <- "blue"
x$color[x$cyl == 8] <- "black"

dotchart(x$mpg,labels = row.names(x), cex = .7,
         group = x$cyl, gcolor = "black", color = x$color,
         pch = 19, 
         main = "vffffffffffffff",xlab = "aaaaaaaaaaaa")


#############################################APD1   -- 趣味知识#############################################
#职业                  行                               列
#统计学家              观测(observation)                变量(variable)
#数据库分析师          记录(record)                     字段(field)
#数据挖掘/机器学习     示例(example)                    属性(attibute)


























