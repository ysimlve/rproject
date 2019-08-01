#!You should strive to learn new things throughout your career, but make sure your understanding is solid before you move on to the next interesting thing!#

##################################%%%%%%%%%%%%%%%%%%%%%Get Started%%%%%%%%%%%%%%%%%%%%%########################################################
#setwd("C:/YuanLe/R/RWkDir/RLearn/RFoundation")
options("encoding" = "utf-8")
options(stringsAsFactors = FALSE)
options(digits = 5)
options(warn=-1)
options(warnPartialMatchDollar = TRUE) 
#sessionInfo()
Sys.setlocale(category = "LC_CTYPE", locale = "Chinese")   #中文显示
#install.packages("Deriv",repos='http://cran.us.r-project.org', lib = .libPaths())
#An R package is a collection of functions, data, and documentation that extends the capabilities of base R
library(tidyverse);

##################################%%%%%%%%%%%%%%%%%%%%%Basic R%%%%%%%%%%%%%%%%%%%%%########################################################
##################################1. Basic##################################
#########What's CRAN?
#######Comprehensive R Archive Network - is a network of ftp and web servers around the world that store identical, up-to-date, versions of code and documentation for R.

#########R 会话(session) & 工作空间(workspace)
#######工作空间(workspace), 是当前会话(session)的工作环境，它储存着所有用户定义的对象（向量、矩阵、函数、数据框、列表）
#######在一个R会话(session)结束时，你可以将当前工作空间保存到一个镜像中(格式后缀.RData)，并在下次启动R时自动载入它
ls()                  #列出当前工作空间中的对象
rm("","")             #移除一个或多个工作空间中的对象 - rm(list = ls())
save.image("myfile")  #保存工作空间到文件myfile中（默认值为.RData???
load("myfile.RData")  #读取一个工作空间到当前会话???
history()             #显示最近使用过???#个命令（默认值为25???
q()                   #退出R session

#########R脚本的运行方式
source("./Script/script_1.R")                   #在当前会话中调用外部R脚本
#R CMD BATCH [options] [infile] [outfile]       #在Linux上运行R脚本的命???
#--在Windonws上运行R脚本的命???
#C:\YuanLe\R\RforWin\R\R-3.4.3\bin\x64\Rscript.exe "C:\Users\andy.yuan\Box Sync\GIM_Share_HK\CD_Lease_Extraction\Markets\006. MMProcess\TEST.R" "Chengdu" "Andy"

#########常用的系统命令
version;                                           #查看当前R的版本;
installr::updateR();                               #升级到最新版本;
library()                                          #显示库中所有可用的包
.libPaths()                                        #显示库所在的路径
search()                                           #显示哪些包已加载并可使用
install.packages("Deriv",repos='http://cran.us.r-project.org', lib = .libPaths())
update.packages("ggplot2")                         #更新包
installed.packages("ggplot2")                      #显示版本号、依赖关系等信息
help(package ="ggplot2")
packageDescription("ggplot2")                      #查看library的详细信息

##################################2. R 知识点##################################
##################################2.1. MD5 Key##################################
##MD5
full$id <- apply(full[,1:19], 1, function(x){openssl::md5(str_c(x,collapse = ""))})

##################################2.2. Memory Usage##################################
memory.size(T)   #查看已分配内存
memory.size(F)   #查看已使用内存
memory.limit()   #查看内存上限 
format(object.size(x_test), units = "auto")

##################################2.3. Generator Function##################################
sequence_generator <- function(start){
  value <- start - 1
  function(){
    value <<- value + 1
    value
  }
}

gen <- sequence_generator(100)
gen
gen()

##################################%%%%%%%%%%%%%%%%%%%%%Advanced R(Hadley)%%%%%%%%%%%%%%%%%%%%%########################################################
##################################1. Names and values##################################
library(lobstr)

# The following code is doing two things:
# 1. It’s creating an object, a vector of values, c(1, 2, 3)
# 2. And it’s binding that object to a name, x
x <- c(1,2,3)  # a name is a reference to a value
y <- x         # run this code don’t get another copy of the value 1:3, just get another binding to the existing object
`111` <- y

obj_addr(x)    # x, y,`111` are binded to the same memory address where c(1,2,3) is stored
obj_addr(y)
obj_addr(`111`)


# Copy-on-modify
# what happened to the shared binding?  
# copy the object to a new address(means created a new object) and then modify, and then rebind to y
# R objects(some,e.g. vector) are unchangeable
x <- c(1,2,3)
y <- x
obj_addr(x) == obj_addr(y)
x == y
y[[3]] <- 4            #copy-on-modify
obj_addr(x) == obj_addr(y)
x == y
y
x
y[[2]] <- 7
obj_addr(y)


# shallow copy
l1 <- list(1,2,3)
l2 <- l1
obj_addr(l1) == obj_addr(l2)
l2[[2]] <- 7
obj_addr(l1) == obj_addr(l2)
obj_addr(l1[[1]]) == obj_addr(l2[[1]])   #TRUE, only second element of l1 is copy-on-modify
ref(l1,l2)                               #To see values that are shared across lists

# data frame is list of vectors
d1 <- data.frame(x = c(1, 5, 6), y = c(2, 4, 3))
d2 <- d1
d2[, 2] <- d2[, 2] * 2                  #modify on column
obj_addr(d1[,1]) == obj_addr(d2[,1])    #TRUE, shallow copy only the second column
d3 <- d1                  
d3[1,] <- c(7,7)                        #modify on row
obj_addr(d1[,1]) == obj_addr(d3[,1])    #FALSE, deep copy entire object

# character vector - 'global string pool'
x <- c("a", "a", "abc", "d")


# object size
x <- runif(1000000,0,1)
obj_size(x)
y <- list(x,x,x)
obj_size(y)                      #80 bytes larger than x
obj_size(list(NULL,NULL,NULL))   #this object is 80 bytes

banana <- "bananas bananas bananas"
bananas <- rep(banana,1000)
obj_size(banana)
obj_size(bananas)      #not 1000 times bigger than banana, because of 'global string pool'


# modify-in-place
#As we’ve seen above, modifying an R object usually creates a copy. There are two exceptions:
#'1. Objects with a single binding get a special performance optimisation.
#'2. Environments, a special type of object, are always modified in place.
v1 <- c(1,2,3)
obj_addr(v1)
v1[[2]] <- 7
obj_addr(v1)       #the address changed? why? actually it's hard to predict whether or not a copy will occur


# environments object always modify-in-place
e1 <- rlang::env(a = 1, b = 2, c = 3)
e2 <- e1
obj_addr(e1) == obj_addr(e2)       #TRUE


# unbinding
x <- c(1:2)
obj_addr(x)
x <- c(2,4)
obj_addr(x)
rm(x)           # although we removed x(only remove the bind), however the two object c(1:2) and c(2,4) are still in memory(neither of them bind to a name)

# garbage collector(GC)
# The garbage collector (GC) is run automatically whenever R needs more memory to create a new object. 
gcinfo(TRUE)                #if you want to find out when the GC runs, call this function
gc()                        #force the garbage collector to run by calling gc()
mem_used() / 1024 / 1024    #prints the total number of bytes used

##################################2. Vector##################################
#Vectors come in two flavours: atomic vectors and lists
#There are four common types of atomic vectors: logical, integer, double, and character. 

v1 <- c(0.1234,1e10,Inf,-Inf,NaN,NA,NULL)
v2 <- c(1,3,4); class(v2); typeof(v2); is.numeric(v2); is.integer(v2); is.double(v2)
v3 <- c(1L,2L,4L); class(v3); typeof(v3); is.numeric(v3); is.integer(v3); is.double(v3)

# ATTRIBUTES
#You can think of attributes as a named list used to attach metadata to an object.
x <- 1:10
attr(x,"at1") <- letters[1:10]
attr(x,"at2") <- c(10:1)
attr(x,"at1")
attributes(x)

# NAME A VECTOR
x <- c(a=1, b=2, c=3)
x["b"]

y <- c(1:3)
names(y) <- c("a","b","c")

names(x)                              # avoid using attr(x, "names")
names(y) <- NULL; names(y); y         # remove names of a vector

class(y)                              # defines the S3 object system


# S3 ATOMIC VECTOR
# factor - A factor is a vector that can contain only predefined values, and is used to store categorical data. 
#          Factors are built on top of integer vectors with two attributes: the class and levels
x <- factor(c("a", "b", "b", "a"))
x
attributes(x)
typeof(x)

sex_char <- c("m", "m", "m")
sex_factor <- factor(sex_char, levels = c("m", "f"))
table(sex_char); table(sex_factor)                # note the deferent between character vector and factor

# ordered factor
y <- ordered(c("a","b","b","a"), levels = c("b","a"))
attributes(y)

# Date vector - are built on top of double vectors.
d <- c(Sys.Date(), as.Date("1970-01-01"))
attributes(d); class(d); typeof(d)
unclass(d)                                        # return the double value of date, represents the number of days since 1970-01-01

# Date times vector - Base R provides two ways of storing date-time information, POSIXct, and POSIXlt. 
# ct: calendar time; lt: local time
now_ct <- as.POSIXct("2018-08-01 22:00", tz = "UTC")
now_ct
typeof(now_ct); attributes(now_ct)
structure(now_ct, tzone = "Asia/Tokyo")
structure(now_ct, tzone = "Asia/Hong_Kong")
structure(now_ct, tzone = "Asia/Chongqing")


# LIST
#an element of a list can be any type
l1 <- list(
  1:3, 
  "a", 
  c(TRUE, FALSE, TRUE), 
  c(2.3, 5.9)
)

typeof(l1)
str(l1)

# DATA FRAME & TIBBLES
#There are two important S3 vectors that are built on top of lists: data frames and tibbles.


# NULL
#NULL is special because it has a unique type, is always length 0, and can’t have any attributes
typeof(NULL); length(NULL)
is.null(NULL)

#There are two common uses of NULL:
#1. To represent an empty vector (a vector of length 0) of arbitrary type. 
x <- c(); x
#2. To represent an absent vector(NULL is often used as a default function argument). 
#   Contrast this with NA which is used to indicate that an element of a vector is absent

##################################3. Subsetting##################################
# SELECTING MULTIPLE ELEMENTS
x <- c(1:100)
#There are six things that you can use to subset a vector:
#1. Positive integers return elements at the specified positions
x[c(1,2,99)]
#2. Negative integers omit elements at the specified positions:
x[-c(1,2,99)]
#3. Logical vectors select elements where the corresponding logical value is TRUE.
x[rep(c(TRUE,FALSE),50)]
x[c(TRUE,FALSE)]        #If the logical vector is shorter than the vector being subsetted, it will be silently recycled to be the same length.
x[x>50 & x<60]
#4. Nothing returns the original vector. This is not useful for 1d vectors, but as you’ll see shortly, is very useful for matrices, data frames, and arrays
#5. Zero returns a zero-length vector.
#6. use character vectors to return elements with matching names(If the vector is named)
x <- c(1:4)
names(x) <- letters[1:4]
x[c("a","d")]


# SELECTING A SINGLE ELEMENT
#[[ is used for extracting single items, and x$y is a useful shorthand for x[["y"]]
#There are also two additional subsetting operators that are needed for S4 objects: @ (equivalent to $), and slot() (equivalent to [[).


# SUBSETTING & ASSIGNMENT
x <- c(1:5)
x[c(1,3)] <- c(7,17)
x

y <- list(a = 1, b = 2)
y[["b"]] <- NULL                #this will remove a component
str(y)

y <- list(a = 1, b = 2)
y[["b"]] <- list(NULL)          #this will change the element in list
str(y)


##################################4. Functions##################################
# Two importants:
#1. Functions are objects, just as vectors are objects.
#2. Functions can be broken down into three components: arguments, body, and environment.

f01 <- function(x,y){                                #create a function object (with function) and bind it to a name with <-
  sin(1 / x ^ y)
}
typeof(f01)                                          #functions also called closures

lapply(mtcars, function(x) length(unique(x)))        #anonymous function

#put functions in a list
funs <- list(
  half = function(x) x / 2,
  double = function(x) x * 2
)
funs$half(2)

#three components of function
formals(f01)                                          #the list of arguments that control how you call the function
body(f01)                                             #the code inside the function
environment(f01)                                      #the data structure that determines how the function finds the values associated with the names

attributes(f01)                                       #attribute 'srcref'(source reference), unlike body(), it contains code comments and other formatting

#primitive(原始的) functions: like sum() and [, call C code directly.
#These functions exist primarily in C, not R, so their formals(), body(), and environment() are all NULL
sum; typeof(sum)
`[`; typeof(`[`)

#pipe(%>%, pronounced as 'and then') with function call
library(magrittr)
x <- runif(100)
x %>% sqrt() %>% mean() %>% sum()

##################################4.1 Lexical scoping##################################
#R uses lexical scoping, it’s a technical CS term that tells us that the scoping rules use a parse-time, rather than a run-time structure.
#R’s lexical scoping follows four primary rules:
#1. Name masking
#2. Functions vs. variables
#3. A fresh start
#4. Dynamic lookup


# NAME MASKING
#The basic principle of lexical scoping is that names defined inside a function mask names defined outside a function.
x <- 10
y <- 20
g02 <- function() {
  x <- 1
  y <- 2
  c(x, y)
}
g02()

#If a name isn’t defined inside a function, R looks one level up.
x <- 2
g03 <- function() {
  y <- 1
  c(x, y)
}
g03()

#The same rules apply if a function is defined inside another function.(all the way up to the global environment), Finally, it looks in other loaded packages.
x <- 1
g04 <- function() {
  y <- 2
  i <- function() {
    z <- 3
    c(x, y, z)
  }
  i()
}
g04()


# FUNCTIONS VS. VARIABLES
#In R, functions are ordinary objects. This means the scoping rules described above also apply to functions
g07 <- function(x) x + 1
g08 <- function() {
  g07 <- function(x) x + 100
  g07(10)
}
g08()


# A FRESH START
g11 <- function() {
  if (!exists("a")) {
    a <- 1
  } else {
    a <- a + 1
  }
  a
}

g11()
a <- g11()
g11()


# DYNAMIC LOOKUP
#Lexical scoping determines where, but not when to look for values. 
#R looks for values when the function is run, not when the function is created. 
g12 <- function() x + 1
x <- 15
g12()
x <- 20
g12()

codetools::findGlobals(g12)            #This function lists all the external dependencies (unbound symbols) within a function

environment(g12) <- emptyenv()         #manually change the function’s environment to the emptyenv(), an environment which contains nothing
g12()


##################################4.2 Lazy evaluation##################################
#In R, function arguments are lazily evaluated: they’re only evaluated if accessed. 
#This is an important feature because it allows you to do things like include potentially expensive computations in function arguments that will only be evaluated if needed.
h01 <- function(x) {
  10
}
h01(stop("This is an error!"))             #no error


# FORCING EVALUATION
h02 <- function(x) {
  force(x)
  10
}
h02(stop("This is an error!"))             #error


# PROMISES
#Lazy evaluation is powered by a data structure called a promise
#A promise has three components:
#1. The expression, like x + y which gives rise to the delayed computation.
#2. The environment where the expression should be evaluated.
#3. The value, which is computed and cached the first time a promise is accessed when the expression is evaluated in the specified environment.

#The value cache ensures that the promise always returns the same value, even when it’s accessed multiple times.
h06 <- function(x) { 
  c(x, x, x)  
}

h06(runif(1))

h06 <- function(x,Y) { 
  c(x, x, Y,Y)  
}

h06(runif(1),runif(1))


# DEFAULT ARGUMENTS
#Thanks to lazy evaluation, default values can be defined in terms of other arguments, or even in terms of variables defined later in the function:
h07 <- function(x = 1, y = x * 2, z = a + b) {
  a <- 10
  b <- 100
  
  c(x, y, z)
}

h07()


# MISSING VALUE
#To determine if an argument’s value comes from the user or from a default, you can use missing()
h09 <- function(x = 10) {
  list(missing(x), x)
}
str(h09())
str(h09(10))

#functions in base R used missing(), e.g. sample()
args(sample)
sample(10)                            #arg 'size' do not have defualt value, it use missing in function
body(sample)

##################################4.3 ... (dot-dot-dot)##################################
#A function with the special argument ... can take any number of additional arguments. 
i04 <- function(...) {
  list(...)
}
str(i04(a = 1, b = 2))

#There are two primary uses of ...
#1. If your function takes a function as an argument, you want some way to pass additional arguments to that function
x <- list(c(1, 3, NA), c(4, NA, 6))
str(lapply(x, mean, na.rm = TRUE))
args(lapply)

#2. If your function is an S3 generic, you need some way to allow methods to take arbitrary extra arguments. 
args(print)
print(factor(letters), max.levels = 4)
print(y ~ x, showEnv = TRUE)


##################################4.4 Exiting a function##################################
#Most functions exit in one of two ways: 
#1. they either return a value, indicating success, 
#2. or they throw an error, indicating failure. 

# IMPLICIT VS. EXPLICIT RETURNS
#There are two ways that a function can return a value:
#1. Implicitly, where the last evaluated expression is the return value
j01 <- function(x) {
  if (x < 10) {
    0
  } else {
    10
  }
}
j01(5)
#2. Explicitly, by calling return()
j02 <- function(x) {
  if (x < 10) {
    return(0)
  } else {
    return(10)
  }
}
j02(5)


# INVISIBLE VALUES
#Most functions return visibly
j03 <- function() 1
j03()

j04 <- function() invisible(1)                 #prevent automatic printing by applying invisible() to the last value
j04()
print(j04())
(j04())
a <- j04()
a


# ERRORS
#If a function cannot complete its assigned task, it should throw an error with stop()
j05 <- function() {
  stop("I'm an error")
  return(10)
}
j05()


##################################4.5 Function forms##################################
#To understand computations in R, two slogans are helpful:        — John Chambers
#1. Everything that exists is an object
#2. Everything that happens is a function call

#Function calls come in four varieties:
#1. prefix
sum(c(1:10))
#2. infix -  these forms are used for many mathematical operators
1 + 2
#3. replacement - functions that replace values by assignment, like names(df) <- c("a", "b", "c")
#4. special - functions like [[, if, and for.


# REWRITING TO PREFIX FORM
#An interesting property of R is that every infix, replacement, or special form can be rewritten in prefix form.
1 + 2
`+`(1,2)

df <- data.frame(a = 1, b = 2)
names(df) <- c("c","d")
df <- `names<-`(df,c("e","f"))

for(i in 1:10) print(i)
`for`(i, 1:10, print(i))

x <- c(1:10)
x[7]
`[`(x,7)

##################################5. Environments##################################
library(rlang)
# The environment is the data structure that powers scoping.

##################################5.1. Basics##################################
#Generally, an environment is similar to a named list, with four important exceptions:
#1. Every name must be unique.
#2. The names in an environment are not ordered (i.e. it doesn’t make sense to ask what the first element of an environment is).
#3. An environment has a parent.
#4. Environments are not copied when modified.

#Create an environment
e1 <- env(
  a = FALSE,
  b = "a",
  c = 2.3,
  d = 1:3
)
e1                     #Printing an environment just displays its memory address
env_print(e1)          #get general information of the env
env_names(e1)          #to get a character vector giving the current bindings
e1$c + 3               #get
e1[["c"]]
e1$f <- "andy"         #set
env_poke(e1, "g", 100) #another way to add bindings to an environment
env_bind(e1, h = 27, j = "Yuan")  #allows you to bind multiple values
env_has(e1, "d")                  #check if a variable already exists in the env
env_unbind(e1,"d")                #remove a variable in the env


e1$e <- e1             #an environment can contain any object, including itself

#global environment is sometimes called your “workspace”
identical(current_env(), global_env())    #to compare environments, use identical(), not ==


#parent
#parent is what’s used to implement lexical scoping: if a name is not found in an environment, then R will look in its parent 
e2a <- env(d = 4, e = 5)
e2b <- env(e2a, a = 1, b = 2, c = 3)       #第一个参数是'parent env'
identical(env_parent(e2b), e2a) 
env_parent(e2a)
env_parents(e2b)                          

empty_env()
env_parent(empty_env())      #only empty environment do not have parent


# SUPER ASSIGNMENT
#Regular assignment, <-, always creates a variable in the current environment. 
#Super assignment, <<-, never creates a variable in the current environment, but instead modifies an existing variable found in a parent environment.
x <- 0
f1 <- function(){
  x <- 1
}
f1();x

f2 <- function(){
  x <<- 1
}
f2();x               #If <<- doesn’t find an existing variable, it will create one in the global environment.


# ADVANCED BINDINGS
#1. env_bind_exprs() creates delayed bindings, which are evaluated the first time they are accessed. 
#Delayed bindings create promises, so behave in the same way as function arguments
env_bind_exprs(current_env(), b = {Sys.sleep(1); 1})
system.time(print(b))
system.time(print(b))

#2. env_bind_fns() creates active bindings which are re-computed every time they’re accessed
env_bind_fns(current_env(), z1 = function(val) runif(1))
z1

env_bind_fns(current_env(), z2 = function(val) {
  if (missing(val)) {
    2
  } else {
    stop("Don't touch z2!", call. = FALSE)
  }
})
z2
z2 <- 3

##################################5.2. Package Environments##################################
#Each package attached by library() or require() becomes one of the parents of the global environment.
library(rlang)
env_parent(global_env())
#The immediate parent of the global environment is the last package you attached
library(ggplot2)
env_parent(global_env())
#And the parent of the last attached package is the second to last package you attached; 这也是为什么我一般会最后attach package tidyverse (consider 'search path')
env_parent(env_parent(global_env())) 


# SEARCH PATH
search_envs()          

##################################5.3. Function Environments##################################
y <- 1
f <- function(x) x + y
fn_env(f)

##################################5.4. Namespaces##################################
# QUESTION:
#How to ensure that package code always works the same way regardless of what packages have been attached by the user in R_GlobalEnv

#Every function in a package is associated with a pair of environments:  (e.g. environment: namespace:stats)
#1. package environment   - external interface to the package, controls how we find the function(attahced package or ::); Its parent is determined by search path
#2. namespace environment - internal interface to the package, controls how the function finds its variables
fn_env(sd)
fn_env(var)
sd
var

# NOTES:
#Every binding in the package environment is also found in the namespace environment; this ensures every function can use every other function in the package. 
#But some bindings only occur in the namespace environment. These are known as internal or non-exported objects, which make it possible to hide internal implementation details from the user

# Every namespace environment has the same set of ancestors:
#1. Each namespace has an imports environment that contains bindings to all the functions used by the package. 
#   The imports environment is controlled by the package developer with the NAMESPACE file.
#2. Parent of the imports environment is the base namespace. 
#3. The parent of the base namespace is the global environment. 
#   This means that if a binding isn’t defined in the imports environment the package will look for it in the usual way. 
#   This is usually a bad idea (because it makes code depend on other loaded packages),so R CMD check automatically warns about such code.

# Summary: (function 'sd' in package 'stats' as example)
# sd -> namespace:stats -> imports:stats -> namespace:base -> R_GlobalEnv -> (search path)

##################################5.5. The caller environment##################################
# CALLER Environment
#This provides the environment from which the function was called, and hence varies based on how the function is called, not how the function was created.
rlang::caller_env()          

##################################5.6. Env as data structure##################################
#Environments are also useful data structures in their own right because they have reference semantics., There are three common problems that they can help solve:
#1. Avoiding copies of large data.
#2. Managing state within a package.
#3. As a hashmap

##################################6. Condition##################################
library(rlang)

##################################6.1. Signalling conditions##################################
#There are three conditions that you can signal in code: 
#1. errors - indicate that there is no way for a function to continue and execution must stop
#2. warnings - indicate that something has gone wrong but the function has been able to at least partially recover
#3. messages
#Another: interrupt - indicates that the user has “interrupted” execution by pressing Escape, Ctrl + Break, or Ctrl + C
stop("This is what an error looks like")
warning("This is what a warning looks like")
message("This is what a message looks like")

# ERROR
f <- function() g()
g <- function() h()
#h <- function() stop("This is an error!", call. = FALSE)
h <- function() abort("This is an error!")                     #rlang::abort(): equivalent to stop()
f()

# WARNING
fw <- function() {
  cat("1\n")
  warning("W1")
  cat("2\n")
  warning("W2")
  cat("3\n")
  warning("W3")
}
fw() #By default, warnings are cached and printed only when control returns to the top level

# MESSAGE
fm <- function() {
  cat("1\n")
  message("M1")
  cat("2\n")
  message("M2")
  cat("3\n")
  message("M3")
}
fm()                            #messages() are displayed immediately

##################################6.2. Ignoring conditions##################################
# The simplest way of handling conditions in R is to simply ignore them:
#1. Ignore errors with try().
#2. Ignore warnings with suppressWarnings().
#3. Ignore messages with suppressMessages().
f1 <- function(x) {
  log(x)
  10
}
f1("a")                 

f2 <- function(x) {
  try(log(x))
  10
}
f2("a")

#messages and warnings don’t terminate execution


##################################6.3. Handling conditions##################################
#Every condition has default behaviour: 
#1. errors stop execution and return to the top level, 
#2. warnings are captured and displayed in aggregate, 
#3. messages are immediately displayed. 

#Condition handlers allow us to temporarily override or supplement the default behaviour. - tryCatch, withCallingHandler


# CONDITION OBJECTS
cnd <- catch_cnd(stop("An error")) 
str(cnd)
conditionMessage(cnd)   #message: the text to display
conditionCall(cnd)      #call: trigger the condition 
attr(cnd,"class")       #class attribute: S3 object, it determines which handlers will match the condition


# EXITING HANDLER
#tryCatch() registers exiting handlers, and is typically used to handle error conditions. It allows you to override the default error behaviour.
f3 <- function(x){
  tryCatch(
    {
      x <- x * 10
      return(log(x))
    },
    error = function(cnd) NA,          #exiting handlers
    finally = {
     
    }
  )
}
f3(1)
f3("x")


tryCatch(
  { 
    1 + 1
    stop("Here is a error")         #condition is signalled
  },
  error = function(cnd){            #handler function(single argument:condition object)
    paste0("--,",conditionMessage(cnd))
  },           
  finally = {                       #a block of code (not a function), This can be useful for clean up, like deleting files, or closing connections. 
    print("Finished")
  }
)


# CALLING HANDLER
#handlers set up by tryCatch() are called exiting handlers, 
#in contrast, withCallingHandlers() sets up calling handlers: code execution continues normally once the handler returns. 
tryCatch(
  message = function(cnd) cat("Caught a message!\n"), 
  {
    message("Someone there?")
    message("Why, yes!")
  }
)

withCallingHandlers(
  message = function(cnd) cat("Caught a message!\n"), 
  {
    message("Someone there?")
    message("Why, yes!")
  }
)

##################################7. Functionals##################################
library(purrr)

#A functional is a function that takes a function as an input and returns a vector as output.
randomise <- function(f) f(runif(1e3))
randomise(mean)
randomise(sum)
#A common use of functionals is as an alternative to for loops. like: apply() in R base, map() in purrr

##################################7.1 purrr::map()##################################
# map(): takes a vector and a function, calls the function once for each element of the vector, and returns the results in a list.
# map(1:3, f) is equivalent to list(f(1), f(2), f(3))
triple <- function(x) x*3
map(1:10, triple)             #return a list with the same length of x(here x = 1:10)
lapply(1:10, triple)

map_dbl(1:10, triple)         #return a double atomic vector
map_chr(mtcars,typeof)        #return a charactor atomic vector
map_int(mtcars, function(x){length(unique(x))})      #data frames are lists containing vectors of the same length
map_int(mtcars, ~ length(unique(.x)))                #shortcut of anonymous function

#the following two forms getting the same result
x <- list(1:5, c(1:10,NA))
map(x, ~mean(.x, na.rm = T))
map(x, mean, na.rm = T)    #because the map functions pass '...' parameter, all parameters after .f will be used as .f(x[i],params)

#the defference between above two forms
plus <- function(x, y) {x + y}
x <- c(0,0,0,0)
map_dbl(x, plus, runif(1))        #extra parameter(here is y=runif(1)) will be evaluated only one time
map_dbl(x, ~plus(.x, runif(1)))   #extra parameter(here is y=runif(1)) will be evaluated every time f() is executed


# PURRR STYLE
by_cyl <- split(mtcars,mtcars$cyl)
str(by_cyl)

by_cyl %>%
  map(~ lm(mpg~wt, data = .x)) %>%
  map(coef) %>%
  map_dbl(2)


##################################7.2 Map Variants##################################
#map_int, map_dbl... are kind of map variant, there are other variants:
#1. modify(): Output same type as input
#2. map2(): Iterate over two inputs
#3. imap(): Iterate with an index
#4. walk(): Return nothing
#5. pmap(): Iterate over any number of inputs 

# MODIFY()
#requirement: double every column in a data frame
df <- data.frame(
  x = 1:3,
  y = 6:4
)

dob <- function(x) x * 2
map(df,~dob(.x))
modify(df, ~dob(.x))

# MAP2()
#requirement: calculate weighted mean
xs <- map(1:8, ~ runif(10))
ws <- map(1:8, ~ rpois(10,5)+1)
map(xs, mean)
map2(xs,ws,weighted.mean)
map2_dbl(xs,ws,function(x,y){mean(x*y)})


# IMAP()
x <- map(1:6, ~ sample(1000, 10))
imap_chr(x, ~ paste0("The highest value of ", .y, " is ", max(.x)))

##################################7.3 Reduce##################################
#reduce() takes a vector of length n and produces a vector of length one by calling a function with a pair of values at a time: 
#reduce(1:4, f) is equivalent to f(f(f(1, 2), 3), 4)
sum(1:4)
reduce(1:4,sum)

#requirement:have a list of numeric vectors, to find the values that occur in every element.
l <- map(1:10, ~sample(1:10,30,replace = T))
reduce(l,base::intersect)

# ACCUMULATE
x <- c(1:10)
reduce(x,`+`)
accumulate(x,`+`)

##################################7. Function factory##################################
#A function factory is a function that makes functions.
library(rlang)
library(ggplot2)
library(scales)




##################################8. Factory fundamentals##################################
power1 <- function(exp) {
  function(x) {
    x ^ exp
  }
}

square <- power1(2); square           #manufactured functions 
cube <- power1(3);   cube
square(5)
cube(5)

env_print(square)
env_print(cube)
fn_env(square)$exp
fn_env(cube)$exp
#This is what makes manufactured functions behave differently from one another: names in the enclosing environment are bound to different values.

##################################9. OOP##################################
library(sloop)
# OOP features:
#1. 多态 - polymorphism      
#2. 封装 - encapsulation
#3. 继承 - inherit 

# There are two main paradigms(范式) of object-oriented programming：
#1. encapsulated OOP: methods belong to objects or classes, and method calls typically look like object.method(arg1, arg2).
#2. functional OOP: methods belong to generic functions, and method calls look like ordinary function calls: generic(object, arg2, arg3).

# OOP in R:
#Base R provides three OOP systems:
#1. S3 - R’s first OOP system, is an informal implementation of functional OOP
#2. S4 - is a formal and rigorous rewrite of S3
#3. RC(reference classes) - implements encapsulated OO
#A number of other OOP systems are provided by CRAN packages:
#1. R6 - implements encapsulated OOP like RC
#2. R.oo
#3. proto 

# SLOOP PACKAGE
sloop::otype(mtcars)

##################################9.1 Base Types##################################
# Base object vs. OO object
is.object(1:10); sloop::otype(1:10); attr(1:10,"class")             #A base object
is.object(mtcars); sloop::otype(mtcars); attr(mtcars, "class")      #An OO object
#Technically, the difference between base and OO objects is that OO objects have a “class” attribute

#While only OO objects have a class attribute, every object has a base type
typeof(1:10)
typeof(mtcars)

# There are 25 different base types:
#1. Vectors - NULL, logical, integer, double, complex, character, list, raw
#2. Functions - closure(regular R functions), special(internal functions), builtin(primitive functions)
#3. Environments - environment 
#4. S4 type -  is used for S4 classes that don’t inherit from an existing base type
#5. Language components - symbol, language, pairlist

#Notes: Never use function 'mode()'

##################################9.2 S3##################################
#An S3 object is a base type with at least a “class” attribute
f <- factor(c("a","b","c"))
typeof(f)
attr(f,"class")
attributes(f)

unclass(f) #get the “underlying” base type 



##################################%%%%%%%%%%%%%%%%%%%%%R for Data Science(Hadley)%%%%%%%%%%%%%%%%%%%%%########################################################
# Import->Tidy->Understand(Transform<->Visualise<->Model)->Communicate
library(tidyverse)

##################################1. Data Visualization - ggplot##################################

##################################2. Data Transformation - dplyr##################################

##################################3. Exploratory Data Analysis##################################
# 1. Generate questions about your data.
# 2. Search for answers by visualising, transforming, and modelling your data.
# 3. Use what you learn to refine your questions and/or generate new questions.

##################################4. Tibble##################################

##################################5. Data Import##################################

##################################6. Tiddy##################################

##################################7. String - stringr##################################

##################################8. Dates & Times - lubridate##################################

##################################9. Pipes - magrittr##################################

##################################10. Model - modelr##################################

##################################11. Communicate - Markdown/Shiny##################################


##################################%%%%%%%%%%%%%%%%%%%%%R Package(Hadley)%%%%%%%%%%%%%%%%%%%%%########################################################
#####Basic structure of a package:
###'1. R Code
###'2. Package metadata
###'3. Documentation
###'4. Vignette
###'5. Tests
###'6. Namespace
###'7. External data 
###'8. Compiled code
###'9. Other components

version
install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
devtools::install_github("r-lib/devtools")
library(devtools)
devtools::session_info()

setwd("C:/YuanLe/R/RWkDir/Packages/")
#create a new package(can use RStudio, like creating project, or the following)
devtools::create("andyp")

#####What's a package?
###'1. Source package - the development version of a package that lives on the computer;A source package is just a directory with components like R/, DESCRIPTION, and so on
###'2. Bundled package - A bundled package is a package that’s been compressed into a single file. (.tar.gz); call devtools::build() to make it
###'3. Binary package - use devtools::build(binary = TRUE) to make a binary package
###'4. Installed package - An installed package is just a binary package that’s been decompressed into a package library
###'5. In memory package - 

#####What's a library?
###A library is simply a directory containing installed packages
.libPaths()
dir(.libPaths())
#When using library(pkg) or require(pkg) to load a package, R looks through each path in .libPaths() to see if a directory called pkg exists. 

##################################%%%%%%%%%%%%%%%%%%%%%知识点%%%%%%%%%%%%%%%%%%%%%########################################################
##################################0. 待学堆栈##################################
#--梯度下降
#--梯度推进(boosting)
#--AdaBoosting
#
##################################1. 科学计数##################################
1e+5 == 100000             #10万的两种表达方式
1.03e-2; 1.03e+2           #e-2表示10^-2; e+2表示10^2

##################################2. 无理数e##################################
#########e的定义:
#######当n趋近于无穷大, (1+1/n)^n的极限就是e; 
n <- 100000
e_ <- (1 + 1/n) ^ n     #e是一个无限不循环小数，其值是2.71828...; e也是自然对数的底数
e <- exp(1)             #exp(1)是R中e的表达式; exp(x) = e ^ x

#########e的由来:
#######e和计算银行利息有关(复利计息)
base <- 10000                        #本金10000元
n <- 20                              #存银行20年
I <- .05                             #年利率5%
Total1 <- base*(1+I*n)               #按单利结算, 20年后可获得的本息总额20000
Total2 <- base*(1+I)^n               #按年复利结算, 20年后可获得的本息总额26533  
Total3 <- base*(1+I/12)^(n*12)       #按月复利结算, 20年后可获得的本息总额27126.4


##################################3. 单变量概括性度量##################################
#########A.数值型变量
x <- mtcars$mpg 
#######A.1. 五数概括
summary(x)      
ggplot(data = data.frame(x = x), aes(x = x)) + geom_density() + geom_vline(aes(xintercept = mean(x)),colour = "red") 
#######A.2. 偏度(skewness) - 统计数据分布偏斜方向和程度的度量，是统计数据分布非对称程度的数字特征值
DescTools::Skew(x); 
DescTools::Skew(rnorm(1000,0,1))

#######A.3. 峰度(Kurtosis) - 表征概率密度分布曲线在平均值处峰值高低的特征值;直观看来，峰度反映了峰部的尖度值
#                            样本的峰度是和正态分布相比较而言的统计量，如果峰度大于3，峰的形状比较尖，比正态分布峰要陡峭。反之亦然
#                            峰度高就意味着方差增大是由低频度的大于或小于平均值的极端差值引起的
x1 <- rnorm(10000,0,3)
x2 <- c(x1, rnorm(2000,9,3))
DescTools::Kurt(x1); DescTools::Kurt(x2)
ggplot(data = data.frame(x = c(x1,x2), K = c(rep("K1",10000),rep("K2",12000))), aes(x = x, colour = K)) + geom_density()

#######A.4. 分布 (histgoram/density)


#########B. 分类型变量
#######列联表
y <- as.character(mtcars$gear)
table(y)
prop.table(table(y))

##################################4. 多变量相关关系分析##################################
#########连续变量
ds <- data.frame(x = c(1:100), y = (sample(1:100,100)))
cor(ds, use = "everything", method = "pearson")                 #相关系数
cov(ds)                                                         #协方差
cov(scale(ds$x),scale(ds$y))                                    #标准化后的协方差 = 相关系数
#--协方差cov(A,B) = SUM( (A-mean(A))*(B-mean(B)) ) / (n-1); 跟方差的计算公式很类似

#########分类型变量
library(vcd)
mytable <- xtabs(~Treatment+Improved, data = Arthritis)
chisq.test(mytable, correct = T)                                #卡方检验  

#########分类型变量与连续型变量 -                               #协方差分析
str(multcomp::cholesterol)
range(multcomp::cholesterol$response)
fit <- aov(response~trt, data = multcomp::cholesterol)
summary(fit)  #F检验非常显著, 说明五种疗法的效果不同.
#---图形展示相关性
gplots::plotmeans(response~trt, 
                  data = multcomp::cholesterol,
                  xlab = "Treatment", ylab = "Response", 
                  main = "Mean Plot\nwith 95% CI")

par(las = 2)
par(mar = c(5,8,4,2))
plot(TukeyHSD(fit))

#########偏相关系数
######偏相关系数是在对其他变量的影响进行控制的条件下,衡量多个变量中某两个变量之间的线性相关程度的指标
ppcor::pcor(mtcars); 

##################################5. 数据的标准化和归一化##################################
ds <- cbind(A = c(1,2,3,4), B = c(15,15,30,60), C= c(30,60,90,120))

#########中心化:用原始数据集中的每一个数减去数据集的平均???
scale(ds,center = T, scale = F)

#########标准化: 中心化之后的数据再除以数据集的标准差
scale(ds,center = T, scale = T)

#########归一化[0,1]:用数据集中的每个数据减去数据集中的最小???,再除以这个数据集的极???
#有一个问题是数据集中新数据的加入可能需要全部重新计???
(ds[,1] - min(ds[,1])) / (max(ds[,1]) - min(ds[,1]))

##################################6. 损失函数############################
#####什么是损失函数以及其作???? https://blog.algorithmia.com/introduction-to-loss-functions/
#####在机器学习中，损失函数就是用来衡量模型的预测能力.它的通用函数公式如下???
L(w,d) = 1/N * SUM( (yi - f(xi))^2 ) + r(d)
#####1. f(xi)就是模型对输入xi的预测???, 及yi_
#####2. N 表示观测的数???
#####3. 函数中的第一??? 1/N * SUM( (yi - f(xi))^2 ) 代表的是模型预测的误??? (在线性回归模型中，就是残差平方和)
#####4. 函数中的第二??? r(d) 代表的是模型的复杂度(或称过拟合程???) (在分类树中我们通过不停的分裂可以创建一个对训练???100%精准的模???,
#       但这样的模型必然存在过拟合问题，复杂度就是用来衡量一个模型是否过拟合)
#####总结: 当我们创建机器学习模型时，一定既要考虑模型的精准度，也要考虑模型是否存在过拟合情???; 即，理想情况下，我们总是希望能创???
#     又简单同时预测误差又很小的模???
#####最小化损失函数(即模型的误差要小，同时不能过拟合???)是我们在创建机器学习模型过程中的一个目???.

#####每种机器学习模型的损失函数计算方法不一样，
#####对于线性回归模型，L(w,d) = 残差平方??? + |W|(|W|代表模型中的变量数量)
#####对于分类树模???, 参加https://www.zhihu.com/question/34075616 ??? https://www.zhihu.com/question/20924039/answer/131421690

##################################7. 交叉验证和自主抽样法############################
####k-择交叉验???(k-fold cross validation) - 初始数据集D随机地划分成k个互不相交的k个子集Di(i->[1:k]每个子集的数据量差不???), 
####训练和检验进行k???, 在第i次迭代中，分区Di用作检验集，其余的子集的集合一起作为训练集.
####留一(leave-one-out) - 是k-fold cross validation的一种特殊情况，k的值被设置为nrow(D), 这样每次都只有一个观测被作为检验集???
D <- rnorm(n = 10000, mean = 0, sd = 1)
k <- 10
rseq <- sample(1:10000, size = 10000, replace = F)
D1 <- D[rseq[1:1000]]; D2 <- D[rseq[1001:2000]]; D3 <- D[rseq[2001:3000]]; D4 <- D[rseq[3001:4000]]; D5 <- D[rseq[4001:5000]];
D6 <- D[rseq[5001:6000]];D7 <- D[rseq[6001:7000]];D8 <- D[rseq[7001:8000]];D9 <- D[rseq[8001:9000]];D10 <- D[rseq[9001:10000]];

Test1 <- D1
Train1 <- D[-index(Test1)]

####自助抽样法(bootstrap) - 从初始数据集D中有放回的均匀抽样。这样任意一个元组(观测)都有可能在多个训练集中出现
####.632自助法 - 设一个数据集D包含d个元组，对该数据集进行有放回的抽样d次，产生d个训练集(或称自主样本).
####.632自助法中，每个元组每次被抽中的概率是1/d, 不被抽中的概率是(1-1/d), d次抽样都不被抽中的概率是(1-1/d)^d, 当d趋近无穷大,
#### (1-1/d)^d 就约等于1/e = 0.368. 因此在平均情况下，将36.8%的元组不会被选中到训练集中，他们将形成检验集.
####进行k次自助抽样，得到k个训练集(平均地每次都有63.2%的数据被作为训练集，36.8%的数据被作为检验集)
D <- rnorm(n = 10000, mean = 0, sd = 1)
d <- length(D)
Train1 <- unique(sample(D, size = d, replace = T))
Test1 <- D[-index(Train1)]
length(Train1)/d   #0.6285
Train2 <- unique(sample(D, size = d, replace = T))
Test2 <- D[-index(Train2)]
length(Train2)/d   #0.6262


##################################9. one-hot encode(独热编码)############################
#########one-hot code: 有多少个状态就有多少比特，而且只有一个比特为1，其他全为0的一种编码方法
#########作用: 在机器学习中，用来对离散型分类变量进行编码处理
head(diamonds)                                             #原始数据
head(model.matrix(~cut-1,data = diamonds))                 #返回一个矩???, 矩阵的行???=数据框diamonds的行???, 矩阵的列???=被编码的变量的因子长???,结合head(diamonds)???
head(cbind(dplyr::select_if(diamonds,is.numeric),
           as.data.frame(model.matrix(~cut-1,diamonds))))  #将编码后的变量和全数据集结合起来


#--创建稀疏矩???
head(sparse.model.matrix(~cut-1,data = diamonds)) 

##################################10. 稀疏矩阵 vs. 稠密矩阵############################
#########稀疏矩阵(sparse matrix): 矩阵中非零元素的个数远远小于矩阵元素的总数，并且非零元素的分布没有规律
#########稠密矩阵(dense matrix):  矩阵中非0元素占所有元素比例较大的矩阵称为稠密矩阵

##################################11. 积分/(偏)导数/方向导数/梯度##################################
#########积分和导数都涉及到求极限的问???

#######设函数f(x)如下(标准正态分布的概率密度曲线)，它在x->[a,b]之间是连续的
x <- rnorm(10000,0,1)
f.x <- 1/sqrt(2*pi) * (2.7183 ^ ( -1/2*(x^2) ) )    
ds.x <- data.frame(x=x,y=f.x) 
ggplot(ds.x, aes(x=x,y=y))+geom_line()   #f.x对应的曲???

#########AAAAA.积分
#######现要求x->[a,b]与曲线f.x围成的面???? 
#######方法: 将x在[a,b]之间分成n段，每段的距离mi = xi-x(i-1), 并使mi无限趋近???0
#######面积F(a,b) = sum(f(ti)*mi ) (ti是xi到x(i-1)之间的任意点)
integrate(f = function(x)1/sqrt(2*pi) * (2.7183 ^ ( -1/2*(x^2) ) ), lower = -2, upper = 2)   #求f.x在[-2,2]之间的积???

#########BBBBB.导数(Derivative)
#######现要求在点P(x,f.x)处的切线(即求切线与x正轴的夹角a)?
#######方法: 取曲线上与P点相邻的点P1(x1,f.x1)(使P1无限趋近P???), 可以求得同时通过P，P1点的直线与x正轴的夹???; 
#######P点的切线夹角的正切值tan(a) = (f.x1 - f.x) / (x1-x)  (x1无限趋近于x)
#########由上述例子可总结导数的定义：
#######1. 当函数y=f(x)的自变量x在一点x0上产生一个增量Δx时，函数输出值的增量Δy与自变量增量Δx的比值在Δx趋于0时的极限a如果存在???
#      a即为函数f(x)在x0处的导数，用导函数f'(x)表示
#######2. 导数反映的是一元函数f(x)的变化率
Deriv::Deriv(f = function(x)1/sqrt(2*pi) * (2.7183 ^ ( -1/2*(x^2) ) ), "x")


#########CCCCC.偏导???(partial Derivative)
#######函数f(xa,xb,...xn)的偏导数, 就是它关于其中一个变量的导数而保持其他变量恒???
#######偏导数反映的是多元函数f(xa,xb,...xn)沿xi轴正方向的变化率???
#######以二元函数z=f(x,y)为例??? ???(x0,y0)附近的一???(x1,y0)表示固定y，而x的增量是Δx(x1-x0,无限趋近???0)；相应地z有增???
#      Δz(称为z对x的偏增量), Δz = f(x1,y0)-f(x0,y0), 如果Δz与Δx之比在当Δx???0时的极限存在???
#      那么此极限值称为函??? z=f(x,y) ??? (x0,y0)处对 x 的偏导数，记??? f'x(x0,y0)
Deriv::Deriv(f = function(x,y) sin(x^2) * y, "x")


#########DDDDD.方向导数(directional Derivative)
#########定义: 点P(x,y)是函数z=f(x,y)上的点，自点P引射线l, x正轴到射线l的转角为a, P'(x+Δx, y+Δy)为l上的另一???, 同时定义p
#        p = sqrt(Δx^2 + Δy^2); ???( (f(x+Δx,y+Δy) - f(x,y)) / p ) 在p无限趋近???0时的极限存在, 则称该极限为f(x,y)在点P???
#        方向l的方向导???; 记作∂f/∂l = ∂f/∂x * cos(a) + ∂f/∂y*sin(a)  (∂f/∂x表示函数z对x的偏导数, ∂f/∂y表示函数z对y的偏导数)
#########方向导数放映的是多元函数f(xa,xb,...xn)在点P沿某一方向l的变化率问题.
#########例子: 求函数z=x*e^2y 在点P(1,0) 处沿从点P(1,0)到P1(2,-1)方向的方向导???.
#######1. 算出射线l的转角a = - pi / 4
#######2. 求函数z在点P(x=1,y=0)偏导??? ∂f/∂x = 1;  ∂f/∂x = 2;
#######3. 求方向导??? ∂f/∂l = 1*cos(-pi/4) + 2*sin(-pi/4)  =  -0.707


#########EEEEE.梯度(gradient)
#######定义: 设多元函数z=f(x,y)在平面区域D上具有一阶连续偏导数，则对每一个点P(x,y)都可以定出一个向???:
#      {∂f/∂x * i + ∂f/∂x * j}, 这个向量就是函数f(x,y)在点P的梯???, 记作grade_f(x,y);
#######定理: 函数z=f(x,y)在某一点P(x,y)的梯度grade_f(x,y)是一个向???, 这个向量的方向与取得最大方向导数的方向一致，
#      它的???(即梯度的长度)为方向导数的最大???, 表示为|grade_f(x,y)| = sqrt( (∂f/∂x)^2 + (∂f/∂y)^2 )
#      也就是说梯度的方向是函数z在点P增长最快的方向. 
#      总结: 求一个函数z在某点P的梯度就是求使函数z在点P增长最快的方向向量.
#      设想: 在一个曲线或曲面上选定一个点, 我们希望找到一系列方向向量的组合，使能最???(最???)地到达另一个点. 就是迭代求梯度的过程.


##################################12. 缺失值的处理方式##################################
#########https://www.cnblogs.com/magle/articles/6110195.html

#########处理不完备数据集的方法主要有以下三大???
#######A. 删除元组: 将存在遗漏信息属性值的对象（元组，记录）删???
#######B. 数据补齐
#####B.1. 人工填写
#####B.2. 特殊值填充 - 如所有的空值都用“unknown”填充???
#####B.3. 平均值填充（Mean/Mode Completer???
###B.3.1. 数值型空值 - 用非空的值的平均值来填补空???
###B.3.2. 分类型空值 - 用该属性在其他所有对象的取值次数最多的值来填充空???
#####B.3. 热卡填充（Hot deck imputation，或就近补齐???- 在完整数据中找到一个与它最相似的对象，然后用这个相似对象的值来进行填充???
#####B.4. K最近距离邻法（K-means clustering???
#####B.5. 使用所有可能的值填充（Assigning All Possible values of the Attribute???
#####B.6. 回归（Regression???- 在某个属性缺失值较多的时候使???. e.g. Titanic 的年龄属性的控制填补就用了GBDT回归预测
#####B.7. 期望值最大化方法（Expectation maximization，EM???,也叫极大似然估计
#####B.8. 多重填补（Multiple Imputation，MI???
#######C. 不处理 - 直接在包含空值的数据上进行数据挖掘。这类方法包括贝叶斯网络和人工神经网络等???

##################################13. 模型的损失函数及性能评估度量(loss & eval_metric)##################################
#########首先一定要搞清楚有监督的机器学习中常用的术语及其区???, e.g. 特征, 响应变量, 假设函数, 目标函数(损失函数+模型复杂???(正则???))
#########机器学习模型(算法)的目标就是最小化目标函数, 因此我可以说模型(算法)的性能评估度量就是模型(算法)的损失函数???(模型复杂度很难评???)

#########分类和回归算法因为响应变量的类型不同而具有截然不同的损失函数，现介绍几种常用???(基于XGB的eval_metric参数???):
#--1. 回归损失函数 - 均方误差平方根RMSE(Root Mean Square Error)
#MSE = sum((yi - yi_)^2) / n  i = 1,2,3..n;

#--2. 回归损失函数 - 平均绝对误差MAE(Mean Absolute Error)
#MAE = sum(abs(yi-yi_)) / n   i = 1,2,3..n;

#--3. 回归损失函数 - 平滑的平均绝对误???(Huber损失) - 它是MES和MAE的综???, 当Huber损失在[0-δ,0+δ]之间时，等价为MSE，而在[-???,δ]和[δ,+∞]时为MAE; δ称为超参???

#--4. 逻辑回归损失函数 - logloss(对数似然损失函数) - 二分类逻辑回归算法常用的损失函???; 如果是多分类逻辑回归，则可以使用mlogloss作为损失函数

#--5. 分类损失函数 - error(二分类错误率) - #(wrong cases)/#(all cases); 如果是多分类，则可以使用merror作为损失函数

#--6. 逻辑回归损失函数 - auc(Area under the curve) - 用于计算ROC曲线面积

##################################14. 主成分分析的原理和概念##################################
library(ggplot2) 
set.seed(123)
x1=1:20+rnorm(10)
x2=1:20+rnorm(10)
(data_raw=data.frame(x1,x2)) 
ggplot(data_raw,aes(x1,x2)) + geom_point(col=2)

data_scale=scale(data_raw,scale=T) 

cov_data <- cov(data_scale)  #协方差矩阵
cor(data_raw)                #相关系数矩阵
#note: 标准化后的协方差矩阵跟相关系数矩阵一样

fea <- eigen(cov_data)         #note: 这个地方需要是标准化后的协方差矩阵
fea$values                     #相关矩阵的特征值
sum(fea$values)                #特性： 特征值的和=相关矩阵的对角线之和
#理解特征值：
#重新构建后的变量其方差即为两个特征值，第一个变量的方差占总方差比例的1.988/2= 0.994，也即能够解释总变异的99.4%，第二个变量仅能解释总变异的0.4%，

#处理这种强相关的两个变量的解决方案:
#1. 将第二个变量剔除掉，仅保留第一个变量用于分析，也不会对结果造成大的影响，这就是降维 。实现这个过程的方法就是所谓的 线性变换 。
#2. 正交变换

A <- fea$vectors  #A称为正交矩阵, 正交矩阵A的两个列向量称为 单位正交基  
data_scale_new <- data_scale %*% A   #正交变换：用正交矩阵与标准化后的原始矩阵进行矩阵乘积
cor(data_scale_new)
data_new <- as.matrix(data_raw) %*% A #主成份
ggplot(data = as.data.frame(data_new), aes(x=V1,y=V2)) + geom_point()
cor(data_new)

#方差最大化旋转
#正交变换是主成分分析的思想核心。它的作用在几何图形上可看做是不改变原始变量形状和大小的前提下，进行坐标旋转 
data_raw1 <- data.frame(No="raw",
                       x1=data_raw[,1],
                       x2= data_raw[,2]) 
data_new1 <- data.frame(No="new",
                       x1=data_new[,1],
                       x2= data_new[,2]) 
all_data <- 
  rbind(data_raw1,data_new1) 

ggplot(all_data,aes(x=x1,y=x2, colour=factor(No))) +
  geom_hline(yintercept=0,lwd=1,col=8) +
  geom_vline(xintercept=0,lwd=1,col=8) +
  geom_point(lwd=3)+stat_ellipse(lwd=1) +
  ylim(-30,30) +
  theme(axis.text = element_text(size = 15),axis.title= element_text(size=15)) +
  theme_minimal()

##################################15. ROC曲线 - AUC##################################
library(pROC) 
data(aSAH)
str(aSAH)

roca<-roc(aSAH$outcome,aSAH$s100b)
rocb<-roc(aSAH$outcome,aSAH$wfns)
rocc<-roc(aSAH$outcome,aSAH$ndka)
auc(roca)
auc(rocb)
auc(rocc)

plot(roca)
plot(rocb)
plot(rocc)
g3<-ggroc(list(s100b=roca,wfns=rocb,ndka=rocc))

##################################%%%%%%%%%%%%%%%%%%%%%概率统计%%%%%%%%%%%%%%%%%%%%%########################################################
##################################1. 离散型变量及常见的分布##################################
#########生成一个离散型随机变量
set.seed(100)
x <- sample(1:6,100,replace = T)
#######x的概率分布(probability distribution)
prop.table(table(x))
#######x的概率函数(probability function)表示为: P(X=xi) = pi; 这个概率函数没有固定的模式, sum(pi) = 1
#######随机变量x的期望值E(X) = sum(xi * pi)

##################################1.1 二项式分布(0-1分布)##################################
#########满足二项分布(Binomial Distribution)的离散型随机变量X表示
#######n次重复独立试???(n重贝努里试验)中事件A出现的次???; (每次试验只有两个可能的结???1或???0); 表示为X~B(n,p)
p <- 0.3    #每次独立试验中出???1的概???
q <- 1 - p  #每次独立试验中出???0的概???
n <- 10     #进行10重贝努里试验
#######二项分布的概率函???: P{X=x} = C(n,x) * p^x * q^(n-x); x = 0,1,2...n; 
P.6 = (factorial(n) / (factorial(6) * factorial(n-6))) * p^6 * q^(n-6)   #P{X=6}
#######二项分布的期望及方差: E(X) = n*p; V(X) = n*p*q
#######当n=1时， 二项式分布就变成0-1分布, 0-1分布的概率函数P{X=x} = p^x * q^(1-x); x= 0,1
#######R中随机生成一个满足B(n,p)二项分布的序???
x <- rbinom(1000, n, p)   #生成100个服从X~B(n=10,p=0.3)的随机变量的序列
prop.table(table(x))      #X的概率分???


##################################1.2 泊松分布##################################    
#########泊松分布(Poisson Distribution) 用来描述在一个指定范围内(时间/面积/体积..)某一事件发生的次数的分布.
#########泊松分布是在已知期望值的情况下，求该事件发生x次的概率P(X=x)
#########成都每天发生交通事故的次数X满足泊松分布, 且根据去年的统计E.X = 10; 表示为X~P(10)
E.X <- 10
V.X <- E.X                   #泊松分布的方??? = 均???
#########泊松分布的概率函???: P{X=x} = E.X^x * exp(1) ^ -E.X/x! (x!表示x的阶???)
P.5 <- (E.X^5 * exp(1)^(-E.X))/factorial(5)
#######R中随机生成一个满足P(E.X)二项分布的序???
x <- rpois(1000, 10)
prop.table(table(x))

#########用泊松分布近似二项分???
#########当二项分布中p <=0.25, n>=20, np <=5, 可以用泊松分布近似二项分布来计算概率.


##################################2. 连续型变量及常见的概率分布##################################
X <- round(sample(1:500, size = 1000, replace = T) / 3, 2)             #X是一个连续型随机变量
ggplot(data = data.frame(x = X), aes(x = x)) + geom_density()          #X的概率密度曲线图
#########由上图来理解以下的概念:
#########概率密度函数(probability density function) - f(x)
#########分布函数(distribution function) - F(x); F(x) = P{X<=x}

##################################2.1 均匀分布##################################
#########连续型随机变量X的值域为[a,b]且满足均匀分布, ???: f(x) = 1/(b-a)
X <- runif(1000, min = 1, max = 7)
f.x <- dunif(X,1,7)
ggplot(data = data.frame(x=X,y=f.x), aes(x=x,y=y))+geom_point()    #概率密度曲线


##################################2.2 指数分布##################################
#########指数分布（也称为负指数分???,Exponential distribution）是描述泊松过程中的事件之间的时间间隔的概率分布
#########设成都单位时间内(每天)发生10次交通事???, 那么单位时间内发生交通事故的次数X就满足泊松分??? E(X)=10
#########随机变量Y(表示单位时间内发???1次交通事故的时间间隔)的概率密度函数f(y) = m * e^(-m*y);(m = E(X), 称为率参数rate parameter) 则称Y满足指数分布,记为Y~E(m)
#########Y的期望E(Y) = 1/m (m = E(X)); 标准差SD(Y)=1/m
Y <- rexp(1000, rate = 2.4)
f.x <- dexp(Y,2.4)
ggplot(data = data.frame(x = Y, y = f.x), aes(x = x, y=y)) + geom_point()

##################################2.3 正太分布##################################
#########正太分布(normal distribution)
#########X~N(m,sd) - 随机变量X服从均值为m，标准差为sd的正太分???, 正太分布的概率密度函???
m <- 0
sd <- 1
x <- rnorm(10000,m,sd)
f.x = 1/sd*sqrt(2*pi) * (e^(-(1/(2*(sd^2)))*((x-m)^2)))   #根据正太分布的概率密度函数计算f.x
f.x = dnorm(x, m, sd)                                     #也可以使用R提供的内置函数计算f.x
F.x <- pnorm(x,m,sd)
ggplot(data = data.frame(x = x, y = f.x), aes(x = x, y =y)) + geom_point()
ggplot(data = data.frame(x = x, y = F.x), aes(x = x, y =y)) + geom_point()

##################################2.4 拉普拉斯分布##################################
#########拉普拉斯分布(Laplace)
#########X~L(u,b) - μ 是位置参数，b 是尺度参???; 如果 μ = 0，那么，正半部分恰好是尺度为 1/2 的指数分???
#########由于它可以看作是两个不同位置的指数分布背靠背拼接在一起，所以它也叫作双指数分布???
u <- 0
b <- 1
x <- rmutil::rlaplace(10000, u, b) 
f.x <- (1/(2*b)) * ( exp(1)^(-(abs(x-u)/b)) )

ggplot(data = data.frame(x=x,f.x=f.x),aes(x=x,y=f.x))+geom_point()

##################################2.5 逻辑斯谛分布##################################
##################################2.5.1 Sigmoid 函数##################################
#########Sigmoid函数(S型曲???)又叫Logistic函数, 函数形式如下:
S(t) = 1 / (1 + e^-t)
#--sample
t <- seq(-10,10,by = 0.01)
S.t <- 1 / (1 + exp(1)^(-t))
ggplot(data = data.frame(x = t, y = S.t), aes(x,y)) + geom_line()    #S型曲???

#--Sigmoid函数(S型曲???)的基本性质:
#' 定义??? t -> [−∞,+∞]
#' 值域 S(t) -> [0,1]
#' S(0) = 0.5
#' 处处可导, S???(t)= S(t)(1-S(t))

#--广义Logistic曲线可以模仿类似技能增长这样的情况，开始阶段曾指数增长，然后随着开始变得饱和，增长变慢，最后到达成熟时增长几乎停止???


##################################2.5.2. 逻辑斯谛分布##################################
#########逻辑斯谛分布(logistic distribution)是一种连续型的概率分布，记作X~L(u,r);  u是散布中???(即期???)，r是散布程???; 其累计分布函数为:
#########F(x) = P(X<=x) = 1 / ( 1 + e^-((x-u)/r) ); 当u=0, r = 1时称为标准的逻辑斯谛分布, 其累计分布函数变???:
#########F(x) = P(X<=x) = 1 / ( 1 + e^-x); 这个函数就是Sigmoid函数;
x <- rlogis(1000,0,1)              #随机生成满足逻辑分布的变量x
f.x <- dlogis(x,0,1)               
F.x <- plogis(x,0,1)
ggplot(data = data.frame(x = x, y = f.x), aes(x = x, y =y)) + geom_point()     #概率密度曲线
ggplot(data = data.frame(x = x, y = F.x), aes(x = x, y =y)) + geom_point()     #分布曲线

##################################2.5.3. 逻辑回归##################################
#########逻辑回归实际上是求Y=1的概率t;

#########逻辑回归的假设函???(逻辑回归函数)???:
hθ(X) = t = P(Y=1|X1=x1,X2=x2,X3=x3....,Xn=xn) = e^(θ0+θ1*x1+θ2*x2+....θn*xn) / (1 + e^(θ0+θ1*x1+θ2*x2+....θn*xn))
#########通过logit变换将逻辑回归函数线性化:
1-t = P(Y=0|X1=x1,X2=x2,X3=x3....,Xn=xn) = 1 / (1 + e^(θ0+θ1*x1+θ2*x2+....θn*xn))
t / (1-t) = e^(θ0+θ1*x1+θ2*x2+....θn*xn); #t / (1-t) 表示事件发生与不发生的概率比???
log(t/(1-t)) = θ0+θ1*x1+θ2*x2+....θn*xn;  #两边同时取自然对???; log(t/(1-t))称为logit

#########逻辑回归的损失函???
#######'1. 0-1损失函数 L(Y,f(X)) = 1 (Y!=f(X)); = 0 (Y=f(x));  就是混沌矩阵的错误率
#######'2. 对数似然损失函数 
L(Y, P(Y=y|x)) = L(Y,hθ(X)) = sum( -yi*log(hθ(xi)) - (1-yi)*log(1-hθ(xi)) ); #hθ(xi)就是假设函数，即P(Y=1|X=xi)的预测???
#####上面这个损失函数分成两部分看:
#####当yi = 1, L(Y=1, hθ(X)) = sum( -yi*log(hθ(xi)) ) = sum( -log(hθ(xi)) )
#####当yi = 0, L(Y=0, hθ(X)) = sum( -(1-yi)*log(1-hθ(xi)) ) = sum( -log(1-hθ(xi)) )
#######'3. AUC (aear under curve)


##################################3. 统计量##################################
#########统计量的定义
#######设X1,X2,X3...,Xn 是从总体X中抽取的容量为n的一个样本，由此样本构造一个不依赖于任何未知参数的函数T(X1,X2...Xn),
#      该函数就是一个统计量???(统计量是样本的一个函???)
#      将样本的具体观测x1,x2,x3....,xn带入T, 计算出T(x1,x2,x3....,xn)的值，这个值称为具体的统计量值???

#########常用的统计量:
#######'1. 样本均???
#######'2. 样本方差
#######'3. 样本偏度
#######'4. 样本峰度


##################################4. 抽样分布##################################
#########统计推断的三个中心内容：
#######'1. 抽样分布
#######'2. 参数估计
#######'3. 假设检验

#########在正太总体条件下，三大抽样分布:
#######'1. 卡方分布
#######'2. t分布
#######'3. F分布

##################################4.1 卡方分布##################################
#########定义: 若n个相互独立的随机变量ξ₁，ξ₂，...,ξn ，均服从标准正态分布（也称独立同分布于标准正态分布）???
#        则这n个服从标准正态分布的随机变量的平方和构成一新的随机变量，其分布规律称为卡方分布（chi-square distribution）???
#########当自由度df趋近于无穷大时，卡方分布的极限分布是正太分布
#########E(χ2) = df; D(χ2) = 2*df
df = 5
x <- rchisq(10000,df)
f.x <- dchisq(x,df)
ggplot(data=data.frame(x=x,f.x=f.x),aes(x=x))+geom_density()
ggplot(data=data.frame(x=x,f.x=f.x),aes(x=x,y=f.x))+geom_point()


##################################4.2 t分布##################################
#########定义：设随机变量X~N(0,1), Y~χ2(n);且X与Y独立，则t=X/sqrt(Y/n)的分布就称为t分布;记作t(n); n为自由度
df = 5
x <- rt(10000,df)
f.x <- dt(x,df)
ggplot(data=data.frame(x=x,f.x=f.x),aes(x=x))+geom_density()
ggplot(data=data.frame(x=x,f.x=f.x),aes(x=x,y=f.x))+geom_point()

##################################4.3 F分布##################################
#########定义：设随机变量Y,Z相互对立，且Y与X分布服从自由度为m和n的??2分布，随机变量X表示???: 
#        X = (Y/m) / (Z/n) = nY/mZ, 则称X服从第一自由度为n, 第二自由度为n的F分布; X~F(m,n)
#########E(X)=n/(n-2);
df1=5; df2=10
x<-rf(10000,df1,df2)
f.x <- df(x,df1,df2)
ggplot(data=data.frame(x=x,f.x=f.x),aes(x=x))+geom_density()
ggplot(data=data.frame(x=x,f.x=f.x),aes(x=x,y=f.x))+geom_point()




##################################%%%%%%%%%%%%%%%%%%%%%特征工程%%%%%%%%%%%%%%%%%%%%%########################################################
#########特征工程至少包括:
#######'1. 理解每个变量代表的业务意???
#######'2. 检查每个变量的数据质量(是否空值，是否一致，是何分布....)
#######'3. 查看变量之间的相关关???
#######'4. 变量选择或生成新的变???
#######'5. ........

##################################%%%%%%%%%%%%%%%%%%%%%回归分析%%%%%%%%%%%%%%%%%%%%%########################################################
#########(多元)线性回归的一般方???:
#######Y = b0 + b1*X1 + b2*X2 + ... bn*Xn + e; 
#######e, 称为随机干扰或随机误???(random error), 它反应了除x与y之间的线性关系之外的随机因素对y的影???

#########对e的更多理???
#######1. 在回归模型的统计假设中，e是一个期望为0, 方差为g^2(常数),且服从正太分布的随机变量, ???:e~N(0,g^2); 
#######2. 且对任意固定的x都相同且独立; 基于此，对任意一个固定的xi, 对应的yi的取值的分布服从yi~N(b0+b1*xi,g^2);
#######3. 统计??? - '估计标准???'(Se)是随机误差e的标准差(g)的无偏估计，
#         其公式为: Se = sqrt( SSE / (n - k - 1) ) (n是观测数，k是回归系数的个数, n-k-1表示残差平方和SSE的自由度)

##################################0. 自定义函数帮助回归分析##################################
#########在特征工程过程中，生成数据集中全部numeric类型的变量的相关矩阵和图???
uf_gen_NumVarCor <- function(ds, RespVar, CorOff = 0.5){  #CorOff用于设置图形中展示的相关系数的最低???
  NumVar <- colnames(ds)[apply(ds,2,FUN = is.numeric)]
  if(length(NumVar)<2){
    print("The dataset has less than two numeric variables.")
  }else{
    ds.num <- ds[,NumVar]
    corMatrix <- cor(ds.num)        #生成相关系数矩阵
    car::scatterplotMatrix(ds, spread = F, main = "scatterplotMatrix")
    
    RespVar_Index <- which(colnames(ds.num) == RespVar)[1]
    sorted_cor <- as.matrix(sort(corMatrix[,RespVar_Index], decreasing = T))
    CorHigh <- names(which(apply(sorted_cor, 1, function(x) abs(x)>CorOff)))   #选出与响应变量相关系数大于CorOff的变???
    corMatrix_High <- corMatrix[CorHigh,CorHigh]
    if(length(CorHigh) > 1) corrplot::corrplot.mixed(corMatrix_High)  
    
    return(corMatrix)
    
  }
  
}

#########生成模型残差数据???
uf_gen_residDataset <- function(orgDs, model, precision){
  n <- length(fitted(model))
  p <- length(coefficients(model))
  avg_hat <- p/n
  cook_cutoff <- 4/(n-p-2)
  ResidDS <- orgDs
  ResidDS$Y_ <- round(fitted(model),precision)
  ResidDS$Resid <- round(residuals(model),precision)
  ResidDS$std_Resid <- round(rstandard(model), precision + 2)
  ResidDS$Is_Outlier <- ifelse(abs(ResidDS$std_Resid)>2, "red","black")
  ResidDS$hatValue <- round(hatvalues(model),precision + 3)
  ResidDS$Is_HighLevg <- ifelse(ResidDS$hatValue > 3*avg_hat, "blue", "black")
  ResidDS$CookD <- round(cooks.distance(model),precision + 3)
  #ResidDS$Is_Influence <- ifelse(ResidDS$CookD > cook_cutoff, "green", "black")
  ResidDS$Is_Influence <- ifelse(ResidDS$CookD > 1, "green", "black")  #???1替代cook_cutoff
  
  XVars <- model$coefficients[-1]
  #--计算解释变量的成分残???
  for(i in 1:length(XVars)){
    ResidDS$cpnt_resid <- round(ResidDS$Resid + XVars[i] * orgDs[,colnames(orgDs) == names(XVars[i])],precision + 2)
    colnames(ResidDS)[which(colnames(ResidDS) == "cpnt_resid")] <- paste0("cpnt_resid","_",names(XVars[i]))
  }
  
  
  return(ResidDS)
}

#########生成自定义残差图(Standard Residuals vs. Fitted), 作用:
#######1. 查看是否有异常点            - 离群???(红色); 高杠杆点(蓝色); 强影响点(绿色) 
#######2. 查看是否???'异方???'问题      - 残差是否明显地随拟合值的增多而增大或变小 (查看黑色的直线是否与y=0平行)
#######3. 查看是否满足误差0均值假???   - 查看绿色的线是否与y=0重合
#######4. 查看是否满足线性假???        - loess曲线有无明显的n次项分布 - 多元回归中，使用成分残差图查看具体是那个变量不满足线???
uf_gen_StdResidPlot <- function(ResidDS){
  plot_StdRes_Fitted <- 
    ggplot(data = ResidDS, aes(x = Y_, y = std_Resid)) + 
    geom_hline(aes(yintercept = 0), colour = "dark gray") + 
    geom_hline(aes(yintercept = 2), linetype = 2) + geom_hline(aes(yintercept = -2), linetype = 2) +
    geom_point(aes(colour = Is_Outlier))  + 
    geom_point(data = ResidDS[ResidDS$Is_Outlier == "black" & ResidDS$Is_HighLevg != "black",],aes(colour = Is_HighLevg)) + 
    geom_point(data = ResidDS[ResidDS$Is_Outlier == "black" & ResidDS$Is_HighLevg == "black" & ResidDS$Is_Influence != "black",],aes(colour = Is_Influence)) + 
    scale_color_identity() +
    geom_text(aes(label = ifelse(Is_Outlier == "black","",row.names(ResidDS))), size = 3, vjust = 1, hjust = -.02) +
    geom_text(data = ResidDS[ResidDS$Is_Outlier == "black" & ResidDS$Is_HighLevg != "black",],aes(label = ifelse(Is_HighLevg == "black","",row.names(ResidDS))), size = 3, vjust = 1, hjust = -.02) +
    geom_text(data = ResidDS[ResidDS$Is_Outlier == "black" & ResidDS$Is_HighLevg == "black" & ResidDS$Is_Influence != "black",],aes(label = ifelse(Is_Influence == "black","",row.names(ResidDS))), size = 3, vjust = 1, hjust = -.02) +
    geom_smooth(method = "loess", colour = "red", se = F, size = .1) + 
    geom_smooth(method = "lm", se = F, colour = "green", linetype = 2) +
    geom_smooth(data = ResidDS[ResidDS$std_Resid >= 0,], method = "lm", colour = "black", se = F, size = .2) +
    geom_smooth(data = ResidDS[ResidDS$std_Resid <= 0,], method = "lm", colour = "black", se = F, size = .2) +
    #geom_smooth(data = ResidDS[ResidDS$Is_Outlier == "black",],method = "loess", colour = "red", se = F, linetype = 2) + 
    theme(panel.grid.minor =  element_blank()) +
    #theme_light() +
    labs(title = "Standard Residuals vs. Fitted", x= "Fitted Y", y = "Standard Residual")  
  
  return(plot_StdRes_Fitted)
}

#########生成成分残差???(单变???), 作用:
#######1. 考察多元模型中，那个解释变量与响应变量之间不是线性关???.
uf_gen_cpntResid <- function(ResidDS,X,cpntResid_X){
  if(any(names(ResidDS) == "X1000") | any(names(ResidDS) == "cpntResid_X1000")){
    print("Can't plot because the dataset contains a column called 'X1000' or 'cpntResid_X1000'!")
  }
  names(ResidDS)[which(names(ResidDS) == X)[1]] <- "X1000"
  names(ResidDS)[which(names(ResidDS) == cpntResid_X)[1]] <- "cpntResid_X1000"
  
  plot <- 
    ggplot(data = ResidDS, aes(x = X1000, y = cpntResid_X1000)) + geom_point(shape=1) + 
    geom_smooth(method = "lm", se = FALSE,colour = "red") +    
    geom_smooth(method = "loess", se = FALSE,colour = "green") +
    theme(panel.grid.minor =  element_blank()) +
    #theme_light() +
    labs(title = "Component Residual Plot", x= X, y = cpntResid_X)  
  
  return(plot)
}

#########生成残差序列???(连续变量&分类变量), 作用:
#######1. 考察模型是否存在自相关问??? (连续几个观测的符号一样，红蓝点分别代表残差大???/小于0的观测点)
#######2. 考察存在异方差多元模型中，那个解释变量引起的异方???
uf_gen_StdResidSeq <- function(ResidDS, X, XType = "C"){
  if(any(names(ResidDS) == "XSeq100")){
    print("Can't plot because the dataset contains a column called 'XSeq100'!")
  }
  names(ResidDS)[which(names(ResidDS) == X)[1]] <- "XSeq100"
  if(XType == "F"){
    plot <- 
      ggplot(data = ResidDS, aes(x = as.factor(XSeq100), y = std_Resid)) + 
      geom_point(aes(colour = ifelse(std_Resid>=0, "blue","red"))) + scale_color_identity() + 
      geom_hline(aes(yintercept = 0), colour = "black", linetype = 2) + 
      stat_summary(aes(colour = I("black")),fun.y = sd, geom = "point", size = 2) + 
      theme_light() +
      labs(title = paste0("Residual sequence Plot by ",X), x= X, y = "Standard Residual")  
  }else if(XType == "C"){
    plot <- 
      ggplot(data = ResidDS, aes(x = XSeq100, y = std_Resid)) + 
      geom_point(aes(colour = ifelse(std_Resid>=0, "blue","red"))) + scale_color_identity() + 
      geom_hline(aes(yintercept = 0), colour = "black", linetype = 2) + 
      geom_smooth(method = "loess", colour = "green", se = F, size = .1) + 
      geom_smooth(data = ResidDS[ResidDS$std_Resid >= 0,], method = "lm", colour = "black", se = F, size = .2) +
      geom_smooth(data = ResidDS[ResidDS$std_Resid <= 0,], method = "lm", colour = "black", se = F, size = .2) +
      #theme(panel.grid.minor =  element_blank()) + 
      theme_light() +
      labs(title = paste0("Residual sequence Plot by ",X), x= X, y = "Standard Residual")  
  }
  
  return(plot)
  
}

##################################1. 最小二乘法参数估计##################################
#########最小二乘法(OLS, ordinary Least Square)进行回归参数估计的原???: 最小化残差平方???, 即min( sum( (yi_ - yi)^2 ) )
#########拟合优度用判定系???(coefficient of determination)来衡???; R_Squre = SSR/SST = sum( (yi_ - mean(yi)) ^ 2 ) / sum( (yi - mean(yi)) ^ 2 ) 
#########相应的，'估计标准???'(Se)也是回归直线拟合优度的度量， Se越小，回归直线预测误差就越小

##################################2. 回归模型的统计假设##################################
#########1. 模型形式的假???
#######1.1 线性假???
#########2. 误差的假设i.i.d(independent and identically distributed) - 随机误差ei被假定为独立同分???(ei~N(0,g^2)); 拆分开来即是：
#######2.1. 正太性假???
#######2.2. 0均值假???
#######2.3. 同方差性假??? - 异方差问???
#######2.4. 误差独立假设 - 自相关问???
#########3. 预测变量的假???
#######3.1. 预测变量是非随机变量
#######3.2. 预测变量是无误差的测量得到的
#######3.3. 预测变量之间没有强相关关??? - 多重共线???

##################################3. 多元回归实例##################################
#########数据集ds.perf - 对雇主的评价
ds.perf <- data.frame(i = c(1:30),
                      Y = c(43,63,71,61,81,43,58,71,72,67,64,67,69,68,77,81,47,65,65,50,50,64,53,40,63,66,78,48,85,82),   #对主管工作情况的总体评价                      
                      X1 = c(51,64,70,63,78,55,67,75,82,61,53,60,62,83,77,90,85,60,70,58,40,61,66,37,54,77,75,57,85,82),  #处理雇员的抱???
                      X2 = c(30,51,68,45,56,49,42,50,72,45,53,47,57,83,54,50,64,65,46,68,33,52,52,42,42,66,58,44,71,39),  #不允许特???
                      X3 = c(39,54,69,47,66,44,56,55,67,47,58,39,42,45,72,72,69,57,57,54,34,62,50,58,48,63,74,45,71,59),  #学习新知识的机会
                      X4 = c(61,63,76,54,71,54,66,70,71,62,58,59,55,59,79,60,79,55,75,64,43,66,63,50,66,88,80,51,77,64),  #依据工作业绩升???
                      X5 = c(92,73,84,84,83,49,68,66,83,80,67,74,63,77,77,54,79,80,85,70,64,80,80,57,75,74,78,83,74,78),  #对不良表现过于吹毛求???
                      X6 = c(45,47,48,35,47,34,35,41,31,41,34,41,25,35,46,36,63,60,46,52,33,41,37,49,33,72,49,38,55,39)   #提升到更好工作的速度
)

#########lm()中formula的形???:
#######'1. y~x1+x2
#######'2. y~x1+x2+x1:x2                #x1:x2 表示预测变量的交互项
#######'3. y~x1*x2*x3                   #表示所有可能交互项的简洁方???, 等同???: y~x1+x2+x3+x1:x2+x1:x3+x2:x3+x1:x2:x3 
#######'4. y~(x1+x2+x3)^2               #表示交互项达到某个次数， 等同???:y~x1+x2+x3+x1:x2+x1:x3+x2:x3
#######'5. y~(x1+x2+x3)^2-x1:x2         #等同???:y~x1+x2+x3+x1:x3+x2:x3
#######'6. y~x1+I(x2^2)                 #多项式回???

##################################3.0 开始建模以前##################################
##################################3.0.1 特征工程##################################

##################################3.0.1.1 统计分析(相关性)##################################
uf_gen_NumVarCor(ds.perf,"Y",0.1)

##################################3.1 生成模型及summary(fit)等函数详解##################################
fit <- lm(Y~X1+X2+X3+X4+X5+X6, data = ds.perf)
summary(fit)
#########0. F检??? p-value - 全部回归参数都为0的概???
#########1. Residuals - 残差的五数概???
#########2. Residual standard error - 回归模型的残差标准误，即是模型的随机误差e的标准差(g)的无偏估???
n = nrow(ds.perf)                           #数据集的记录???
k = length(coefficients(fit)) - 1      #回归系数的个???
df = n - k - 1                              #自由???
SSE = sum(residuals(fit) ^ 2)          #残差平方???
Se = round(sqrt( SSE / df ),3)              #残差标准???, 注意跟残差的标准???(sd(residuals(fit)))不同, 考虑了自由度???
#########3. Coefficients - 参数估计的结???
#######3.1. Estimate - 回归系数的无偏估???
b1_ <- 0.49991827
#######3.2. Std. Error - 回归系数的标准误 - 标准误越小，估计精度越高
Se_X1 <- 0.20983434
#######3.3. t value - 对单个回归系数进行统计检???(原假设H0: bj = 0)的t统计???
t_X1 <- round(b1_ / Se_X1, 2)  #t_X1 = 2.38, 服从自由度为n-k-1的t分布
#####3.3.1 查t分布表可得，a显著水平下自由度???(n-k-1)的临界值t(df,a/2) (因为要进行双边检验，所有显著水平要除以2)
t_.001_df <- round(qt(1-.001/2, df = df),2)           #0.001显著水平(99.9%置信区间),自由???23, t分布表对应的临界??? - 3.77
t_.01_df <- round(qt(1-.01/2, df = df),2)             #0.01显著水平 (99%置信区间),  自由???23, t分布表对应的临界??? - 2.81
t_.05_df <- round(qt(1-.05/2, df = df),2)             #0.05显著水平 (95%置信区间),  自由???23, t分布表对应的临界??? - 2.07
t_.1_df <- round(qt(1-.1/2, df = df),2)               #0.1显著水平  (90%置信区间),  自由???23, t分布表对应的临界??? - 1.71
#####3.3.2 比较t_X1与t(df,a/2)的???, 如果 abs(t_X1) >= t(df,a/2), 则表示在显著水平a(置信区间1-a)下拒绝原假设H0: b1_ = 0
abs(t_X1) > t_.05_df                                  #在显著性水???0.05下拒绝原假设
#######3.4. Pr(>|t|) - 该p值表示abs(t_X1)小于服从自由度为df的t分布的随机变量绝对值的概率
p_X1 <- 0.02585                #p_X1 < 0.05, 表示在显著性水???0.05下拒绝原假设
#######3.5. Multiple R-squared: - 判定系数
SST <- sum( (ds.perf$Y - mean(ds.perf$Y)) ^ 2 )   #总变???
SSR <- sum( (fitted(fit) - mean(ds.perf$Y)) ^ 2 )
R_Squared = round(SSR / SST,4)
#######3.5.1. 修正R-squared
Adj_R_squared <- round(1 - ((SSE/df) / (SST/(n-1))),4)
#######4. 其他关于模型的信???
#####4.1. 获取回归参数
coefficients(fit)
#####4.2. 构建回归参数的置信区???, 下面两种方式都可以得???:
confint(fit, parm = "X1", level = 0.95)
c(b1_ - t_.05_df*Se_X1, b1_ + t_.05_df*Se_X1)   

#########其他模型函数:
#######1. anova(fit) - 列出拟合模型的方差分析表
anova(fit)
#######2. AIC(fit,.,.) - 常用来比较两个或多个模型的，AIC值越小，模型越好
AIC(fit)

##################################3.1.1 变量相对重要性/标准化回归变量##################################
lm.beta::lm.beta(fit)                #计算标准化回归参??? 
relaimpo::calc.relimp(fit)           #计算解释变量的相对重要???
##################################3.2 将模型用于预测##################################
ds.perf.test <- data.frame(i = c(31:40),
                           Y = rep(0,10),   #对主管工作情况的总体评价                      
                           X1 = c(30,51,68,45,56,25,35,46,36,63),  #处理雇员的抱???
                           X2 = c(48,35,47,34,35,72,67,64,67,69),  #不允许特???
                           X3 = c(66,88,80,51,77,85,70,64,80,80),  #学习新知识的机会
                           X4 = c(70,63,78,55,67,40,63,66,78,48),  #依据工作业绩升???
                           X5 = c(92,73,84,84,83,49,42,50,72,45),  #对不良表现过于吹毛求???
                           X6 = c(78,55,67,75,82,57,57,54,34,62)   #提升到更好工作的速度
)

#######预测方式一：预测限 - 预测给定x对应的y的预测值的置信???
predict.lm(fit,ds.perf.test,interval = "prediction",level = .95)
#######预测方式二：置信??? - 预测给定x对应的y的期望值的置信???
predict.lm(fit,ds.perf.test,interval = "confidence",level = .95)


##################################3.3 生成模型残差数据##################################
ResidDS.fit <- uf_gen_residDataset(orgDs = ds.perf, model = fit, precision = 0)

##################################3.3.1 复相关系数(Y与Y_的相关系数)##################################
#########Y与Y_的相关系数称作复相关系数 - 可用来判断Y与X1-X6之间线性关系的强弱
cor.test(ResidDS.fit$Y_, ResidDS.fit$Y); round(sqrt(R_Squared),4)
ggplot(data = ResidDS.fit, aes(x = Y, y = Y_)) + geom_point() + 
  geom_smooth(method = "lm", se = F, colour = "red", linetype = 2) +
  geom_line(aes(x = Y, y = Y), colour = "green")

##################################3.4 回归模型诊断及优化##################################
#########R提供的一个函数可对模型进行综合的诊断
gvlma(fit)
plot(fit)


##################################3.4.1 自定义残差图##################################
#########自定义残差图(Standard Residuals vs. Fitted)
#######1. 查看是否有异常点            - standard residual > 2 用红色的点表???
#######2. 查看是否???'异方???'问题      - 残差是否明显地随拟合值的增多而增大或变小 (查看黑色的直线是否与y=0平行)
#######3. 查看是否满足误差0均值假???   - 查看绿色的线是否与y=0重合
#######4. 查看是否满足线性假???        - loess曲线有无明显的n次项分布 - 多元回归中，使用成分残差图查看具体是那个变量不满足线???
plot_StdRes_Fitted <- uf_gen_StdResidPlot(ResidDS.fit)
plot_StdRes_Fitted



##################################3.4.2 解决非线性-成分残差图  -解释变量变换##################################
#########成分残差???(partial residual plot)用于: 判断每个解释变量与响应变量的线性假设是否合???
#########通过成分残差图可以确认是否需要对某些解释变量进行变换
car::crPlots(fit)

plot_cpnt_Resid_X1 <- uf_gen_cpntResid(ResidDS = ResidDS.fit,X = "X1",cpntResid_X = "cpnt_resid_X1")
plot_cpnt_Resid_X2 <- uf_gen_cpntResid(ResidDS = ResidDS.fit,X = "X2",cpntResid_X = "cpnt_resid_X2")
plot_cpnt_Resid_X3 <- uf_gen_cpntResid(ResidDS = ResidDS.fit,X = "X3",cpntResid_X = "cpnt_resid_X3")
plot_cpnt_Resid_X4 <- uf_gen_cpntResid(ResidDS = ResidDS.fit,X = "X4",cpntResid_X = "cpnt_resid_X4")
plot_cpnt_Resid_X5 <- uf_gen_cpntResid(ResidDS = ResidDS.fit,X = "X5",cpntResid_X = "cpnt_resid_X5")
plot_cpnt_Resid_X6 <- uf_gen_cpntResid(ResidDS = ResidDS.fit,X = "X6",cpntResid_X = "cpnt_resid_X6")

Rmisc::multiplot(plot_cpnt_Resid_X1,plot_cpnt_Resid_X2,plot_cpnt_Resid_X3,plot_cpnt_Resid_X4,plot_cpnt_Resid_X5,plot_cpnt_Resid_X6, 
                 layout=matrix(c(1:6),2,3,byrow=TRUE))


##################################3.4.3 解决异方差-自定义残差图-响应变量变换##################################
#########如果已知响应变量Y服从某种特定的分???:
#######1. 泊松分布     - 对Y进行变换为sqrt(Y) (如果Y值较小，一般可变换为sqrt(Y+0.5));
#######2. 二项式分???   - 对Y进行变换为sin(-1)sqrt(Y)
#######3. 正太分布     - 视情况而定，因为服从正太分布的随机变量的均值和方差独立

#########对非正态分布的因变量，也可以使用广义线性模型glm()来解决问???, e.g. 
#######1. Logistic回归（因变量为类别型???
#######2. 泊松回归（因变量为计数型）???

#########无论是对解释变量还是对响应变量，一般的变化就是n次幂变换，n的取值一般是(-2,-1,0,0.5,2):
#######n = -1, 倒数转换
#######n = -0.5, 开方倒数转换
#######n = 0, 对数转换
#######n = 0.5, 平方根转???
#######n = 1, 不需要转???
#######n = 2, 平方转换


#########异方差的的诊断和识别，可用两种方法：
#######1. 画图 - ???3.4.1中讲的自定义残差???
#######2. ncvTest() - ncvTest()函数生成一个计分检验，零假设为误差方差不变，备择假设为误差方差随着拟合值水平的变化而变化???
#                     若检验显著，则说明存在异方差性（误差方差不恒定）???

##################################3.4.3.1 一个例子说明异方差和非线性##################################
ds.factor <- data.frame(i = c(1:27),
                        X = c(294,247,267,358,423,311,450,534,438,697,688,630,709,627,615,999,1022,1015,700,850,980,1025,1021,1200,1250,1500,1650),
                        Y = c(30,32,37,44,47,49,56,62,68,78,80,84,88,97,100,109,114,117,106,128,130,160,97,180,112,210,135))
ggplot(data = ds.factor, aes(x = X, y = Y)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, colour = "green") + 
  geom_smooth(method = "loess", se = FALSE, colour = "red")

fit.factor <- lm(Y~X, data = ds.factor)
summary(fit.factor)

#--生成模型残差数据??? - refer to 3.3
ResidDS.factor <- uf_gen_residDataset(orgDs = ds.factor, model = fit.factor, precision = 0)

#--画自定义残差???(Standard Residuals vs. Fitted) - refer to 3.4.1
plot_StdRes_factor <- uf_gen_StdResidPlot(ResidDS.factor)
plot_StdRes_factor                       #从图中可以看出明显的异方差问???

#--使用ncvTest()函数验证
car::ncvTest(fit.factor)                 #p值很小，说明存在异方差问???
car::spreadLevelPlot(fit.factor)         #这个函数提供如何对响应变量进行变换的建议
summary(car::powerTransform(ds.factor$Y))


#--根据建议，对响应变量进行'取对???'变换
ds.factor$LogY <- log(ds.factor$Y)
fit.factor.log <- lm(LogY~X, data = ds.factor)
summary(fit.factor.log)
#--生成模型残差数据???
orgDs <- ds.factor
model <- fit.factor.log
precision <- 4
ResidDS.factor.log <- uf_gen_residDataset(orgDs, model, precision)
#--画自定义残差???(Standard Residuals vs. Fitted)
plot_StdRes_factorLog <- uf_gen_StdResidPlot(ResidDS.factor.log)
plot_StdRes_factorLog                       
Rmisc::multiplot(plot_StdRes_factor,plot_StdRes_factorLog, layout=matrix(1:2,1,2))  #异方差问题基本解决了
#--使用ncvTest()函数验证
car::ncvTest(fit.factor.log)                 #p???>0.05

#--根据自定义残差图看出，模型的线性假设得不到满足，故对解释变量X进行变换
#--如何对X进行变换??? 理论上可以用下面的函数来得到需要变换的???
car::boxTidwell(LogY~X,data = ds.factor)
#--实际上取平方或开方等是比较好解释???
ds.factor$X_Square <- sqrt(ds.factor$X)
fit.factor.square <- lm(LogY~X_Square + X, data = ds.factor)
summary(fit.factor.square)
#--生成模型残差数据???
orgDs <- ds.factor
model <- fit.factor.square
precision <- 4
ResidDS.factor.square <- uf_gen_residDataset(orgDs, model, precision)
#--画自定义残差???(Standard Residuals vs. Fitted)
plot_StdRes_factorSquare <- uf_gen_StdResidPlot(ResidDS.factor.square)
plot_StdRes_factorSquare                       
Rmisc::multiplot(plot_StdRes_factor,plot_StdRes_factorLog, plot_StdRes_factorSquare, layout=matrix(1:3,1,3))  #非线性问题也基本解决???
#--使用ncvTest()函数验证
car::ncvTest(fit.factor.square)                 #p???>0.05


##################################3.4.4 解决异方差-加权回归##################################
#########理论: 通常来讲，简单线性回归模型如果存在误差方差不相同的时候，误差方差都是随着预测变量(X)的增加而增???

#########针对3.4.3.1中的例子进行加权
summary(fit.factor)           #原始模型
plot_StdRes_factor            #原始模型的自定义残差???

#--残差序列???(by X) 
uf_gen_StdResidSeq(ResidDS.factor,"X","C")

lm(abs(Resid)~X, data = ResidDS.factor)   #有时间想想这个问??????????

#--加权wi = 1/xi
fit.factor.wt <- lm(Y~X, data = ds.factor, weights = 1/X)
summary(fit.factor.wt)        #加权模型

#--使用ncvTest()函数验证
car::ncvTest(fit.factor.wt)     

##################################3.4.5 解决自相关-DW统计量/残差序列图-变量变换-##################################
#########回归模型的统计假设之一就是误差独立性，即第i个和第j个观测值对应的误差项ei和ej是不相关???.

#########用一个例子来说明如何识别自相关问题，以及如何解决自相关问???
#######数据集：消费者支出与货币存量的关???
ds.comsue <- data.frame(i = c(1:20),
                        year = rep(c(1952:1956),each = 4),  #???
                        sess = rep(c(1:4), times = 5),      #季度
                        ###消费者支???
                        y = c(214.6,217.7,219.6,227.2,230.9,233.3,234.1,232.3,233.7,236.5,238.7,243.2,249.4,254.3,260.9,263.3,265.6,268.2,270.4,275.6),
                        ###货币存量
                        x = c(159.3,161.2,162.8,164.6,165.9,167.9,168.3,169.7,170.5,171.6,173.9,176.1,178.0,179.1,180.2,181.2,181.6,182.5,183.3,184.3))                            

fit.comsue <- lm(y~x, data = ds.comsue)
summary(fit.comsue)
#--生成模型残差数据???
ResidDS.comsue <- uf_gen_residDataset(orgDs = ds.comsue, model = fit.comsue, precision = 1)

##################################3.4.5.1 验证模型是否存在自相关问题##################################
#########检查模型是否存在自相关问题的方???:
#######1. DW检??? - car::durbinWatsonTest()  or  lmtest::dwtest()
#######2. 画残差序列图

#--DW检???
car::durbinWatsonTest(fit.comsue)     #因为p = 0, 说明模型存在自相关问??? (Autocorrelation = 0.75061)
lmtest::dwtest(fit.comsue)            #与DW检查类似的自相关检测函???
#--画残差序列图
plot_ResidSeq_comsue <- uf_gen_StdResidSeq(ResidDS.comsue, X = "i") 
plot_ResidSeq_comsue                  #残差在变???'i'序列上的一个模???: 连续几个观测的符号一???, 这是典型的自相关问题

##################################3.4.5.2 解决自相关-变量变换##################################
#########消除模型自相关问题的方法???
#######1. 对变量进行变???
#######2. 引进具有时间顺序效应的其他变???

#########如何进行变量变换? 
#######1. 计算自相关系???
r <- car::durbinWatsonTest(fit.comsue)[[1]]     #自相关系???(Autocorrelation) = 0.75061
#######2. 将响应变量yi变换为yi- r*yi-1;  将解释变量xi变化为xi=xi-p*xi-1
ds.comsue$yr <- round(c(0, ds.comsue$y[-1] - r * ds.comsue$y[-nrow(ds.comsue)]),1)
ds.comsue$xr <- round(c(0, ds.comsue$x[-1] - r * ds.comsue$x[-nrow(ds.comsue)]),1)
#######3. 用新生成的变量进行重新拟???
fit.comsue.r <- lm(yr~xr, data = ds.comsue[-1,])
summary(fit.comsue.r)
#--生成模型残差数据???
ResidDS.comsue.r <- uf_gen_residDataset(orgDs = ds.comsue[-1,], model = fit.comsue.r, precision = 1)
#--DW检???
car::durbinWatsonTest(fit.comsue.r)     #自相关系数降低了, p值也大于0.05
lmtest::dwtest(fit.comsue.r)            #与DW检查类似的自相关检测函???
#--残差序列???
plot_ResidSeq_comsue_r <- uf_gen_StdResidSeq(ResidDS.comsue.r, X = "i") 
plot_ResidSeq_comsue_r                  #残差在变???'i'序列上的模式消失

#########注意: 这种解决方案无法解决-误差的相关性具有高阶自回归结构的情???

##################################3.4.5.3 解决自相关-加入新的序列变量##################################
ds.sales <- data.frame(i = c(1:40),
                       JD = c("Q1/64","Q2/64","Q3/64","Q4/64","Q1/65","Q2/65","Q3/65","Q4/65",
                              "Q1/66","Q2/66","Q3/66","Q4/66","Q1/67","Q2/67","Q3/67","Q4/67",
                              "Q1/68","Q2/68","Q3/68","Q4/69","Q1/69","Q2/69","Q3/69","Q4/69",
                              "Q1/70","Q2/70","Q3/70","Q4/70","Q1/71","Q2/71","Q3/71","Q4/71",
                              "Q1/72","Q2/72","Q3/72","Q4/72","Q1/73","Q2/73","Q3/73","Q4/73"), #季度
                       Amount = c(37.0,33.5,30.8,37.9,37.4,31.6,34.0,38.1,40,35,34.9,40.2,41.9,34.7,38.8,43.7,44.2,40.4,38.4,45.4,44.9,41.6,44.0,48.1,49.7,43.9,41.6,51,52,46.2,47.1,52.7,52.2,47,47.8,52.8,54.1,49.5,49.5,54.3),     #销售量
                       PID =c(109,115,113,116,118,120,122,124,126,128,130,132,133,135,138,140,143,147,148,151,153,156,160,163,166,171,174,175,180,184,187,189,191,193,194,196,199,201,202,204),         #个人可支配收???
                       JJ = rep(c(1,0,0,1),times = 10)           #季节
)

fit.sale <- lm(Amount~PID, data = ds.sales)
summary(fit.sale)
#--生成模型残差数据???
ResidDS.sales <- uf_gen_residDataset(orgDs = ds.sales, model = fit.sale, precision = 1)
#--DW检???
car::durbinWatsonTest(fit.sale)     #根据结果，表明无自相关问???
lmtest::dwtest(fit.sale)            
#--残差序列???
plot_ResidSeq_sales <- uf_gen_StdResidSeq(ResidDS.sales, X = "i") 
plot_ResidSeq_sales                  #看似并无自相关问???
#--残差序列???(on variable 'JJ')
uf_gen_StdResidSeq(ResidDS = ResidDS.sales, X = "JJ")   #问题来了，JJ=0的残差全小于0??? JJ=1的残差全大于0

#--解决这个问题的方???: 把季节变???(JJ)加入模型???
fit.sale.jj <- lm(Amount~PID+JJ, data = ds.sales)
summary(fit.sale.jj)
#--生成模型残差数据???
ResidDS.sales.jj <- uf_gen_residDataset(orgDs = ds.sales, model = fit.sale.jj, precision = 1)
#--DW检???
car::durbinWatsonTest(fit.sale.jj)     #根据结果，表明无自相关问???       
#--残差序列???
plot_ResidSeq_sales_jj <- uf_gen_StdResidSeq(ResidDS.sales.jj, X = "i") 
plot_ResidSeq_sales_jj                  #看似并无自相关问???
#--残差序列???(on variable 'JJ')
uf_gen_StdResidSeq(ResidDS = ResidDS.sales.jj, X = "JJ")   #没有问题???

##################################3.4.6 时间序列数据集的回归##################################
#########对包含时间序列的数据???(非截面数据集)进行分析???(refer to 3.4.5.3)，需要特别注意自相关和季节???. 一些可能的解决方案:
#######1. 考虑把时间变量和季节变量加入模型???
#######2. 考虑把某些解释变量的滞后值也包含到模型中, ???: yi = b0 + b1*x1i + b2*x1i-1 + b3*x2 + ei
#######3. 如之前讲过的对响应变量和预测变量同时进行变换, ???: yi - p*yi-1 = b0 + b1*(xi-p*xi-1)

##################################3.4.7 解决共线性-VIF-主成分回归##################################
#######模型存在多重共线性，即表示模型中的解释变量之间存在很强的相关性???
#######需要注意的???: 对模型是否存在共线性问题的检查，应当在觉得模型已经满意了(满足了所有的统计假设,且符合常???)后开始???
#######是否存在多重共线性可用VIF(方差膨胀因子)值来判断；VIF代表的是预测变量之间的关系???
#######VIFj = 1/(1-Rj^2), Rj是预测变量Xj与其他预测变量做回归得到的复相关系数之平???; VIF>10被视为数据有共线性问题???
car::vif(fit)

#########解决共线性问题的方法???
#######1. 从模型中移除一些变???
#######2. 应用'主成???'回归方法

##################################3.5 异常点##################################
#########一个全面的回归分析要覆盖对异常值的分析，包括离群点、高杠杆值点和强影响???

##################################3.5.1 离群点##################################
#########离群点是指那些模型预测效果不佳的观测点。它们通常有很大的、或正或负的(标准)残差
#########鉴别离群点的方法:
#######1. Q-Q Plot - 落在置信区间带外的点即可被认为是离群???
car::qqPlot(fit,
            labels = row.names(ds.perf),
            id.method = "identify",
            simulate = TRUE,
            main = "Q-Q Plot")
#######2.标准化残差值大???2或者小于???2的点可能是离群点
ResidDS.fit[abs(ResidDS.fit$std_Resid) >= 2,]
#######3. outlierTest()函数
car::outlierTest(fit)

##################################3.5.2 高杠杆值点##################################
#########高杠杆值点是由异常的预测变量值造成的，与响应变量无???

#########高杠杆值的认定: 
#######1. 观测的帽子??? 大于 帽子均值的两倍或三???, refer to '模型残差数据???'

##################################3.5.3 强影响点##################################
#########强影响点会很大程度地影响模型的参数估??? - e.g. 若移除模型的一个观测点时模型的参数会发生巨大的改变

#########认定强影响点的方???:
#######1.观测的Cook’s D值大???4/(n-k-1)，则表明它是强影响点, refer to '模型残差数据???'

##################################3.6 回归中需要考虑的其他情况##################################
##################################3.6.1 交互性##################################
#########什么情况下需要在模型中引入交互项?
#########交互项的解释: 若两个预测变量的交互项显著，说明响应变量与其中一个预测变量的关系依赖于另外一个预测变量的水平
##################################3.6.2 变量选择-全子集/逐步回归##################################
#########全子集方???
fullSub.reg <- regsubsets(Y~X1+X2+X3+X4+X5+X6, data = ds.perf, nbest = 6)
plot(fullSub.reg, scale = "adjr2")                                           #评分标准: R_Square???

#########逐步回归方法
MASS::stepAIC(lm(Y~X1+X2+X3+X4, data = ds.perf), direction = "both")         #评分标准: AIC???


##################################4. 其他回归方法##################################
##################################4.1 广义线性模型##################################
#########对非正态分布的因变量，可以使用广义线性模型glm()来解决问???, e.g. 
#######1. Logistic回归（因变量为类别型???
#######2. 泊松回归（因变量为计数型）???

##################################4.2 加权最小二乘回归模型##################################
#########关于WLS???
#######1. 如果用最小二乘法(OLS)得到的模型不是误差i.i.d的，即可尝试WLS，WLS等价于对变换后的变量实施OLS???
#######2. WLS不仅可以解决异方???/自相关问题，它本身也是一种在某些情况下优于OLS的方???.
#######3. 在应用WLS时，我们不需要假定误差ei方差相等，但还是需要假定误差相互独立，且ei~N(0,gi^2)    
#######4. 参数估计的原???: 通过最小化SUM((yi - yi_) * wi)得到 (???:具有较小权重的观测的影响被削???) 
#######5. 如何确定wi? - 一般取wi = 1/(gi^2), 即wi与误差方差gi成反???. 

#########什么情况下使用WLS：如果OLS模型存在异方差，且模型的残差随某一解释变量的增加而增???(或减???)- 可用'残差序列???'考察 - 例子参见3.4.4

##################################4.2.1 一个例子说明如何确定合理的权重##################################
ds.edu <- data.frame(i = c(seq(from = 1, to = 49, by = 2),seq(from = 2, to = 50, by = 2)),
                     State = c("ME","VT","RI","NY","PA","IN","MI","MN","MO","SD","KS","MD","WV","SC","FL","TN","MS","LA","TX","ID",
                               "CO","AZ","NV","OR","AK","NH","MA","CT","NJ","OH","IL","WI","IA","ND","NB","DE","VA","NC","GA","KY",
                               "AL","AR","OK","MT","WY","NM","UT","WA","CA","HI"),   #州名???
                     #响应变量 - 1975年人均教育费???
                     Y = c(235,270,300,387,300,264,379,378,231,230,337,330,214,233,243,212,215,244,269,268,304,332,291,316,546,231,261,317,285,221,308,342,232,246,268,344,261,245,250,216,208,221,234,302,323,317,315,312,332,311),       
                     #X1 - 1973年人均收???
                     X1 = c(3944,4011,4780,5663,4894,4908,5439,4921,4672,4296,5057,5331,3828,3817,4647,3946,3448,3825,4336,4323,5046,4504,5560,4697,5613,4578,5233,5889,5759,5012,5753,4634,4869,4782,4827,5540,4715,4120,4243,3967,3724,3680,4189,4418,4813,3764,4005,4989,5438,5309),
                     #X2 - 1974年每1000人中年龄???18岁以下的居民???
                     X2 = c(325,328,303,301,300,329,337,330,309,330,304,323,310,342,287,315,358,355,335,344,324,340,330,305,386,323,305,307,310,324,320,328,318,333,318,328,317,321,339,325,332,320,306,335,331,366,378,313,307,333),
                     #X3 - 1970年每1000人中居住在城市的居民???
                     X3 = c(508,322,871,856,715,649,738,664,701,446,661,766,390,476,805,588,445,661,797,541,785,796,809,671,484,564,846,774,889,753,830,659,572,443,615,722,631,450,603,523,584,500,680,534,605,698,804,726,909,831),
                     #reg - 州所在区???(1-东北???2-中北???3-南部???4-西部)
                     reg = c(1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4)
)
ds.edu <- ds.edu[order(ds.edu$i),]
row.names(ds.edu) <- as.character(c(1:50))  
ds.edu <- ds.edu[-49,]

fit.edu <- lm(Y~X1+X2+X3, data = ds.edu)
summary(fit.edu)
#--模型残差数据???
ResidDs.edu <- uf_gen_residDataset(ds.edu,fit.edu,precision = 0)
#--自定义残差图
uf_gen_StdResidPlot(ResidDs.edu)
#--残差序列???
q1 <- uf_gen_StdResidSeq(ResidDs.edu,"X1","C")
q2 <- uf_gen_StdResidSeq(ResidDs.edu,"X2","C")
q3 <- uf_gen_StdResidSeq(ResidDs.edu,"X3","C")
q4 <- uf_gen_StdResidSeq(ResidDs.edu,"reg","F")
Rmisc::multiplot(q1, q2, q3, q4,  layout=matrix(c(1:4),2,2,byrow=TRUE))

#########以下内容，以后找时间整理
######对fit.edu进一步优化，我们就需要来考虑加权???
######此时注意到，fit3中，我们并没有把reg变量也添加到模型中，后续可以考虑引入，因为R-Squred值较???
######本例中，我们假定四个区域各自有不同的误差方差，分别是(c1*g)^2,(c2*g)^2,(c3*g)^2,(c3*g)^2, 其中g是模型的误差标准???, cj的取值不???
######此时，根据WLS的原理，回归系数应当由最小化下式得到:
######Sw = S1+S2+S3+S4, Sj = SUM( 1/(cj^2) * (yi-b0-b1*xi1-b2*xi2-b3*xi3-b4*xi4)^2 ), j = 1,2,3,4, 1/(cj^2)是权重，这样就能保证整个模型没有异方???
######但是cj的值是未知的，如何来确定啦???- 两阶段估计法
######第一阶段 - 
######得到原始模型(fit3, 删除了一个异常点)，及残差
######计算分类变量的列联表,得到分类变量每个值对应的观测数量
######第二阶段??? 计算cj^2 
resid_avg_total <- sum(ds.edu.rlt3$resid3 ^ 2) / nrow(ds.edu.rlt3)
resid_avg_reg1 <- sum(ds.edu.rlt3$resid3[ds.edu.rlt3$reg == "1"] ^ 2) / length(ds.edu.rlt3$resid3[ds.edu.rlt3$reg == "1"])
resid_avg_reg2 <-sum(ds.edu.rlt3$resid3[ds.edu.rlt3$reg == "2"] ^ 2) / length(ds.edu.rlt3$resid3[ds.edu.rlt3$reg == "2"]) 
resid_avg_reg3 <-sum(ds.edu.rlt3$resid3[ds.edu.rlt3$reg == "3"] ^ 2) / length(ds.edu.rlt3$resid3[ds.edu.rlt3$reg == "3"]) 
resid_avg_reg4 <-sum(ds.edu.rlt3$resid3[ds.edu.rlt3$reg == "4"] ^ 2) / length(ds.edu.rlt3$resid3[ds.edu.rlt3$reg == "4"]) 
cj <- sqrt(c(resid_avg_reg1,resid_avg_reg2,resid_avg_reg3,resid_avg_reg4) / avg_total)
wi <- c(rep(1/cj[1],8),rep(1/cj[4],13),rep(1/cj[3],16),rep(1/cj[4],12))

fit3_wt <- lm(Y~X1+X2+X3, data = ds.edu3, weights = wi)
summary(fit3_wt)
plot(fit3_wt, 1)

ds.edu.rlt3_wt <- cbind(ds.edu3, 
                        fitted3 = round(fitted(fit3),0), resid3 = round(residuals(fit3),0), std_resid3 = round(rstandard(fit3),2),
                        fitted3_wt = round(fitted(fit3_wt) * wi, 0)
)
ds.edu.rlt3_wt$resid3_wt <- ds.edu.rlt3_wt$Y * wi - ds.edu.rlt3_wt$fitted3_wt
ggplot(ds.edu.rlt3_wt, aes(x = fitted3, y = resid3)) + geom_point()
ggplot(ds.edu.rlt3_wt, aes(x = fitted3_wt, y = resid3_wt)) + geom_point()   #异方差不明显???
ggplot(ds.edu.rlt3_wt, aes(x = reg, y = resid3_wt)) + geom_point()
ggplot(ds.edu.rlt3_wt, aes(x = reg, y = resid3)) + geom_point()


##################################4.3 稳健回归-强影响点诊断##################################
#########什么时候需要用稳健回归:
#######当数据集包含过多的离群点或强影响点，就可以用稳健回归代替OLS。稳健回归也是一种用于发现强影响点的方法
#######稳健回归是通过迭代重复加权最小二乘（iterated re-weighted least squares ，IRLS)来完成的

##################################4.4 偏最小二乘法-相对重要性分析##################################
#########偏最小二乘法(PLS)可用于相对重要性分???

##################################4.4 局部加权线性回归(LWS)-loess曲线##################################
#########LWS与OLS,WLS的不???:
#######1. OLS的损失函数是J(θ0,θ1...θn) = 1/2m * SUM((hθ(xi) - yi)^2)
#######2. LWS的损失函数是J(θ0,θ1...θn) = 1/2m * SUM( wi * ((hθ(xi) - yi)^2) ), wi是权重，且并非一个定值；
#######3. WLS只是对OLS中的变量进行转换后再进行OLS??? 其损失函数跟OLS是一样的

x=c(4,4.5,5,8,9,6,7,10.8,10,9.7,4,4,5,8.6,6.8) 
y=c(4,3,5,10,8,4,13,5,5.5,7.5,4.5,2.5,7,11,8) 
testdata=data.frame(x,y)


#--普通最小二乘法
ols <- lm(y~x, testdata)
summary(ols)


#--局部加权回???
lws <- loess(y~x,data=testdata,span=0.75) 
summary(lws)

#--曲线对比
ggplot(data = testdata, aes(x = x, y = y)) + geom_point() + 
  geom_smooth(method = "lm", se=F, colour = "green") + 
  geom_smooth(method = "loess",span = 0.75, se=F, colour = "red")


##################################4.5 主成分回归-解决共线性问题##################################
##################################5. 梯度下降及其在线性回归中的应用##################################
#########梯度下降(Gradient Descent)

##################################5.1 梯度下降的相关概念##################################
#########梯度下降的直观解???(下山)
#########初始时，我们在山的某一点P0; 我们的目标是找到最快下山的???; 根据这个目标采取的策???:
#######1. 求解当前位置P0的梯度，沿着梯度的负方向(最陡峭的方???)向下走一???, 到达P1;
#######2. 到达P1后，求P1的梯度，到达P2; 
#######3. 迭代步骤2到直至Pn停止，Pn有可能不是山脚，而是某一局部的山峰低估.

#########梯度下降的相关概???:
#######1. 步长(Learning rate)：步长决定了在梯度下降迭代的过程中，每一步沿梯度负方向前进的长度???
#######2. 假设函数(hypothesis function): 假设函数（hypothesis function）：在监督学习中，为了拟合输入样本，而使用的假设函数???
#         记为hθ(x)。比如对于样本（x1i,x2i,...xni,yi???(i=1,2,...m),可以采用拟合函数如下：hθ(x) = θ0+θ1x1 + θ2x2+ ...θnxn;
#######3. 损失函数(loss function): 用于度量模型拟合的程???; 损失函数越小，表示模型拟合程度越???, 对应的模型参数即为最优参数???
#         在线性回归中，损失函数记???:J(θ0,θ1...θn) = 1/2m * SUM((hθ(xi) - yi)^2)

#########OLS中θ的求解过程: 最小化损失函数; (特别特别特别注意: 此时损失函数J中X和Y是已知的，θi才是变量)
#######1. 即通过∂J/∂??0 = 0;  ∂J/∂??1 = 0; ....∂J/∂θi = 0; 来求 θ0,θ1...θn; 

#########OLS vs. 梯度下降
#######OLS是直接对损失函数求导找出全局最小，而非迭代法；
#######梯度下降是一种迭代法，先设θi等于某个初始值，然后向损失函数下降最快的方向调整θi,在若干次迭代之后找到局部最???.

##################################5.2 从一个最简单的例子来深入解释OLS与梯度下降##################################
#########数据集ds只有一个特征x和一个响应变量y; 且只???3个观???
ds <- data.frame(x = c(1,2,3,4), y = c(1,2,3.5,4))
ggplot(data = ds,aes(x=x,y=y))+geom_point() + geom_line() + geom_smooth(method = "lm", se=F,colour = "green")
summary(lm(y~x,data = ds))
#######假设函数hθ(x) = θ0 + θ1*x = θ1*x (θ0 = 0)
#######损失函数J(θ1) = 1/6 * SUM( (θ1*xi - yi)^2 ) =  32.25*(θ1^2) - 63*θ1 + 30
#######目标: 最小化J(θ1)
#######OLS方法 - 求损失函数J(θ1)???(???)导函数等???0时的θ1值即??? - θ1 = 63/64.5
Deriv::Deriv(f = function(θ1) 32.25*(θ1^2) - 63*θ1 + 30, "θ1")
#######梯度下降的方??? - 迭代计算
θ1 <- 0                                     #1. 给??1一个初始???(自己设定)      θ1 = 0
J <- 32.25*(θ1^2) - 63*θ1 + 30              #2. 在初始值时损失函数的???        J(θ1) = 30
#θ1 <- θ1 - a * ∂J/θ1  #计算公式如下         #3. 将??1赋值为θ1减去速率因子a乘以损失函数J(θ1)关于θ1的偏导数
J <-  32.25*(θ1^2) - 63*θ1 + 30             #4. 用得到的新的θ1值计算J
#5. 迭代步骤3???4直到收敛 
#######对上述算法的一个简单实???
a <- 0.0001
θ1 <- 2
J <- 32.25*(θ1^2) - 63*θ1 + 30    
θ1_c <- c(θ1)
J_c <- c(J)
for(i in 1:10000){
  θ1 <- θ1 - a * (64.5 * θ1 - 63)
  θ1_c[i+1] <- θ1
  J <- 32.25*(θ1^2) - 63*θ1 + 30    
  J_c[i+1] <- abs(J)
}
θ1_c[which.min(J_c)]

#########梯度下降需要注意的???:
#######1. 需要注意的梯度下降算法中的参数a(表示步长)和??1初始值的选定
#######2. 当梯度下降到一定数值后，每次迭代的变化很小，这时可以设定一个阈值，
#         只要变化小于该阈值，就停止迭代，而得到的结果也近似于最优解???(类似与CART中的cp参数)

#########梯度下降算法调优:
#######1. 算法的步长选择???
#######2. 算法参数的初始值选择???
#######3. 归一化???

#########梯度下降的算法族???
#######1. 批量梯度下降法（Batch Gradient Descent??? -- 在更新参数θ时使用所有的样本来进行更???
#######2. 随机梯度下降法（Stochastic Gradient Descent??? - 在更新参数θ时使用所有样本的子集来进行更???
#######3. 小批量梯度下降法（Mini-batch Gradient Descent???- 介于1???2之间

##################################6. 项目实战-土拍地价预测##################################
#--C:\YuanLe\R\RWkDir\RLearn\RFoundation\Script\Titanic.R

##################################%%%%%%%%%%%%%%%%%%%%%分类与回归算法%%%%%%%%%%%%%%%%%%%%%########################################################
######分类树算法的核心:
######1. 分裂准则(Splitting Criterion) - 使得每个分支上的输出数据分区（Dj）尽可能‘纯’??? 可用的方???:
######A) 信息增益
######B) 增益率
######C) 基尼指数
######D) 卡方检验
######2. 树剪枝 - 处理过拟合情???
######A). 分裂停止条件 - 树的最大深度,叶节点的个数，分裂次数，叶节点的观测数量

##################################1. CART-rpart详解##################################
#######rpart的工作原理:
#######1. 根据分裂准则循环递归创建二叉树(binary tree),直到该树满足一定的条件(e.g. 叶节点的最小观测数;树的深度;再分裂不能提高准确率)
#######2. 使用交叉验证(cross-validation)来修剪步骤1中创建的树

#######示例数据Titanic
library(rpart)
library(rpart.plot) 

s.data <- read_csv("./data/Titanic/All_TEed3.csv")
glimpse(s.data)
s.data <- s.data[!is.na(s.data$Survived),]
#s.data <- s.data[sample(1:nrow(s.data),100),c("PassengerId","Survived","Pclass","Sex","Age")] 
s.data$Pclass <- ifelse(s.data$Pclass == 1, "A", ifelse(s.data$Pclass == 2,"B","C"))
s.data$Age <- as.integer(round(s.data$Age,0))

View(s.data)

######rpart建模
r.ct <- rpart.control(minsplit = 50,cp = 0.01,xval = 10, maxdepth = 30,
                      maxcompete = 4,
                      maxsurrogate = 1,
                      usesurrogate = 1)
###rpart.control说明:
###1. 用于控制树的生长过程
###2. 参数'minsplit = n' 表示被分裂的节点必须包含至少n个观测， 但是叶节点可以包含少于n个观???; 默认???20
###3. 参数'cp = p' 表示每次分裂，对模型的拟合优度的提高程度必须大于p; 默认???0.01
#(这样来理解这个变量：当我们做回归模型的时候，拟合优度用R_Squared来衡量，如果我们通过添加变量来试图提高拟合优度，
# 这时cp的值表示每次添加的变量对拟合优度的提升必须高于p,否则就不用加这个变量到模型中???
# 那么分类树的拟合优度是怎么算的啦？)
####4. 参数'xval = k' 表示模型使用k折交叉验???; 默认???10
####5. 参数'maxdepth = n' 表示树的最大深???, 默认???30
####6. 参数'maxcompete = n' 表示summary(tree_fit)的输出中'primary splits'下面显示的除当前分裂外的继续分裂数量不能大于n
#      这个参数的设置不会改变模型的预测能力; 默认???4
####7. 参数'maxsurrogate = n' 表示summary(tree_fit)的输出中'Surrogate splits'显示的分裂数量不能大于n
#      这个参数的设置不会改变模型的预测能力; 默认???5; 这个值越小，生成模型的计算量就越???
#      如何理解surrogate split, 参考下面的例子:
progstat <- factor(stagec$pgstat, levels = 0:1, labels = c("No", "Prog"))
cfit <- rpart(progstat ~ age + eet + g2 + grade + gleason + ploidy,
              data = stagec, method = 'class')
print(cfit)
summary(cfit)
rattle::fancyRpartPlot(cfit)
##从这个例子中看到, Node1的primary split ??? grade < 2.5 to the left
##Node1的first compete split和first surrogate split 是gleason < 5.5    to the left,  agree=0.863, adj=0.672, (0 split)
##first surrogate split应该是最优的替代primary split的方???, agree和adj是怎么来的？表示什么意???, 
##查看执行下面的代码得到的结果, 该结果中，正对角线的值的???(42+84)表示按fisrt surrogate split分的方案???126个都与primary split一???
##agree = (42+84) / 146 = 0.863; adj = (126-85)/(146-85) = 0.672;  146是当前节点的观测???,85是majority rule gets 85 correct???
with(stagec, table(cut(grade, c(0, 2.5, 4)),
                   cut(gleason, c(2, 5.5, 10)),
                   exclude = NULL))
#(2,5.5] (5.5,10] <NA>
#  (0,2.5]      42       16    3
#(2.5,4]       1       84    0
####8. 参数'usesurrogate=n' 0表示只是显示???1表示primary split的变量为空的时候使用surrogate split,2(默认)表示


####s.fit on s.data
s.fit <- rpart(Survived~Pclass+Sex+Age, data = s.data, method = "class", 
               parms = list(prior = c(0.61616161616, 0.38383838384), split = "gini"),
               control = r.ct)
###rpart说明:
###1. 参数'parms' 用来参数设置，当method='class'??? 可设置的参数???:
###1.1 先验概率(prior),  默认为prop.table(table(s.data$Survived)); 先验概率是已知的，为什么还可以随意设置?????
###1.2 分裂准则(split), 默认???'gini', 可???'information'
###1.3 损失矩阵(loss), '损失函数'详解???'知识??? - A.8. 损失函数'

printcp(s.fit)  #查看每次分裂的cp(对拟合优度提升的???)
#####第n个CP的??? = (??? n个rel error的??? - 第n-1个rel error的???)/2
s.fit$cptable #如果要引用cp的值可以这样做   
#xerror: 交叉验证的估计误??? - 我们希望模型的估计误差越小越???
#xstd: 交叉验证的估计误差的标准???
#平均相对误差=xerror±xstd 

print(s.fit) 
###输出详解
###1. n = 891 表示???891个观???
####node), split, n, loss, yval, (yprob)
###2. node) 表示第几个节???, 如果最后有*符号,表示该节点是叶节??? e.g. 5)
###3. split 表示父级节点分裂条件的一个结??? e.g. node5 Age<13表示node5中包含的是原始数据集s.data中Sex=male & Age<13的观???
###4. n 表示该节点包含的观测数量, e.g. node5的观测数??? = nrow(s.data[s.data$Sex == "male" & s.data$Age < 13,]) = 41
###5. yval 表示类标号的一个??? - 该类标号对应的观测数量在当前节点中数量最???; e.g. node5中的类标号数量如下：
table(s.data[s.data$Sex == "male" & s.data$Age < 13,]$Survived)  #Dead - 18个观???; Survived - 23个观???
#因为类标???'Survived'对应的观测数量更多，所有该节点上显示的???'Survived'; 这样做的原因???: 当该节点是叶节点时，显示的类标号就是
#最终模型预测的类标???
###6. (yprob) 表示该节点中对应的不同类标号的占???, 占比较少的就是最终模型在该节点上预测错误的概??? 
prop.table(table(s.data[s.data$Sex == "male" & s.data$Age < 13,]$Survived))
###7. loss 表示占比较少的类标号对应的观测数???, 如果该节点是叶节点就表示最终模型预测错误的??? e.g. node5  41*0.43902439024 = 18

###模型的详细输出信???
summary(s.fit)


###画图
plot(s.fit); text(s.fit)
rattle::fancyRpartPlot(s.fit)

######对生产的树进行剪???
######剪枝理论:
######1. 第一步，一般我们设置较小的minsplit和cp进行树的构建, 如下:
s.fit.p <- rpart(Survived~Pclass+Sex+Age, data = s.data, method = "class", 
                 control = rpart.control(cp = 0,minbucket = 5))
######2. 第二步，查看生成的树的情况，根据最小化损失函数值的原则，我们选取的标准是预测误差小，同时模型复杂程度又小的，
#        本例中我觉得xerror = 0.51461988的符合标???
printcp(s.fit.p); plot(s.fit.p); text(s.fit.p); print(s.fit.p)
######3. 第三步，在剪枝函数中设置对应的cp值进行剪枝得到新的模???,本例中设cp=0.004
s.fit.p.1 <- prune(s.fit.p,cp=0.004)      #设置cp=0.003
printcp(s.fit.p.1); plot(s.fit.p.1); text(s.fit.p.1)
######也可以在创建模型时就将cp设置???0.003进行数的创建，之所以看到每次生成的模型的误差都不一样，是交叉验证的随机抽样造成???
s.fit.p2 <- rpart(Survived~Pclass+Sex+Age, data = s.data, method = "class", 
                  control = rpart.control(cp = 0.004,minbucket = 5))
printcp(s.fit.p2); plot(s.fit.p2); text(s.fit.p2)


##################################2. Bagging - randomForest详解##################################
#######randomForest的工作原???:
#######1. 基于CART(rpart)回归/分类???,构建森林
#######2. 在构建决策树过程中，不进行任何剪枝动作，通过Bootstrap自助法随机挑选观测（行）和变量（列）形成每一棵树
#######3. 对于分类模型，随机森林将根据投票法为待分类样本进行分类；对于预测模型，随机森林将使用单棵树的简单平均值来预测样本的Y值???

#######随机森林分类性能的评???:
#######1. 每棵树生长越茂盛，组成森林的分类性能越好???
#######2. 每棵树之间的相关性越差，或树之间是独立的，则森林的分类性能越好???
#######注意：减小特征选择个数m，树的相关性和分类能力也会相应的降低；
#            增大m，两者也会随之增大??? (树的相关性与分类能力是一对相互制约的属???)
#            所以建立随机森林的关键问题是如何选择最优的m（或者是范围???


#######示例数据Titanic
library(randomForest)
library(ggplot2)
setwd("C:/YuanLe/R/RWkDir/RLearn/RFoundation/")
options(stringsAsFactors = FALSE)
options(java.parameters = "-Xmx4g")
options(digits = 10)

set.seed(10001)
data.rf <- read.csv("./Data/Titanic/All_TEed.csv"); names(data.rf)
data.rf$Survived <- as.factor(data.rf$Survived)
data.rf$Pclass <- as.factor(data.rf$Pclass)
data.rf$Sex <- as.factor(data.rf$Sex)
data.rf$Embarked <- as.factor(data.rf$Embarked)
data.rf$NameTitle <- as.factor(data.rf$NameTitle)
data.rf$ParTog <- as.factor(data.rf$ParTog)
data.rf$ChiTog <- as.factor(data.rf$ChiTog)
data.rf$SibTog <- as.factor(data.rf$SibTog)
data.rf$Role <- as.factor(data.rf$Role)
data.rf.subCol <- data.rf[,c("Survived","Pclass","Sex","Age","SibSp","Parch","Fare","Embarked","NameTitle","Total","ParTog","ChiTog","SibTog")]
train.rf <- data.rf.subCol[!is.na(data.rf.subCol$Survived),]
test.rf <- data.rf.subCol[is.na(data.rf.subCol$Survived),]
#View(data.rf)
#View(train.rf)


#######开始模???
rf.fit <- randomForest(Survived~.,
                       data = train.rf,
                       #指定用于构建每颗决策树的变量个数，默认情况下数据集变量个数的二次方根（分类模型）或三分之一（预测模型）???
                       #一般是需要进行人为的逐次挑选，确定最佳的m值； m值的确定很关???
                       mtry = round(sqrt(length(names(data.rf)))) + 1, 
                       na.action = na.omit,
                       ntree = 500,  #指定随机森林所包含的决策树数目，默认为500???
                       replace = T,  #指定Bootstrap随机抽样的方式，默认为有放回的抽样； 
                       nodesize = 1, #指定每颗决策树叶节点的最小个数，默认情况下，分类模型???1，回归模型为5???
                       importance = F, #是否计算各个变量在模型中的重要性，默认不计算，该参数主要结合importance()函数使用???
                       norm.votes = T, #默认为T, 如果为F, 将显示每棵树的投票结???
                       proximity = T, #是否计算模型的临近矩阵，主要结合MDSplot()函数使用???
                       do.trace = F #默认为F, 如果设置为T, 则输出更详细的随机森林模型运行过???
)
print(rf.fit)
#####OOB(out-of-bag) estimate of error rate解释:
#####1. OOB的量一般是数据集总数量的30%左右(依据是随机森林的bootstrap抽样方式决定???- 36.8%)
#####2. OOB error(袋外错误???)的??? = 混沌矩阵(confusion matrix)中负对角线的值之??? / 初始训练集的观测数量 = 
(rf.fit$confusion[1,2] + rf.fit$confusion[2,1]) / round(sum(rf.fit$confusion))
#####3. 调整参数mtry可以改变OOB error的值， 一个最优的随机森林模型即使OOB error最小的模型

######通过下面的方式可以获取模型rf.fit的更多信???
rf.fit$type    #随机森林的类型，是分类还是回???
rf.fit$predicted #fitted - 模型的拟合???, 根据这个结果和原始值可以算出模型的总体错误???
rf.fit$y         #actual - 原始数据   
rf.fit$err.rate  #每一颗树的OOB
rf.fit$confusion  #模型的OOB混沌矩阵
rf.fit$votes      #每一个观测的投票结果，用概率表示
rf.fit$importance  #模型中的变量的重要度
rf.fit$proximity   #模型的临近矩阵， 什么是临近矩阵
rf.fit$ntree       #森林中的树的棵数
rf.fit$mtry        #模型中每颗树用到的变量数
rf.fit$forest      #森林的详细信???


############对模型进行优??? - 找到最优的mtry
#####原则??? 使模型的OOB error最???
#names(train.rf)
n <- ncol(train.rf) - 1   #因变量个???
errRate <- c(1)           #用于存储每次循环的OOB???
for(i in 2:n){
  fm <- randomForest(Survived~.,data = train.rf,mtry = i, ntree = 200,proximity = T)
  errRate[i] <- mean(fm$err.rate)
}
print(errRate)  mean(rf.fit$err.rate)
####选择最小的error rate对应的mtry进行建模
####如何选择最佳的ntree????? (ntree值设置过低会导制错误率偏高，ntree值过高会提升模型复杂度，降低效率???)
rf.fit.I1 <- randomForest(Survived~.,data = train.rf,mtry = which.min(errRate)) 
plot(rf.fit.I1)
#####由图可知,当ntree???100附近是比较好???
rf.fit.I1 <- randomForest(Survived~.,data = train.rf,mtry = which.min(errRate), ntree = 100,proximity = T) 
print(rf.fit.I1)
plot(rf.fit.I1)

######得到一个优化后的模型后，可以考察模型中变量的重要???
rf.fit.I1$importance
importance(rf.fit.I1)
varImpPlot(rf.fit.I1)


######用优化后的模型rf.fit.I1进行预测
pred.T.pd <- predict(rf.fit.I1,test.rf)
pred.T.pd <- as.data.frame(pred.T.pd)
#View(pred.T.pd)
sbmt.T.pd <- data.frame(PassengerId =  as.integer(row.names(pred.T.pd)) , 
                        Survived = ifelse(pred.T.pd$pred.T.pd == "D", 0, 1) )
test.rf$Survived <- sbmt.T.pd$Survived
prop.table(table(sbmt.T.pd$Survived))
prop.table(table(train.rf$Survived))
write.csv(sbmt.T.pd,"./Data/Titanic/submission.csv",row.names = FALSE)


##################################4. Boosting - GBDT##################################
#########梯度提升决策树（Gradient Boosting Decision Tree，GBDT）代???:陈天奇的XGBoost和微软的LightGBM???
library(gbm)
library(ggplot2)
getwd()

s.data <- read.csv("./Data/Titanic/All_TEed2.csv")
View(s.data)
head(s.data)

uf_gbmInfo <- function(model, name){
  ds.gbmInfo <- data.frame(model = "model", Attibute = "", Value = "", Desc. = "")
  
  ds.gbmInfo <- rbind(ds.gbmInfo, data.frame(model= name, Attibute = "response.name", Value = model$response.name,Desc. = "响应变量"))
  ds.gbmInfo <- rbind(ds.gbmInfo, data.frame(model= name, Attibute = "distribution", Value = model$distribution$name[1],Desc. = "响应变量分布"))
  ds.gbmInfo <- rbind(ds.gbmInfo, data.frame(model= name, Attibute = "var.names", Value = paste(model$var.names, collapse="//"),Desc. = "解释变量"))
  ds.gbmInfo <- rbind(ds.gbmInfo, data.frame(model= name, Attibute = "var.monotone", Value = paste(model$var.monotone, collapse="//") ,Desc. = "显示指定解释变量与响应变量的正负相关???"))
  
  ds.gbmInfo <- rbind(ds.gbmInfo, data.frame(model= name, Attibute = "nTrain", Value = model$nTrain,Desc. = "训练数据???"))
  ds.gbmInfo <- rbind(ds.gbmInfo, data.frame(model= name, Attibute = "train.fraction", Value = model$train.fraction,Desc. = "用于训练的数据量比例，其他的数据用于验证"))
  ds.gbmInfo <- rbind(ds.gbmInfo, data.frame(model= name, Attibute = "bag.fraction", Value = model$bag.fraction,Desc. = "每次迭代随机选取的数据量比例,其他的数据用于计算oob error"))
  ds.gbmInfo <- rbind(ds.gbmInfo, data.frame(model= name, Attibute = "shrinkage", Value = model$shrinkage,Desc. = "模型的学习速率"))
  ds.gbmInfo <- rbind(ds.gbmInfo, data.frame(model= name, Attibute = "n.trees", Value = model$n.trees,Desc. = "模型迭代次数"))
  ds.gbmInfo <- rbind(ds.gbmInfo, data.frame(model= name, Attibute = "interaction.depth", Value = model$interaction.depth,Desc. = "每次迭代可以分裂的次???"))
  ds.gbmInfo <- rbind(ds.gbmInfo, data.frame(model= name, Attibute = "n.minobsinnode", Value = model$n.minobsinnode,Desc. = "叶节点至少包含的观测???"))
  
  
  ds.gbmInfo <- rbind(ds.gbmInfo, data.frame(model= name, Attibute = "initF", Value = model$initF,  Desc. = "If train.fraction=1, initF = 响应变量的平均???"))
  ds.gbmInfo <- rbind(ds.gbmInfo, data.frame(model= name, Attibute = "train.error", Value = paste0(range(model$train.error)[1]," ~ ", range(model$train.error)[2]),  
                                             Desc. = "每次迭代的损失函数???(基于训练数据计算)"))
  ds.gbmInfo <- rbind(ds.gbmInfo, data.frame(model= name, Attibute = "valid.error", Value = paste0(range(model$valid.error)[1]," ~ ", range(model$valid.error)[2]),  
                                             Desc. = "每次迭代的损失函数???(基于验证数据计算，train.fraction<0的时候计???)"))
  ds.gbmInfo <- rbind(ds.gbmInfo, data.frame(model= name, Attibute = "oobag.improve", Value = paste0(range(model$oobag.improve)[1]," ~ ", range(model$oobag.improve)[2]),  
                                             Desc. = "每次迭代的oobag提升???(基于袋外数据计算，bag.fraction<0的时候计???)"))
  ds.gbmInfo <- rbind(ds.gbmInfo, data.frame(model= name, Attibute = "cv.folds", Value = model$cv.folds,
                                             Desc. = "交叉验证的折???"))
  ds.gbmInfo <- rbind(ds.gbmInfo, data.frame(model= name, Attibute = "cv.error", Value = ifelse(is.null(model$cv.error),"no cv", paste0(range(model$cv.error)[1]," ~ ", range(model$cv.error)[2])),
                                             Desc. = "交叉验证的损失函数???"))
  
  return(ds.gbmInfo[-1,])
  
}
#########建模目的：用GBDT算法来预测缺失的年龄
ds.AP <- s.data
str(ds.AP)

ds.AP$Pclass <- as.factor(ds.AP$Pclass)
ds.AP$Sex <- as.factor(ds.AP$Sex)
ds.AP$Embarked <- as.factor(ds.AP$Embarked)
ds.AP$NameTitle2 <- as.factor(ds.AP$NameTitle2)
ds.AP$ParTog <- as.factor(ds.AP$ParTog)
ds.AP$ChiTog <- as.factor(ds.AP$ChiTog)
ds.AP$SibTog <- as.factor(ds.AP$SibTog)
ds.AP$Role <- as.factor(ds.AP$Role)

ds.AP.ForPred <- ds.AP[is.na(ds.AP$Age),]
ds.ap.ForModel <- ds.AP[!is.na(ds.AP$Age),]
randomSeq <- sample(1:nrow(ds.ap.ForModel),nrow(ds.ap.ForModel),replace = F)
ds.AP.Train <- ds.ap.ForModel[randomSeq[1:round(length(randomSeq) * .9, 0)],]
ds.AP.Test <- ds.ap.ForModel[!(rownames(ds.ap.ForModel) %in% rownames(ds.AP.Train)),]
names(ds.ap.ForModel)

#########gbm()函数构建梯度提升模型，需要注意：
#######'1. 解释变量必须是数值，因子或有序因???
Age.gbm.fit <- 
  gbm(formula = Age~Pclass+Sex+NameTitle2+ParTog+ChiTog+Embarked,
      data = ds.AP.Train,
      var.monotone = rep(0,6),       #解释变量与响应变量的相关关系的正???, 0表示不确???
      distribution = "gaussian",     #指定响应变量的分布类???
      
      shrinkage = 0.005,             #学习速率(梯度的步???), 一般介???0.01-0.001之间；在不考虑性能的情况下，越小越???
      n.trees = 10000,               #跟shrinkage配合使用，shrinkage越小，需要的迭代(???)量就越大
      
      interaction.depth = 3,         #number of splits it has to perform on a tree 
      n.minobsinnode = 1,            #跟interaction.depth配合, 每颗树不能太深，叶节点的观察量不能太小以防止过拟???
      
      cv.folds = 0,                  #k折交叉验???; 要特别注意如果数据量太小容易出现问题
      
      bag.fraction = 1,              #每次迭代抽样的比例，=1表示每次迭代用全???
      train.fraction = .8,           #第一次迭代建树投入的样本数量比例
      verbose = FALSE)
#########gbm参数详解:
#######'1. 'distribution', 设定响应变量的分???, 不同的响应变量分布对应不同的损失函数; 在开始建模之前一定要搞清楚响应变量对应的分布类型
#####'1.1. gaussian(高斯分布)       - 平方误差
#####'1.2. laplace(拉普拉斯分布)    - 误差绝对???
#####'1.3. tdist(学生t-分布)        - t-distribution loss (default degrees of freedom is 4，可修改 distribution=list(name="tdist", df=DF) )
#####'1.4. bernoulli(贝努力分???)    - ogistic regression for 0-1 outcomes
#####'1.5. huberized
#####'1.6. multinomial(多项式分???)  - classification when there are more than 2 classes
#####'1.7. adaboost                 - the AdaBoost exponential loss for 0-1 outcomes 
#####'1.8. poisson(泊松分布)        - count outcomes
#####'1.9. coxph
#####'1.10 quantile                 -
#####'1.11 pairwise
#######'2. 'n.trees'      - 设置为多少是合理????????
#######'3. 'cv.folds'     - k折交叉验证，rpart中一般取10??? 注意输出cv.error(泛化误差,generalization error)
#######'4. 'interaction.depth'  - number of splits it has to perform on a tree ;
#######'      As each split increases the total number of nodes by 3 and number of terminal nodes by 2 
#######'      (node ??? {left node, right node, NA node}) the total number of nodes in the tree will be 3∗N+1 and 
#######'      the number of terminal nodes 2∗N+1. This can be verified by having a look at the output of pretty.gbm.tree function
#######'5. "n.minobsinnode" - 叶节点的观测数量的最小????????
#######'6. "shrinkage" - 学习速率,也可以理解为梯度下降中提到的步长;
#######'                 gbm作者的经验法则是设置参数在0.01-0.001之间
#######'7. "bag.fraction" - 再抽样比???, 下次迭代中随机抽样数据集量的比???; 数据量小的时候可以设置为1
#######'                    可理解为跟随机梯度下降里面每次迭代选取样本的比值一样???
#######'                    跟RF模型里面算out-of-bag error(OOB error)一??? 
#######'8. "train.fraction" - 第一次迭代建树投入的样本数量比例；如???<1??? 剩余的样本将用作计算out-of-sample的损失函数???
#######'9. "verbose" - if TRUE, 将打印建模过???
#######'10. "class.stratify.cv"  - 是适用于distribution是c("multinomial","bernoulli");
#######'                           为TRUE表示在交叉验证抽样的过程中要保证训练集中的观测要包含所有的类标

#########gbm.object的详细信???
Age.gbm.fit
Age.gbm.fit$initF             #the "intercept" term, the initial predicted value to which trees make adjustments
Age.gbm.fit$fit               #fitted value for training set
Age.gbm.fit$train.error       #每次迭代的损失函数???(训练数据)  range(Age.gbm.fit$train.error)
Age.gbm.fit$valid.error       #每次迭代的损失函数???(验证数据)  range(Age.gbm.fit$valid.error)
Age.gbm.fit$oobag.improve     #只有当bag.fraction<1时才有???
Age.gbm.fit$cv.folds          #交叉验证的折???
Age.gbm.fit$cv.error          #交叉验证的损失函数???(用于交叉验证的测试集)
Age.gbm.fit$cv.fitted         #交叉验证的拟合???
Age.gbm.fit$trees[[10000]]　　#每颗树的节点的数???=3*interaction.depth+1
Age.gbm.fit$c.splits[[1]]
Age.gbm.fit$var.levels

#--最佳迭代次???
best.iter <- gbm.perf(Age.gbm.fit)
#--变量的重要???
summary(Age.gbm.fit,best.iter)

plot.gbm(Age.gbm.fit,"ParTog",best.iter)
plot.gbm(Age.gbm.fit,"Pclass",best.iter)
#plot.gbm(Age.gbm.fit,"Fare",best.iter)
plot.gbm(Age.gbm.fit,"NameTitle2",best.iter)
plot.gbm(Age.gbm.fit,"ChiTog",best.iter)
plot.gbm(Age.gbm.fit,"Embarked",best.iter)

#--创建一个数据集来分析训练集的残???
ds.AP.Train.Rlt <- ds.AP.Train
ds.AP.Train.Rlt$Age_Pred <- round(Age.gbm.fit$fit,0)
ds.AP.Train.Rlt$Resid <- ds.AP.Train.Rlt$Age_Pred - ds.AP.Train.Rlt$Age

ggplot(data = ds.AP.Train.Rlt, aes(x = Resid)) + geom_density()

#--用模型来预测测试数据
ds.AP.Test$Age_Pred <- round(predict.gbm(Age.gbm.fit, ds.AP.Test,n.trees = best.iter))
ds.AP.Test$Resid <- ds.AP.Test$Age_Pred - ds.AP.Test$Age
ggplot(data = ds.AP.Test, aes(x = Resid)) + geom_density()

#--用模型来预测待预测数???
ds.AP.ForPred$Age_Pred <- round(predict.gbm(Age.gbm.fit, ds.AP.ForPred,n.trees = best.iter))
View(ds.AP.ForPred)

#--将预测到的年纪更新到源数据中???
s.data[s.data$PassengerId %in% ds.AP.ForPred$PassengerId,c("Age")] <- ds.AP.ForPred[,c("Age_Pred")]
write.csv(s.data,"./Data/Titanic/All_TEed2.csv", row.names = F)

##################################5. Boosting - XGBoost##################################
library(xgboost); library(tidyverse); library(data.table); library(vcd); library(Matrix); library(Ckmeans.1d.dp)

#########About XGBoost(in R): https://xgboost.readthedocs.io/en/latest/R-package/xgboostPresentation.html
#######'1. Xgboost is short for eXtreme Gradient Boosting package.(It is an efficient and scalable implementation of gradient boosting framework )
#######'2. It supports various objective functions, including regression, classification and ranking. 
#######'3. Features of XGBoost:
#######'3.1. Speed: it can automatically do parallel computation on Windows and Linux, with OpenMP. It is generally over 10 times faster than the classical gbm.
#######'3.2. Input Type:
#######'3.2.1. Dense Matrix: R’s dense matrix, i.e. matrix ;
#######'3.2.2. Sparse Matrix: R’s sparse matrix, i.e. Matrix::dgCMatrix ;
#######'3.2.3. Data File: local data files ; e.g. csv, LibSVM 
#######'3.2.4. xgb.DMatrix: its own class (recommended).
#######'3.3. Sparsity: it accepts sparse input for both tree booster and linear booster, and is optimized for sparse input ;
#######'3.4. Customization: it supports customized objective functions and evaluation functions.

#########Xgboost manages only numeric vectors.

##################################5.1. Get Started##################################
data(Arthritis)                           #原始数据
head(Arthritis); str(Arthritis)

#--将原始数据集转换成稀疏矩??? (one-hot encode 处理)
spMtrx.Arthritis <- sparse.model.matrix(Improved~.-1, data = Arthritis, na.action = "na.pass")     
spMtrx.Arthritis@Dimnames; class(spMtrx.Arthritis)
output_vector <- Arthritis[,"Improved"]            #将类标号单独存到一个向量中
output_vector[output_vector=="Some"] <- "Marked"; output_vector <- ifelse(output_vector == "Marked",1,0)

#--xgb model
bst <- xgboost(data = spMtrx.Arthritis,
               label = output_vector,
               max.depth = 4,
               eta = 1, 
               nthread = 2, 
               nround = 10,
               objective = "binary:logistic")
importance <- xgb.importance(feature_names = spMtrx.Arthritis@Dimnames[[2]], model = bst); importance
#########结果解释:
#######'1. Gain:  代表分裂变量X后对每个节点纯度的提??? (所有树的所有节点的综合指标)
#######'2. Cover: 代表由分裂变量X所得的每个节点的观测的数量 (所有树的所有节点的综合指标)

##思???: 如果解释变量A与解释变量B高度相关，分别用Random Forest ??? XGB计算 variable importance时，得到的结果会不一???; XGB的会更准确一些，为什????
xgb.plot.importance(importance); xgb.ggplot.importance(importance)


##################################5.2. 重要参数介绍##################################
#--模型参数设置(下面参数的值是Kaggle HomeCredit项目设置的???)
#--???***的是必须设置和调试的参数
myxgb.params <-  
  list(
    #--General Parameters
    booster = "gbtree",             #tree based model; if "gblinear", then use linear functions
    silent = 0,                     #print running message; if "1", then silent mode
    nthread = 4,                    #Number of parallel threads used to run XGBoost; if not set, then default to maximum number of threads available
    
    #--Booster Parameters
    eta = 0.05,                     #***学习速率(step size shrinkage / learning_rate); default=0.3; 建议初始???0.1, 调试范围: [0.05,0.3]; 
    gamma = 0,                      #***min_split_loss, 节点分裂所需的最小损失函数下降???; default=0; 建议初始???0, 调试范围: [0,0.2]; 根据损失矩阵调试 
    max_depth = 6,                  #***每次迭代生成的树的最大深???;default=6;range: [0,∞]; 树太深容易过拟合，不够深会导致欠拟合; 建议初始???5, 调试范围: [3,10]; 
    min_child_weight = 30,          #***叶节点样本权重和，更叶节点样本数量不太一样，参考boosting中样本的权重机制；建议初始???1, 根据数据量和数据平衡度调???
    max_delta_step = 0,             #限制每棵树权重改变的最大步???; 0表示没有限制
    subsample = 0.8,                #***每次迭代数据随机采样比例。跟gbm中bag.fraction一???; 1表示每次迭代都使用全部数???;减小这个参数的值，算法会更加保守，避免过拟???; 建议初始???0.8, 调试范围: [0.5,0.9]; 
    colsample_bytree = 0.7,         #***每次迭代特征随机采样比例??? 建议初始???0.8, 调试范围: [0.5,0.9]; 根据数据的变量数调试
    colsample_bylevel = 0.632,      #***每次迭代构建树时的每一级的每一次分裂的特征随机采样比例???
    lambda = 0,                     #权重的L2正则化项;用来控制XGBoost的正则化部分???; default=1
    alpha = 0,                      #权重的L1正则化项; default=1
    scale_pos_weight = 1,           #在各类别样本十分不平衡时，把这个参数设定为一个正值，可以使算法更快收???; A typical value to consider: sum(negative instances) / sum(positive instances)
    
    #--Learning task parameters(学习目标参数)
    objective = "binary:logistic",  #***根据目标变量的类型改???
    eval_metric = "auc",            #***验证数据的评估指???;默认值根据objective的值设???;
    seed = 0,                       #随机数的种子;设置它可以复现随机数据的结果，便于参数调???;default=0
    nrounds = 2000                  #迭代booster的次???
  )

##################################5.3. General Sample##################################
all <- read.csv("./Data/Others/HomeCredit_Sub_TEed.csv", stringsAsFactors = T)
str(all);head(all); dim(all)

#--将数据分割成训练数据/测试数据/预测数据
FP <- all %>% filter(is.na(TARGET))
FM <- all %>% filter(!is.na(TARGET))
set.seed(1111)
RmdSeq <- sample(1:nrow(FM),nrow(FM),replace = F)
train <- FM[RmdSeq[1:round(length(RmdSeq)*.8)],]
test <- FM[!(FM$SK_ID_CURR %in% train$SK_ID_CURR),]
train <- train %>% select(-2); nrow(train)
test <- test %>% select(-2);   nrow(test)
nrow(test)+nrow(train)+nrow(FP)==nrow(all)

#--将原数据集转换成稀疏矩???
options(na.action="na.pass"); options("na.action");
spM.train <- sparse.model.matrix(TARGET~.-1, data = train); nrow(spM.train)
spM.test <- sparse.model.matrix(TARGET~.-1, data = test); nrow(spM.test)

#--将稀疏矩阵转换为xgb.DMatrix
dtrain <- xgb.DMatrix(data = spM.train, label = train$TARGET); class(dtrain); dtrain; xgb.DMatrix.save(dtrain, "./Data/Others/xgb.dtrain")
dtest  <- xgb.DMatrix(data = spM.test,  label = test$TARGET);  class(dtest);  dtest;  xgb.DMatrix.save(dtest, "./Data/Others/xgb.dtest")
getinfo(dtrain,"label");getinfo(dtrain,"nrow");getinfo(dtrain,"weight"); getinfo(dtrain,"base_margin")

#--设置xgb参数的初始???
ini.params <-  
  list(
    #--General Parameters
    booster = "gbtree",             
    silent = 0,                     
    nthread = 2,                    
    
    #--Booster Parameters
    eta = 0.05,                     
    gamma = 0,                      
    max_depth = 5,                  
    min_child_weight = 1,            
    subsample = 0.8,                
    colsample_bytree = 0.8,         
    colsample_bylevel = 0.8,      
    lambda = 1,                     
    alpha = 1,                
    
    #--Learning task parameters(学习目标参数)
    objective = "binary:logistic",  
    #eval_metric = "auc",
    eval_metric = "error"
  )

set.seed(1112)
fit.xgb <- xgb.train(ini.params, 
                     dtrain, 
                     missing = NA,   #--tell XGBoost that if the value of a feature is NA, then handle the record as missing value.
                     nrounds = 10,
                     watchlist = list(train = dtrain, val = dtest), 
                     print_every_n = 2, 
                     #--xgboost will terminate the training process if the performance is getting worse in the iteration
                     #--early_stopping_rounds=10 means if the performance is not getting better for 3 steps, then the program will stop.
                     early_stopping_rounds = 10 
)

#--用当前fit.xgb来预测训练集
pred_train <- predict(fit.xgb, dtrain, outputmargin=TRUE) #outputmargin indicates that we don’t need a logistic transformation of the result.
#--将预测结果加入训练集，以便继续基于预测结果生成更多的???
setinfo(dtrain, "base_margin", pred_train)

#--基于已经生成的模型和预测结果，继续更多的迭代以生成过多的???
fit.xgb <- xgb.train(ini.params, 
                     dtrain,           #这个训练集包含了"base_margin"信息
                     missing = NA,
                     nrounds = 10,
                     watchlist = list(train = dtrain, val = dtest), 
                     print_every_n = 2, 
                     early_stopping_rounds = 10 
)

#--基于模型fit.xgb生成训练数据的混沌矩阵，以检验错误率与模型的"train-error"输出是否匹配
pred_train <- as.numeric(predict(fit.xgb, dtrain, ntreelimit = 10) > 0.5) #ntreelimit 表示用多少棵树的模型进行预测
cfu_matrix <- table(train$TARGET, pred_train)
errRate <- (cfu_matrix[1,2] + cfu_matrix[2,1])/length(pred_train)  #0.080189, 与模型的evl_metric匹配 


#--变量重要性分???
#--How do we define feature importance in xgboost?
#--In xgboost, each split tries to find the best feature and splitting point to optimize the objective. 
#--We can calculate the gain on each node, and it is the contribution from the selected feature. 
#--In the end we look into all the trees, and sum up all the contribution for each feature and treat it as the importance. 
impt.xgb <- xgb.importance(feature_names = spM.train@Dimnames[[2]], model = fit.xgb)
xgb.ggplot.importance(importance_matrix = impt.xgb,top_n = 30)

#--查看迭代的详细过???
xgb.dump(fit.xgb, with_stats = T, dump_format = "text")

#--Save/Load model
xgb.save(bst, "./Data/Others/fit.xgb")
fit.xgb <- xgb.load("./Data/Others/fit.xgb")

##################################5.4. Model Inspection##################################
library(DiagrammeR)
data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test

#--创建模型
bst <- xgboost(data = train$data, label = train$label, max.depth = 15, min_child_weight = 30,
               eta = 1, nthread = 2, nround = 10, objective = "binary:logistic")

#--查看每棵树的结构
xgb.plot.tree(feature_names = agaricus.train$data@Dimnames[[2]], model = bst, trees = c(0,1,4))

#--查看变量相对重要???
impt.xgb <- xgb.importance(feature_names = agaricus.train$data@Dimnames[[2]], model = bst)
xgb.ggplot.importance(importance_matrix = impt.xgb,top_n = 30)

#--Visualizes distributions related to depth of tree leafs.
#--可以根据'weighted cover'来调整模型参???'min_child_weight'; 根据'leaf depth'来调整模型参???'max.depth'
#--上图表示: 深度为x的叶节点数量y;  下图表示: 深度为的x叶节点的noramlized weighted cover
xgb.ggplot.deepness(model = bst)

##################################5.5. Sample of hyper parameter tuning##################################
train <- read_csv("./Data/Others/Train_HyperParamTuning_XGBoost.csv")
test <- read_csv("./Data/Others/Test_HyperParamTuning_XGBoost.csv")
nrow(train); nrow(test)
View(train); View(test)

#########Feture Engineering
all <- rbind(train[,c(1:24,26)], cbind(test[,1:24],Disbursed = NA) )
#View(all); str(all); dim(all); names(all)
all %>% filter(!is.na(Disbursed)) %>% select(Disbursed) %>% table() %>% prop.table()   #响应变量的分???

#--remove variable 'City'
all <- all %>% select(-City)

#--DOB converted to Age (current = 2010???) | DOB dropped
all$Age <- 110 - as.integer(substr(all$DOB,8,10))
all <- all %>% select(-DOB)

#--EMI_Loan_Submitted_Missing created which is 1 if EMI_Loan_Submitted was missing else 0 
all$EMI_Loan_Submitted_Missing <- ifelse(is.na(all$EMI_Loan_Submitted),1,0)
all <- all %>% select(-EMI_Loan_Submitted)

#--remove variable 'Employer_Name'
all <- all %>% select(-Employer_Name)

#--Existing_EMI imputed with 0 
all[is.na(all$Existing_EMI),"Existing_EMI"] <- 0

#--Interest_Rate_Missing created which is 1 if Interest_Rate was missing else 0
all$Interest_Rate_Missing <- ifelse(is.na(all$Interest_Rate),1,0)
all <- all %>% select(-Interest_Rate)

#--remove Lead_Creation_Date 
all <- all %>% select(-Lead_Creation_Date)

#--Loan_Amount_Applied, Loan_Tenure_Applied imputed with median values
all[is.na(all$Loan_Amount_Applied),"Loan_Amount_Applied"] <- median(all$Loan_Amount_Applied, na.rm = T)
all[is.na(all$Loan_Tenure_Applied),"Loan_Tenure_Applied"] <- median(all$Loan_Tenure_Applied, na.rm = T)

#--Loan_Amount_Submitted_Missing created which is 1 if Loan_Amount_Submitted was missing else 0
all$Loan_Amount_Submitted_Missing <- ifelse(is.na(all$Loan_Amount_Submitted),1,0)
all <- all %>% select(-Loan_Amount_Submitted)

#--Loan_Tenure_Submitted_Missing created which is 1 if Loan_Tenure_Submitted was missing else 0
all$Loan_Tenure_Submitted_Missing <- ifelse(is.na(all$Loan_Tenure_Submitted),1,0)
all <- all %>% select(-Loan_Tenure_Submitted)

#--remove Salary_Account
all <- all %>% select(-Salary_Account)

#--Processing_Fee_Missing created which is 1 if Processing_Fee was missing else 0 
all$Processing_Fee_Missing <- ifelse(is.na(all$Processing_Fee),1,0)
all <- all %>% select(-Processing_Fee)

#--Source ??? top 2 kept as is and all others combined into different category
unique(all$Source)
ggplot(data = all, aes(Source)) + geom_bar()
all[!(all$Source %in% c("S122","S133")),"Source"] <- "other"

#--NA value check
apply(all, 2, FUN = function(x){any(is.na(x))})

write_csv(all,"./Data/Others/all_HyperParamTuning_XGBoost.csv")

#########Read EFed data
all <- read_csv("./Data/Others/all_HyperParamTuning_XGBoost.csv")
#View(all); str(all); dim(all); names(all)




##################################6. Boosting - AdaBoost##################################
##################################7. 朴素贝叶斯##################################
##################################7.1 预备知识: 概率##################################
#----概率的基本性质
#--1. 对任意事件A, 0<=P(A)<=1;
#--2. 对于任意两个随机事件A和B, P(A U B) = P(A) + P(B) - P(A ??? B)
#例子:
#已知???
#P(A) = 20% 表示读甲类报子的概率
#P(B) = 16% 表示读乙类报子的概率
#P(A ??? B) = 8% 表示两种报子都读的概???, 及事件A与B的交集的概率
#求：
#P(A U B) = ? 表示至少对一种报子的概率, 及事件A与B的并集的概率
#P(A u B) = P(A) + P(B) - P(A ??? B) = 28%

#----条件概率
#--P(B|A) 表示事件A已经发生的条件下时间B发生的概???. 
#--P(B|A) = P(AB) / P(A); P(AB)即是P(A ??? B)即是联合概率

#----贝叶斯定???
P(B)P(A|B) = P(A)P(B|A)


##################################7.2 Naive Bayesian##################################
#########朴素贝叶斯（Naïve Bayesian）分类法的工作过程如下：
#######1. ???

##################################8. Lasso & Ridge Regression##################################
library(glmnet); library(ISLR)
#View(Hitters); str(Hitters); dim(Hitters)
Hitters <- na.omit(Hitters)
RmdSeq <- sample(1:nrow(Hitters),nrow(Hitters),replace = F)
trainSeq <- RmdSeq[1:round(length(RmdSeq)*.8)]
testSeq <- RmdSeq[-trainSeq]
train <- Hitters[trainSeq,]
test <- Hitters[testSeq,]

Dtrain <- model.matrix(Salary~., train)[,-1]       #将原数据集转换为矩阵，同时进行one-hot encoding
Dtrain.label <- train$Salary                             #响应变量
Dtest <- model.matrix(Salary~., test)[,-1]
Dtest.label <- test$Salary

#--设置初始超参({param0})开始建???
fit.lasso <- 
  glmnet(x = Dtrain,y = Dtrain.label,
         family = "gaussian",                    #根据响应变量的分布设???
         weights = rep(1,length(Dtrain.label)),  #每个观测的权???
         alpha = 1,                              #alpha???0表示岭回归模型，???1表示lasso回归模型
         nlambda = 100,                          #lambda值的数量，default = 100
         lambda.min.ratio = 0.0001,              #0.0001 for regression, 0.01 for classification
         #lambda = grid,                         #一般不要手动设???
         exclude = c(11),                        #指定不需要引入模型的解释变量
         penalty.factor = c(rep(1,14),0,rep(1,4)), #手动设置每个解释变量系数的惩罚参数，默认都为1,0则表示不对该变量惩罚
         #lower.limits = c(rep(-100,19)),           #手动设置每个解释变量系数的下限???
         #upper.limits = c(rep(100,19)),            #手动设置每个解释变量系数的上限???
         intercept = T,                          #是否需要截距项? default = T 
         standardize = T,                        #是否对解释变量进行标准化
         thresh = 1E-7                           #坐标下降的收敛阀???
  ) 
print(fit.lasso)
#--查看每个λ值对应模型的信息(为什么没???100个模????? - 当连续几??? %Dev 变化很小??? glmnet() 会自动停???)
#--Df 是自由度，代表了非零的线性模型拟合系数的个数
#--%Dev 代表了由模型解释的残差的比例，对于线性模型来说就是模型拟合的R^2（R-squred???
#--正则项越???(λ???)越大，系数越???; 当??=0时，模型就等同于OLS
range(fit.lasso$lambda);range(log(fit.lasso$lambda)); #λ值的范围
dim(coef(fit.lasso))                                  #20*100 matrix, 每个变量都有100个λ对应的系数
coef(fit.lasso, s=fit.lasso$lambda[c(32,76)])         #查看特定λ值对应的模型的系???; 
predict(fit.lasso, s = 10, type = "coefficients")     #用新的λ来生成新的变量系数
fit.lasso$nobs                                        #表示length(unique(响应变量))
fit.lasso$df                                          #表示每个λ对应的模型中引用的解释变量的数目
plot(fit.lasso, xvar="lambda", label=TRUE)            #每个变量的系数随λ值的变化情况
plot(fit.lasso, xvar="norm", label=TRUE)              #每个变量的系数随L1正则项值的变化情况
plot(fit.lasso, xvar="dev", label=TRUE)               #每个变量的系数随L1正则项值的变化情况

#--综上，当λ<=0.3460时，%Dev达到最???, 但不能就此决定使用??=0.3460来建模就能使模型最优，
#--需要做交叉验证来保证模型没有over-fitting


#--用交叉验证来选择最优的λ???
library(doParallel)
set.seed(1)

cl <- makeCluster(2)
registerDoParallel(cl)
fit.cv <- cv.glmnet(x=Dtrain,y=Dtrain.label,family = "gaussian",alpha = 1,
                    nlambda = 1000, lambda.min.ratio = 0.0001, standardize = T,
                    nfolds = 10,              #10折交叉验???
                    type.measure = "mae",     #设置评估函数 
                    parallel = T              #开启并行进行交叉验证的机制
) 
stopCluster(cl)

plot(fit.cv)
#--查看λ值与评估度量(loss function)的关???
#--阴影部分表示评估度量的置信区???
#--图中两条虚线代表
#----1. lambda.min 指在所有的λ值中，得到最小评估度量均值对应的λ???
#----2. lambda.1se 指在lambda.min一个方差范围内得到最简单模型的那一个λ??? (lambda.1se 给出的就是一个具备优良性能但是自变量个数最少的模型)
c(log(fit.cv$lambda.min), log(fit.cv$lambda.1se))

#--用得到的模型进行预测
pred_test <- predict(fit.cv, newx=Dtest, type="response", s="lambda.1se")

#--计算测试集的MAE
sum(abs(pred_test-Dtest.label))/length(pred_test)


####特别说明:
#--lasso预测的时候用个参数是type:
#' 1. type="link" 给出线性预测值，即进行Logit变换之前的???
#' 2. type="response" 给出概率预测值，即进行Logit变换之后的???
#' 3. type="class" 给出0/1预测???
#' 4. type="coefficients" 罗列出给??? λ 值时的模型系???
#' 5. type="nonzero" 罗列出给??? λ 值时，不为零模型系数的下???

####特别说明:
#--当已有了一个模型之后，我们又得到了几个新的自变量，如果想知道这些新变量能否在第一个模型的基础上提高模型性能??? 
#--可以把第一个模型的预测因变量作为一个向量放到函数选项 offset 中，再用 glmnet 或??? cv.glmnet 进行拟合???

##################################9. Hyperparameter tuning - CARET##################################
library(caret); library(randomForest); library(glmnet);library(doParallel); library(Metrics) 


##########data load and feature engneering
player_statistics <- read_csv("./Data/Others/PUBG_Player_Statistics.csv")
nrow(player_statistics); str(player_statistics); names(player_statistics)

set.seed(1234)
player_statistics <- player_statistics %>% select(starts_with("solo"))
head(player_statistics)
max_min <- data_frame(max = apply(player_statistics, 2, max),
                      min = apply(player_statistics, 2, min),
                      columns = names(player_statistics))
useless_columns <- max_min$columns[max_min$min == max_min$max]
useless_columns <- paste("-", useless_columns, sep = "")
player_statistics <- player_statistics %>%
  select_(.dots = useless_columns )
player_statistics <- player_statistics %>%
  group_by(solo_WinRatio) %>% #use group_by to protect our target variable
  select(-contains("win")) %>% # remove any columns with "win" in the name
  select(-contains("loss")) %>% # remove any columns with "loss" in the name
  ungroup() # remove grouping
player_statistics <- player_statistics %>%
  na.omit() %>% 
  select_if(is.numeric)
str(player_statistics)

##########split data into train and test
training_indexs <- createDataPartition(player_statistics$solo_WinRatio, p = .7, list = F)
train <- player_statistics[training_indexs, ]; #names(train) ; dim(train)
test  <- player_statistics[-training_indexs, ]
label <- train$solo_WinRatio

##################################9.1. Random Forest##################################
Rtrain <- train %>% select(-solo_WinRatio) %>% as.matrix()

#--Step1: 设置初始超参???
param0.rf <- list(ntree = 100, mtry = round(sqrt(length(names(train)))) + 1)

#--Step2: 用初始超参训练模???
fit.rf.init <- randomForest(x = Rtrain, y = label,
                            ntree = param0.rf[["ntree"]],
                            mtry = param0.rf[["mtry"]]) 
fit.rf.init
plot(fit.rf.init,log = "y")  #选择ntree = 100
#--计算训练集和测试集的损失函数(模型评估度量MSE)
rmse(fit.rf.init$predicted, label) #sqrt(10.785)
rmse(predict(fit.rf.init, test), test$solo_WinRatio)

#--Step3: 使用交叉验证, grid search 或??? CARAT优化超参
mtry <- c(5:20)
rf.gs <- data.frame(ntree = 1, mtry = 1, rmse_train = 0, rmse_test = 0)
rf.gs <- rf.gs[-1,]
for(i in mtry){
  print(i)
  fit.rf <- randomForest(x = Rtrain, y = label,
                         ntree = 100,
                         mtry = mtry)
  rf.gs <- rbind(rf.gs, data.frame(ntree = 100, mtry = i, 
                                   rmse_train = rmse(fit.rf$predicted, label), #sqrt(10.785), 
                                   rmse_test = rmse(predict(fit.rf, test), test$solo_WinRatio)))
}

ggplot(data = rf.gs, aes(rmse_train,rmse_test,colour = as.factor(mtry))) + geom_point() +
  geom_text(aes(label = mtry), size = 3, vjust = 2, hjust = -.02)

#--由上图，选取 mtry = 11 作为参数

#--Step4: 拟合最终的模型
fit.rf.fn <- randomForest(x = Rtrain, y = label,
                          ntree = 100,
                          mtry = 11) 
fit.rf.fn
plot(fit.rf.fn,log = "y")  
randomForest::varImpPlot(fit.rf.fn, n.var = min(20,nrow(fit.rf.fn$importance)))
mse_rf <- rmse(fit.rf.fn$predicted, label) 
mse_rf <- rmse(predict(fit.rf.fn, test), test$solo_WinRatio)    #2.4788

##################################9.2. Lasso##################################
Mtrain <- model.matrix(solo_WinRatio~., train)[,-1]       #将原数据集转换为矩阵，同时进行one-hot encoding
Mtest <- model.matrix(solo_WinRatio~., test)[,-1]

#--Step1: 设置初始超参???
param0.lasso <- list(family = "gaussian", 
                     alpha = 1,
                     nlambda = 100,
                     standardize = T)

#--Step2: 用初始超参训练模???
fit.lasso.init <- glmnet(x = Mtrain, y = label,
                         family = param0.lasso[["family"]],
                         alpha = param0.lasso[["alpha"]],
                         nlambda = param0.lasso[["nlambda"]],
                         standardize = param0.lasso[["standardize"]])
print(fit.lasso.init)
plot(fit.lasso.init, xvar="lambda", label=TRUE)   

#假设选取λ = 0.016400 (log(0.016400))，计算训练集和测试集的损失函???(模型评估度量MSE)
rmse(predict(fit.lasso.init, newx=Mtrain, type="response", s=0.016400),label) 
rmse(predict(fit.lasso.init, newx=Mtest, type="response", s=0.016400),test$solo_WinRatio) 

#--Step3: 使用交叉验证, grid search 或??? CARAT优化超参
cl <- makeCluster(2)
registerDoParallel(cl)
fit.lasso.cv <- cv.glmnet(x=Mtrain,y=label,family = param0.lasso[["family"]],alpha = param0.lasso[["alpha"]],
                          nlambda = param0.lasso[["nlambda"]],standardize = T,
                          nfolds = 10,              #10折交叉验???
                          type.measure = "mse",     #设置评估函数 
                          parallel = T              #开启并行进行交叉验证的机制
) 
stopCluster(cl)

plot(fit.lasso.cv)
c(log(fit.cv$lambda.min), log(fit.cv$lambda.1se))
rmse(predict(fit.lasso.cv, newx=Mtrain, type="response", s="lambda.min"),label) 
rmse(predict(fit.lasso.cv, newx=Mtest, type="response", s="lambda.min"),test$solo_WinRatio) 
mse_lasso <- rmse(predict(fit.lasso.cv, newx=Mtrain, type="response", s="lambda.1se"),label) 
mes_lasso <- rmse(predict(fit.lasso.cv, newx=Mtest, type="response", s="lambda.1se"),test$solo_WinRatio)  #6.84

##################################9.3. XGBoost##################################
library(xgboost); library(data.table); library(vcd); library(Matrix); library(Ckmeans.1d.dp)
#对missing value采取consistency 处理
#apply(train,2,FUN = function(x) any(is.na(x)))
spM.train <- sparse.model.matrix(solo_WinRatio~.-1, data = train); nrow(spM.train)
spM.test <- sparse.model.matrix(solo_WinRatio~.-1, data = test); nrow(spM.test)
dtrain <- xgb.DMatrix(data = spM.train, label = train$solo_WinRatio); 
dtest  <- xgb.DMatrix(data = spM.test,  label = test$solo_WinRatio);  

#--Step1: 设置初始超参???
param0.xgb <- list(objective = "reg:linear",
                   eval_metric = "rmse",
                   
                   eta = 0.05,
                   
                   gamma = 0,
                   max_depth = 7,
                   min_child_weight = 10,
                   
                   subsample = 0.8,
                   colsample_bytree = 0.7,
                   colsample_bylevel = 0.6,
                   
                   lambda = 1,
                   alpha = 1,
                   
                   booster = "gbtree",
                   nthread = 2, 
                   seed = 1234
)

#--Step2: 用初始超参训练模???
fit.xgb.init <- xgb.train(param0.xgb, 
                          dtrain, 
                          missing = NA,  
                          nrounds = 500,
                          watchlist = list(train = dtrain, val = dtest), 
                          print_every_n = 1, 
                          early_stopping_rounds = 10 
)
fit.xgb.init
fit.xgb.init$best_iteration; fit.xgb.init$best_ntreelimit; fit.xgb.init$best_score; fit.xgb.init$niter; 

iter.error <-  data.frame(iter = rep(1:fit.xgb.init$niter,time=2),
                          error_set = rep(c("train","val"),each=fit.xgb.init$niter),
                          error_p0 = c(fit.xgb.init$evaluation_log$train_rmse,fit.xgb.init$evaluation_log$val_rmse))
ggplot(data = iter.error, aes(x = iter, y = error_p0, colour = error_set)) + 
  geom_line(size = 1) 
#--从上图可知，随着迭代次数的增加，val数据的mse下降的速度没有train数据的mse下降???
rmse(predict(fit.xgb.init, dtest, ntreelimit = 500),test$solo_WinRatio)
impt.xgb.init <- xgb.importance(feature_names = fit.xgb.init$feature_names, model = fit.xgb.init)
xgb.ggplot.importance(importance_matrix = impt.xgb.init,top_n = min(30,fit.xgb.init$nfeatures))
xgb.ggplot.deepness(model = fit.xgb.init)

#--Step3: 使用交叉验证, grid search 或??? CARAT优化超参
fit.xgb.cv <- xgb.cv(param0.xgb,
                     dtrain, 
                     missing = NA,  
                     nrounds = 1000,
                     nfold = 10,
                     prediction = TRUE,   #return the prediction using the final model 
                     showsd = TRUE,       #standard deviation of loss across folds
                     #stratified = TRUE,   #sample is unbalanced; use stratified sampling(分层抽样)
                     verbose = TRUE,
                     print_every_n = 5, 
                     early_stopping_rounds = 100
)
fit.xgb.cv
fit.xgb.cv$best_iteration; fit.xgb.cv$best_ntreelimit; fit.xgb.cv$best_score; fit.xgb.cv$niter; 
cv.error <-  data.frame(iter = rep(1:fit.xgb.cv$niter,time=3),
                        error_set = rep(c("train","val","diff"),each=fit.xgb.cv$niter),
                        error_p0 = c(fit.xgb.cv$evaluation_log$train_rmse_mean,fit.xgb.cv$evaluation_log$test_rmse_mean,abs(fit.xgb.cv$evaluation_log$train_rmse_mean-fit.xgb.cv$evaluation_log$test_rmse_mean)),
                        error_sd = c(rep(0,fit.xgb.cv$niter),fit.xgb.cv$evaluation_log$test_rmse_std,rep(0,fit.xgb.cv$niter))
)

inter_bais_variance <- which.min(abs(cv.error[cv.error$error_set == "train","error_p0"] - cv.error[cv.error$error_set == "diff","error_p0"]))
ggplot(data = cv.error, aes(x = iter, y = error_p0, colour = error_set)) + 
  geom_line(size = 1) + 
  #geom_errorbar(aes(ymin=error_p0-error_sd, ymax=error_p0+error_sd), colour="light green", width=.1) +
  geom_ribbon(data = cv.error[cv.error$error_set == "val",],aes(ymin=error_p0-error_sd,ymax=error_p0+error_sd), fill="light green", alpha="0.5") + 
  geom_vline(xintercept = inter_bais_variance, colour = "black", linetype = 2) +
  geom_hline(yintercept = c(cv.error[cv.error$error_set == "train",]$error_p0[inter_bais_variance],
                            cv.error[cv.error$error_set == "val",]$error_p0[inter_bais_variance]),
             colour = "black", linetype = 2) +
  geom_text(x = inter_bais_variance, y = 0, label = inter_bais_variance, size = 4, vjust = 2, hjust = -.2) + 
  geom_text(x=0,y=cv.error[cv.error$error_set == "train",]$error_p0[inter_bais_variance], label = cv.error[cv.error$error_set == "train",]$error_p0[inter_bais_variance], size = 4, vjust = 0, hjust = 0)+
  geom_text(x=0,y=cv.error[cv.error$error_set == "val",]$error_p0[inter_bais_variance], label = cv.error[cv.error$error_set == "val",]$error_p0[inter_bais_variance], size = 4, vjust = 0, hjust = 0)+
  theme_bw()


#######################################################10. CASE STUDY - TITANIC#################################################################################
setwd("C:/YuanLe/R/RWkDir/RLearn/RFoundation/")
library(xgboost);                 
library(rpart); library(rpart.plot)
library(randomForest);
library(tidyverse);
options(stringsAsFactors = FALSE)
options(digits = 10)

data.T <- read.csv("./Data/XgbDeepLearn/Titanic_TEed3.csv")
data.T <- data.T %>% select(PassengerId,Survived,Pclass,Sex,SexPclass,Age,SibSp,Parch,FareAdj,Embarked,NameTitle,Total,FamilyRole,TicketType,CabinLevel)
str(data.T); head(data.T); dim(data.T)
data.T$Pclass <- factor(data.T$Pclass);
data.T$Sex <- factor(data.T$Sex);
data.T$SexPclass <- factor(data.T$SexPclass);
data.T$Embarked <- factor(data.T$Embarked);
data.T$NameTitle <- factor(data.T$NameTitle);
data.T$FamilyRole <- factor(data.T$FamilyRole);
data.T$TicketType <- factor(data.T$TicketType);
data.T$CabinLevel <- factor(data.T$CabinLevel);

set.seed(1000)
test.T <- data.T %>% filter(is.na(Survived))
train_val.T <- data.T %>% filter(!is.na(Survived))
RndSeq.T <- sample(1:nrow(train_val.T), nrow(train_val.T), replace = F)
train.T <- train_val.T[RndSeq.T[1:round(length(RndSeq.T) * .8)],]
val.T <- train_val.T[!(train_val.T$PassengerId %in% train.T$PassengerId),]
nrow(train.T) + nrow(val.T) + nrow(test.T) == nrow(data.T)

#######################################################10.1. 分类和回归二叉树??? - CART#################################################################################
r.ct <- rpart.control(cp = 0,                       #***每次分裂损失函数下降的值都必须大于cp  
                      minsplit = 5,                 #***可分裂的节点最少需要包含的观测数量
                      xval = 10,                    #***交叉验证的折???
                      maxdepth = 30,                #***树的最大深???
                      maxcompete = 4,               #表示'primary splits'下面显示的除当前分裂外的继续分裂数量不能大于n; 这个参数的设置不会改变模型的预测能力; 默认???4
                      maxsurrogate = 1,             #表示'Surrogate splits'显示的分裂数量不能大于n;这个参数的设置不会改变模型的预测能力; 默认???5; 这个值越小，生成模型的计算量就越???                    
                      usesurrogate = 2              #0 - 不使???; 1, 2 - 使用
)

fit.cart.T <- rpart(Survived~SexPclass+NameTitle+Age+SibSp+Parch+Total+FareAdj+Embarked+FamilyRole+TicketType+CabinLevel, 
                    data = train.T, 
                    method = "class", 
                    parms = list(prior = prop.table(table(train.T$Survived)), split = "gini"),
                    control = r.ct)

#--查看损失函数(对数似然损失函数???)变化
printcp(fit.cart.T)    
#--剪枝 - 综合验证???/训练集的损失???
fit.cart.T2 <- rpart::prune(fit.cart.T, cp = 0.008)
printcp(fit.cart.T2)                         #min(L(θ)) = 0.40590406
summary(fit.cart.T2)
rattle::fancyRpartPlot(fit.cart.T2); plot(fit.cart.T2); text(fit.cart.T2)
#--用测试集进行验证
P1 <- predict(fit.cart.T2, val.T)[,2]      #P(Survived=1|x)
#--求测试集的对数似然损失函数???
yi <- val.T$Survived
hx <- P1
L.T <- sum(ifelse(hx %in% c(0,1),0,-yi*log(hx) - (1-yi)*log(1-hx)))/length(yi)  #0.4782579145
#--求测试集的混沌矩阵和错误???
yi_ <- as.numeric(P1>=0.5)  
cfu.T <- table(yi,yi_)
errRate.T <- (cfu.T[1,2] + cfu.T[2,1]) / sum(cfu.T)                             #0.197740113


#######################################################10.2. Bagging - randomForest详解#####################################################################
train.T.rf <- train.T[-1]
train.T.rf$Survived <- as.factor(train.T.rf$Survived)
fit.rf.T <- randomForest(Survived~.,
                         data = train.T.rf,  
                         mtry = round(sqrt(length(names(train.T)))) + 1, #***默认值的设定方式,指定构建每颗决策树的变量个数
                         na.action = na.pass,
                         ntree = 500,          #***指定树的数量
                         replace = T,          #指定Bootstrap随机抽样的方式，默认为有放回的抽样； OOB的观测量一般占总量???36.8%
                         nodesize = 1,         #指定每颗决策树叶节点的最小个数，默认情况下，分类模型???1，回归模型为5???
                         #maxnodes = MAX,      #RF的特性就是让每颗树尽量生长，以此来降低单可树的偏差，再用投票来降低方???
                         importance = T,       #是否计算各个变量在模型中的重要性，默认不计算，该参数主要结合importance()函数使用???
                         norm.votes = T,       #默认为T, 如果为F, 将显示每棵树的投票结???
                         proximity = T,        #是否计算模型的临近矩阵，主要结合MDSplot()函数使用???
                         do.trace = F          #建模过程中输出更多的信息
)
print(fit.rf.T)            #OOB error计算的是混沌矩阵错误???    #17.09%
fit.rf.T$confusion;        #混沌矩阵 [1,1]-TN(真阴???, ture negitive); [2,1] - FP(伪阳???, false positive); [1,2]-FN; [2,2]-TP;
fit.rf.T$type              #随机森林的类型，是分类还是回???
fit.rf.T$predicted         #fitted - 模型的拟合???, 根据这个结果和原始值可以算出模型的总体错误???
fit.rf.T$err.rate          #每一颗树的OOB, 以及伪阴???(0)/伪阳???(1)比率
fit.rf.T$votes             #每一个观测的投票结果，用概率表示(所有数中投该观测为1/0的比???)
fit.rf.T$proximity         #模型的临近矩???(nrow*nrow的矩???), 表示观测两两之间的相似度
fit.rf.T$ntree             #森林中的树的棵数
fit.rf.T$mtry              #模型中每颗树用到的变量数
#--展示树的数目和Error的关系，以此可以确定参数ntree取那个范围比较好
plot(fit.rf.T,log = "y")
#--下面的ANDY自定义图跟plot(fit.rf.T,log = "y")效果一???
errRate.rf <- data.frame(ntree = rep(1:fit.rf.T$ntree, 3),
                         Type = rep(c("OOB","FN","FP"), each = 500),
                         Error = c(fit.rf.T$err.rate[,1],fit.rf.T$err.rate[,2],fit.rf.T$err.rate[,3]))
ggplot(data = errRate.rf, aes(x = ntree, y = Error, colour = Type)) + geom_line()

#--变量的相对重要???
randomForest::varImpPlot(fit.rf.T, n.var = min(20,nrow(fit.rf.T$importance)))


#####--RF调优 - 使OOB error尽可能小
#' 1. 确定树的棵树               --   循环3-7          -- 设定???5
#' 2. 确定每颗树变量的数目       --   循环5-500        -- 设定???400

#--用测试集进行验证
yi_ <- predict(fit.rf.T,val.T)
#--求测试集的混沌矩阵和错误???
cfu.T.rf <- table(yi,yi_)
errRate.T.rf <- (cfu.T.rf[1,2] + cfu.T.rf[2,1]) / sum(cfu.T.rf)                             #0.197740113

#--伪阴???(FN)太高，为什么？如何改进?

#######################################################10.3. Boosting - XGBoost#####################################################################

##################################%%%%%%%%%%%%%%%%%%%%%聚类算法%%%%%%%%%%%%%%%%%%%%%########################################################
#########聚类
#########就是按照某个特定标准(如距离准???)把一个数据集分割成不同的类或簇，使得同一个簇内的数据对象的相似性尽可能大，同时不在同一个簇中的数据对象的差异性也尽可能地大???
#########聚类属于无监督学???

##################################1. K-Means(K均???)########################################################
#########理论:
#########假设数据集D有n个观???, K-Means算法将D中的观测划分到k个簇C1,C2...Ci...Ck???(1<=i<=k); 使得Ci属于D, Ci与Cj不相???;
#########每个簇Ci的形???(概率上讲就是该簇的中心，实际上有多种定义形式)用c_i表示;
#########设对象p属于簇Ci, p与该簇的形心c_i只差用dist(p,c_i)度量??? 其中dist(x,y)是两点x与y的欧几里德距???.
#########簇Ci的组内相似性可???'簇内变差'度量, 它是Ci中所有对象与形心c_i之间的误差的平方???; 


#########算法: (簇的形心定义???: 簇中所以对象的均值来表示???)
#######输入: k - 簇的数目;   D - 包含n个观测的数据???
#######输出: k个簇
#######方法:
#####1. 从D中任意选择k个对象作为k个簇的初始形???;
#####2. repeat
#####3.   根据簇中对象的均值，将D中的每个对象分配到最相似的簇???
#####4.   更新簇均???
#####5. until不再发生变化

##################################1.1. K-Means by R########################################################
#--测试数据
head(iris)
as.matrix(iris)
fit.km <- 
  kmeans(x = iris[,1:4],                      #数据D
         centers = 3,                                #簇的数目, 也可以显示指定每个簇的初始形???
         iter.max = 10,                              #允许的最大迭代次???
         algorithm = "Lloyd")
#########参数说明:
#######A. algorithm - 指定用哪种算法进行迭???, 可选的???:
#####A.1. Lloyd, ???'K-Means(K均???)'中介绍的算法一???, 这样出来的结果可能每个簇的观测量不平???
#####A.2. MacQueen, 
#####A.3. Hartigan-Wong, 改进Lloyd不平衡问???

#--对结果的可视???
iris.t <- iris
iris.t$kmType <- fit.km$cluster
ggplot(data = iris.t,aes(x = Sepal.Length, y = Sepal.Width, colour = as.factor(kmType), shape = Species)) +
  geom_point()

##################################2. DBSCAN########################################################
library(fpc)
library(factoextra)
data("multishapes")
df <- multishapes[,1:2]
ggplot(data = df, aes(x = x, y = y)) + geom_point()

clsts <- 
  dbscan(data = df,
         eps = 0.15,
         MinPts = 5,
         scale = FALSE)
clsts
clsts$cluster       #查看每一个样本点的分类结果，0表示噪声点
fviz_cluster(clsts, df, stand = FALSE, frame= FALSE, geom = "point")

## 如何选择最优的eps?????
# 计算每个点到其最近邻的k个点的平均距离。k的取值根据MinPts由用户指定。
kNNdistplot(df, k = 5)
abline(h = 0.15, lty = 2)




##################################%%%%%%%%%%%%%%%%%%%%%可视化ggplot2%%%%%%%%%%%%%%%%%%%%%######################################################
##################################B. ggplot2######################################################
library(ggplot2)
#update.packages("ggplot2")
?ggplot2

##################################B.1. 图形图层语法简???(Grammer of Graphic)############################################
#什么是统计图形? --- geom(几何对象) + aes(mapping) + stats(统计变换) + coord(坐标???) + facet(分面)
#一???'统计图形'就是'数据'???'几何对象'(geometric object, 缩写为geom, 包括???,???,条形???)???
#'图形属???'(aesthetic attributes, 缩写aes, 包括颜色,形状,大小???)的一个映???,

#此外, 图形中还可能包含数据???'统计变换'(statistical transformation, 缩写stats),
#最后绘制在特定的坐标系(coordinate system, 缩写coord)???,
#???'分面'(facet,将绘图窗口划分成若干子窗???)则可以用来生成数据不同子集的图形???

#一???'统计图形'可由一个或多个'图层'叠加而成.

##################################B.1.0 图层############################################
#一个图层由四个基本部分组成:
#1. mapping: 数据和图形属性的映射 (由标度控???)
#2. stats: 一种统计变???
#3. geom: 一种几何对???
#4. 一种位置调整方???

##################################B.1.1 映射与标???############################################
#'图形属???'是影响数据如何进行展示的视觉属???,e.g. x轴位???, y轴位???, shape, size, colour....
#每一???'变量'???'图形属???'的映??? - 对应了一个称???'标度'的函???, 其作用是将数据的取值映射到该图形属性的有效取???(如像素或颜色???);也可以通过I()函数手动设置映射
#我们称这个过程叫'标度变换' (scaling), 转换前的???(factor(cyl) -> [4,8]), 转换后的值是计算机能懂的颜色标识???.

#变量 <---mapping---> 图形属???,e.g. 
#displ <---mapping---> x???
#hwy   <---mapping---> y???
#factor(cyl) <---mapping---> 颜色属???
ggplot(data = mpg, 
       mapping = aes(x = displ, y = hwy, colour = factor(cyl))) +
  geom_point()


##################################B.1.2 几何对象(geom)############################################
#'几何对象'描述了该用何种图形对象来对数据进行展示???
#每种几何对象都有一组它能识别的图形属性和一组绘图所需的???,e.g.
#geom_point: 颜色，大小，形状...
#geom_line: 宽度，边界颜色，填充颜色...

#同时每种几何对象都有一个默认的统计变换, 反之每个统计变化都对应一个默认的几何对象. (参看 DS-R-ggplot2-统计变换和几何对象的默认对应)

#二维变量关系展示的常???'几何对象'(geom)???:
#1. 'point'(散点???)
#2. 'smooth' - 拟合一条平滑曲???
#3. 'boxplot'(箱图) - 查看数据的分布情???, 寻找异常???.
#4. 'path'(路线???) or 'line'(线条???) - 在数据点之间绘制连线

#一维变量的分布展示的常???'几何对象'(geom)???:
#1. 'histogram'(直方???) - 展示连续变量的分布情???
#2. 'freqpoly' - 绘制频率多变???
#3. 'density' - 绘制概率密度曲线
#4. 'bar' - 绘制分类型变量的条形???


##################################B.1.3 统计变换(stat)############################################
#统计变换 - 通常以某种方式对数据进行汇???
#e.g. 几何对象geom_smooth对应的统计变???'smooth'即是根据'最小二乘法'(method = 'lm')计算X变量对应的Y???.
#e.g. 几何对象geom_histogram对应的统计变???'bin', 该统计变换将生成一些新的生成变???(generated variables):
####1. count: 每个组里观测值的数量
####2. density: 每个组里观测值的密度(占整体的百分???/组宽)
####3. x: 组的中心位置


#几何对象'geom_histogram'对应的y轴的默认值是'count', 可以修改???'density'
#注意: 引用生成变量时，其名字需要用..X..围起???
ggplot(data = diamonds, aes(x = carat)) + geom_histogram(binwidth = .5, fill = "red")  #箱距被设置为.2, y轴默认为count 
ggplot(data = diamonds, aes(x = carat)) + 
  geom_histogram(binwidth = .1, aes(y = ..density..))                    #将y轴修改为'density'

#e.g. 几何对象'geom_point'对应的统计变换是'identity',表示不对数据进行统计变换，因为在geom_point中需要显示设置x和y轴的映射
ggplot(data = mpg, 
       mapping = aes(x = displ, y = hwy, colour = factor(cyl))) +
  geom_point()


####在选择了几何对象后，一定要注意该几何对象对应的统计变换是什么，必要时，需要手动修改一个几何对象默认的统计变换???
####geom_bar就是很好的例???

##################################B.1.4 位置调整############################################
#位置调整 - 对该图层中的元素位置进行微调.(一般作用于离散型变???), 如下5种位置调整参???:
#1. dodge - 避免重叠，并排放???
#2. fill - 堆叠图形元素，并将高度标准化???1
#3. identity - 不做任何调整
#4. jitter - 给点添加扰动，避免重???
#5. stack - 将图形元素堆叠起???

ds1 <- 
  data.frame(name = c("andy","andy","andy","jie","jie","jie","sam","sam","sam"),
             subject = c("yuwen","shuxue","yingyu","yuwen","shuxue","yingyu","yuwen","shuxue","yingyu"),
             score = sample(50:100,9,replace = T))
head(ds1)

ggplot(data = ds1, aes(x = name)) + geom_bar() #默认情况下geom_bar对应的统计变换是'bin',即计算每个组里的观测数量
#stat = identity: 不对数据进行统计变换
ggplot(data = ds1, aes(x = name, y = score,fill = subject)) + geom_bar(position = "dodge", stat = "identity")
ggplot(data = ds1, aes(x = name, y = score,fill = subject)) + geom_bar(position = "stack", stat = "identity")
ggplot(data = ds1, aes(x = name, y = score,fill = subject)) + geom_bar(position = "fill", stat = "identity") 


##################################B.1.5 分面(facet)############################################
#如前谈论，可以利???'图形属???'中的颜色,形状,大小等来对数据进行分组展???(同一个几何对象中)???
#分面 - 是另一种实现分组展示数据的方法, 它将数据分割成若干子集，然后创建一个图形的矩阵，特???:
#1. 每个数据子集绘制到图形矩阵的不同窗格(子图)???
#2. 每个子图采用的是相同的图形类???

##################################B.1.6 小小总结############################################
#####要展示同样的数据，通过ggplot可以选择不同的图以及作图方式

#####直方???
d <- ggplot(data = diamonds, aes(x = carat)) + xlim(0,3)

d + geom_histogram(binwidth = .3)

######直方图的一些其他表现方???
d + stat_bin(aes(ymax = ..count..),binwidth = .1, geom = "area")  #这个图没意义，看都看不懂
d + geom_point(aes(size = ..density..), binwidth = .1, stat = "bin", position = "identity")
d + stat_bin(aes(ymax = 1, fill = ..count..), binwidth = .1, geom = "tile", posistion = "identity")

##################################B.2. 用到的数据集############################################
head(diamonds)
View(diamonds)
str(diamonds)
#meta of dataset 'diamonds'
#1. carat - 克拉重量
#2. cut - 切工
#3. color - 颜色
#4. clarity - 净???
#(以上四个变量反映砖石质量的四???'C')
#5. depth - 深度
#6. table - 砖面宽度
#7. price - 价格
#8-10. x, y, z (物理上的???,???,???)
#(变量table和depth分别与z和x有函数关???)


#从数据集'diamonds'中随机抽???100个样???
set.seed(1410)
dsmall <- diamonds[sample(nrow(diamonds), 100),]
names(diamonds)

head(mpg)
str(mpg)
names(mpg)
#meta of dataset 'mpg'
#1. displ - 排量
#2. cyl - 汽缸???
#3. trans - 变速箱
#4. cty - 城市道路驾驶的耗油???
#5. hwy - 高速公路驾驶的耗油???

##################################B.3. qplot(quick plot)############################################
##################################B.3.1. qplot基本参数简???############################################
####例A
qplot(x = log(carat),          #x轴上的变???, 并支持将变量的函数作为参???
      y = log(price),          #y轴上的变???, 并支持将变量的函数作为参???
      data = dsmall            #数据???
      ,geom = "auto"           #图形类型
      #指定其他一???'图形属???'的参数???,e.g. 颜色,形状,大小....
      ,colour = color          #‘标称函数’自动将变量'color'的不同取???(D,E,F,G,H,I,J)映射成不同的颜色(红色,篱笆???,....黑色);也可以手动设置这种映???
      ,shape = cut
      ,size = price
      ,alpha = I(1/5)          #'图形属???'-'透明???', 用这种表示方???,其中分母的意思是'经过多少次重???,颜色变得不透明'; 这也是手动设定映射的例子
      #其他一些参???
      #,xlim = c(0,1.5)         #x,y轴的显示区间
      #,ylim = c(5,12)
      #,log = "xy"              #log = "x" 表示对x轴取对数, log = "xy" 表示对x,y轴都取对???
      ,main = "砖石的大小和价格散点???"
      ,xlab = "砖石大小"
      ,ylab = "砖石价格"
)      
##################################B.3.2. 几何对象-平滑曲线############################################
names(dsmall)
qplot(x = carat, y = price, data = dsmall, geom = c("point", "smooth"), method = "loess", span = 0.8)
qplot(x = carat, y = price, data = diamonds, geom = c("point", "smooth"))
qplot(x = carat, y = price, data = dsmall, geom = c("point", "smooth"), method = "lm", formula = y~poly(x,2))

###平滑器类???(method):
##1. 'loess' - 当数据量较小时，默认使用该种方法，e.g. data = dsmall
##2. 'gam' - 当数据量超过1000的默认方???, e.g. data = diamond
##3. 'lm' -拟合线性模???, 默认拟合一条直线，也可以通过指定formula = y ~ poly(x,2)来拟合一个二次多项式

###曲线的平滑程???(span): span -> [0,1] ([很不平滑, 很平滑])

##################################B.3.2. 几何对象-箱线图和扰动点图############################################
##目的:分析数据集中分类型变量对连续型变量的取值是否有显著影响???

qplot(x = color, y = price / carat, data = diamonds, geom = "jitter", colour = color)

qplot(x = color, y = price / carat, data = diamonds, geom = "boxplot", colour = color)


##################################B.3.3. 几何对象-直方图和密度曲线???############################################
##目的: 查看单一连续型变量的分布情况
qplot(x = carat, data = diamonds, geom = "histogram", binwidth = .2, fill = color)
qplot(x = carat, data = diamonds, geom = "density", colour = color)

##################################B.3.3. 几何对象-条形???############################################
#砖石颜色的普通条形图 - y = count(1) by color
qplot(x = color, data = diamonds, geom = "bar")
#按重量加权的条形??? - y = sum(carat) by color
qplot(x = color, data = diamonds, geom = "bar", weight = carat) + scale_y_continuous("carat")


##################################B.3.3. 几何对象-线条图和路径???############################################
##用于可视化时间序列数???
head(economics)
names(economics)

#失业率随时间变化的情???
qplot(x = date, y = unemploy/pop, data = economics, geom = "line")
#失业时间长度随时间变化的情况
qplot(x = date, y = uempmed, data = economics, geom = "line")

#失业率与失业时间长度随时间变化的路径
qplot(x = unemploy/pop, y = uempmed, data = economics, geom = "point")
qplot(x = unemploy/pop, y = uempmed, data = economics, geom = "line")
qplot(x = unemploy/pop, y = uempmed, data = economics, geom = c("point","path"), colour = as.POSIXlt(economics$date)$year + 1900)


##################################B.3.4. 分面############################################
#分面
qplot(x = carat, data = diamonds, facets = color ~ ., geom = "histogram", binwidth = .1, xlim = c(0,3))

##################################B.4. ggplot2()############################################
#使用ggplot2()来创建图形的步骤:
#1. 创建图形对象 - 指定数据和图形属性映???
p <- ggplot(data = dsmall)  #数据
p <- p + aes(x = carat, y = price, colour = color) #映射
p
summary(p)
#2.给图形对象添加一个几何对???(geom), 使用图层函数geom_XXXX()来实???
#note: 图层函数geom_XXXX()和stat_XXXX()被称???'快捷函数', 每一个几何对象都对应着一个默认的'统计变换'???'位置参数'
p <- p + geom_point(na.rm = T) #图层1
p
summary(p)

p <- p + geom_smooth(method = "lm", se = T, colour = alpha("red",0.5), na.rm = T, size = 1)  #图层2
p
summary(p)


##################################B.4.1 注意:图形属性的参数设定和映射的区别############################################
p <- ggplot(data = mtcars, aes(x = mpg, y = wt))

#通过参数设定图形属???-颜色
p + geom_point(colour = "darkblue")

#通过映射
#原理: 先创建一个变量X, 再???'darkblue'赋予X, 再将'darkblue'映射成电脑能识别的颜???
p + geom_point(aes(colour = "darkblue"))


##################################B.4.2 图形属???-分组(群组几何对象)############################################
##几何对象分类:
##1. 个体几何对象 - 对数据框中的每一条记录绘制一个可以区别于其他个体的图形对???
##2. 群组几何对象 - 用来表示多条观测
library(nlme)
head(Oxboys)
View(Oxboys)
names(Oxboys)

ggplot(data = Oxboys, aes(x = age, y = height)) + geom_point()

#需??? - 查看每个男孩的身高变化趋???
#这种需要一般出现在纵向数据???

ggplot(data = Oxboys, aes(x = age, y = height)) + geom_line()
ggplot(data = Oxboys, aes(x = age, y = height, group = Subject)) + geom_line()

ggplot(data = Oxboys, aes(x = age, y = height, colour = factor(Subject))) + geom_point()
ggplot(data = Oxboys, aes(x = age, y = height, colour = factor(Subject))) + geom_line()
ggplot(data = Oxboys, aes(x = age, y = height, group = Subject, colour = factor(Subject))) + geom_line()

#不同的图层使用不同的分组图形属???
#如果对平滑曲线图层使用和线条图层一样分组，将对每一个线条图绘制一个平滑曲???
ggplot(data = Oxboys, aes(x = age, y = height, group = Subject, colour = factor(Subject))) + 
  geom_line() + 
  geom_smooth(method = "lm", size = .5, se = FALSE, aes(group = Subject),colour = "black")

#将平滑曲线图层的图形属???'group'设置为一个常数，将只得到一条平滑曲???
#总结 - 可以在不同的图层使用不同的分???(图形属???)设置
ggplot(data = Oxboys, aes(x = age, y = height, group = Subject, colour = factor(Subject))) + 
  geom_line() + 
  geom_smooth(method = "lm", size = .5, se = T, aes(group = 1),colour = "black")

ggplot(data = Oxboys, aes(x = age, y = height, colour = factor(Subject))) + 
  geom_line() + 
  geom_smooth(method = "lm", size = .5, se = T, aes(group = 1),colour = "black")

#修改不同图层的分组图形属???
ggplot(data = Oxboys, aes(x = factor(Occasion), y = height)) +
  geom_boxplot() +
  geom_line()

ggplot(data = Oxboys, aes(x = factor(Occasion), y = height)) +
  geom_boxplot() +
  geom_line(aes(group = Subject, colour = factor(Subject)))



##################################B.4.3 图层叠加的总体策略############################################
#总体来说，图层有三种作用:
#1. 用以展示数据本身 - e.g. 散点???
#2. 用以展示数据???'统计摘要' - e.g. 平滑曲线
#3. 用以添加额外的元数据, 上下文信息和注解

##################################B.4.5 统计摘要图层的制???############################################
##在ggplot2中，用函???'stat_summary' 来实现计算每个x值对应的y值的统计摘要.
##统计摘要感觉就像是group by x计算得到的聚合???(aggregated)

ds2 <- data.frame(year = c(rep("2010",3),rep("2011",2),rep("2012",4),rep("2013",3),rep("2014",4),rep("2015",3)),
                  dijia = c(sample(50:100,19)))


ymad <- function(x){sqrt(x)*log(x)} 
ysum <- function(x){sum(x)/length(x)}
ggplot(data = ds2, aes(x= year, y = dijia)) + 
  geom_point() +
  stat_summary(aes(colour = I("red")), fun.y = mean, geom = "point", size = 3)  +
  stat_summary(aes(colour = I("black"),group = 1), fun.y = mean, geom = "line", size = 1)  +
  stat_summary(aes(colour = I("blue")), fun.y = ymad, geom = "point", size = 1.5) +    #x对应的每个y值的点的个数
  stat_summary(aes(colour = I("green")), fun.y = ysum, geom = "point", size = 1.5)     #x对应的所有y值的聚合???

##################################B.4.6 添加图形注解(综合例子P111)############################################
head(economics)
names(economics)
head(presidential)

economics2 <- economics[,c("date","pce","pop","psavert","uempmed","unemploy")]
presidential2 <- presidential[-(1:2),]
View(economics2)
View(presidential2)

#创建线条图形 - 反映时间与失业人数的关系
p <- ggplot(data = economics2, aes(x = date, y = unemploy)) + geom_line()

yrng <- range(economics2$unemploy)
xrng <- range(economics2$date)

#添加图层 - '竖直???' - 使用不同的数据集
p <- p + geom_vline(data = presidential2, aes(xintercept = as.numeric(start)), colour = "dark gray") 


library(scales)

#添加图层 - '2维的矩形???'
p <- p + geom_rect(data = presidential2, 
                   aes(NULL, NULL,xmin = start, xmax = end, fill = party), 
                   ymin = yrng[1], ymax = yrng[2],
                   alpha = .2) +
  scale_fill_manual(values = c("green","red"))

#计算每任总统任期???'中间时间???'
presidential2$date_middle <- presidential2$start + round(difftime(presidential2$end, presidential2$start , units = "day") / 2,0)

#添加图层 - '文本注释'
p <- p + geom_text(data = presidential2, aes(x = start, y = yrng[1], label = start), size = 3, vjust = 2, hjust = -.02)
p <- p + geom_text(data = presidential2, aes(x = date_middle, y = yrng[2], label = name), size = 4, hjust = .5, vjust = 0)
p <- p + geom_text(data = data.frame(x=xrng[2],y=yrng[2]), aes(x,y,label = paste(strwrap("Umemployment rates", 20), collapse = "\n")), hjust = 0.3, vjust = -2, size = 4)

#计算每一天economics2的记录所处的总统
count_e <- nrow(economics2)
count_p <- nrow(presidential2)
economics2$pdate <- as.Date("1900-01-01")
for(n in 1:count_e){
  ed <- economics2$date[n] 
  for(m in 1:count_p){
    if(ed < presidential2$end[m] & ed >= presidential2$start[m]){
      economics2$pdate[n] <- as.Date(presidential2$date_middle[m])
    }
  }
}

#添加图层 - '平滑曲线'
p <- p + geom_point(data = aggregate(unemploy~pdate, data = economics2, FUN = mean), 
                    aes(x = pdate, y = unemploy),
                    size = 3, colour = "blue") 

p <- p + geom_line(data = aggregate(unemploy~pdate, data = economics2, FUN = mean), 
                   aes(x = pdate, y = unemploy),
                   size = 1, colour = "blue") 


p <- p + geom_smooth(method = "lm", se = FALSE, colour = "red")
p <- p + geom_smooth(method = "loess", se = FALSE, colour = "green", linetype = 2)

p
summary(p)


##################################B.4.7 标度/坐标???/图例############################################
#标度(scale): 控制数据到图形属性的映射. 标度将数据转化为视觉上可以感知的东西：e.g. 位置，大小，颜色，形状等...
#标度跟图???/坐标轴有密切的联????????

#标度的工作原???
#1. 理解标度的定义域(即数据空???) 
#   1.1. 如果标度对应的是分类变量, 其定义域就是一个因子集???
#   1.2. 如果标度对应的是连续型变???, 其定义域就是一个实值区???, e.g. (1,10)
#2. 理解标度的值域(即图形属性空???)

#执行标度的过???(即将定义域映射到值域)分为三步:
#1. 变换(transformation)
#2. 训练(training)
#3. 映射(mapping)

#每一个图形属性都对应一个默认的标度 (参见DC-图形属性对应的默认标度)
ggplot(data = mpg, aes(x = cty, y = hwy)) + geom_point() + scale_x_continuous()   #x是连续变量，所有默认的标度是scale_x_continuous
ggplot(data = mpg, aes(x = drv, y = hwy)) + geom_point() + scale_x_discrete()     #x是离散型变量，所有默认的标度是scale_x_discrete

View(msleep)
msleep1 <- msleep[!is.na(msleep$sleep_total) & !is.na(msleep$sleep_cycle) & !is.na(msleep$vore), ]
#使用默认的标??? - 隐式
ggplot(data = msleep1, aes(x = sleep_total, y = sleep_cycle, colour = vore)) + geom_point()
#显式使用默认的标???
ggplot(data = msleep1, aes(x = sleep_total, y = sleep_cycle, colour = vore)) + geom_point() + scale_color_hue()
#显式使用非默认的标度 - 每一个图形属性对应多个可选的标度(离散型和连续型不一???)
ggplot(data = msleep1, aes(x = sleep_total, y = sleep_cycle, colour = vore)) + geom_point() + scale_color_grey()
ggplot(data = msleep1, aes(x = sleep_total, y = sleep_cycle, colour = vore)) + geom_point() + scale_color_brewer(palette = "Set1")
ggplot(data = msleep1, aes(x = sleep_total, y = sleep_cycle, colour = vore)) + geom_point() + scale_color_brewer(palette = "Set2")
#修改默认标度的参???
ggplot(data = msleep1, aes(x = sleep_total, y = sleep_cycle, colour = vore)) + geom_point() + 
  scale_color_hue("What does \nit eat?",
                  breaks = c("omni","herbi","carni","insecti"),             #变量'vore'的取??? - unique(msleep1$vore)
                  labels = c("AAA","BBB","CCC","DDD"))                       #要显示的???, 自定义任意???


#标度大致可分为四???:
#1. 位置标度 - 将变???(离散\连续\日期)映射到位置区域，并构建对应的坐标???
#2. 颜色标度 - 将变???(离散\连续)映射到颜???
#3. 手动标度 - 将变???(离散)映射到大小、形状、颜色等，并创建对应的图???
#4. 同一性标??? - 直接将变量值绘制为图形属???.(e.g. 比如变量本身存储的就是颜色值，就不需要再映射)

##################################B.4.7.0 标度的通用参数############################################

#标度的通用参数:
#1. name - 设置坐标轴和图例上显示的标签
ggplot(data = mpg, aes(x = cty, y = hwy, colour = displ)) + geom_point() + scale_x_continuous(name = "City mpg")
#等同???
ggplot(data = mpg, aes(x = cty, y = hwy, colour = displ)) + geom_point() + xlab("City mpg")
#更方便的做法
ggplot(data = mpg, aes(x = cty, y = hwy, colour = displ)) + geom_point() + labs(x = "City mpg", y = "Highway", colour = "Displacement")

#2. limits - 固定标度的定义域, 控制显示在图形上的元???
#3. breaks - 控制着显示在坐标轴或图例上的元???
#4. lables - 指定为每个break显示的标签文???
ggplot(data = mtcars, aes(x = cyl, y = wt)) + geom_point() + scale_x_continuous(breaks = c(5:6)) #控制x轴只显示4,5,6
#######注意limits和break的区???
ggplot(data = mtcars, aes(x = cyl, y = wt)) + geom_point() + scale_x_continuous(limits = c(5:6)) #控制只显示cyl介于[5,6]的点元素
ggplot(data = mtcars, aes(x = cyl, y = wt, colour = cyl)) + geom_point() + scale_colour_gradient(breaks = c(5:6))
ggplot(data = mtcars, aes(x = cyl, y = wt, colour = cyl)) + geom_point() + scale_colour_gradient(limits = c(5:6))

ggplot(data = mtcars, aes(x = cyl, y = wt, colour = cyl)) + geom_point() + 
  scale_colour_gradient(limits = c(5:6)) + scale_x_continuous(limits = c(5:6))

##################################B.4.7.1 位置标度############################################
#针对连续型变量的位置标度变换???
ggplot(data = diamonds, aes(x = carat, y = price)) + geom_point()
#注意下面两种写法x,y轴的值不一???, 第一种x,y的值是log(x), log(y), 第二种x,y的值还是原???
#但是两图的绘图区域是完全一致的
ggplot(data = diamonds, aes(x = log(carat), y = log(price))) + geom_point()

ggplot(data = diamonds, aes(x = carat, y = price)) + geom_point() + scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10")


#日期和时间变量对应的位子标度
ggplot(data = economics, aes(x = date, y = psavert)) + geom_line() +
  scale_y_continuous(name = "Personal Saving Rate") + 
  geom_hline(yintercept = 0, colour = "grey50") +
  scale_x_date(limits = as.Date(c("2004-01-01","2005-01-01")))


##################################B.4.7.2 颜色标度############################################
#连续型变量的颜色标度
f2d <- with(faithful, MASS::kde2d(eruptions, waiting, h = c(1,10), n = 50))
df <- with(f2d, cbind(expand.grid(x,y),as.vector(z)))
names(df) <- c("eruptions", "waiting", "density")

ggplot(df, aes(waiting, eruptions, fill = density)) + geom_bar(stat = "identity")

p <- ggplot(df, aes(waiting, eruptions, fill = density)) + geom_tile() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))


p + scale_fill_gradient(limits = c(0,0.04)) 
p + scale_fill_gradient(limits = c(0,0.04), low = "white", high = "black") 
p + scale_fill_gradient2(limits = c(-0.04,0.04), midpoint = mean(df$density))


#离散型变量的颜色标度
ggplot(data = msleep, aes(x = sleep_total, y = sleep_cycle, colour = vore)) + geom_point() + scale_color_brewer(palette = "Set1")

##################################B.4.7.3 同一型标???##################################
##################################B.4.8 分面的应???############################################
#####ggplot2提供两种分面类型:
#####1. 网格???(facet_grid)
#####2. 封装???(facet_wrap)
mpg2 <- subset(mpg, cyl!=5 & drv %in% c("4","f"))

##################################B.4.8.1 网格分面############################################
####用颜色来区分同一图形里的不同类别(cyl)
ggplot(data = mpg2, aes(x = cty, y = hwy, colour = factor(cyl))) + geom_point()
####一行多列地展示不同的类???
ggplot(data = mpg2, aes(x = cty, y = hwy)) + geom_point() + 
  facet_grid(.~cyl)    
####一列多行地展示不同的类???
ggplot(data = mpg2, aes(x = cty, y = hwy, colour = factor(cyl))) + geom_point() + 
  facet_grid(cyl~.)
####多列多行地展示不同的类别
ggplot(data = mpg2, aes(x = cty, y = hwy)) + geom_point() + 
  facet_grid(drv~cyl)
####添加分面的边际图 - margins = T
ggplot(data = mpg2, aes(x = cty, y = hwy)) + geom_point() + 
  facet_grid(drv~cyl, margins = T)

ggplot(data = mpg2, aes(x = cty, y = hwy)) + geom_point() + geom_smooth(aes(colour = cyl),method = "lm", se = FALSE) +
  facet_grid(drv~cyl, margins = T)


##################################B.4.8.2 封装分面############################################
####这种方法适用于处???'单个多水平变???'
ggplot(data = mpg2, aes(x = cty, y = hwy)) + geom_point() + 
  facet_wrap(~cyl, ncol = 2, scales = "fixed") 
###参数scales的可能取???: (网格分面中该参数的用法一???)
###1. fixed - x和y的标度在所有面板中都相???
###2. free - x和y的标度在每个面板中根据数据自由调???
###3. free_x - x标度可自由调???
###4. free_y - y标度可自由调???

ggplot(data = mpg2, aes(x = cty, y = hwy)) + geom_point() + 
  facet_wrap(~cyl+drv, ncol = 2)    


#######怎样根据连续型变量来进行分面
mpg2$disp_ww <- cut_interval(mpg2$displ, length = 1)
ggplot(data = mpg2, aes(x = cty, y = hwy)) + geom_point() + 
  facet_wrap(~disp_ww, nrow = 2)  


##################################B.4.9 坐标???############################################
#####ggplot2中包???6种坐标系:
#####1. 笛卡尔坐标系 - coord_cartesian
#####2. 同尺度笛卡尔坐标??? - coord_equal
#####3. 翻转的笛卡尔坐标??? - coord_filp
#####4. 变换的笛卡尔坐标??? - coord_trans
#####5. 地图射影 - coord_map
#####6. 极坐标系 - coord_polar

##################################B.4.9.1 笛卡尔坐标系############################################
#####笛卡尔坐标系有两个用于设置范围的参数xlim, ylim, 需要注意这两个参数的用法与位置标度的区???
p <- ggplot(data = mtcars, aes(x = disp, y = wt)) + geom_point() + geom_smooth()
p + scale_x_continuous(limits = c(325,500))   #对数据进行了过滤之后在画???
p + coord_cartesian(xlim = c(325,500))        #用原始数据集画好图之后再截取设定的范???


d <- ggplot(data = diamonds, aes(x = carat, y = price)) + stat_bin2d(bins = 25, colour = "grey70") + theme(legend.position = "none")
d
d+scale_x_continuous(limits = c(0,2))
d+coord_cartesian(xlim = c(0,2))

#####X/Y轴变???
p <- ggplot(data = mpg, aes(x = cty, y = displ)) + geom_point() + geom_smooth()
p
p+coord_flip() #与下图不同，此处只是简单的对原图p进行90度的反转
ggplot(data = mpg, aes(x = displ, y = cty)) + geom_point() + geom_smooth()

##################################B.4.9.2 极坐标系############################################
#####画饼???
ggplot(data = mtcars, aes(x = factor(1), fill = factor(cyl))) + geom_bar(width = 1) + coord_polar(theta = "y")


##################################B.4.10 主题:对图像外观进行控???############################################
####主题元素 - 参加 DS-R-ggplot2-主题元素

ggplot(data = mtcars, aes(x = disp, y = wt)) + geom_point() + geom_smooth() + labs(title = "GOOOOOOOOOOD")
theme(axis.text.x = element_text(size = 10, colour = "red") ,
      plot.title = element_text(size = 10, colour = "red") )

##################################%%%%%%%%%%%%%%%%%%%%%R Markdown%%%%%%%%%%%%%%%%%%%%%########################################################
library(rmarkdown)
library(tinytex)
library(TeX)





##################################%%%%%%%%%%%%%%%%%%%%%Deep Learning%%%%%%%%%%%%%%%%%%%%%######################################################
#--预备数据
m <- 10000  # number of samples
x1 <- rnorm(m, mean = 0, sd = 1)             #feature 1
x2 <- rnorm(m, mean = 1, sd = 1.5)           #feature 2
x3 <- rnorm(m, mean = 10, sd = 3)            #feature 3
x4 <- rnorm(m, mean = 19, sd = 5)            #feature 4
ds.x <- data.frame(x1,x2,x3,x4)

actual_w <- c(3,-3,0.9,-0.9)
actual_b <- 5.27
e <- rnorm(m, 0, sd = 5)
y <-  actual_w[1] * x1 + actual_w[2] * x2 + actual_w[3] * x3 + actual_w[4] * x4 + actual_b + e

#--vectorization
X <- as.matrix(ds.x); dim(X)    
X <- t(X); dim(X); X[,1]                   #Input feature vector
Y <- t(as.matrix(y))                       #actual Y


##--现假设我们构建一个最简单的神经网络(没有隐藏层,也就没有激活函数)用以线性回归.
#--输入层: X
#--输出层: Y
#--初始化权重矩阵W - 4 * 1
W <- matrix(c(0.01,0.01,0.01,0.01), nrow = 1, ncol = 4)
b <- 0

#--forward propagation
f_z <- function(W,X,b){z <- W %*% X + b}
f_a <- function(z){a <- z}
a <- f_a(f_z(W,X,b)) 

#--


##################################%%%%%%%%%%%%%%%%%%%%%mxnet-Gluon%%%%%%%%%%%%%%%%%%%%%######################################################
#-install mxnet 
#cran <- getOption("repos")
#cran["dmlc"] <- "https://s3-us-west-2.amazonaws.com/apache-mxnet/R/CRAN/"
#options(repos = cran)
#install.packages("mxnet",dependencies = T)
library(mxnet)

##################################0. mxnet Introduction##################################
#################MLP Sample
#-MLP: multiple layer perceptron, its multiple layer and non-linear avtivation function distingush MLP from linear perceptron.
fit.mlp <- mx.mlp(data = train.x, 
                  label = train.y,
                  hidden_node = 2,
                  out_node = 1,
                  #dropout = 0,   #随机失活
                  #activation = "",   
                  out_activation = "softmax",
                  num.round=20,
                  array.batch.size=1000, 
                  learning.rate=0.07, 
                  momentum=0.9,
                  
                  ctx = mx.cpu(2),
                  eval_metric = mx.metric.mse)

#################Customize evaluation metric
#--自定义模型评估度量in mxnet, sample:
demo.metric.mae <- mx.metric.custom("mae", function(label, pred) {
  pred <- mx.nd.reshape(pred, shape = 0)
  res <- mx.nd.mean(mx.nd.abs(label-pred))
  return(res)
})


#################NDArray API
##NDArray is the basic vectorized operation unit in MXNet for matrix and tensor computations, outstanding features:
#'1. Multiple devices: All operations can be run on various devices including CPUs and GPUs.
#'2. Automatic parallelization: All operations are automatically executed in parallel with each other.
a <- mx.nd.ones(shape = c(1,3))
b <- mx.nd.ones(shape = c(3,1)) + 1

#################Symbol API
##The computational unit NDArray requires a way to construct neural networks. MXNet provides a symbolic interface, named Symbol, to do this. Symbol combines both flexibility and efficiency.
data <- mx.symbol.Variable("data")
w <- mx.symbol.Variable("myweight")
net <- mx.symbol.FullyConnected(data=data,weight = w, name="fc1", num_hidden=128)
net <- mx.symbol.Activation(data=net, name="relu1", act_type="relu")
net <- mx.symbol.FullyConnected(data=net, name="fc2", num_hidden=64)
net <- mx.symbol.SoftmaxOutput(data=net, name="out")
class(net); net; arguments(net)



##################################1. Linear Regression##################################
#######准备数据
x0 <- 1
x1 <- rnorm(10000,mean = 10, sd = 10)
x2 <- rnorm(10000,mean = 2, sd = 1)
x1 <- scale(x1, center = T, scale = T)[,]
x2 <- scale(x2, center = T, scale = T)[,]

X <- matrix(data = c(rep(x0,length(x1)),x1,x2),nrow = 3, ncol = length(x1), byrow = T)
w <- t(as.matrix(c(0.57,5,-7)))

e <- rnorm(10000,0,1)

Y <- w %*% X + e
y <- t(Y)[,1]     #same with all(0.57+5*x1-7*x2 + e == y)

set.seed(1111)
RndSeq <- sample(1:length(y),length(y),replace = F)
trainSeq <- RndSeq[1:7000]
testSeq <- RndSeq[7001:10000]
##################################1.1. lm##################################
train <- data.frame(y=y[trainSeq],x1=x1[trainSeq],x2=x2[trainSeq])
test <- data.frame(y=y[testSeq],x1=x1[testSeq],x2=x2[testSeq])
ggplot(data = train, aes(x = x1, y = y)) + geom_point(aes(colour=I("blue"))) + geom_point(aes(x=x2,colour=I("red")))

fit.lm <- lm(y~x1+x2,data = train)
mse(fit.lm,train)   #same with sum(abs(fit.lm$residuals)^2) / nrow(train)
mse(fit.lm,test)    #1.0063

##################################1.2. xgboost##################################
#library(xgboost); library(data.table); library(vcd); library(Matrix); library(Ckmeans.1d.dp)
spM.train <- sparse.model.matrix(y~., data = train)[,-1]; nrow(spM.train)
spM.test <-  sparse.model.matrix(y~., data = test)[,-1]; nrow(spM.test)
dtrain <- xgb.DMatrix(data = spM.train, label = train$y); 
dtest  <- xgb.DMatrix(data = spM.test,  label = test$y);  

#--Step1: 设置初始超参
param0.xgb <- list(objective = "reg:linear",
                   eval_metric = "rmse",
                   
                   eta = 0.1,
                   
                   gamma = 0,
                   max_depth = 3,
                   min_child_weight = 5,
                   
                   subsample = 0.8,
                   colsample_bytree = 1,
                   colsample_bylevel = 1,
                   
                   lambda = 1,
                   alpha = 0,
                   
                   booster = "gbtree",
                   nthread = 2, 
                   seed = 1234
)

#--Step2: 用初始超参训练模
fit.xgb.init <- xgb.train(param0.xgb, 
                          dtrain, 
                          missing = NA,  
                          nrounds = 500,
                          watchlist = list(train = dtrain, val = dtest), 
                          print_every_n = 1
                          #early_stopping_rounds = 10 
)
fit.xgb.init
fit.xgb.init$best_iteration; fit.xgb.init$best_ntreelimit; fit.xgb.init$best_score; fit.xgb.init$niter; 

iter.error <-  data.frame(iter = rep(1:fit.xgb.init$niter,time=2),
                          error_set = rep(c("train","val"),each=fit.xgb.init$niter),
                          error_p0 = c(fit.xgb.init$evaluation_log$train_rmse,fit.xgb.init$evaluation_log$val_rmse))
ggplot(data = iter.error, aes(x = iter, y = error_p0, colour = error_set)) + 
  geom_line(size = 1) 
#--从上图可知，随着迭代次数的增加，val数据的mse下降的速度没有train数据的mse下降???
iter.error %>% filter(error_set=="train") %>% select(error_p0) %>% min()
iter.error %>% filter(error_set=="val") %>% select(error_p0) %>% min()

impt.xgb.init <- xgb.importance(feature_names = fit.xgb.init$feature_names, model = fit.xgb.init)
xgb.ggplot.importance(importance_matrix = impt.xgb.init,top_n = min(30,fit.xgb.init$nfeatures))
xgb.ggplot.deepness(model = fit.xgb.init)

#--Step3: 使用交叉验证, grid search 或??? CARAT优化超参
fit.xgb.cv <- xgb.cv(param0.xgb,
                     dtrain, 
                     missing = NA,  
                     nrounds = 1000,
                     nfold = 10,
                     prediction = TRUE,   #return the prediction using the final model 
                     showsd = TRUE,       #standard deviation of loss across folds
                     #stratified = TRUE,   #sample is unbalanced; use stratified sampling(分层抽样)
                     verbose = TRUE,
                     print_every_n = 5, 
                     early_stopping_rounds = 1000
)
fit.xgb.cv
fit.xgb.cv$best_iteration; fit.xgb.cv$best_ntreelimit; fit.xgb.cv$best_score; fit.xgb.cv$niter; 
cv.error <-  data.frame(iter = rep(1:fit.xgb.cv$niter,time=3),
                        error_set = rep(c("train","val","diff"),each=fit.xgb.cv$niter),
                        error_p0 = c(fit.xgb.cv$evaluation_log$train_rmse_mean,fit.xgb.cv$evaluation_log$test_rmse_mean,abs(fit.xgb.cv$evaluation_log$train_rmse_mean-fit.xgb.cv$evaluation_log$test_rmse_mean)),
                        error_sd = c(rep(0,fit.xgb.cv$niter),fit.xgb.cv$evaluation_log$test_rmse_std,rep(0,fit.xgb.cv$niter))
)

inter_bais_variance <- which.min(abs(cv.error[cv.error$error_set == "train","error_p0"] - cv.error[cv.error$error_set == "diff","error_p0"]))
ggplot(data = cv.error, aes(x = iter, y = error_p0, colour = error_set)) + 
  geom_line(size = 1) + 
  #geom_errorbar(aes(ymin=error_p0-error_sd, ymax=error_p0+error_sd), colour="light green", width=.1) +
  geom_ribbon(data = cv.error[cv.error$error_set == "val",],aes(ymin=error_p0-error_sd,ymax=error_p0+error_sd), fill="light green", alpha="0.5") + 
  geom_vline(xintercept = inter_bais_variance, colour = "black", linetype = 2) +
  geom_hline(yintercept = c(cv.error[cv.error$error_set == "train",]$error_p0[inter_bais_variance],
                            cv.error[cv.error$error_set == "val",]$error_p0[inter_bais_variance]),
             colour = "black", linetype = 2) +
  geom_text(x = inter_bais_variance, y = 0, label = inter_bais_variance, size = 4, vjust = 2, hjust = -.2) + 
  geom_text(x=0,y=cv.error[cv.error$error_set == "train",]$error_p0[inter_bais_variance], label = cv.error[cv.error$error_set == "train",]$error_p0[inter_bais_variance], size = 4, vjust = 0, hjust = 0)+
  geom_text(x=0,y=cv.error[cv.error$error_set == "val",]$error_p0[inter_bais_variance], label = cv.error[cv.error$error_set == "val",]$error_p0[inter_bais_variance], size = 4, vjust = 0, hjust = 0)+
  theme_bw()


##################################1.3. mxnet##################################
Mtrain <- data.matrix(train)
Mtest <- data.matrix(test)

train.x <- Mtrain[,-1]
train.y <- Mtrain[,1]
test.x <- Mtest[,-1]
test.y <- Mtest[,1]

####a flexible way to configure neural networks in mxnet
#--step1: INPUT LAYER - define the input data
data <- mx.symbol.Variable("data")
#--step2: HIDDEN LAYER - create a fully connected (hidden) layer, activation unit use 'linear transformation(Y=XW^T+b)' as default for FC
fc1 <- 
  mx.symbol.FullyConnected(data = data,           #input data
                           #weight = ?,           #weight matrix
                           #no.bias = 0,          #whether to disable bias parameter, 0 represent enable bias parameter
                           #bias = ?,             #bias parameter
                           num.hidden = 2         #number of hidden nodes (the output of this fully connected layer)
  )
#--step3: HIDDEN LAYER - more hidden layer can be difined if you want
fc2 <- mx.symbol.FullyConnected(data = data,num.hidden = 1)

#--step4: OUTPUT LAYER - use linear regression for the output layer(out_activation)
lro <- mx.symbol.LinearRegressionOutput(fc2)      #optimizes for squared loss during backward propagation


fit.nn <- mx.model.FeedForward.create(symbol = lro,         #symbolic configuration of the neural network
                                      X = train.x,
                                      y = train.y,
                                      optimizer = "sgd",   #sgd: stochastic gradient descent 
                                      array.batch.size = 1000,
                                      momentum = 0.9,      #动量参数
                                      #initializer = ?,    #initialization scheme for parameters
                                      
                                      eval.data = list(data = test.x,label = test.y),    #dataset for validation evaluation 
                                      eval.metric = mx.metric.rmse,       #evaluation metric for model
                                      
                                      num.round = 100,
                                      learning.rate = 2e-3,
                                      ctx = mx.cpu(),
                                      verbose = TRUE
)

preds <- predict(fit.nn,test.x)      #preds[1,]
sqrt(mean((preds-test.y)^2))         #rmse=1.0032


##################################%%%%%%%%%%%%%%%%%%%%%Keras%%%%%%%%%%%%%%%%%%%%%######################################################
#####Keras is a high-level nerual networks API. Features:
###'1. Allow same code to run on GPU and CPU;
###'2. User-friendly API which makes it easy to quickly prototype deep learning models;
###'3. Built-in support to CNN,RNN and any combinition of both;
###'4. Support arbitrary network architecture: multi-input, multi-output, layer sharing, model sharing, etc;
###'5. Is capable of running on top of multiple back-ends including: TensorFlow, CNTK and Theano.

#devtools::install_github("rstudio/keras")
#install.packages("keras")

#library(keras)
#install_keras() #CPU-based installations of Keras and TensorFlow
#install_keras(tensorflow = "gpu")

library(tidyverse)
library(keras)
library(xgboost)
library(lubridate)

##################################0. Data Preperation for Keras & XGBoost(sample)##################################
td <- data.frame(X1 =  rep(c("A","B","C"),10), 
                 X2 = rep(c("GOOD","BAD"),15), 
                 X3 = rep(c("M","F","S","T","Q"),6), 
                 X4 = c(1:30), 
                 X5 = c("C","D",NA,"A","A","A",rep("B",24)),
                 Y = runif(30) )
str(td)

options("na.action"); options(na.action="na.pass"); 
spM <- sparse.model.matrix(Y~.-1, data = td, na.action = "na.pass") 
M <- model.matrix(Y~.-1, data = td)
options(na.action="na.omit"); options("na.action");
class(M)
##################################1. Get Started##################################
library(keras)
library(tidyverse)

mnist <- dataset_mnist()
str(mnist)             
str(mnist$train)       
str(mnist$train$x) 
str(mnist$train$y)

#extract
x_train <- mnist$train$x; str(x_train)   #x_train is a 3D tensor (60000,28,28)
y_train <- mnist$train$y; str(y_train)   #y_train is a 1D tensor (60000)
x_test <- mnist$test$x
y_test <- mnist$test$y

# reshape
x_train <- array_reshape(x_train, c(nrow(x_train), 784))    #x_train is reshaped as a 2d tensor (60000,784)
x_test <- array_reshape(x_test, c(nrow(x_test), 784))

str(x_train); class(x_train); dim(x_train); x_train[1:10,1:10]; range(x_train);
ggplot(data = as_tibble(y_train)) + geom_bar(aes(x=as.factor(value))) + labs(x = "Digit")
ggplot(data = as_tibble(y_test)) + geom_bar(aes(x=as.factor(value))) + labs(x = "Digit")


# rescale to range(0,1)
x_train <- x_train / 255
x_test <- x_test / 255

# one-hot encode 'y'
y_train <- to_categorical(y_train, 10); str(y_train)  #y_train is transformed to a 2D tensor(60000,10)
y_train[1:10,]
y_test <- to_categorical(y_test, 10)

#####Define the model
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 256, 
              activation = "relu", 
              use_bias = TRUE,            
              kernel_initializer = initializer_glorot_uniform(),   
              bias_initializer = initializer_zeros(),
              #kernel_regularizer = regularizer_l2(l = 0.001),           # 添加L2正则化项
              #kernel_constraint = constraint_nonneg(),                  #
              input_shape = c(784)) %>%
  layer_dropout(rate = 0.4) %>%                                          # 为当前layer添加drop out
  layer_dense(units = 128, activation = "relu") %>% 
  #layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = "softmax")

summary(model)

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(lr = 0.001),
  metrics = c('accuracy')
)

history <- model %>% fit(x = x_train, 
                         y = y_train,
                         batch_size = 128,
                         validation_split = 0.2,
                         #validation_data = x_test,
                         shuffle = TRUE,
                         initial_epoch = 0,
                         epochs = 30)

# check the general information of the model
history
plot(history)

# evaluate the loss & metrics of the model on new dataset
model %>% evaluate(x_test,y_test) 
# make prediction on new data
y_test_pred_prop <- model %>% predict(x_test[1:100,])    #this will get the propbility 
y_test_pred_class <- model %>% predict_classes(x_test[1:100,]) 




##################################2. Tensor##################################
##################################2.1 What's Tensor##################################
#####Tensor: are a generalization of vectors and matrices to an arbitrary number of dimensions(axis)
###'1. 0D tensor(scalar) 
###'2. 1D tensor(vector): a one-dimensional array
x <- c(1:10)
str(x); class(x)

x1 <- as.array(x)
x1 <- array(c(1:10), dim = c(10))
str(x1); class(x1); dim(x1)

# Note the difference:
# one-dimensional array or 1D tensor: means a vector with unknown/abitrary elements
# five-dimensional vector: means a vector with 5 elements

###'3. 2D tensor(matrix) - shape(samples, features) - e.g. data frame
y <- matrix(c(1:10),nrow = 2)
str(y); class(y); dim(y)
y1 <- array(c(1:10),dim = c(2,5))
str(y1); class(y1); dim(y1)

###'4. 3D tensor(array) 
# - timestampe data or sequence data
# - shape(samples, timestamp/sequence, features) 
# - e.g. campaign contact: sample is customer contact hisroty, sequence is the contact number, features are clint information and contact record etc.
z <- array(c(1:24),dim = c(2,3,4))
str(z); class(z); dim(z)
z[1,,]; z[,1,]

###'5. 4D tensor(array) - TensorFlow_shape(samples, height, width, channel) / Theano_shape(samples, channel, height, width)- e.g. image data
###'6. 5D tensor(array) - shape(samples, frames, height, width, channels) - e.g. video data

###Concrete sample
##mnist is a 3D tensor - it's an array of 60,000 matrices of 28*28 integer(we prefer to descrip is like this, it's a 3D shape, image, we can slice it from any axis)
mnist <- dataset_mnist()
train_images <- mnist$train$x
str(train_images); class(train_images); dim(train_images); length(dim(train_images)); typeof(train_images)
train_images[1,,]                              #the matrix of fivth image
plot(as.raster(train_images[5,,], max = 255))  #plot the fivth image
slice_t <- train_images[1:10,,]; dim(slice_t); #slice

#####Common sense of tensor:
###'1. In general, the first axis of all tensor will be treated as the samples axis.
dim(train_images)    #there are 60,000 samples, each sample is a image
###'2. Normally, the entire dataset will be broken into small batchs in deep learning process.
batch <- train_images[1:128,,]   #the first batch of train_images(batch_size = 128), then the first axis of this batch dataset is called batch axis

##################################2.2 Tensor Operation##################################
####Very normal used tensor operations are:
##'1. tensor dot
##'2. tensor reshaping
##'3. gradient descent in backpropagation

##########1D tensor %*%(dot product) 1D or 2D tensor
set.seed(0)
x_1 <- sample(1:10,5,replace = T)         #x_i: i_th training sample
x_2 <- sample(1:10,5,replace = T)
x_3 <- sample(1:10,5,replace = T)
x_4 <- sample(1:10,5,replace = T)
x_5 <- sample(1:10,5,replace = T)
x_6 <- sample(1:10,5,replace = T)
x_7 <- sample(1:10,5,replace = T)
x_8 <- sample(1:10,5,replace = T)
x_9 <- sample(1:10,5,replace = T)
X <- matrix(c(x_1,x_2,x_3,x_4,x_5,x_6,x_7,x_8,x_9), nrow = 9, byrow = T)
XT <- t(X)             #transpose X to XT, then X[i,j] = X[j,i]
w <- sample(1:10,5,replace = T) / 10
b <- 1

y_1 <- x_1 %*% w + b
y_2 <- x_2 %*% w + b
y_3 <- x_3 %*% w + b
y_4 <- x_4 %*% w + b
y_5 <- x_5 %*% w + b
y_6 <- x_6 %*% w + b
y_7 <- x_7 %*% w + b
y_8 <- x_8 %*% w + b
y_9 <- x_9 %*% w + b

Y <- c(y_1,y_2,y_3,y_4,y_5,y_6,y_7,y_8,y_9)

X %*% w + b
w %*% XT + b


##########2D tensor %*%(dot product) 2D tensor
set.seed(0)
a_1 <- sample(1:10,5,replace = T)
a_2 <- sample(1:10,5,replace = T)
a_3 <- sample(1:10,5,replace = T)
a_4 <- sample(1:10,5,replace = T)
A <- matrix(c(a_1,a_2,a_3,a_4),nrow = 4, byrow = T)  #shape(A) = dim(A)

B <- t(A)[,1:3]                  #shape(B) = dim(B)

#A %*% B must ncol(A) == nrow(B)
C <- A %*% B #C[1,1] = A[1,] %*% B[,1]; C[1,2] = A[1,] %*% B[,2]; C[i,j] = A[i,] %*% B[,j]


##########Tensor reshaping - include transposation
dim(train_images); train_images[1,,]; sum(train_images[1,,])
train_images_rp <- array_reshape(train_images, dim = c(60000,28*28))
dim(train_images_rp); train_images_rp[1,]; sum(train_images_rp[1,])



##################################3. Learn Keras by example##################################
#####The typical Keras element/workflow is:
###'1. sample & target -           Define your training data: input tensor and output tensor
###'2. layers & model -            Define a network of layers that maps the inputs and output
###'3. loss function & optimizer - Configure/Compile the learning process by choosing a loss function, an optimizer, and some metric to monitor
###'3.1. loss function
###'     - defines the feedback signal used for learning
###'3.2. optimizer 
###'     - determines how learning proceeds (SDG, Momentum, Adam, RMSprop). 
###'     - optimizer will use loss value to update the networks weights
###'4. fit -                       Iterate on the training data by calling the fit() method of the model


### Summary of steps
#'S1 data loading 
#'S2 Cleansing & EDA (in case)
#'S3 data transformation - vectorization
#'S4 data normalization(in case)
#'S5 split train data into train and validation set
#'S6 remove unnecessary objects (in case)
#'S7 design the neural network architecture
#'S8 start training
#'S9 check the model
#'S10 model tuning (at least no overfitting)
#'S11 model evaluation
#'S12 make prediction (on test set)


##################################3.1 Binary Classification Example##################################
imdb <- dataset_imdb(num_words = 10000)
c(c(train_data,train_label), c(test_data,test_label)) %<-% imdb
str(train_data)

vectorize_sequences <- function(sequences, dimension = 10000) {
  results <- matrix(0, nrow = length(sequences), ncol = dimension) 
  for (i in 1:length(sequences))
    results[i, sequences[[i]]] <- 1
  results
}

# data transformation - vectorization
x_train <- vectorize_sequences(train_data); 
x_test <- vectorize_sequences(test_data)
y_train <- as.numeric(train_label)
y_test <- as.numeric(test_label)
str(x_train);attributes(x_train); x_train[1:10,1:10]; range(x_train)  #x_train每一列的值都是0或1, 10000列代表1000个单词, 0/1表示这个单词是否在review中出现
str(y_train)
table(y_train)

# data normalization(in case) - no need here, cause all value of all columns are either 0 or 1
#mean, sd, scale

# split train data into train and validation set
val_indices <- 1:10000
x_val <- x_train[val_indices,];            #str(x_val)
partial_x_train <- x_train[-val_indices,]; #str(partial_x_train); class(partial_x_train); str(partial_x_train[1:10,1:10])
y_val <- y_train[val_indices]
partial_y_train <- y_train[-val_indices];  #str(partial_y_train); class(partial_y_train)

# remove unnecessary objects
memory.size(T)   
memory.size(F)   
format(object.size(x_train), units = "auto")
rm(list = c("imdb","train_data","test_data","x_train","y_train"))
gc()   #garbage collection

# design the neural network architecture
model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")
summary(model)
model %>% compile(
  optimizer = optimizer_rmsprop(lr = 0.001),                    #optimizer_rmsprop
  loss = loss_binary_crossentropy,                              #loss_binary_crossentropy
  metrics = c("accuracy")                                       #metric_binary_accuracy
)

# start training
history <- model %>% fit(
  partial_x_train, 
  partial_y_train,
  validation_data = list(x_val, y_val),
  shuffle = TRUE,
  batch_size = 512,
  epochs = 20
)

# check the model information
str(history)    #start to overfit after 2nd epoch
history$params
history$metrics
plot(history)

history_df <- as.data.frame(history)
str(history_df)
history_df

# model tuning
# as the loss of val is getting increase from 4th epoch, so let's do just 4 iteration for the model
history1 <- model %>% fit(
  partial_x_train, 
  partial_y_train,
  epochs = 4,
  batch_size = 512,
  validation_data = list(x_val, y_val)
)

history1$metrics
plot(history1)

# evaluate the model on 'test' data
test_evl <- model %>% evaluate(x_test,y_test,
                               batch_size = 512)
test_evl; history1$metrics$val_acc

#make prediction
y_test_pred <-  model %>% predict(x_test)
y_test_pred_class <- model %>% predict_classes(x_test)
(length(y_test) - sum(abs(y_test_pred_class - y_test))) / length(y_test)   #this value is the same as test_evl$acc

#####How to tune this model to improve performance???? - no overfittin, increase accuracy of val/test 
#####As the accuracy on x_test is 0.85744, what can we try to do to improve the accuracy?
###'1. Try using one or three or more hidden layers
###'2. Try using layers with more unit, e.g.32,64...
###'3. Try using tanh activation function instead of ReLu



##################################3.2 Multi-Class Classification Example##################################
reuters <- dataset_reuters(num_words = 10000)
c(c(train_data, train_labels), c(test_data, test_labels)) %<-% reuters
str(train_data); train_data[[1]]
str(train_labels); table(train_labels)

# vectorize samples
x_train <- vectorize_sequences(train_data); 
x_test <- vectorize_sequences(test_data)

# vectorize target label
y_train <- to_categorical(train_labels, length(unique(train_labels)))
y_test <- to_categorical(test_labels, length(unique(test_labels)))
y_train[2,]; train_labels[2]

# split data into train and val
val_indices <- 1:1000
x_val <- x_train[val_indices,]
partial_x_train <- x_train[-val_indices,]
y_val <- y_train[val_indices,]
partial_y_train <- y_train[-val_indices,]

# model definition
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 64, activation = "relu", input_shape = c(dim(x_train)[[2]])) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 46, activation = "softmax")
summary(model)
model %>% compile(loss = loss_categorical_crossentropy,
                  optimizer = optimizer_rmsprop(lr = 0.001),
                  metrics = "accuracy")
history <- model %>% 
  fit(x = partial_x_train,
      y = partial_y_train,
      validation_data = list(x_val,y_val),
      batch_size = 128,
      epochs = 6)
plot(history)

results <- model %>% evaluate(x_test, y_test)
results

predictions <- model %>% predict(x_test)
dim(predictions)


# if we do not one-hot encode the label(that mean preserve their integer value [0-45])
range(train_labels)
model %>% compile(loss = loss_sparse_categorical_crossentropy,    #change the loss to sparse_categorical_crossentropy
                  optimizer = optimizer_rmsprop(lr = 0.001),
                  metrics = "accuracy")



##################################3.3 Regression Example##################################
#'S1 data loading 
datset <- dataset_boston_housing()
c(c(train_data, train_targets), c(test_data, test_targets)) %<-% datset
str(train_data); attributes(train_data); str(train_targets); class(train_data); dim(train_data); train_data[1:10,1:10];

#'S2 Cleansing & EDA (in case)
#'S3 data transformation - vectorization (in case)
#'S4 data normalization(in case)
means <- apply(train_data, 2, FUN = mean);
sds <- apply(train_data, 2, FUN = sd);
train_data <- scale(train_data, center = means, scale = sds)
test_data <- scale(test_data, center = means, scale = sds)

#'S5 split train data into train and validation set
#'S6 remove unnecessary objects (in case)
#'S7 design the neural network architecture
boston_model <- function(){
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = activation_relu, input_shape = dim(train_data)[2]) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)
  model %>%
    compile(optimizer = "rmsprop",
            loss = "mse",
            metrics = c("mae")
    )
}

#'S8 start training with cv
set.seed(0)
k <- 4
indices <- sample(1:nrow(train_data))
folds <- cut(indices, breaks = k, labels = FALSE)
table(folds)

num_epochs <- 500
all_scores <- c()
for (i in 1:k) {
  cat("processing fold #",i,"\n")
  
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- train_data[val_indices,]
  val_targets <- train_targets[val_indices] 
  partial_train_data <- train_data[-val_indices,]
  partial_train_targets <- train_targets[-val_indices]
  
  model <- boston_model()
  
  history <- 
    model %>% fit(partial_train_data, 
                  partial_train_targets, 
                  #validation_data = list(val_data, val_targets),
                  epochs = num_epochs,
                  batch_size = 32,
                  verbose = 1)
  
  results <- model %>% evaluate(val_data,val_targets,verbose = 1)
  all_scores <- c(all_scores, results$mean_absolute_error)
}

#'S9 check the model
all_scores
#'S10 model tuning (at least no overfitting)
#'S11 model evaluation
#'S12 make prediction (on test set)




##################################5. Understand text-vectorization##################################
# What's text-vectorization?
#1. Vectorizing text is the process of transforming text into numeric tensors.
#2. tokens - the different units into which you can break down text (words, characters,or n-grams) are called tokens.
#3. tokenization - breaking text into such tokens is called tokenization
#4. All text-vectorization processes consist of applying some tokenization scheme and then associating numeric vectors with the generated tokens.

# Two major approaches of text-vectorization:
#1. one-hot encoding of tokens
#2. word embedding

##################################5.1. Understand n-grams and bag-of-words##################################
# n-grams are groups of N (or fewer) consecutive words that you can extract from a sentence.

# sample of decomposing sentence 'The cat sat on the mat.' into 2-grams: (bag-of-2-grams)
#{"The", "The cat", "cat", "cat sat", "sat", "sat on", "on", "on the", "the", "the mat", "mat"}
# or 3-grams: (bag-of-3-grams)
#{"The", "The cat", "cat", "cat sat", "The cat sat", "sat", "sat on", "on", "cat sat on", "on the", "the", "sat on the", "the mat", "mat", "on the mat"}
# This kind of tokenization methods is called bag-of-words.

# bag-of-words isn’t an order-preserving tokenization method, it tends to be used in shallow languageprocessing models rather than in deep-learning models.

# n-gram & bag-of-words is a powerful, unavoidable feature-engineering tool when using lightweight, shallow, textprocessing models such as logistic regression and random forests

##################################5.2. One-hot encoding of words##################################
samples <- c("The cat sat on the mat.", "The dog ate my homework.")
token_index <- list()
for(sample in samples){
  for(word in strsplit(sample," ")[[1]]){
    if (!word %in% names(token_index)){
      token_index[[word]] <- length(token_index) + 2
    }
  }
}
max_length <- 10
results <- array(0, dim = c(length(samples),                  # 2 samples
                            max_length,                       # get the top min(max_length, length(sample)) words
                            max(as.integer(token_index))))    # the size of the vocabulary (token list); in real case this should be bigger than 10,000
dim(results); str(results)
for (i in 1:length(samples)) {
  sample <- samples[[i]]
  words <- head(strsplit(sample, " ")[[1]], n = max_length)
  for (j in 1:length(words)) {
    index <- token_index[[words[[j]]]]
    results[[i, j, index]] <- 1
  }
}
results[1,,]

# use Keras for one-hot encoding
tokenizor <- text_tokenizer(num_words = 1000) %>%
  fit_text_tokenizer(samples)

tokenizor$index_word; tokenizor$num_words; tokenizor$word_index

sequences <- texts_to_sequences(tokenizor, samples)
sequences[[1]]

one_hot_results <- texts_to_matrix(tokenizor, samples, mode = "binary")
str(one_hot_results)      #应该是一个3D tensor才对
sequences[[1]]; one_hot_results[1,]; 


# one-hot hashing trick - can use when the number of unique tokens in your vocabulary is too large to handle explicitly.
# Instead of explicitly assigning an index to each word and keeping a reference of these indices in a dictionary, you can hash words into vectors of fixed size.


##################################5.3. Word Embedding##################################
# Unlike the word vectors obtained via one-hot encoding, word embeddings are learned from data.
# It’s common to see word embeddings that are 256-dimensional, 512-dimensional, or 1024-dimensional (one-hot encoding words generally leads to vectors that are 20,000dimensional or greater)

# There are two ways to obtain word embeddings:
#1. Learn word embeddings jointly with the main task you care about (such as document classification or sentiment prediction).
#   (In this setup, you start with random word vectors and then learn word vectors in the same way you learn the weights of a neural network.)
#2. Pretrained word embeddings - 

# Note:
#what makes a good word-embedding space depends heavily on your task: 
#the perfect word-embedding space for an English-language movie-review sentiment analysis model may look 
#different from the perfect embedding space for an English-language legal-document-classification model, 
#because the importance of certain semantic relationships varies from task to task
# It’s thus reasonable to learn a new embedding space with every new task.

##################################5.3.1. layer_embedding In Keras##################################
embedding_layer <- layer_embedding(input_dim = 1000,            # the number of tokens (vocabulary)
                                   input_length = maxlen,       # words count of each sample(move-review)
                                   output_dim = 64              # the dimensionality of the embeddings
                                   )         
# understand layer_embedding: as a dictionary that maps integer indices (which stand for specific words) to dense vectors. 
# (Word Index --> Embedding Layer --> Corresponding word vector)

# Input of layer_embedding:
#2D integer tensor, shape(samples, sequence_length); think about above example(cat,dog), sequence_length should be the same for every sample (through padding)
# Output of layer_embedding:
#3D floating-point tensor, shape(samples, sequence_length, embedding_dimensionality), the 3D tensor then can be processed by an RNN or 1D conv layer

# How layer_embedding works?
#1. When instantiate an embedding layer, its weights (its internal dictionary of token vectors) are initially random, just as with any other layer.
#2. During training, these word vectors are gradually adjusted via backpropagation, structuring the space into something the downstream model can use.
#3. Once fully trained, the embedding space will show a lot of structure—a kind of structure specialized for the specific problem for which you’re training your model


# Understand with example - IMDB movie-review sentiment-prediction
max_features <- 20000              # length of token (vacobulary space)
maxlen <- 200                      # words count of each sample(move-review)
embedding_dimensionality <- 16     # hyper-parameter
imdb <- dataset_imdb(num_words = max_features)
c(c(x_train, y_train), c(x_test, y_test)) %<-% imdb
str(x_train)                       # 当前数据集中每条评论的长度不一致
x_train <- pad_sequences(x_train, maxlen = maxlen)   #通过padding, 将每条评论的长度弄成一样为maxlen
x_test <- pad_sequences(x_test, maxlen = maxlen)
str(x_train)


model <- 
  keras_model_sequential() %>%
  layer_embedding(input_dim = max_features, 
                  input_length = maxlen,
                  output_dim = embedding_dimensionality) %>%
  layer_lstm(units = 16) %>%
  layer_dense(units = 1, activation = "sigmoid")
summary(model)
model %>% compile(loss = "binary_crossentropy",
                  optimizer = optimizer_rmsprop(lr = 0.001),
                  metrics = c("acc"))
history <- 
  model %>%
  fit(x_train, y_train,
      validation_data = list(x_test,y_test),
      batch_size = 32,
      epochs = 20)
  

##################################5.3.2. Pretrained word embeddings - GloVe##################################
# There are various precomputed databases of word embeddings that you can download and use in a Keras embedding layer:
#1. word2vec
#2. GloVe - Global Vectors for Word Representation

P211


##################################6. RNN##################################
##################################6.1 RNN for IMDB##################################
max_features <- 10000
maxlen <- 500
batch_size <- 128

imdb <- dataset_imdb(num_words = max_features)
c(c(input_train, y_train), c(input_test, y_test)) %<-% imdb
str(input_train); length(input_train); 
range(sapply(input_train, FUN = function(x){length(x)}))   #input_train共包含25000条评论，评论的长度范围是[11,2494]
first_review <- input_train[[1]]

# 对每条评论进行padding 处理，让每条评论的长度都变成500
input_train <- pad_sequences(input_train, maxlen = maxlen)
input_test <- pad_sequences(input_test, maxlen = maxlen)
str(input_train); dim(input_train)
input_train[1,] 

# 构建RNN模型
#Simple RNN
model <- 
  keras_model_sequential() %>%
  layer_embedding(input_dim = max_features, output_dim = 32) %>%
  layer_simple_rnn(units = 32,
                   activation = "tanh",
                   return_sequences = FALSE) %>%
  layer_dense(units = 1, activation = "sigmoid")
# LSTM RNN
model <- 
  keras_model_sequential() %>%
  layer_embedding(input_dim = max_features, output_dim = 32) %>%
  layer_lstm(units = 32,
                   activation = "tanh",
                   return_sequences = FALSE) %>%
  layer_dense(units = 1, activation = "sigmoid")

summary(model)

model %>% 
  compile(loss = loss_binary_crossentropy,
          optimizer = optimizer_rmsprop(lr = 0.001),
          metrics = c("acc"))
history <- model %>%
  fit(input_train,
      y_train,
      batch_size = batch_size,
      validation_split = .2,
      epochs = 10                            
      )
plot(history)
test_evl <- model %>% evaluate(input_test,y_test)  #acc is 0.828 of simple RNN, is 0.866 of LSTM RNN


##################################6.2 temperature-forecasting##################################
dir.create("./data/jena_climate", recursive = TRUE)
download.file(
  "https://s3.amazonaws.com/keras-datasets/jena_climate_2009_2016.csv.zip",
  "./data/jena_climate/jena_climate_2009_2016.csv.zip"
)
unzip(
  "./data/jena_climate/jena_climate_2009_2016.csv.zip",
  exdir = "./data/jena_climate"
)

# dataset statement: 
# In this dataset, 14 different quantities (such air temperature...) were recorded every 10 minutes, over several years.
data <- read_csv("./data/jena_climate/jena_climate_2009_2016.csv")
glimpse(data)
head(data)  
View(data[1:1000,])

ggplot(data, aes(x=1:nrow(data), y = `T (degC)`)) + geom_line()
ggplot(data[1:14400,], aes(x=1:1440, y = `T (degC)`)) + geom_line() #only look at first 1440 rows (每10分钟记录一条数据，1440条数据即记录了10天的数据)
1440 * 10 / 60 / 24


####Problem: 
# 给定往后推10天的数据(即1440 samples,每6个timesteps(一个小时)进行一次抽样,即每个sequence有1440/6=240 条数据), 来预测未来24小时这个时间点的气温
# 即input 是shape(simples, timesteps = 240, features = length(names(data)) - 1)
#   target是(Ty = 1)

####预备知识: generator function
sequence_generator <- function(start){
  value <- start - 1
  function(){
    value <<- value + 1
    value
  }
}

gen <- sequence_generator(1)
gen() 


### data preperation for NN model
glimpse(data)
data <- data.matrix(data[,-1])      #convert to matrix
data[1:5,]

train_data <- data[1:200000,]       #use the first 200000 sample as train set
means <- apply(train_data, 2, mean)
sds <- apply(train_data, 2, sd)
data <- scale(data, center = means, scale = sds)
data[1:5,]
range(data[,3])

### create generator
generator <- function(data,
                      lookback = 1440,    #look back 10 days
                      step = 6,           #sample draw data every 1h
                      delay = 144,        #predict temp within next 24hs
                      min_index = 0,      #we will generate train/val/test, this use to split
                      max_index = 200000,
                      shuffle, 
                      batch_size = 128){
  if(is.null(max_index)) max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function(){
    if(shuffle){
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    }else{
      if(i + batch_size >= max_index){i <<- min_index + lookback}
      rows <- c(i:min(i+batch_size, max_index)) 
      i <<- i + length(rows)
    }
    samples <- array(0, dim = c(length(rows),
                                lookback / step,
                                dim(data)[[-1]]))
    #dim(samples)
    targets <- array(0, dim = c(length(rows)))
    
    for(j in 1:length(rows)){
      indices <- seq(rows[[j]] - lookback, rows[[j]], length.out = dim(samples)[[2]])  #1:1440中按照均匀分布抽取240个samples, 即每隔1h抽取一个样本
      samples[j,,] <- data[indices,] 
      targets[[j]] <- data[rows[[j]] + delay,2]
    }
    list(samples, targets)
  }
}

lookback <- 1440
step <- 6
delay <- 144
batch_size <- 128

train_gen <- generator(
  data,
  lookback = lookback,
  step = step,
  delay = delay,
  min_index = 1,
  max_index = 200000,
  shuffle = TRUE,
  batch_size = batch_size
)
#train_s1 <- train_gen()
#str(train_s1[[1]])

val_gen <- generator(
  data,
  lookback = lookback,
  step = step,
  delay = delay,
  min_index = 200001,
  max_index = 300000,
  shuffle = FALSE,
  batch_size = batch_size
)

test_gen <- generator(
  data,
  lookback = lookback,
  step = step,
  delay = delay,
  min_index = 300001,
  max_index = NULL,
  shuffle = FALSE,
  batch_size = batch_size
)

val_steps <- (300000 - 200001 - lookback) / batch_size
test_steps <- (nrow(data) - 300001 - lookback) / batch_size
train_steps <- (200000 - lookback) / batch_size

model <- keras_model_sequential() %>%
  layer_gru(units = 32, 
            dropout = 0.1,
            recurrent_dropout = 0.5,
            return_sequences = TRUE, 
            input_shape = list(NULL, dim(data)[[-1]])) %>%
  layer_gru(units = 64, 
            activation = "relu",
            dropout = 0.1,
            recurrent_dropout = 0.5) %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(lr = 0.001),
  loss = "mae"
)

history <- model %>% 
  fit_generator(
    train_gen,
    steps_per_epoch = 500,               #use train_steps, take much more time
    epochs = 10,
    validation_data = val_gen,
    validation_steps = val_steps)

evl_test <- model %>% evaluate_generator(test_gen,
                                         steps = test_steps)

plot(history)

##################################7. Keras functions##################################
# create a fully connectted layer - output = activation(dot(input, kernel) + bias); 
# 'kernel' is a weights matrix
layer_dense(
  units = 32,                                 # number of hidden units in this layer
  activation = "relu",                        # activation function, e.g. relu, tanh, sigmoid, softmax, linear... 
  input_shape = c(100),                       # not including sample axis, only for first layer
  
  batch_input_shape = c(32,100),              # almost not use
  batch_size = 32,                            # almost not use   
  
  use_bias = TRUE,                            # DEFAULT
  kernel_initializer = "glorot_uniform",      # DEFAULT
  bias_initializer = "zero"                   # DEFAULT
)

# loss function(objective function) - the quantity that will be minimized during training
loss_mean_squared_error()                  #mse
loss_mean_absolute_error()                 #mae
loss_binary_crossentropy()                 #log-likehood
loss_categorical_crossentropy()            #softmax(activation)

# metrics(模型评估度量)
# 可自定义
uf_metrics_log <- function(y_true, y_pred){
  return(log(y_true - (exp(1) ^ y_pred - 1) + 1) ^ 2)
}
compile(model, 
        metrics = list(metric_binary_accuracy(),uf_metrics_log))


# optimizer - determine how the network will be updated based on the loss function
# focus on hyper-parameter 'learning rate', for the other hyper-parameters, leave it as default
# the following 3 optimizer is used mostly
optimizer_sgd(lr = 0.01,                     # learning rate(IMPORTANT), need to tune
              momentum = 0.9,                # if >0 (e.g. 0.9), then 'sgd with momentum'
              
              decay = 0,                     #learning rate decay (lr = 1/(1+decay*epoch_num) * lr0)
              clipnorm = NULL,               #用于防止梯度爆炸和梯度消失??
              clipvalue = NULL               #DEFAULT
              )

optimizer_rmsprop(lr = 0.001,                #learning rate
                  rho = 0.9,                 #DEFAULT (指数衰减率)
                  
                  decay = 0,                 #DEFAULT
                  clipnorm = NULL,           #DEFAULT
                  clipvalue = NULL           #DEFAULT
                  )

optimizer_adam(lr = 0.001,
               beta_1 = 0.9,                 #DEFAULT (指数衰减率 for momentum)
               beta_2 = 0.99,                #DEFAULT (指数衰减率 for rmsprop)
               
               decay = 0,                    #DEFAULT
               clipnorm = NULL,              #DEFAULT
               clipvalue = NULL              #DEFAULT
               )

# model fit
keras::fit(
  input_tensor,
  target_tensor,
  batch_size = 128,
  epochs = 10,
  validation_data = list(x_val,y_val),      #will override validation_split
  validation_split = 0.2,
  shuffle = TRUE,                           #whether to shuffle the training data before each epoch
  initial_epoch = 1,                        #useful for resuming a previous training run
  steps_per_epoch = NULL                    #default NULL is equal to the number of samples in your dataset divided by the batch size
)


##################################8. Keras functional API##################################
# sequential model
seq_model <- 
  keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu", input_shape = c(64)) %>%
  layer_dense(units = 32, activation = "relu") %>% 
  layer_dense(units = 10, activation = "softmax")

# correspond functional API
input_tensor <- layer_input(shape = c(64))
output_tensor <- 
  input_tensor %>% 
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 10, activation = "softmax")
model <- keras_model(inputs = input_tensor, 
                    outputs = output_tensor)
summary(model)

# compling, fitting, and evaluating are the same
model %>% compile(loss = "categorical_crossentropy",
                  optimizer = "rmsprop")
x_train <- array(runif(1000 * 64), dim = c(1000, 64))
y_train <- array(runif(1000 * 10), dim = c(1000, 10))
history <- model %>% 
  fit(x_train, y_train, epochs = 10, batch_size = 128)


##################################8.1. Beyond the sequencial model(More network architecture)##################################
##################################8.1.1. Multi-input model##################################
# Example: a question-answering model
text_vocabulary_size <- 1000
ques_vocabulary_size <- 1000
answer_vocabulary_size <- 50

#input - text
text_input <- layer_input(shape = list(NULL),
                          dtype = "int32",
                          name = "text")
encoded_text <- text_input %>%
  layer_embedding(input_dim = text_vocabulary_size, output_dim = 64) %>%
  layer_lstm(units = 32)

#input - question 
question_input <- layer_input(shape = list(NULL),
                              dtype = "int32",
                              name = "question")
encoded_question <- question_input %>%
  layer_embedding(input_dim = ques_vocabulary_size, output_dim = 32) %>%
  layer_lstm(units = 16)

#concatenate both input
concatenated <- layer_concatenate(list(encoded_text, encoded_question))

#outout
answer <- concatenated %>%
  layer_dense(units = answer_vocabulary_size,
              activation = "softmax")

#model
model <- keras_model(list(text_input, question_input),
                     answer)

summary(model)

#compile
model %>% compile(loss = "categorical_crossentropy",
                  optimizer = "rmsprop",
                  metrics = c("acc"))
  
#data
num_samples <- 1000
max_length <- 100
random_matrix <- function(range, nrow, ncol){
  matrix(sample(range, size = nrow * ncol, replace = TRUE),
         nrow = nrow, ncol = ncol)
}
text <- random_matrix(1:text_vocabulary_size, num_samples, max_length); dim(text)
question <- random_matrix(1:ques_vocabulary_size, num_samples, max_length); dim(question)
answers <- random_matrix(0:1, num_samples, answer_vocabulary_size); dim(answers)

#fit
model %>% fit(
  list(text = text, question = question),
  answers,
  batch_size = 128,
  epochs = 10
)

##################################8.1.2. Multi-output model##################################
##################################8.1.3. Directed acyclic graphs of layers##################################

##################################8.2. Callbacks##################################
# refer to: P283
# callback_*

## callback_early_stopping: 
## to interrupt training once a target metric being monitored has stopped improving for a fixed number of epochs.
## this callback is typically used in combination with 
## callback_model_checkpoint: which lets you continually save the model during training.

## callback_reduce_lr_on_plateau:
## to reduce the learning rate when the validation loss has stopped improving.

## customize callback by creating a new R6 class that inherits from the KerasCallback class.
## on_epoch_begin/on_epoch_end/on_batch_begin/on_batch_end/on_train_begin/on_train_end

callbacks_list <- list(
  callback_early_stopping(
    monitor = "acc",
    patience = 1
  ),
  callback_model_checkpoint(
    filepath = "./outputs/my_model.h5",
    monitor = "val_loss",
    save_best_only = TRUE
  ),
  callback_reduce_lr_on_plateau(
    monitor = "val_loss",
    factor = 0.1,
    patience = 2
  )
)

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

model %>% fit(
  x,y,
  validation_data = list(val_x,val_y),
  batch_size = 128,
  epochs = 100,
  
  callbacks = callbacks_list
)


# sample of customize Keras callback
library(R6)
LossHistory <- 
  R6Class("LossHistory",
          inherit = KerasCallback,
          public = list(
            losses = NULL,
            on_batch_end = function(batch, logs = list()){
              self$losses <- c(self$losses, logs[["loss"]])
            }
  ))

callback_batchLoss <- LossHistory$new()

model %>% fit(
  x,y,
  batch_size = 128,
  epochs = 100,
  callbacks = list(callback_batchLoss)
)

##################################8.4. TensorBoard##################################
# TensorBoard - a browser-based visualization tool, is used to help you visually monitor everything that goes on inside your model during training.
max_features <- 2000
max_len <- 500
imdb <- dataset_imdb(num_words = max_features)
c(c(x_train, y_train), c(x_test, y_test)) %<-% imdb
x_train <- pad_sequences(x_train, maxlen = max_len); str(x_train); x_train[1,]; range(x_train)
x_test <- pad_sequences(x_test, maxlen = max_len)

model <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_features, output_dim = 128,
                  input_length = max_len, name = "embed") %>%
  layer_conv_1d(filters = 32, kernel_size = 7, activation = "relu") %>%
  layer_max_pooling_1d(pool_size = 5) %>%
  layer_conv_1d(filters = 32, kernel_size = 7, activation = "relu") %>%
  layer_global_max_pooling_1d() %>%
  layer_dense(units = 1)

summary(model)

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

TB_log_dir <- "./outputs/tensorBoard_log"

tensorboard(TB_log_dir)
callbacks <- list(
  callback_tensorboard(
    log_dir = TB_log_dir,
    histogram_freq = 1,
    embeddings_freq = 1
  )
)

model %>% fit(
  x_train,y_train,
  batch_size = 128,
  validation_split = .2,
  epochs = 20,
  callbacks = callbacks
)


##################################%%%%%%%%%%%%%%%%%%%%%Andrew Ng%%%%%%%%%%%%%%%%%%%%%######################################################
##https://www.deeplearning.ai/

##Data
##Computation
##Algorithm

##################################神经网络基础##################################
#sigmoid function
sigm <- function(z){return(1/(1+exp(1)^-z))}
z <- runif(10000,min = -5, max = 5)  #z=t(W)*X+b
g <- sigm(z)
sigm(0)
ggplot(data = tibble(g=g,z=z), aes(x=z,y=g))+geom_line()

#假设函数
y_hat = g(wx+b)
#coss function/loss function
J = yi*ln(yi-y_hati) + (1-yi)*ln(yi)




















































































































































































































































































































