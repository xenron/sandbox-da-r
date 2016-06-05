#####---lesson 2---#####

###-Data Type


##-is.datatype & as.datatype()
a<-c(1,2,3)
mode(a)
is.numeric(a)
is.vector(a)
mode(matrix())

a<-as.character(a);a
is.numeric(a)
is.vector(a)
is.character(a)

##-Numeric
x<-3;mode(x)
y<-"hello";mode(y)
z<-TRUE;mode(z)

as.numeric(c(1,3.65,"3","sd"))


#format
pow <- 1:3
(powers_of_e <- exp(pow))  #外面的括号表示直接输出power_of_e
formatC(powers_of_e)
formatC(powers_of_e, digits = 3) # 指定三个数字
formatC(powers_of_e, digits = 3, width = 10) # 前面加上一个空格
formatC(powers_of_e, digits = 3, format = "e") # 科学格式
formatC(powers_of_e, digits = 3, flag = "+") # 前面加上+

sprintf("%s %d = %f", "Euler's constant to the power", pow, powers_of_e)
sprintf("To three decimal places, e ^ %d = %.3f", pow, powers_of_e)
sprintf("In scientific notation, e ^ %d = %e", pow, powers_of_e)

format(powers_of_e)
format(powers_of_e, digits = 3) # 至少三个数字
format(powers_of_e, digits = 3, trim = TRUE) # 去掉多余的0
format(powers_of_e, digits = 3, scientific = TRUE)
prettyNum(
  c(1e10, 1e-20),
  big.mark = ",",
  small.mark = " ",
  preserve.width = "individual",
  scientific = FALSE
)



##-Logical
lo<-c(TRUE, TRUE, FALSE, TRUE, FALSE);lo
lo2<-c(T, T, F, F, T);lo2
lo3<-c(True, False)   #do not make such mistake
mode(lo2)

#Missing Value
y<-c(1,2,3,NA)
is.na(y)
is.na(mtcars[2:10,6:10])    #all are available
na.omit

##-Character
#input and output
c(
  "You should use double quotes most of the time",
  'Single quotes are better for including " inside the string'
)
paste(c("red", "yellow"), "lorry")
paste(c("red", "yellow"), "lorry", sep = "-")
paste(c("red", "yellow"), "lorry", collapse = ", ")
paste0(c("red", "yellow"), "lorry")

x <- c("a", "b", "c","aaaaaaaaaaa")
toString(x)
toString(x, width = 8)

cat(c("red", "yellow"), "lorry")

x <- c(
  "I", "saw", "a", "saw", "that", "could", "out",
  "saw", "any", "other", "saw", "I", "ever", "saw"
)
y <- noquote(x)
x
y


#special character
cat("foo\tbar", fill = TRUE)
cat("foo\nbar", fill = TRUE)
cat("foo\0bar", fill = TRUE) # 这会抛出一个错误
cat("foo\\bar", fill = TRUE)
cat("foo\"bar", fill = TRUE)
cat('foo\'bar', fill = TRUE)
cat("foo'bar", fill = TRUE)
cat('foo"bar', fill = TRUE)
cat("\a")
alarm()

#C&S
toupper("I'm Shouting")
tolower("I'm Whispering")

#substing and substr
#1
substr("abcdef", 2, 4)
substr(rep("abcdef", 4), 1:4, 4:5)
x <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")
substr(x, 2, 5)
substring(x, 2, 4:6)

#2
woodchuck <- c(
  "How much wood would a woodchuck chuck",
  "If a woodchuck could chuck wood?",
  "He would chuck, he would, as much as he could",
  "And chuck as much wood as a woodchuck would",
  "If a woodchuck could chuck wood."
)
substring(woodchuck, 1:6, 10)

substr(woodchuck, 1:6, 10)

#strsplit
strsplit(woodchuck, " ", fixed=T)   #注意空格
strsplit(woodchuck, ",? ")          #注意空格

#the workspace
getwd()
setwd("d:/workspace/rstudio")
getwd()

file.path("c:", "Program Files", "R", "R-devel")
R.home() # 同样也是R 的安装目录
path.expand(".")
path.expand("..")
path.expand("~")
file_name <- "C:/Program Files/R/R-devel/bin/x64/RGui.exe"
basename(file_name)
dirname(file_name)


##-Complex
matrix(1i^ (-6:5), nrow = 4) #- all columns are the same

zz <- (rep(1:4, len = 9) + 1i*(9:1))/10;zz
zz.shift <- complex(modulus = Mod(zz), argument = Arg(zz) + pi)
zz.shift
plot(zz, xlim = c(-1,1), ylim = c(-1,1), col = "red", asp = 1,
     main = expression(paste("Rotation by "," ", pi == 180^o)))
abline(h = 0, v = 0, col = "blue", lty = 3)
points(zz.shift, col = "orange")


##-Raw
r<-raw(2)

##-Date
mydates<-as.Date(c("2015-12-25","2016-01-12"))
strDates<-c("02/05/1998","05/21/1998")
dates<-as.Date(strDates, "%m/%d/%Y");dates

d<-c("02/05/14","01/24/98");mode(d)
myformate<-"%m/%d/%y"
dates2<-as.Date(d, myformate);dates2
mode(dates2)

Sys.Date()
date()

today<-Sys.Date()
format(today,format="%B %d %Y")
format(today,format="%A")

start<-as.Date("1994-06-19")
end<-as.Date("2016-01-13")
days<-end-start;days

today<-Sys.Date()
birth<-as.Date("1968-10-12")
difftime(today, birth, units="weeks")

###-Data Structure

##-vector
5.5:8.5
c(1,1:3,c(5,8),13)

a <- c(1,2,3,4,5,6,-3,-29);a
b <- c("one", "Two", "three");b
c <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE);c
d <- c(1:5);d
a[2]
a[c(2,4)]
a[3:5]
a <- c(a[1:3],26,-3,-a[6]);a

vector("numeric",5)     #numeric(5)
vector("complex",5)     #complex(5)
vector("logical",5)     #logical(5)
vector("character",5)   #character(5)
vector("list",5)        #list(5)

seq(from=12, to=30, by=3)
seq(from=1.4, to=7.8, by=2.3)

#length
length(1:5)
length(c(TRUE,FALSE,NA))
sn <- c("Sheena", "leads", "Sheila", "needs")
length(sn)
nchar(sn)

#naming
c(apple=1, banana=2, kiwi fruit=3,4)      #wrong sentence
c(apple=1, banana=2, "kiwi fruit"=3,4)    #right one

x<-1:4
names(x)<-c("apple","banana","kiwi fruit", "")
names(x)
names(1:4)

#calculate
2+3
"+"(2,3)

x<-c(1,3,5)
x+c(3,5,7)
x*c(3,5,7)
x/c(3,5,7)
x%%c(3,5,7)    #取余数

#index
y<-c(1,2,3,4,0.5,-89)
y[c(1,3)]
y[2:4]
v<-3:4
y[v]
z<-y[c(1,1,4,4,5)];z
y[-1];y
y[-1:-2];y
y[1:(length(y)-1)]
y[-length(y)]

#repeat
x<-rep(6,3);x
rep(c(2,4,6),4)
rep(1:3,5)
rep(c(2,4,6),each=2)


##-Matrix and Array
#creating matrix
y <- matrix(1:20, nrow=5, ncol=4);y
cells <- c(1,25,24,68)
rnames <- c("R1","R2")
cnames <- c("C1","C2")
mymatrix <- matrix(cells, nrow=2, ncol=2, byrow=T,
                   dimnames=list(rnames, cnames))
mymatrix

x <- matrix(1:10, nrow=2);x
x[2, ]
x[ ,2]
x[2,4]
x[1,c(3,4)]

#加减乘除
y <- matrix(c(1:4), nrow=2);y
y %*% y     #mathematical matrix multiplication
3*y         #mathematical multiplication of matrix by scalar
y+y         #mathematical matrix addition

#矩阵筛选
x<-matrix(c(1,2,3,2,3,4),ncol=2);x
x[x[ ,2]>=3,]

#apply()
apply(x, 1, mean)

f<-function(x) x/c(2,8)
y<-apply(x,1,f);y

t(apply(x,1,f))

copymaj<-function(rw,d){
  maj<-sum(rw[1:d])/d
  return(if(maj>0.5) 1 else 0)
}
x<-matrix(c(1,1,1,0,0,1,0,1,1,1,0,1,1,1,1,1,0,0,1,0),nrow=4);x
apply(x,1,copymaj,3)
apply(x,1,copymaj,2)

#rbind() and cbind()
a<-c(1,2,3,4);a
b<-matrix(c(5:16),nrow=4);b
C<-c(2,4,5);C    #最好不要小写c，容易与c函数混淆
rbind(b,C)
b<-cbind(a,b)

q<-cbind(c(1,2),c(3,4));q
m<-matrix(1:6,nrow=3);m
m<-m[c(1,3),]

#array
dim1 <- c("A1","A2")
dim2 <- c("B1","B2","B3")
dim3 <- c("C1","C2","C3","C4")
z <- array(1:24, c(2,3,4),dimnames=list(dim1, dim2, dim3));z

##-data.frame
patientID<-c(1,2,3,4)
age<-c(25,34,28,52)
diabetes<-c("Type1","Type2","Type1","Type1")
status<-c("Poor","Improved","Excellent","Poor")
patientdata<-data.frame(patientID, age, diabetes, status)
patientdata

patientdata[1:2]
patientdata[c("diavetes","status")]
patientdata$age
table(patientdata$diabetes, patientdata$status)

#attach(), detach() and with()
summary(mtcars$mpg)
plot(mtcars$mpg,mtcars$disp)
plot(mtcars$mpg,mtcars$wt)

attach(mtcars)
summary(mpg)
plot(mpg,disp)
plot(mpg,wt)
detach(mtcars)

mpg<-c(23,2,5,345)
attach(mtcars)
plot(mpg, wt)    #an error appears

with(mtcars,{
  summary(mpg, disp, wt)
  plot(mpg, disp)
  plot(mpg, wt)
  })

with(mtcars,{
  stats<-summary(mpg)
  stats
})
stats

#NA
pat2<-edit(patientdata)
pat2
complete.cases(pat2)
pat3<-pat2[complete.cases(pat2),];pat3
na.omit(pat2)     #or we can use na.omit

#rbind(), cbind()
patientdata
rbind(patientdata, list(1,35,"Type2","Excellent"))

math<-c(89,98,59,76)
chinese<-c(67,79,86,46)
english<-c(100,65,86,99)
grade<-data.frame(math, chinese,english);grade
class(grade)
grade<-cbind(grade, grade$math-grade$chinese);grade
class(grade)

#apply()
apply(grade,1,max)
apply(grade,1,mean)

#merge()
math2<-c(89,56)
chinese2<-c(69,78)
english2<-c(99,86)
grade2<-data.frame(math2, chinese2, english2);grade2
merge(grade, grade2)

##-factors
patientID<-c(1,2,3,4)
age<-c(25,34,28,52)
diabetes<-c("Type1","Type2","Type1","Type1")
status<-c("Poor","Improved","Excellent","Poor")
diabetes<-factor(diabetes)
status<-factor(status, order=T)
patientdata<-data.frame(patientID, age, diabetes, status)
str(patientdata)
summary(patientdata)

##-list
j<-list(name="Joe",salary=55000, union=T)
j <-list("Joe", 55000, T)

z<-vector(mode=”list)
z[[“abc”]]<-3

#index
j$salary
j[["salary"]]      #注意使用双重括号
j[[2]]
j[1:2]
j2<-j[2];j2
class(j2)
str(j2)

j[[1:2]]
j3<-j[[1:2]];j3
class(j3)

#增加删除列表元素
j
j$sex<-"M"    #增加性别
j

j[[5]]<-36
j[6:8]<-c(T,F,T);j

j$union<-NULL;j
c(list("Grace",5000,T),list(33))

#apply()
lapply(list(1:3,25:29),median)
sapply(list(1:3,25:29),median)








