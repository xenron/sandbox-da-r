###################################################################################
# 步骤一：识别缺失数据
# 三个判断缺失值的函数
is.na()
is.nan()
is.infinite()

x1<-NA
x2<-0/0
x3<-1/0

is.na(x3)
is.nan(x3)
is.infinite(x3)

# 数据中特殊缺失值的处理
read.table("d:/data/lesson4/Data1.txt",na.strings ="--" )





###################################################################################
# 步骤二：探究缺失数据

# 探究缺失数据的比例
mat<-matrix(1:60,nrow=10)
mat[1,4]<-NA
mat[6,1:3]<-NA
mat[9:10,6]<-NA
mat

sum(is.na(mat[9,]))

mean(!is.na(mat))>0.9

# 缺失值的位置研究
as.vector(attributes(na.omit(mat))$na.action)

which(rowSums(is.na(mat))!=0)

which(complete.cases(mat)==F)


matbig<-matrix(1:100000000,nrow=1000)
matbig[1,4]<-NA
matbig[6,1:3]<-NA
matbig[9:10,6]<-NA
head(matbig)

system.time(as.vector(attributes(na.omit(matbig))$na.action))
system.time(which(rowSums(is.na(matbig))!=0))
system.time(which(complete.cases(matbig)==F))


###################################################################################
# 缺失数据的图形可视化VIM包

library(VIM)

aggr(mat,numbers=T,prop= F)

matrixplot(mat)



###################################################################################
# 相关性探索

mat

mat_NA<-mat[,which(complete.cases(t(mat))==F)]
mat_NA<-abs(is.na(mat_NA))
cor(mat_NA)




###################################################################################
# 案例1：Data.csv数据为气象站的测风数据，包含部分缺失数据NA，请处理Data中的缺失值。

Data <- read.table("D:/tmp/Data2.csv",sep=",",header = T)

# 第一步：识别缺失数据
which(is.na(Data))
which(is.nan(Data))
which(is.infinite(Data))

# 第二步：探究缺失数据
mean(is.na(Data))

Data_NA<-Data[,which(complete.cases(t(Data))==F)]
Data_NA<-abs(is.na(Data_NA))
cor(Data_NA)

# 第三步：处理缺失数据
allIndex<-which(rowSums(is.na(Data[,-1]))==4)
Data<-Data[-allIndex,]

Y<-names(Data[,-1])
H<-as.numeric(sub('H(\\d)m','\\1',Y))
D<-as.matrix(dist(H))
X<-Y[apply(D, 1, order)]

# Y[1]
mean(is.na(Data[,Y[1]]))
tempX<-X[2:4]

NArow<-which(is.na(Data[,Y[1]]))
NArow2<-which(is.na(Data[,tempX[1]]))
Fillrow<-NArow[!(NArow %in% NArow2)]
Data[Fillrow,Y[1]]<-Data[Fillrow,tempX[1]]


NArow<-which(is.na(Data[,Y[1]]))
NArow2<-which(is.na(Data[,tempX[2]]))
Fillrow<-NArow[!(NArow %in% NArow2)]
Data[Fillrow,Y[1]]<-Data[Fillrow,tempX[2]]


NArow<-which(is.na(Data[,Y[1]]))
NArow2<-which(is.na(Data[,tempX[3]]))
Fillrow<-NArow[!(NArow %in% NArow2)]
Data[Fillrow,Y[1]]<-Data[Fillrow,tempX[3]]

# Y[2]
mean(is.na(Data[,Y[2]]))
tempX<-X[6:8]

NArow<-which(is.na(Data[,Y[2]]))
NArow2<-which(is.na(Data[,tempX[1]]))
Fillrow<-NArow[!(NArow %in% NArow2)]
Data[Fillrow,Y[2]]<-Data[Fillrow,tempX[1]]


NArow<-which(is.na(Data[,Y[2]]))
NArow2<-which(is.na(Data[,tempX[2]]))
Fillrow<-NArow[!(NArow %in% NArow2)]
Data[Fillrow,Y[2]]<-Data[Fillrow,tempX[2]]


NArow<-which(is.na(Data[,Y[2]]))
NArow2<-which(is.na(Data[,tempX[3]]))
Fillrow<-NArow[!(NArow %in% NArow2)]
Data[Fillrow,Y[2]]<-Data[Fillrow,tempX[3]]


# Y[3]



# 根据以上的填充思路，写一个函数
Fill<-function(Data){
  
  return(newData)
}










