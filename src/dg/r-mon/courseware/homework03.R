#1
three_d6 <- function(n)
{
  random_numbers <- matrix(
    sample(6, 3 * n, replace = TRUE),
    nrow = 3
  )
  colSums(random_numbers)
}

scores <- three_d6(1000)
bonuses <- cut(
  scores,
  c(2, 3, 5, 8, 12, 15, 17, 18),
  labels = -3:3
)
table(bonuses)

#2
zodiac_sign <- function(x)
{
  month_x <- month(x, label = TRUE)
  day_x <- day(x)
  switch(
    month_x,
    Jan = if(day_x < 20) "Capricorn" else "Aquarius",
    Feb = if(day_x < 19) "Aquarius" else "Pisces",
    Mar = if(day_x < 21) "Pisces" else "Aries",
    Apr = if(day_x < 20) "Aries" else "Taurus",
    May = if(day_x < 21) "Taurus" else "Gemini",
    Jun = if(day_x < 21) "Gemini" else "Cancer",
    Jul = if(day_x < 23) "Cancer" else "Leo",
    Aug = if(day_x < 23) "Leo" else "Virgo",
    Sep = if(day_x < 23) "Virgo" else "Libra",
    Oct = if(day_x < 23) "Libra" else "Scorpio",
    Nov = if(day_x < 22) "Scorpio" else "Sagittarius",
    Dec = if(day_x < 22) "Sagittarius" else "Capricorn"
  )
}
# 可如下使用
nicolaus_copernicus_birth_date <- as.Date("1473-02-19")
zodiac_sign(nicolaus_copernicus_birth_date)

#3
A=matrix(c(1,1,3,5,2,6,-2,-1,-3),nrow=3,byrow=T)
A
#(1)
A%*%A%*%A
#(2)
A[,3]=A[,2]+A[,3]
A

#4
tmp <- matrix(c(10,-10,10), b=T, nc=3, nr=15)
t(tmp)%*%tmp

tmp <- matrix(c(rep(c(10,-10),7),10), b=T, nc=15, nr=15)
t(tmp)%*%tmp

#5
#(a)
outer(0:4,0:4,"+")%%5
matrix(0:4+rep(0:4,times=rep(5,5)),nc=5)%%5
#(b)
outer(0:9,0:9,"+")%%10
matrix(0:9+rep(0:9,times=rep(10,10)),nc=10)%%10
#(c)
outer(0:8,0:8,"-")%%9
matrix(0:8-rep(0:8,times=rep(9,9)),nc=9)%%9

#6
#(a)
sum( (1:20)^4 ) * sum( 1/(4:8) )
sum(outer((1:20)^4,4:8,"/"))
#使用循环写矩阵
x=matrix(nrow=20,ncol=5)
for(i in 1:20)
{
  x[i,]=i^4/(3+(1:5))
}
sum(x)
#(b)
sum( (1:20)^4 / (3 + outer(1:20,1:5,"*")))

x=matrix(nrow=20,ncol=5)
for(i in 1:20)
{
  x[i,]=i^4/(3+i*(1:5))
}
sum(x)

#7
queue <- c("Steve", "Russell", "Alison", "Liam")
queue[length(queue)+1]="Barry"
queue
queue[-which(queue=="Steve")]
queue
queue[-which(queue=="Barry")]
queue
queue[-which(queue=="Alison")]
queue

#8
x <- 0:99
sqrt_x <- sqrt(x)
is_square_number <- sqrt_x == floor(sqrt_x)
square_numbers <- x[is_square_number]
groups <- cut(square_numbers,
              seq.int(min(x), max(x), 10),
              include.lowest = TRUE,
              right = FALSE)
split(square_numbers, groups)

#9
beaver1$id <- 1
beaver2$id <- 2
both_beavers <- rbind(beaver1, beaver2)
subset(both_beavers, as.logical(activ))

#10

myListFn <- function(n)
{
  xVec <- rnorm(n)
  xBar <- mean(xVec)
  yVec <- sign(xBar)*rexp(n, rate=abs(1/xBar))
  count <- sum( abs(yVec) > abs(xVec) )
  list(xVec=xVec, yVec=yVec, count=count)
}

myList <- lapply( rep(10,4), myListFn )
myMatrix <- sapply( rep(10,4), myListFn )