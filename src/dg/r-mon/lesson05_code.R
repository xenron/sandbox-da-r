#####---lesson 5---#####

###-conditional statement
##-if-else
if(TRUE) message("It was true!")
if(runif(1) > 0.5) message("This message appears with a 50% chance.")

x <- 3
if(x > 2)
{
  y <- 2 * x
  z <- 3 * y
}
y
z

r=5
if(r==4){
  x<-1
}else{
  x<-3
  y<-4
}
x
y

#this is wrong:
r=5
if(r==4){
  x<-1
}
else{
  x<-3
  y<-4
}

#赋值
x<-2
y<-if(x==2) x else x+1
y

##-ifelse
if(c(TRUE, FALSE)) message("two choices")
ifelse(rbinom(10, 1, 0.5), "Head", "Tail")

(yn <- rep.int(c(TRUE, FALSE), 6))
ifelse(yn, 1:3, -1:-12)
yn[c(3, 6, 9, 12)] <- NA
ifelse(yn, 1:3, -1:-12)

##-switch
#greek
(greek <- switch(
  "gamma",
  alpha = 1,
  beta = sqrt(4),
  gamma =
{
  a <- sin(pi / 3)
  4 * a ^ 2
}
))

#greek2
(greek <- switch(
  "delta",
  alpha = 1,
  beta = sqrt(4),
  gamma =
{
  a <- sin(pi / 3)
  4 * a ^ 2
},
4
))

#numbers
switch(
  3,
  "first","second","third","fourth")

#big numbers
switch(
  as.character(2147483647),
  "2147483647" = "a big number",
  "another number"
)

#feelings
feelings<-c("happy","angry")
for(i in feelings)
  print(
    switch(i,
           happy="I am glad you are happy",
           afraid="There is nothing to fear",
           sad="Cheer up!",
           angry="Calm down now"
           )
    )

#dice game
two_d6 <- function(n)
{
  random_numbers <- matrix(
    sample(6, 2 * n, replace = TRUE),
    nrow = 2
  )
  colSums(random_numbers)
}
score <- two_d6(1)
if(score %in% c(2, 3, 12))
{
  game_status <- FALSE
  point <- NA
} else if(score %in% c(7, 11))
{
  game_status <- TRUE
  point <- NA
} else
{
  game_status <- NA
  point <- score
}


###-loop statement
##-repeat
#repeat{message("Happy Groundhog Day!")}

#sample-break
repeat
{
  message("Happy Groundhog Day!")
  action <- sample(
    c(
      "Learn French",
      "Make an ice statue",
      "Rob a bank",
      "Win heart of Andie McDowell"
    ),
    1
  )
  message("action = ", action)
  if(action == "Win heart of Andie McDowell") break
}

#sample-next
repeat
{
  message("Happy Groundhog Day!")
  action <- sample(
    c(
      "Learn French",
      "Make an ice statue",
      "Rob a bank",
      "Win heart of Andie McDowell"
    ),
    1
  )
  if(action == "Rob a bank")
  {
    message("Quietly skipping to the next iteration")
    next
  }
  message("action = ", action)
  if(action == "Win heart of Andie McDowell") break
}


##-while
#sample-while
action <- sample(
  c(
    "Learn French",
    "Make an ice statue",
    "Rob a bank",
    "Win heart of Andie McDowell"
  ),
  1
)
while(action != "Win heart of Andie McDowell")
{
  message("Happy Groundhog Day!")
  action <- sample(
    c(
      "Learn French",
      "Make an ice statue",
      "Rob a bank",
      "Win heart of Andie McDowell"
    ),
    1
  )
  message("action = ", action)
}

#single one
i<-10
while(i>0){
  print("Hello");
  i<-i-1
}

##-for
#integer
for(i in 1:5) message("i=",i)
#integer2
for(i in 1:5)
{
  j <- i ^ 2
  message("j = ", j)
}

#more1
for(month in month.name)
{
  message("The month of ", month)
}

#more2
for(yn in c(TRUE, FALSE, NA))
{
  message("This statement is ", yn)
}

#more3
l <- list(
  pi,
  LETTERS[1:5],
  charToRaw("not as complicated as it looks"),
  list(
    TRUE
  )
)
for(i in l)
{
  print(i)
}

#norm
norm<-rnorm(100,1,1)
min.norm<-100
for(i in 1:100){
  if(norm[i]<=min.norm){
    min.norm=norm[i]
    min.count=i
  }
}
min.norm
min.count

#gambling
craps <- function() {
  #returns TRUE if you win, FALSE otherwise
  initial.roll <- sum(sample(1:6,2,replace=T))
  if (initial.roll == 7 || initial.roll == 11) return(TRUE)
  while (TRUE) {
    current.roll <- sum(sample(1:6,2,replace=T))
    if (current.roll == 7 || current.roll == 11) {
      return(FALSE)
    } else if (current.roll == initial.roll) {
      return(TRUE)
    }
  }
}
mean(replicate(10000, craps()))

##lapply

x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
lapply(x, mean)
lapply(x, quantile, probs = 1:3/4)

##-get
u<-matrix(c(1,2,3,1,2,4),nrow=3);u
v<-matrix(c(8,12,20,15,10,2),nrow=3);v
for(m in c("u","v")) {
  z<-get(m)
  print(lm(z[,2]~z[,1]))
}

###-environment
an_environment<-new.env()
an_environment[["pythag"]]<-c(12, 15, 20, 21) # 请查看http://oeis.org/A156683
an_environment$root<-polyroot(c(6, -5, 1))

assign(
  "moonday",
  weekdays(as.Date("1969/07/20")),
  an_environment
)
get("moonday", an_environment)
an_environment[["pythag"]]
an_environment$root

ls(envir = an_environment)
ls.str(envir = an_environment)

exists("pythag", an_environment)

#list
(a_list<-as.list(an_environment))

as.environment(a_list)  #再转换回来，以下两行代码效果相同
list2env(a_list)

nested_environment<-new.env(parent=an_environment)
exists("pythag", nested_environment)
exists("pythag", nested_environment, inherits=FALSE)

non_stormers<<-c(3, 7, 8, 13, 17, 18, 21) # 参见 http://oeis.org/A002312
get("non_stormers", envir=globalenv())
head(ls(envir=baseenv()), 20)

###-functions
##-structure
rt
#mystats()
mystats<-function(x,parametric=TRUE, print=FALSE){
  if(parametric){
    center<-mean(x);spread<-sd(x)
  }else{
    center<-median(x);spread<-mad(x)
  }
  if(print & parametric){
    cat("Mean=",center, "\n","SD=",spread,"\n")
  }else if(print& !parametric){
    cat("Median=",center, "\n","MAD=",spread,"\n")
  }
  result<-list(center=center, spread=spread)
  return(result)
}
set.seed(1234)
x<-rnorm(500)
y<-mystats(x)
y$center;y$spread

y<-mystats(x,parametric=FALSE,print=TRUE)

#hypotenuse
hypotenuse <- function(x, y)
{
  sqrt(x ^ 2 + y ^ 2)
}

hypotenuse<- function(x, y) sqrt(x ^ 2 + y ^ 2)
hypotenuse(3, 4)
hypotenuse(y = 24, x = 7)

hypotenuse <- function(x = 5, y = 12)
{
  sqrt(x ^ 2 + y ^ 2)
}
hypotenuse()    # 与hypotenuse(5, 12) 相等

#formals(),args(),formalArgs()
formals(hypotenuse)
args(hypotenuse)
formalArgs(hypotenuse)

#body
(body_of_hypotenuse <- body(hypotenuse))
deparse(body_of_hypotenuse)

#normalize
normalize <- function(x, m = mean(x), s = sd(x))
{
  (x - m) / s
}
normalized <- normalize(c(1, 3, 6, 10, 15))
mean(normalized) # 几乎是0
sd(normalized)

normalize(c(1, 3, 6, 10, NA))

#normalize-2
normalize <- function(x, m = mean(x, na.rm=na.rm),
                s=sd(x, na.rm=na.rm), na.rm=FALSE){
  (x - m) / s
}
normalize(c(1, 3, 6, 10, NA))
normalize(c(1, 3, 6, 10, NA), na.rm=TRUE)

#normalize-3
normalize<-function(x, m=mean(x, ...), s=sd(x, ...), ...)
{
  (x-m)/s
}
normalize(c(1, 3, 6, 10, NA))
normalize(c(1, 3, 6, 10, NA), na.rm=TRUE)


###-replication
rep(runif(1),5)
replicate(5,runif(1))

#traffic
time_for_commute <- function()
{
  # 选择当时所用的交通工具
  mode_of_transport <- sample(
    c("car", "bus", "train", "bike"),
    size = 1,
    prob = c(0.1, 0.2, 0.3, 0.4)
  )
  # 根据交通工具，找到出行的时间
  time <- switch(
    mode_of_transport,
    car = rlnorm(1, log(30), 0.5),
    bus = rlnorm(1, log(40), 0.5),
    train = rnorm(1, 30, 10),
    bike = rnorm(1, 60, 5)
  )
  names(time) <- mode_of_transport
  time
}
replicate(5,time_for_commute())