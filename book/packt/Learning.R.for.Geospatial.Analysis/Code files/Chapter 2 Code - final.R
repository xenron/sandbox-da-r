10

print(10)

c(1,5,10,4)
c("cat","dog","mouse","apple")

7:20

33:24

c(TRUE,FALSE,TRUE,TRUE)

33:24
as.character(33:24)

as.numeric(as.character(33:24))

factor(c("cat","dog","dog"))
factor(c("cat","dog","dog","mouse"))

v = 1:10

v

v <- 1:10

one = 1
two = 2
one = two
one
two

one = 1
two = 2
one == two

x = 55
x
x = "Hello"
x

ls()

rm("v")
ls()

rm(list = ls())
ls()

v = 1:10
mean(v)
min(v)
max(v)

range(v)

v = c(4,2,3,9,1)
length(v)

l = c(TRUE, FALSE, FALSE, TRUE)
any(l)
all(l)

l = c(TRUE, FALSE, FALSE, TRUE)
sum(l)

which(l)

which.min(v)
which.max(v)

v = c(5,6,2,2,3,0,-1,2,5,6)
unique(v)

1:10 * 2
1:10 - 10
sqrt(c(4,16,64))

x = 1:5
x
x >= 3

1 %in% 1:10
11 %in% 1:10

any(1:10 == 1)
any(1:10 == 11)

paste("There are", "5", "books.")

paste("Image", 1:5)

x = 80
paste("There are", x, "books.")

paste(1, 2, 3, sep = "")
paste0(1, 2, 3)

c(1,2,3) * c(10,20,30)

1:4 * 3
1:4 * c(3,3,3,3)

c(1,2,3,4) * c(3,5)

1:5 * c(1,10,100)

seq(from = 100, to = 150, by = 10)
seq(from = 190, to = 150, by = -10)

seq(from = 5, to = 10, by = 1)
seq(5, 10, 1)

mean(1:10)
mean(x = 1:10)

seq(to = 10, by = 1, from = 5)

seq(5, 10, 1)
seq(5, 10)

vector(mode = "numeric", length = 2)
vector(mode = "character", length = 10)
vector(mode = "logical", length = 3)

rep(x = 22, times = 10)

x = c(18, 0, 9)
rep(x, 3)

x = "Subsetting strings"
substr(x, start = 1, stop = 14)
substr(x, 6, 14)
substr(x, 1, 3)

x = c(5,6,1,2,3,7)
x[3]
x[1]
x[6]

x[length(x)]

x = 1:3
x
x[2] = 300
x

x = c(43,85,10)
x[1:2]
x[c(3,1)]

x = 33:24
x
x[length(x):1]

x = c(43,85,10)
x[rep(3,4)]

x = 1:10
x[3:8] = c(15,16)
x

x = seq(85, 100, 2)
x
x > 90
x[x > 90]

x
x[x>85 & x<90]
x[x>92 | x<86]

x>90 & x<90
x[x>90 & x<90]

x = 1:10
x[100]

x = c(2,5,1,0)
mean(x)
x[2] = NA
x
mean(x)

x = c(2,5,1,0)
x[2] = NA
x
is.na(x)

!is.na(x)

x[!is.na(x)]

mean(x[!is.na(x)])

x = c(3,8,2,NA,1,7,5,NA,9)
mean(x)
mean(x, na.rm = TRUE)
max(x)
max(x, na.rm = TRUE)

add_five = function(x) {
	x_plus_five = x + 5
	return(x_plus_five)
	}

add_five(5)
add_five(7)

#x_plus_five

result = add_five(3)
result

add_five = function(x) x + 5

add_five = function(x) x + 5
#add_five()
add_five = function(x = 1) x + 5
add_five()

vector()

#add_five("one")

dat = read.csv("C:\\Data\\338284.csv", stringsAsFactors = FALSE)
time = dat$DATE
tmax = dat$TMAX

x = Sys.Date()
x
class(x)
y = Sys.time()
y
class(y)

x + 7
x - 1000

x = as.character(x)
x
class(x)

x = as.Date(x)
x
class(x)

seq(from = as.Date("2013-01-01"), 
	to = as.Date("2013-02-01"), 
	by = 3) 

#as.Date("07/Aug/12")
as.Date("07/Aug/12", format = "%d/%b/%y")
#as.Date("2012-August-07")
as.Date("2012-August-07", format = "%Y-%B-%d")

d = as.Date("1955-11-30")
d
format(d, "%d")
format(d, "%B")
format(d, "%Y")
format(d, "%m/%Y")

class(time)
class(tmax)

time[1:10]
tmax[1:10]

time = as.Date(as.character(time), format = "%Y%m%d")
time[1:10]
class(time)

tmax[tmax == -9999] = NA

tmax = tmax / 10
tmax[1:10]

range(time)
range(tmax, na.rm = TRUE)

range_t = range(time)
all_dates = seq(range_t[1], range_t[length(range_t)], 1)
length(all_dates)
length(time)

all(all_dates %in% time)

which(!(all_dates %in% time))

all_dates[which(!(all_dates %in% time))]

max(tmax, na.rm = TRUE)
time[which.max(tmax)]

w = time > as.Date("2005-12-31") & time < as.Date("2014-1-1")

sum(w) / length(w)

time = time[w]
tmax = tmax[w]

range(time)

################
# Figure 02_02 #
################
png("C:\\Data\\4367OS_02_02.png",width=5.5,height=5.5,units="in",res=300)
plot(tmax)
dev.off()

################
# Figure 02_03 #
################
png("C:\\Data\\4367OS_02_03.png",width=5.5,height=4,units="in",res=300)
plot(time, tmax, type = "l")
dev.off()

plot(tmax ~ time, type = "l")

pdf("C:\\Data\\time_series.pdf")
plot(tmax ~ time, type = "l")
dev.off()

dat = data.frame(time = time, tmax = tmax)

# Base graphics

#################
# Figure 02_04a #
#################
png("C:\\Data\\4367OS_02_04a.png",width=5.5,height=5.5,units="in",res=300)
plot(tmax ~ time, dat, type = "l")
dev.off()

# lattice
library(lattice)

#################
# Figure 02_04b #
#################
png("C:\\Data\\4367OS_02_04b.png",width=5.5,height=5.5,units="in",res=300)
xyplot(tmax ~ time, data = dat, type = "l")
dev.off()

# ggplot2
library(ggplot2)

#################
# Figure 02_04c #
#################
png("C:\\Data\\4367OS_02_04c.png",width=5.5,height=5.5,units="in",res=300)
ggplot(dat, aes(x = time, y = tmax)) + 
	geom_line()
dev.off()