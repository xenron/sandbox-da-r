num = 1:4
lower = c("a","b","c","d")
upper = c("A","B","C","D")
df = data.frame(num, lower, upper)
df

df = data.frame(num, lower, upper, 
	stringsAsFactors = FALSE)
df

row5 = c(5,"e","E")
rbind(df, row5)

word = c("One","Two","Three","Four")
cbind(df, word, stringsAsFactors = FALSE)

dat = read.csv("C:\\Data\\343452.csv")

head(dat)

nrow(df)
ncol(df)
nrow(dat)
ncol(dat)

dim(dat)

colnames(dat)

colnames(dat) = tolower(colnames(dat))
colnames(dat)

str(df)

df$num
df$lower
df$upper

dat$tpcp[dat$tpcp == -9999] = NA
dat$mmxt[dat$mmxt == -9999] = NA
dat$mmnt[dat$mmnt == -9999] = NA

dat$tpcp = dat$tpcp / 10
dat$mmxt = dat$mmxt/ 10
dat$mmnt = dat$mmnt / 10

df[2, 3]

df[2, ]
df[ ,3]

df[ ,3, drop = FALSE]

df[df$lower %in% c("a","d"), c("lower","upper")]

#dat[complete.cases(dat), ]

df
df$word[df$num == 2] = "Two"
df
  
dat$date = as.Date(as.character(dat$date), format = "%Y%m%d")
dat$month = as.numeric(format(dat$date, "%m"))
dat$year = as.numeric(format(dat$date, "%Y"))
head(dat)

#write.csv(df, "C:\\Data\\df.csv")

x = 3
if(x > 2) {print("x is large!")}

x = 0
if(x > 2) {print("x is large!")}

x = 3
if(x > 2) {print("x is large!")} else {print("x is small!")}

x = 1
if(x > 2) {print("x is large!")} else {print("x is small!")}

dat$mmxt[1:7]
ifelse(dat$mmxt[1:7] < 10, "cold", "warm")

x = c(-1,-8,2,5,-3,5,-9)
ifelse(x < 0, -x, x)

for(i in 1:5) {print(i)}

iris = read.csv("C:\\Data\\iris2.csv", stringsAsFactors = FALSE, row.names = 1)
iris

x = tapply(iris$Petal.Width, iris$Species, mean)
x

x["setosa"]

as.numeric(x)

any(is.na(dat[dat$station_name == "IZANA SP", "tpcp"]))

result = tapply(
	dat$tpcp,
	dat$station_name,
	function(x) any(is.na(x)))

result[1:10]

sum(result)

result[result]

names(result[result])

apply(iris[, 1:4], 1, mean)

apply(iris[, 1:4], 2, mean)

iris[3,2] = NA
iris
apply(iris[, 1:4], 2, mean)
apply(iris[, 1:4], 2, mean, na.rm = TRUE)

#library("lattice")

#install.packages("reshape2")

library(reshape2)
iris_melt = melt(iris, id.vars = "Species")

head(iris_melt)

dat_melt = melt(dat, measure.vars = c("tpcp","mmxt","mmnt"))
head(dat_melt)

dat2 = dcast(dat_melt, ... ~ variable)

head(dat2)

dat2 = dcast(dat_melt, station+station_name+variable+year~month)
head(dat2)

dat2 = dcast(dat_melt, year ~ variable, mean, na.rm = TRUE)
head(dat2)

library(plyr)
ddply(iris,
	.(Species),
	summarize,
	sepal_area = mean(Sepal.Length * Sepal.Width),
	petal_area = mean(Petal.Length * Petal.Width))

ddply(iris,
	.(Species),
	transform,
	sepal_area = mean(Sepal.Length * Sepal.Width),
	petal_area = mean(Petal.Length * Petal.Width))

dat3 = ddply(dat_melt,
	.(station, year, variable),
	transform,
	months_available = length(value[!is.na(value)]))
head(dat3)

nrow(dat3)
dat3 = dat3[dat3$months_available == 12, ]
nrow(dat3)

spain_stations = ddply(dat3,
	.(station),
	summarize,
	latitude = latitude[1],
	longitude = longitude[1],
	elevation = elevation[1])
head(spain_stations)

nrow(spain_stations)

write.csv(spain_stations, "C:\\Data\\spain_stations.csv",
	row.names = FALSE)

spain_annual = ddply(dat3,
	.(station, variable, year),
	summarize,
	value = ifelse(variable[1] == "tpcp",
		sum(value, na.rm = TRUE),
		mean(value, na.rm = TRUE)))
head(spain_annual)

write.csv(spain_annual, "C:\\Data\\spain_annual.csv",
	row.names = FALSE)

dates = read.csv("C:\\Data\\modis_dates.csv")
head(dates)

dates$date = as.Date(
	paste(dates$year, dates$month, dates$day, sep = "-"))
head(dates)

month = c(12, 1:11)
month
season = rep(c("winter","spring","summer","fall"), each = 3)
season
seasons = data.frame(month, season)
seasons

dates = join(dates, seasons, "month")
head(dates)

combined = join(spain_stations, 
	spain_annual, 
	by = "station", 
	type = "right")

head(combined)



