#########################################
# White Noise
#########################################

set.seed(9571)
white_noise<-rnorm(100,mean=0,sd=3.0)

library(ggplot2)
p <- ggplot(data=as.data.frame(white_noise), aes(x=1:length(white_noise), y=white_noise))
p <- p + geom_line() 
p <- p + ggtitle("Gaussian White Noise")
p <- p + theme(plot.title = element_text(lineheight=.8, face="bold", vjust=2))
p <- p + xlab("Time")  
p <- p + ylab("") 
p

white_noise_acf <- acf(white_noise)
plot(white_noise_acf, main="Gaussian White Noise ACF Function")

#########################################
# Random Walk
#########################################

set.seed(874234242)
random_walk<-cumsum(rnorm(100,mean=0,sd=3.0))

p <- ggplot(data=as.data.frame(random_walk), aes(x=1:length(random_walk), y=random_walk))
p <- p + geom_line() 
p <- p + ggtitle("Random Walk")
p <- p + theme(plot.title = element_text(lineheight=.8, face="bold", vjust=2))
p <- p + xlab("Time")  
p <- p + ylab("") 
p

random_walk_acf<-acf(diff(random_walk))
plot(random_walk_acf, main="Random Walk ACF Function")

#########################################
# MA Processes
#########################################

set.seed(2357977)
ma_ts1<-arima.sim( model=list(ma = c(0.84, 0.62), sd = 1.2), n=1000)

set.seed(2357977)
ma_ts2<-arima.sim( model=list(ma = c(0.84, -0.62), sd = 1.2), n=1000)

par(mfrow = c(2, 1))
acf(ma_ts1, main=expression(bold(paste("ACF of an MA(2) process with ",theta[1]," = 0.84 ", theta[2], " = 0.62", sep=""))))
acf(ma_ts2, main=expression(bold(paste("ACF of an MA(2) process with ",theta[1]," = 0.84 ", theta[2], " = -0.62", sep=""))))

#########################################
# AR Processes
#########################################

set.seed(634090)
ma_ts3<-arima.sim( model=list(ar = c(0.74), sd = 1.2), n=1000)
acf(ma_ts3, main=expression(bold(paste("ACF of an AR(1) process with ",phi[1]," = 0.74", sep=""))))
pacf(ma_ts3, main=expression(bold(paste("PACF of an AR(1) process with ",phi[1]," = 0.74", sep=""))))

#########################################
# Unit Root Tests
#########################################

library(tseries)
adf.test(random_walk,alternative = "stationary")

#########################################
# Earthquakes
#########################################

library("RCurl")
seismic_raw <- read.table(textConnection(getURL("http://www.geophysics.geol.uoa.gr/catalog/catgr_20002008.epi")), sep="", header=F)
names(seismic_raw)<-c("date","mo","day","hr","mn","sec","lat","long","depth","mw")
head(seismic_raw, n=5)

library("plyr")
seismic<-count(seismic_raw,c("date", "mo"))
seismic_ts<-ts(seismic$freq, start=c(2000, 1), end=c(2008, 1), frequency=12)

p <- ggplot(data=as.data.frame(random_walk), aes(x=1:length(random_walk), y=random_walk))
p <- p + geom_line() 
p <- p + ggtitle("Random Walk")
p <- p + theme(plot.title = element_text(lineheight=.8, face="bold", vjust=2))
p <- p + xlab("Time")  
p <- p + ylab("") 
p

plot(seismic_ts, main="Eathquakes in Greece in the Period 2000-2008\n (Magnitude>4.0)", ylab="Number of Earthquakes")

d<- 0:2
p<- 0:6
q<- 0:6
seismic_models<-expand.grid(d=d, p=p, q=q)

getTSModelAIC <- function(ts_data, p, d, q) {
  ts_model<-arima(ts_data, order = c(p, d, q))
  return(ts_model$aic)
}

getTSModelAICSafe <-function(ts_data, p, d, q) {
  result = tryCatch({
    getTSModelAIC(ts_data, p, d, q)
  }, error = function(e) {
    Inf
  })              
}

seismic_models$aic<-mapply(function(x,y,z) getTSModelAICSafe(seismic_ts,x,y,z), seismic_models$p, seismic_models$d, seismic_models$q)
subset(seismic_models,aic==min(aic))

library("forecast")
seismic_model<-arima(seismic_ts, order = c(1, 1, 1))
plot(forecast(seismic_model,10))

#########################################
# Trapped Lynx
#########################################

d <- 0:2
p <- 0:6
q <- 0:6
lynx_models <- expand.grid(p = p, d = d, q = q)
lynx_models$aic <-mapply(function(x,y,z) 
  getTSModelAICSafe(lynx, x, y, z), lynx_models$p, lynx_models$d, 
  lynx_models$q)
subset(lynx_models,aic == min(aic))
lynx_model <- arima(lynx, order = c(4, 2, 5))
plot(forecast(lynx_model, 10))

#########################################
# Forex
#########################################

eurofxref.hist <- read.csv("eurofxref-hist.csv", stringsAsFactors=F)
selector<-(eurofxref.hist$Date <= as.Date('2013-12-31')) & (eurofxref.hist$Date >= as.Date('2005-01-01'))
eurofxref.hist <- eurofxref.hist[selector,]
euro_usd <- ts(rev(eurofxref.hist$USD))

library("fGarch")
euro_usd_garch <- garchFit(~ 1 + garch(1, 1), data  = euro_usd, trace = FALSE)
predict(euro_usd_garch, n.ahead = 20, plot = TRUE)
