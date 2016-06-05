# Loading Packages -----------

library(RODBC)
library(xts)
library(tseries)

# Read Market Return -------------

mkt.all.data <- read.table("D:/Stk Data/Index/TRD_Index.txt",header=TRUE)
head(mkt.all.data)

ret.mkt <- mkt.all.data[mkt.all.data$Indexcd == 902, c("Trddt","Retindex")]
ret.mkt.xts <- xts(ret.mkt$Retindex, order.by=as.Date(ret.mkt$Trddt))
names(ret.mkt.xts) <- "ret.mkt"
head(ret.mkt.xts)

# Set Risk Free Rate ---------------
rf <- (1.0325)^(1/360) - 1

# Connect with Access File -------------

conn <- odbcConnectAccess2007(access.file="D:/Stk Data/Stock/Stock.accdb",
                              uid="test",pwd="test")


