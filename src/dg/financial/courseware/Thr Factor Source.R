library(RODBC)
library(xts)

conn <- odbcConnectAccess2007(access.file="D:/Stk Data/Stock/Stock.accdb",
                              uid="test",pwd="test")

stk.list <- sqlQuery(channel=conn, "SELECT DISTINCT Stock.Stkcd 
                                    FROM Stock
                                    INNER JOIN Company
                                    ON Stock.Stkcd = Company.Stkcd
                                    WHERE Company.Listdt <= #1/1/2009#")[[1]]

thr.factor.all <- read.csv("D:/Stk Data/Tri Factors/THR_Factor.csv", header=TRUE)

thr.factor <- thr.factor.all[thr.factor.all$Excgflg == 0 & thr.factor.all$Mktflg == 'A', 
                             c("Date", "Rmrf", "Smb", "Hml")]

thr.factor.xts <- xts(thr.factor[, -1], order.by=as.Date(thr.factor[, 1]))
