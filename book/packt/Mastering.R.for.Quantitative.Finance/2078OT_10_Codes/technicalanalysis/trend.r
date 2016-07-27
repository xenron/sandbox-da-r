
OHLC <- function(d){
k = nrow(d)
windows(width=30, height=15)
par(bg = rgb(.9,.9,.9)); 
plot(x=NULL, y = NULL, xlim=c(1,k+1), ylim=c(min(d),max(d)), xlab="", ylab="", xaxt = "n")
abline(h = axTicks(2), v=axTicks(1), col = rgb(.5,.5,.5), lty=3); 
axis(1, at = axTicks(1), las = 1, cex.axis = 0.6)
for(i in 1:k){polygon(c(i,i+1,i+1,i), c(d[i,1], d[i,1], d[i,4], d[i,4]), col = ifelse(d[i,1]<=d[i,4], rgb(0,0.8,0), rgb(0.8,0,0)))
lines(c(i+1/2, i+1/2), c(d[i,2], max(d[i,1], d[i,4])));lines(c(i+1/2, i+1/2), c(d[i,3], min(d[i,1], d[i,4])))}
}


bitcoin <- read.table("D:\\Bitcoin.csv", header = T, sep = ";")
n <- nrow(bitcoin)


#trend keresés:

is.trend <- function(ohlc,i,j){
avg1 = mean(ohlc[(i-25):i,4])
avg2 = mean(ohlc[(j-25):j,4])
if(avg1 >= avg2) return(FALSE)
ohlc <- ohlc[i:j,]
n <- nrow(ohlc)
candle_l <- pmin(ohlc[,1],ohlc[,4])
valley <- rep(FALSE, n)
for(i in 2:(n-1)) valley[i] <- ( (candle_l[i-1] >= candle_l[i]) & (candle_l[i+1] >= candle_l[i]) )
z = candle_l[valley]

if(all(z == cummax(z))) return(TRUE)
return(FALSE)
}


a = 26
while( a <= n-8 ){ 
b = a+2
while(is.trend(bitcoin, a,b)) b = b+1
b = b-1
if ( b-a > 8) {
print(c(a,b))
OHLC(bitcoin[(a):(b+20),]); title(main = paste(a,b))
}
if(b==n) break
a = b
}







