---
title: "FINAL PROJECT 570"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```



```{r Begin program}
library(zoo)
library(xts)
library(highfrequency)
library(TTR)
library(timeDate)
library(quantmod)
library(InfoTrad)
```
The code consists of three things 

LIQUIDITY MEASURE

GLOSTEN - MILGROM MODEL

PROBABILITY OF INFORMED TRADING MODEL
```{r , echo=FALSE}
load("~/Desktop/MICROSTRUCTURE_CODES/taqdata_KO_20220301.RData")
# read in data in GMT time zone
Sys.setenv(TZ = "EST")  # work in East Coast Time Zone
options(digits.secs=3)

volume_day<-sum(as.numeric(tqdata$SIZE))
volume_day
time_required<- "2022-03-01 14:30:01.254::2022-03-01 20:59:59.903"
tqdata<-tqdata[time_required]
head(tqdata)
tail(tqdata)
asks <- as.numeric(tqdata$OFR)
bids <- as.numeric(tqdata$BID)
mids <- 0.5*bids + 0.5*asks
pr<-as.numeric(tqdata$PRICE)
dpr <- diff(pr)
pmin = min(as.numeric(tqdata$PRICE))
pmax = max(as.numeric(tqdata$PRICE))
plot(as.numeric(tqdata$PRICE),col="red", type="l", ylab="Trade price", 
     xlab="Trade #", main="Trade price (2:30-9:00)", ylim=c(pmin-0.1,pmax+0.1))
lines(mids, type="l", col="blue")

plot(as.numeric(tqdata$SIZE),col="red", type="l", 
     ylab="Trade size", 
     xlab="Trade #", main="Trade volume", ylim=c(0,10000))


td <- getTradeDirection(tqdata)
td[1]=1
plot(td[0:50],main="trade signs (XXX)",type="b",col="blue")

td_ac <- acf(td,main='NA',type="correlation",na.action = na.fail, lag.max=20,plot=FALSE)

plot(td_ac,main="trade signs autocorrelation (XXX)", col="red")

# improved Roll model estimation, relaxing the assumption of independence of trades

deps <- diff(td)
mids <- (as.numeric(tqdata$OFR) + as.numeric(tqdata$BID))/2

dm <- diff(mids)
(fit.lm <- lm(dpr ~ dm + deps))
roll_spread<-as.numeric(fit.lm$coeff[3])#spread by improved roll model


ten.mins <- '2022-03-01 15:00:00::2022-03-01 15:10:00'

tqdata.10mins <- tqdata[ten.mins]
plot(as.numeric(tqdata.10mins$PRICE), 
     type="p", col="black", main="15:00-15:10am", 
     ylab="price", xlab="trading time")
lines(as.numeric(tqdata.10mins$OFR), col="red")
lines(as.numeric(tqdata.10mins$BID), col="blue")

liq.m <- getLiquidityMeasures(tqdata)
average.es <- mean(na.omit(as.numeric(liq.m$effectiveSpread)))
rspread <- mean(na.omit(as.numeric(liq.m$realizedSpread)))

name<-c("Roll model spread","EffectiveSpread","RealizedSpread")
spread<-data.frame(roll_spread,average.es,rspread)
names(spread)<-name

spread

calculate_vol <- function(x, start_date, end_date) {

   data <- get.hist.quote(x,start_date, end_date, quote = "Close")
   price <- data$Close
   ret <- log(lag(price)) - log(price)
   ret[is.na(ret)]<-0
   vol <- sd(ret) * sqrt(252) * 100
   return(vol)
}
sum=0
for(i in 1:length(tqdata$PRICE)-1)
{
  ret[i] <- log((as.numeric(tqdata$PRICE[i+1])))-log((as.numeric(tqdata$PRICE[i])))
}
sd(ret)*100
```


GLOSTEN - MILGROM MODEL

Delta - The Probability of v=V_down

1-Delta- The Probability of V=V_up

mu- Percentage of informed traders 

1-mu- Percentage of uninformed traders 

V_up- security value goes up 

V_down- security value goes down

dt- Trade directions 

pr- Transaction prices

```{r}

numsims <- 3
numsteps <- 200
v.max <- max(pr)
v.min <- min(pr)
mu <- 0.2
delta <- 0.7

asks <- numeric(numsteps+1)
bids <- numeric(numsteps+1)


for (i in 1:numsims) {
  informeds <- c(0, rbinom(numsteps, 1, mu))
  buys <- c(0, rbinom(numsteps, 1,1/2))
  # assume v = v.max; hence informeds only buy
  buys[informeds == 1] <- 1
  n.buys <- cumsum(buys)
  n.sells <- 0:numsteps - n.buys
  askz <- (v.min*delta*(1-mu)**(n.buys+1)*(1+mu)**n.sells +
           v.max*(1-delta)*(1+mu)**(n.buys+1)*(1-mu)**n.sells)/
             (delta*(1-mu)**(n.buys+1)*(1+mu)**n.sells +
              (1-delta)*(1+mu)**(n.buys+1)*(1-mu)**n.sells)
  bidz <- (v.min*delta*(1-mu)**n.buys*(1+mu)**(n.sells+1) +
           v.max*(1-delta)*(1+mu)**n.buys*(1-mu)**(n.sells+1))/
             (delta*(1-mu)**n.buys*(1+mu)**(n.sells+1) +
              (1-delta)*(1+mu)**n.buys*(1-mu)**(n.sells+1))
  asks <- asks + askz
  bids <- bids + bidz


}
asks <- asks/numsims
bids <- bids/numsims
spread <- asks - bids

prices <- numeric(numsteps+1)

for (i in 1:numsteps) {
if(buys[i]==1) prices[i]=asks[i] 
else prices[i]=bids[i] 
}

#asks

#bids

#prices

plot(0:numsteps, asks, xlim=c(0, numsteps), ylim=c(v.min, v.max),
     xlab="Time", ylab="Price", type='l', col="red", main="Glosten-Milgrom")
lines(0:numsteps, bids, col="blue")
points(0:numsteps, prices)
# only print this for a single-run plot

 points(1:numsteps-0.5, rep(61.6, numsteps),        pch=c("S", "B")[buys[2:(numsteps+1)]+1])
 
 numsims <- 3
numsteps <- 200
v.max <- max(pr)
v.min <- min(pr)
mu <- 0.01
delta <- 0.5

asks <- numeric(numsteps+1)
bids <- numeric(numsteps+1)


for (i in 1:numsims) {
  informeds <- c(0, rbinom(numsteps, 1, mu))
  buys <- c(0, rbinom(numsteps, 1,1/2))
  # assume v = v.max; hence informed traders only buy
  buys[informeds == 1] <- 1
  n.buys <- cumsum(buys)
  n.sells <- 0:numsteps - n.buys
  askz <- (v.min*delta*(1-mu)**(n.buys+1)*(1+mu)**n.sells +
           v.max*(1-delta)*(1+mu)**(n.buys+1)*(1-mu)**n.sells)/
             (delta*(1-mu)**(n.buys+1)*(1+mu)**n.sells +
              (1-delta)*(1+mu)**(n.buys+1)*(1-mu)**n.sells)
  bidz <- (v.min*delta*(1-mu)**n.buys*(1+mu)**(n.sells+1) +
           v.max*(1-delta)*(1+mu)**n.buys*(1-mu)**(n.sells+1))/
             (delta*(1-mu)**n.buys*(1+mu)**(n.sells+1) +
              (1-delta)*(1+mu)**n.buys*(1-mu)**(n.sells+1))
  asks <- asks + askz
  bids <- bids + bidz


}
asks <- asks/numsims
bids <- bids/numsims
spread <- asks - bids

prices <- numeric(numsteps+1)

for (i in 1:numsteps) {
if(buys[i]==1) prices[i]=asks[i] 
else prices[i]=bids[i] 
}

#asks

#bids

#prices

plot(0:numsteps, asks, xlim=c(0, numsteps), ylim=c(v.min, v.max),
     xlab="Time", ylab="Price", type='l', col="red", main="Glosten-Milgrom")
lines(0:numsteps, bids, col="blue")
points(0:numsteps, prices)
# only print this for a single-run plot

 points(1:numsteps-0.5, rep(61.6, numsteps),        pch=c("S", "B")[buys[2:(numsteps+1)]+1])
 
 numsims <- 3
numsteps <- 200
v.max <- max(pr)
v.min <- min(pr)
mu <- 0.1
delta <- 0.6

asks <- numeric(numsteps+1)
bids <- numeric(numsteps+1)


for (i in 1:numsims) {
  informeds <- c(0, rbinom(numsteps, 1, mu))
  buys <- c(0, rbinom(numsteps, 1,1/2))
  # assume v = v.max; hence informeds only buy
  buys[informeds == 1] <- 1
  n.buys <- cumsum(buys)
  n.sells <- 0:numsteps - n.buys
  askz <- (v.min*delta*(1-mu)**(n.buys+1)*(1+mu)**n.sells +
           v.max*(1-delta)*(1+mu)**(n.buys+1)*(1-mu)**n.sells)/
             (delta*(1-mu)**(n.buys+1)*(1+mu)**n.sells +
              (1-delta)*(1+mu)**(n.buys+1)*(1-mu)**n.sells)
  bidz <- (v.min*delta*(1-mu)**n.buys*(1+mu)**(n.sells+1) +
           v.max*(1-delta)*(1+mu)**n.buys*(1-mu)**(n.sells+1))/
             (delta*(1-mu)**n.buys*(1+mu)**(n.sells+1) +
              (1-delta)*(1+mu)**n.buys*(1-mu)**(n.sells+1))
  asks <- asks + askz
  bids <- bids + bidz


}
asks <- asks/numsims
bids <- bids/numsims
spread <- asks - bids

prices <- numeric(numsteps+1)

for (i in 1:numsteps) {
if(buys[i]==1) prices[i]=asks[i] 
else prices[i]=bids[i] 
}

#asks

#bids

#prices

plot(0:numsteps, asks, xlim=c(0, numsteps), ylim=c(v.min, v.max),
     xlab="Time", ylab="Price", type='l', col="red", main="Glosten-Milgrom")
lines(0:numsteps, bids, col="blue")
points(0:numsteps, prices)
# only print this for a single-run plot

 points(1:numsteps-0.5, rep(61.6, numsteps),        pch=c("S", "B")[buys[2:(numsteps+1)]+1])
 

```

As we can infer from the above plots, we can come to the conclusion that, when we alter the percentage of the number of informed traders in the market the Bid-ask spread varies a lot.
If we compare the first plot with the second one, we can see a steep price difference when the number of uninformed traders increases from 1% to 20%.
And, as we increase the mu value to 0.5 or more, we can see the spread is more shifted to the buy side indicating if the market contains more informed traders, who know the price of the security value prior, they get to affect the market price drastically.


PROBABILITY OF INFORMED TRADING MODEL
```{r}
tqdata_KO_1d <- tqdata["T14/T20"]
tqdata_KO_5m <- split(tqdata_KO_1d, f="mins", k=5)
tqdata_KO_15m <- split(tqdata_KO_1d, f="mins", k=15)
tqdata_KO_30m <- split(tqdata_KO_1d, f="mins", k=30)

which_buy <- function(x){which(x > 0)} 
which_sell <- function(x){which(x < 0)}

td <- getTradeDirection(tqdata_KO_1d)
tradeDirection_KO <- matrix(td)
buy_side_KO <- which(tradeDirection_KO > 0) 
num_buy_side_KO <- length(matrix(buy_side_KO))
num_sell_side_KO <- length(tradeDirection_KO) - length(matrix(buy_side_KO)) 
ntrades_KO <- cbind(num_buy_side_KO, num_sell_side_KO)
head(ntrades_KO)

res_KO <- YZ(ntrades_KO)
res_KO$PIN <- (res_KO$alpha * res_KO$mu)/
(res_KO$alpha * res_KO$mu + res_KO$epsilon_b + res_KO$epsilon_s)
print(res_KO)

td5 <- lapply(tqdata_KO_5m, getTradeDirection)
buy_side_KO_5m <- lapply(td5, which_buy) 
num_buy_side_KO_5m <- sapply(buy_side_KO_5m, length)
sell_side_KO_5m <- lapply(td5, which_sell) 
num_sell_side_KO_5m <- sapply(sell_side_KO_5m, length)
ntrades_KO_5m <- cbind(num_buy_side_KO_5m, num_sell_side_KO_5m) 
head(ntrades_KO_5m)

res_KO_5m <- YZ(ntrades_KO_5m)
res_KO_5m$PIN <- (res_KO_5m$alpha * res_KO_5m$mu)/
(res_KO_5m$alpha * res_KO_5m$mu + res_KO_5m$epsilon_b + res_KO_5m$epsilon_s)
print(res_KO_5m)

td15 <- lapply(tqdata_KO_15m, getTradeDirection)
buy_side_KO_15m <- lapply(td15, which_buy) 
num_buy_side_KO_15m <- sapply(buy_side_KO_15m, length)
sell_side_KO_15m <- lapply(td15, which_sell) 
num_sell_side_KO_15m <- sapply(sell_side_KO_15m, length)
ntrades_KO_15m <- cbind(num_buy_side_KO_15m, num_sell_side_KO_15m) 
head(ntrades_KO_15m)

res_KO_15m <- YZ(ntrades_KO_15m)
res_KO_15m$PIN <- (res_KO_15m$alpha * res_KO_15m$mu)/
(res_KO_15m$alpha * res_KO_15m$mu + res_KO_15m$epsilon_b + res_KO_15m$epsilon_s)
print(res_KO_15m)

td30 <- lapply(tqdata_KO_30m, getTradeDirection)
buy_side_KO_30m <- lapply(td30, which_buy) 
num_buy_side_KO_30m <- sapply(buy_side_KO_30m, length)
sell_side_KO_30m <- lapply(td30, which_sell) 
num_sell_side_KO_30m <- sapply(sell_side_KO_30m, length)
ntrades_KO_30m <- cbind(num_buy_side_KO_30m, num_sell_side_KO_30m) 
head(ntrades_KO_30m)

res_KO_30m <- YZ(ntrades_KO_30m)
res_KO_30m$PIN <- (res_KO_30m$alpha * res_KO_30m$mu)/
(res_KO_30m$alpha * res_KO_30m$mu + res_KO_30m$epsilon_b + res_KO_30m$epsilon_s)
print(res_KO_30m)

Parameter <- c("Alpha", "Delta", "Epsilon(buy)", "Epsilon(sell)", "Mu", "PIN")

YZ_1d <- c(res_KO$alpha, res_KO$delta, res_KO$epsilon_b,
res_KO$epsilon_s, res_KO$mu, res_KO$PIN)
YZ_5m <- c(res_KO_5m$alpha, res_KO_5m$delta, res_KO_5m$epsilon_b,
res_KO_5m$epsilon_s, res_KO_5m$mu, res_KO_5m$PIN)
YZ_15m <- c(res_KO_15m$alpha, res_KO_15m$delta, res_KO_15m$epsilon_b,
res_KO_15m$epsilon_s, res_KO_15m$mu, res_KO_15m$PIN)
YZ_30m <- c(res_KO_30m$alpha, res_KO_30m$delta, res_KO_30m$epsilon_b,
res_KO_30m$epsilon_s, res_KO_30m$mu, res_KO_30m$PIN)
print(data.frame(Parameter, YZ_1d, YZ_5m,  YZ_15m, YZ_30m))

```
