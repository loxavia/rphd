# Time Series

#libraries - zoo, xts, lubridate, 
pacman::p_load(gsheet, lubridate, zoo, xts, quantmod, TTR, forecast) 

start <- as.Date("2020-01-01")
end <- as.Date("2020-02-29")
end - start

getSymbols("SBIN.NS", src = "yahoo", from = start, to = end)

df = SBIN.NS
head(df)
gsub(".", names(df))
colnames(df)
#create new column names
unlist(strsplit("a.b.c", "\\."))
unlist(strsplit(names(df), "\\."))
(newColNames <- unlist(strsplit(names(df), "\\."))[seq(3,18,3)])
names(df)= newColNames

head(df)
str(df)
index(df)  #rownames
coredata(df) #column values

#times series
plot(df$Open)
plot(df, legend.loc = 'left')
plot(df[,c('Open','Close')], legend.loc = 'top')
plot(df[,c('Open','Close')], legend.loc ='right', subset="2020-01-01/2020-01-15")
plot(df[,1:4], multi.panel = T)
plot(df[,1:4], multi.panel = T, type='h')
plot(df[,1:4], multi.panel = T, type='h', horiz=)



#properties
periodicity(df)
to.weekly(df)
to.monthly(df)
to.quarterly(df)
to.yearly(df)
to.period(df,period="weeks")
nyears(df)
nmonths(df)
nweeks(df)
ndays(df)
.indexwday(df)  #weekday number
weekdays(index(df))
start(df)
end(df)
time(df)
head(df)
tail(df)

#apply functions
apply.weekly(df, FUN=mean)
apply.monthly(df, FUN=mean)
apply.quarterly(df, FUN=mean)
?apply.weekly

#endpoint
endpoints(df, on='weeks')
endpoints(df, on='months')
df[endpoints(df, on='months'),]

#means
(wep <- endpoints(df, on='weeks'))
period.apply(df,INDEX=wep,FUN=mean)

#split data
split(df, f='months')


#subset
df['2020']
df['2020-01']
df['2020-01-16']

#first & last
first(df,'1 week') #Extract first 1 week
first(last(df,'1 week'),'3 days') #Get first 3 days of the last week of data


df[c('2020-01-16', '2020-01-16')]
#weekend
.indexwday(df)
df[.indexwday(df)==5,]

#------------------------------------------
#TS data
(inputData = as.vector(AirPassengers))
length(inputData)
ts (inputData, frequency = 4, start = c(1959, 2))
# frequency 4 => Quarterly Data
ts (inputData, frequency = 12, start = 1990)
# freq 12 => Monthly data. 
ts (inputData, start=c(1950), end=c(2020), frequency=1)
# Yearly Data

#lag
(monTS <- ts (inputData, frequency = 12, start = 1990)) #Monthly data. 
(monTSlagged <- stats::lag(monTS, k=-1))
monTS
monTSlagged
(monTS - monTSlagged)

diff(monTS, lag=1)
cbind(monTS, monTSlagged, difference=(monTS - monTSlagged), diff(monTS))
head(monTS)
head(diff(monTS,2)) #
c((132-112),(129-118),(121-132))


#Rolling Mean----
monTS
zoo::rollmean(monTS,k=1)
zoo::rollmean(monTS,k=2)
head(monTS)
c((112+118)/2, (118+132)/2)
zoo::rollmean(monTS,k=2, align='right')  #mean should start from Feb

zoo::rollapply(monTS, width=2, FUN=mean)
zoo::rollapply(monTS, width=2, FUN=mean, by=2)
#what is by??
zoo::rollapply(monTS, width=12, FUN=mean, align='right')

#Autocorrellations
acf(monTS)
Box.test(monTS)

#Moving Average, SMA, ES----------
library(TTR)
head(monTS)
SMA(monTS,n=3)
EMA(monTS,n=3)

#Exponential ------
exp

#find Trend, Seasonal, Irregular components
AirPassengers
plot(AirPassengers)
plot(decompose(AirPassengers))
#what are the different trends

#forecast & arima
library(forecast)
auto.arima(monTS)
#best order - d=2,p=1,q=1
#seasonality = P=0, D=1,Q=0
#mov avg m=4
autoplot(monTS)
#without seasonsal
auto.arima(monTS, seasonal = F)

#Model
arimaModel = auto.arima(monTS)
checkresiduals(arimaModel)
#read about these

#Forecast
fcModel <- forecast(arimaModel)
fcModel$mean  #10 elements ahead
autoplot(fcModel)

#Lag
df.open = df[,'Open']
df.open
str(df.open)

# function to compute simple returns
simple.ret <- df.open / lag(df.open)
# plot the close and add a panel with the simple returns
plot(df.open)
lines(simple.ret, type="h", on=NA)

plot(df.open)
lines(TTR::SMA(simple.ret), on=NA, col="blue")


#arima

start1 <- as.Date("2010-01-01") ;end1 <- as.Date("2020-02-10")
getSymbols("SBIN.NS", src = "yahoo", from = start1, to = end1)
dim(SBIN.NS)
df1 <- SBIN.NS[1:(nrow(SBIN.NS)-100),]  #excl last 10 rows
df1.open = df1$SBIN.NS.Open
arimaModel2 = forecast::auto.arima(df1.open)
(fcModel2 = forecast(arimaModel2,100))
tail(SBIN.NS[,1],100)
autoplot(fcModel2)
