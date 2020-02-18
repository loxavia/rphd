#dates


as.Date('1/15/2001',format='%m/%d/%Y')
as.Date('April 26, 2001',format='%B %d, %Y')
as.Date('22JUN01',format='%d%b%y')
#Internally, Date objects are stored as the number of days since January 1, 1970, using negative numbers for earlier dates. The as.numeric function can be used to convert a Date object to its internal form

# dates can be specified by a numeric value, representing the number of days since January 1, 1970. To input dates stored as the day of the year, the origin= argument can be used to interpret numeric dates relative to a different date.

#The default format for times consists of the hour, minutes and seconds, separated by colons. Alternative formats can use the codes 
dtimes = c("2002-06-09 12:45:40","2003-01-29 09:30:40",  "2002-09-04 16:45:40","2002-11-13 20:00:40", "2002-07-07 17:30:40")
dtparts = t(as.data.frame(strsplit(dtimes,' ')))
dtparts
row.names(dtparts) = NULL
library(chron)
thetimes = chron(dates=dtparts[,1],times=dtparts[,2],  format=c('y-m-d','h:m:s'))
thetimes


#1915/6/16, 2005-06-24 11:25, 1990/2/17 12:20:05
dts = c("2005-10-21 18:47:22","2005-12-24 16:39:58", "2005-10-28 07:30:05 PDT")
as.POSIXlt(dts)

mydate = strptime('16/Oct/2005:07:51:00',format='%d/%b/%Y:%H:%M:%S')
mydate

textdate = "14 February 2020, 4:28 PM"
strptime(textdate, format ='%d %B %Y, %I:%M %p')

#https://statistics.berkeley.edu/computing/r-dates-times