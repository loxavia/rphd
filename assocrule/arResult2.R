#Association Rule 

options(digits=2)
#data1 = read.csv(file=file.choose(),header=T, stringsAsFactors = T)

library(gsheet)
library(arules)
library(arulesViz)

url = 'https://docs.google.com/spreadsheets/d/1fHrF7ia4sBrYbCFRdZQa26eGKZ_7O9t18h7PdN6GY2c/edit#gid=1341903081'

data1 = as.data.frame(gsheet2tbl(url))
head(data1)

# Taking data as numerical values and then converting it into categorical values
str(data1)
data2= data1
breaks = seq(50,100,by=10)
# Table values of Attendance
attnd.cut = cut(data1$attendance,breaks,right=F)
attnd.freq = table(attnd.cut) ; attnd.freq
#all columns convert to categories
data2[data2 >= 70] = 'Good'
data2[data2 >= 60 & data2 < 70 ] = 'Avg'
data2[data2 < 60] = 'Poor'
str(data2)

data2[] <- lapply(data2, factor) # All Colns of DF
str(data2)

rules1 = apriori(data2)
inspect(rules1)
head(edndata14b)

# Custom Rules result = 'Poor'
arules2 = apriori(data2, parameter = list(minlen=2,supp=.005,conf=0.8),list(rhs=c('result=Poor'),default="lhs"), control=list(verbose=F))

arules2.sorted = sort(arules2, by="lift")
inspect(arules2.sorted)

head(inspect(arules2.sorted))



# Plotting Rules
# Visualising
library(arulesViz)
library(igraph)
plot(arules2)

plot(arules2, method="graph", control=list(type="items"))

plot(arules2, method="paracoord", control=list(reorder=TRUE))

inspect(rules2.sorted)
