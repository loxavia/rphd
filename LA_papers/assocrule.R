#Association Rule 
#Finding relationship between choosing electives

library(gsheet)
library(arules)
library(arulesViz)


#or load the data from csv
data1 = read.csv(file.choose())  #elective1.csv
head(data1)
rownames(data1) = data1$rollno
data1$rollno = NULL
str(data1)
data2 <- as.matrix(data1)
str(data2)
head(data2)

data3 <- as(data2, "transactions")
str(data3)
summary(data3) # check what’s in the transaction data.

#With inspect(), check each transaction’s items.
inspect(data3)

#want to know frequency of items, I can plot as below;
itemFrequencyPlot(data3, type='absolute')
#Making association rules ####
options(digits=2)
rules1 <- apriori(data3, parameter = list(maxlen=3, support=0.04, confidence=0.6, ext=TRUE))


rules1L = sort(rules1, by='lift', decreasing=T)
inspect(rules1L)

inspect(sort(subset(rules1L, subset= (support > 0.4))))

#specific rules : rhs=elective6  and lift=1

rules1S1 <- subset(rules1, subset=(rhs %in% "elective6") & (lift>1.0))
rules1S1
inspect(rules1S1)

#either elective3 or elective4 in lhs
rules1S2 <- subset(rules1, subset=(lhs %in% c('elective3','elective4')) & (lift > 1.5))
rules1S2C = sort(rules1S2, by='confidence', decreasing=T)
inspect(rules1S2C)

rules1S3 <- subset(rules1, subset=(lhs %in% "elective1") & (rhs %in% "elective6"))
rules1S3C = sort(rules1S3, by='support', decreasing=T)
inspect(rules1S3C)

