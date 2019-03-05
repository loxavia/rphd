#Association Rule
# data set having Gradution, Attendance, Assignment, Unit Test and Univ Result Results
# Aim - to develop Association Rule 

options(digits=2)
#data1 = read.csv(file=file.choose(),header=T, stringsAsFactors = T)

library(gsheet)
library(arules)
library(arulesViz)

url = 'https://docs.google.com/spreadsheets/d/1fHrF7ia4sBrYbCFRdZQa26eGKZ_7O9t18h7PdN6GY2c/edit#gid=1208330417'

data1 = as.data.frame(gsheet2tbl(url))
head(data1)

# Check Properties of Data Set 
str(data1)
dim(data1)
cols= names(data1)
data1[cols] = lapply(data1[cols], factor)
str(data1)
# Find Association Rules with Default Settings
rules1 = apriori(data1)
inspect(rules1)

# Custom Rules
rules2 = apriori(data1, parameter = list(minlen=2,supp=.05,conf=0.6),   list(rhs=c("result=Good"),default="lhs"), control=list(verbose=F))
rules2.sorted = sort(rules2, by="lift")
inspect(rules2.sorted)


