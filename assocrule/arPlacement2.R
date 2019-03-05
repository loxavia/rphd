#Association Rule

# Data set - Placement Selection Criteria

#options(digits=2)
#data1 = read.csv(file=file.choose(),header=T, stringsAsFactors = T)

library(gsheet)
library(arules)
library(arulesViz)

url = 'https://docs.google.com/spreadsheets/d/1fHrF7ia4sBrYbCFRdZQa26eGKZ_7O9t18h7PdN6GY2c/edit#gid=948058905'

data1 = as.data.frame(gsheet2tbl(url))
head(data1)
rownames(data1) = data1$rollno
data1$rollno = NULL
str(data1)
head(data1)
data2 <- as.matrix(data1)
str(datar2)

head(datar2)
#If all the columns except the row name are two-valued data (0 or 1), matrix class is easier to deal with in association rules.
#IMP If all the columns are string data, I just convert the data.frame into transaction data without converting it into matrix data.
#Next, I convert onsen matrix into transaction class.
data3 <- as(data2, "transactions")
str(data3)
summary(data3) # check what’s in the transaction data.
#With inspect(), check each transaction’s items.
inspect(data3)

#want to know frequency of items, I can plot as below;
itemFrequencyPlot(data3, topN=8, type="absolute")
itemFrequencyPlot(data3, ylim=c(0,1))
#Making association rules ####

options(digits=2)
rule1 <- apriori(data3, parameter = list(maxlen=3, support=0.04, confidence=0.8, ext=TRUE))


rule1C = sort(rule1, by='confidence', decreasing=T)

summary(rule1C)

#In order to inspect the contents of association rules, I can do it as below;
inspect(rule1C[1:5])

#If I want to extract specific association rules, I use subset() as below.
#In this case, I want the result to have “nerve_pain” as the rule body and more than 1 lift.

rule1 <- subset(datar2.rule, subset=(rhs %in% "comnskills") & (lift>1.0))
rule1
inspect(rule1)

rule2 <- subset(datar2.rule, subset=(rhs %in% c('extraco','gender')) & (lift>1.7))
rule2
inspect(rule2)

rule3 <- subset(datar2.rule, subset=(rhs %in% "select") & (lift>1.0))
rule3
inspect(rule3)

