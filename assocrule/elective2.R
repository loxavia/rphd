#Association Rule 
#Finding relationship between choosing electives

library(gsheet)
library(arules)
library(arulesViz)

url = 'https://docs.google.com/spreadsheets/d/1fHrF7ia4sBrYbCFRdZQa26eGKZ_7O9t18h7PdN6GY2c/edit#gid=2063933998'

data1 = as.data.frame(gsheet2tbl(url))
head(data1)
rownames(data1) = data1$rollno
data1$rollno = NULL
str(data1)
data2 <- as.matrix(data1)
str(data2)
head(data2)
#If all the columns except the row name are two-valued data (0 or 1), matrix class is easier to deal with in association rules.
#IMP If all the columns are string data, I just convert the data.frame into transaction data without converting it into matrix data.
#Next, I convert onsen matrix into transaction class.
data3 <- as(data2, "transactions")
str(data3)
summary(data3) # check what’s in the transaction data.
#With inspect(), check each transaction’s items.
inspect(data3)

#want to know frequency of items, I can plot as below;
itemFrequencyPlot(data3, ylim=c(0,1))
#Making association rules ####
#Now make association rules.
options(digits=2)
rules1 <- apriori(data3, parameter = list(maxlen=3, support=0.04, confidence=0.6, ext=TRUE))


#apriori() is a function to make association rules.
#maxlen means a number of electives in rules 
#support means how often it happens (or probability of an event). 0.04 is not so rare, not too often.
#And confidence means how often it occurs under specific situations. If this is so small, it may be not be worth making association rules in the first place.
#Now, check summary of the association rules  made, with summary().
#check a number of rules and how many rules each association rule has.
rules1L = sort(rules1, by='lift', decreasing=T)
inspect(rules1L)
summary(data3rule.lift)
inspect(sort(subset(rules1L, subset= (support > 0.4))))

#If you want to extract specific association rules, use subset() as below.
#In this case, want the result to have “elective6” as the rule body and more than 1 lift.

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
