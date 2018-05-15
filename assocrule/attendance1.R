# Association Rule Analysis on Attendance of Students

#import Data

data = read.csv(file='./data/sub1.csv', row.names = 1)
str(data)
data2 = data
library(car)  #for recode
data3= lapply(data2, function(x) recode(x, "'A'=1;'P'=0"))
data4= as.data.frame(data3)
data5= t(data4)
dim(data5)
data5[1:5,1:6]
#name columns as students s1 to 
dim(data5)[2]
cnames = paste('s',1:dim(data5)[2], sep='')
head(cnames)
colnames(data5) = cnames
data5[1:5,1:6]

colSums(as.matrix(data5))
rowSums(as.matrix(data5))

df = data5[, 1:5]

#now data is structured

#convert into transaction format
library(arules)


slist = as(df, "transactions")
slist
inspect(slist[1:4])

# Most Frequent Items
itemFrequency(slist, type = "relative")
itemFrequencyPlot(slist,topN = 5)
itemFrequencyPlot(slist,topN = 20, type='absolute')
barplot(sort(itemFrequency(slist), decreasing=FALSE))

# aggregated data
rules = apriori(slist, parameter=list(support=0.05, confidence=0.8))
inspect(rules[1:5])
rulesL = sort(rules, by='lift', decreasing = TRUE)
inspect(head(rulesL))


library(arulesViz)
topRules <- rules[1:10]
plot(topRules)

plot(topRules, method="graph")


#subset Rules
#Coerce into data frame
as(rulesL, "data.frame");

#Restrict LHS to only certain value (here, "s3")
inspect(subset(rulesL, (lhs %in% c("s3"))))
inspect( subset( rulesL, subset = lhs %pin% c("s3") ) )

#Restrict RHS to only certain value (here, "s3")
inspect(subset(rulesL, (rhs %in% c("s3"))))
inspect(subset(rulesL, subset = rhs %pin% c("s3") ) )

#s3 anywhere lhs or rhs
inspect(subset(rulesL , items %in% "s3"))
