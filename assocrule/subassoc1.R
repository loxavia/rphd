# Practise Code
# Association Rule
data1 = read.csv("./data/sub2.csv")
str(data1)

write.csv(dataar1,file='dar1w.csv',row.names = F)

(data2 = data1[-7])

library(arules)
str(data2)

# change to factors for Association Rule
data3 = lapply(data2,function(x) as.factor(x))
str(data3)
data4 = as.data.frame(data3)
head(data4)

rules = apriori(data4)
rules = apriori(data4, parameter=list(support=0.5, confidence=0.8))

inspect(rules[1:5])
rules

# more than 300 rules generated
# Refining Rules
rhsrule = c('java=Yes','cpp=Yes')

rules2 = apriori(data4, parameter = list(minlen=2,supp=.05,conf=0.8),
      list(rhs=rhsrule,default="lhs"), control=list(verbose=F))

rules.sorted = sort(rules2,by='lift')
inspect(rules.sorted)


# Visualising
library(arulesViz)
plot(rules2)
plot.new()
plot(rules,method='graph',control=list(type='items'))
plot(rules,method='paracoord',control=list(reorder=T))

# Using Weka Library ####
#Using Weka 
library(RWeka)
library(rattle)
rattle()
Apriori()



#----
# Prune Redundant Rules
subset.matrix = is.subset(rules.sorted,rules.sorted)
subset.matrix
subset.matrix[lower.tri(subset.matrix,diag=T)] = NA
subset.matrix
redundant <- colSums(subset.matrix,na.rm=T) >=1
subset.matrix
redundant
which(redundant)
rules.pruned = rules.sorted[!redundant]
inspect(rules.pruned)
