#Decision Tree

library(gsheet)
library(rpart)
library(rpart.plot)

url = 'https://docs.google.com/spreadsheets/d/1fHrF7ia4sBrYbCFRdZQa26eGKZ_7O9t18h7PdN6GY2c/edit#gid=517909014'
options(digits=3)
data1 = as.data.frame(gsheet2tbl(url))
head(data1)
str(data1)
data1[,c('gender','course','batch','finalresult')] = lapply(data1[,c('gender','course','batch','finalresult')], factor)
str(data1); dim(data1)
data1 = data1[,-12]
head(data1)
dtree1 = rpart(finalresult ~ . , data = data1[-1], model=T )
print(dtree1)

printcp(dtree1)
rpart.plot(dtree1, extra=104, fallen.leaves = T)


