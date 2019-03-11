
#Predicting DV from IV
#Predicting Final Pass/ Fail from other IVs

library(gsheet)
library(dplyr)
library(rpart)
library(rpart.plot)

url = 'https://docs.google.com/spreadsheets/d/1fHrF7ia4sBrYbCFRdZQa26eGKZ_7O9t18h7PdN6GY2c/edit#gid=517909014'

data1 = as.data.frame(gsheet2tbl(url))
head(data1)
str(data1)
data1[ c('gender', 'finalresult')] = lapply(data1[ c('gender','finalresult')], factor)
data1$rollno = NULL
data1$finalmarks = NULL

#data for Decision Modeling
str(data1)
head(data1)
names(data1)

dtree1 = rpart(finalresult ~ . , data= data1)
dtree1
printcp(dtree1)

rpart.plot(dtree1, extra=104, nn=T, main='Decision Tree to Predict Result Class- Pass/ Fail')

ndata1 = dplyr::sample_n(data1, 2)
p1=predict(dtree1, newdata=ndata1, type='class')
cbind(ndata1, p1)

#
prp(dtree1, extra=104, nn=T, main='Decision Tree to Predict Result Class- Pass/ Fail')