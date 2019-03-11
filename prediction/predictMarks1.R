#Predicting DV from IV
#Predicting Final Scores from attendance and other IVs

library(gsheet)
library(dplyr)

url = 'https://docs.google.com/spreadsheets/d/1fHrF7ia4sBrYbCFRdZQa26eGKZ_7O9t18h7PdN6GY2c/edit#gid=624492169'

data1 = as.data.frame(gsheet2tbl(url))
head(data1)
str(data1)
data1[ c('gender', 'cat')] = lapply(data1[ c('gender', 'cat')], factor)
data1$rollno = NULL
#data for Linear Modeling
str(data1)
head(data1)

names(data1)
#Linear Modeling
model1 = lm(btech ~ . , data=data1)
summary(model1)
#keep only significant variables
model2 = lm(btech ~ attnd + class12, data=data1)
summary(model2)

#verifying Model Assumptions
plot(model2)

#Predict 
ndata1= data.frame(attnd = c(.70,.80),class12 = c(.60, .75))
p1 = predict(model2, newdata= ndata1, type='response')
cbind(ndata1, p1)
