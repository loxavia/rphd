# Classification of Students Performance

#Create columns
gender = sample(x=c('M','F'), size=100, replace=TRUE, prob=c(.4, .6))

#create subject names and which sem they were taught
java = sample(x=c(3,4,5), size=100, replace=T, prob=c(.3, .4, .3))
daa  = sample(x=c(3,4,5), size=100, replace=T, prob=c(.4, .3, .3))
cpp  = sample(x=c(3,4,5), size=100, replace=T, prob=c(.3, .3, .4))
dbms = sample(x=c(3,4,5), size=100, replace=T, prob=c(.2, .2, .6))
aggmarks = round(runif(100, 50,100),0)

#create class of selection in their placements
select = sample(x=c('Yes','No'), size=100, replace=TRUE, prob=c(.4, .6))

#combine into a DF
placement = data.frame(gender, java, daa, cpp, dbms, aggmarks, select)
#convert columns into factors
cols = c('gender', 'java', 'daa', 'cpp', 'dbms', 'select')
placement[cols] = lapply(placement[cols], factor)
summary(placement)
xtabs( ~ select + gender + java + daa + cpp + dbms, data= placement)

# create Decision Tree

library(rpart)
library(rpart.plot)

model1 = rpart(select ~ . , data=placement)
model1

rpart.plot(model1, cex=.8)
