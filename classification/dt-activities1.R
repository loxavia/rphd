# Grouping / Predict Student on the basis of sequence of activity undertaken

# 5 Activities in different sequence

library(combinat)
#actseq = c('A1', 'A2', 'A3', 'A4', 'A5')
actseq = c(1:5)
actcomb = permn(actseq)
df = as.data.frame(do.call(rbind, actcomb))
cnames = c('A1', 'A2', 'A3', 'A4', 'A5')
colnames(df) = cnames
head(df)

set.seed(1234)
df$grades = sample(c('Pass','Fail'), size= 120, replace=TRUE, prob=c(.4, .6))
head(df)
summary(df)
cnames2 = colnames(df)
df[cnames2] = lapply(df[cnames2], factor)
summary(df)

#Decision Tree
library(rpart)

model1 = rpart(grades ~ ., data=df)
model1
model1$frame
attr(model1, "ylevels")
#ylevels[model1$frame[2, ]$yval]
path.rpart(model1, nodes = c(3,7))
unlist(model1, use.names = FALSE)[1:5]
#https://stackoverflow.com/questions/11831794/testing-rules-generated-by-rpart-package

library(rpart.utils)
rpart.lists(model1)
rpart.rules.table(model1)
rpart.subrules.table(model1)

library(rpart.plot)
rpart.plot(model1)
