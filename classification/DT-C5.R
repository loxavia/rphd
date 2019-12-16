#C4.5 / C5 - Placement
#https://cran.r-project.org/web/packages/C50/C50.pdf

ibrary(dplyr)
library(cluster)
library(factoextra)
library(ggplot2)
library(C50)

#data generate
ns=600
(rollno = 1:ns)
(acadskill = round(rnorm(n=ns, mean=.5, sd=.1),2))
(techskill = round(rnorm(n=ns, mean=.6, sd=.1),2))
(softskill = round(rnorm(n=ns, mean=.4, sd=.1),2))
(finalresult = sample(x=c(0,1), size=ns, replace=T, prob=c(.4,.6)))
sapply(list(acadskill, techskill, softskill),range)
students = data.frame(rollno, acadskill, techskill, softskill, finalresult)
head(students)

#decision tree
#C5.0.default ; churn; C5.0Control ;plot.C5.0 ; predict.C5.0
students$finalresult = factor(students$finalresult, level=c(0,1))
#label=c('No','Yes'))
str(students)
mod1 <- C5.0(finalresult ~ ., data = students[,-1])
mod1
plot(mod1)
plot(mod1, subtree = 3)
plot(mod1, trial = 1)

(results <- predict(object=mod1, newdata=students[1:5,], type="class"))
table(students[1:5,5], results)

#------------------------
data(churn)
churnTest
names(churnTrain)
head(churnTrain)
treeModel <- C5.0(x = churnTrain[, -20], y = churnTrain$churn)
treeModel
summary(treeModel)
ruleModel <- C5.0(churn ~ ., data = churnTrain, rules = TRUE)
ruleModel
summary(ruleModel)

treeModel <- C5.0(x = churnTrain[, -20],y = churnTrain$churn,  control = C5.0Control(winnow = TRUE))
summary(treeModel)

#imp
treeModel <- C5.0(x = churnTrain[, -20], y = churnTrain$churn)
C5imp(treeModel)
C5imp(treeModel, metric = "splits")


mod1 <- C5.0(Species ~ ., data = iris)
plot(mod1)
plot(mod1, subtree = 3)
mod2 <- C5.0(Species ~ ., data = iris, trials = 10)
plot(mod2) ## should be the same as above
## plot first weighted tree
plot(mod2, trial = 1)


treeModel <- C5.0(x = churnTrain[, -20], y = churnTrain$churn)
predict(treeModel, head(churnTest[, -20]))
predict(treeModel, head(churnTest[, -20]), type = "prob")
