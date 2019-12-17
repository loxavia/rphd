#C4.5
#The C4.5 algorithm, created by Ross Quinlan, implements decision trees. The algorithm starts with all instances in the same group, then repeatedly splits the data based on attributes until each item is classified. To avoid overfitting, sometimes the tree is pruned back. C4.5 attempts this automatically. C4.5 handles both continuous and discrete attributes.
#load packages First we load the RWeka and caret packages.
#J48 is an open source Java implementation of the C4.5 algorith available in the Weka package.

library(RWeka)
library(caret)

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

set.seed(1958)  # set a seed to get replicable results
train1 <- createFolds(students$finalresult, k=10)
train1
C45Fit1 <- train(finalresult ~ ., method="J48", data=students[,-1], tuneLength = 5,   trControl = trainControl( method="cv", indexOut=train1))
C45Fit1
C45Fit1$finalModel


#iris
set.seed(1958)  # set a seed to get replicable results
train2 <- createFolds(iris$Species, k=10)
C45Fit2 <- train(Species ~., method="J48", data=iris, tuneLength = 5,   trControl = trainControl( method="cv", indexOut=train2))
C45Fit2
C45Fit2$finalModel
plot(C45Fit2)
iris[1:2,]
predict(object=C45Fit2, newdata = iris[1:2,], type='prob')

#C5.0
#Quinlan made improvements to C4.5 and called it C5.0. The newer algorithm is faster, requires less memory and gets results similar to C4.5 but with smaller decision trees.#The algorithm is availabe in package C50. The printr package is a companion to knitr.

library(C50)
library(printr)
#Split iris data into train and test
train.indices <- sample(1:nrow(iris), 100)
iris.train <- iris[train.indices, ]
iris.test <- iris[-train.indices, ]

#Train decision tree
model <- C5.0(Species ~., data=iris.train)
#Test
results <- predict(object=model, newdata=iris.test, type="class")
#Look at confusion matrix
table(results, iris.test$Species)
plot(model)

#Comparing C4.5 and C5.0 results
#Comparing the splits between the two algorithms we see that C4.5 made 4 splits and got 96% accuracy whereas C5.0 made only 3 splits and got 98% accuracy.

#Note that the seed influenced how the data was split and therefore influenced the result. This is a small data set and therefore more influenced by the test/train split than a larger data set is likely to be.

