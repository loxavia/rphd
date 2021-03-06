# Classification K knn

train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
train
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
test
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
cl
knn(train, test, cl, k = 3, prob=TRUE)
attributes(.Last.value)
