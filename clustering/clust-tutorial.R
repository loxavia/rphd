# Clustering 
# Group students in two Gps - identify for tutorials
#create data for gps and combine them into 1 DF
java1 = round(rnorm(100, 50,3),0)
daa1 = round(rnorm(100, 55,6),0)
cpp1 = round(rnorm(100, 57,5),0)
dbms1 = round(rnorm(100, 51,8),0)
gp1 = data.frame(java=java1,daa=daa1,cpp=cpp1, dbms=dbms1)
#gp2
java2 = round(rnorm(100, 60,5),0)
daa2 = round(rnorm(100, 65,8),0)
cpp2 = round(rnorm(100, 77,5),0)
dbms2 = round(rnorm(100, 71,5),0)
gp2 = data.frame(java=java2,daa=daa2,cpp=cpp2,dbms=dbms2)

student = rbind(gp1, gp2)
library(dplyr)
student2 <- student %>% sample_frac(1, replace=FALSE)
head(student2)

#clustering - create 2 groups
kmodel1 = kmeans(student2, 2)
kmodel1$cluster
kmodel1$centers

student2[kmodel1$cluster ==1,]
student2[kmodel1$cluster ==2,]  # Tutorial Group

