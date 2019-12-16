#Clustering

#http://ijcsit.com/docs/Volume%206/vol6issue03/ijcsit2015060371.pdf

#Sheet 1 contain the combined and average data of Academic, Technical skill,Soft skill and Training & Project. While sheet 2, sheet 3, and sheet 4 having detailed record of student performance in Academics, Technical skill and Soft skill. At last the 5th sheet contains only the roll no. and final result (which is the average of all fields/ sheets). The whole values in dataset are in between 0 and 1 range so that it becomes easy to compute and calculate the final result

library(dplyr)
library(cluster)
library(factoextra)
library(ggplot2)

#data generate
ns=600
(rollno = 1:ns)
(acadskill = round(rnorm(n=ns, mean=.5, sd=.1),2))
(techskill = round(rnorm(n=ns, mean=.5, sd=.1),2))
(softskill = round(rnorm(n=ns, mean=.5, sd=.1),2))
(finalresult = sample(x=c(0,1), size=ns, replace=T, prob=c(.4,.6)))
range(acadskill)
students = data.frame(rollno, acadskill, techskill, softskill, finalresult)
head(students)

students1 = students[, -c(1,5)]
head(students1)

#cluster
k3 = kmeans(students1, centers=3)
k3
attributes(k3)
k3$cluster
k3$centers

library(factoextra)
fviz_cluster(k3,data=students1)

#select no of clusters
library(NbClust)

# Elbow method
fviz_nbclust(students1, kmeans, method = "wss") +  geom_vline(xintercept = 4, linetype = 2)+  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(students1, kmeans, method = "silhouette") + labs(subtitle = "Silhouette method")
#gap stats
set.seed(123)
fviz_nbclust(students1, kmeans, nstart = 25,  method = "gap_stat", iter=15,nboot = 50) +  labs(subtitle = "Gap statistic method")

#---
NbClust(data = students1, distance = "euclidean",  min.nc = 2, max.nc = 15, method = 'kmeans')

#suggested
#clusters=4
k4 = kmeans(students1, centers=4)
k4$centers
fviz_cluster(k4,data=students1)
students %>% group_by(GP=k4$cluster)  %>% summarise_each(list(mean), acadskill, techskill, softskill, finalresult)

#clusters=2
k2 = kmeans(students1, centers=2)
k2$centers
fviz_cluster(k2,data=students1)
students %>% group_by(GP=k2$cluster)  %>% summarise_each(list(mean), acadskill, techskill, softskill, finalresult)

#Quest
#1. What type of students the college have according to their academic scoring?
#2. How many students will eligible to attend placement process and having chances to get campus placement?
#3. Predict in advance, the same for pre - final year students?

#https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/