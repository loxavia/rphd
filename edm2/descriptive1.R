#Descriptive Analysis
#Questions appeared from Paper

library(dplyr)
library(gsheet)
#library(xlsx)
url='https://docs.google.com/spreadsheets/d/1fHrF7ia4sBrYbCFRdZQa26eGKZ_7O9t18h7PdN6GY2c/edit#gid=691928977'

data1 = as.data.frame(gsheet2tbl(url))
head(data1)
rownames(data1) = data1$rollno
data1$rollno = NULL
dim(data1)

#scores by each student
rowSums(data1, na.rm=T)
#Ques each student appeared on 
rowSums(!is.na(data1)) 


#scores in each Qs
colSums(data1, na.rm=T)
#No of students who appeared for each ques
colSums(is.na(data1))
colSums(!is.na(data1))


boxplot(data1$G1Q1)
range(data1$G1Q1, na.rm=T)
lapply(data1, range, na.rm=T)

