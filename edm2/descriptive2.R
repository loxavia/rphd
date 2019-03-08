#Descriptive Analysis
#exam results with student data

library(dplyr)
library(gsheet)
#library(xlsx)
url='https://docs.google.com/spreadsheets/d/1fHrF7ia4sBrYbCFRdZQa26eGKZ_7O9t18h7PdN6GY2c/edit#gid=261737316'

data1 = as.data.frame(gsheet2tbl(url))
head(data1)
rownames(data1) = data1$rollno
data1$rollno = NULL
dim(data1)
head(data1)
str(data1)

data1$dob = as.Date(data1$dob, '%d-%b-%y')
data1$gender = as.factor(data1$gender)
class(data1$dob)
class(Sys.Date())

difftime(Sys.Date() - (Sys.Date() +1 ))
data1$age = round(as.numeric((Sys.Date() - data1$dob)/365),2)
head(data1)

names(data1)
#summaries
data1 %>% group_by(gender) %>%  summarise_at(vars(JAVA:CBNST), mean, na.rm = TRUE)
