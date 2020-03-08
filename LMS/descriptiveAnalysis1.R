#analysing data from LA LMS
library(dplyr)
library(ggplot2)
library(tidyverse)  #load forcats
library(magrittr)  #<> function
library(lubridate)

#from Adminer, export data from mdl_logstore_standard_log
#into csv - gzip format
#data 
zz=gzfile('E:/LMS/mdl_logstore_standard_log.csv.gz','rt')  
df=read.csv(zz,header=T)
summary(df)
data=df
names(df)
names(df)[1] = 'id'
head(data)
dim(data)
names(data)
str(data)
summary(data)
data %>% group_by(crud) %>% summarise(n=n())
data$crud = factor(data$crud, ordered=T, levels=c('c','r','u','d'))

#CRUD actions---
ggplot(data, aes(x=crud, y=..count.., fill=crud)) + geom_bar(stat='count') + geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(title='Summary of Activities in LMS', subtitle = 'c- Create, D-Delete, R-Read, U-Update') + scale_fill_discrete(name = "LMS Computer Action", breaks=c("c", "r","u","d"), labels = c("Create", "Read", "Update", "Delete"))

summary(data)

#Sources of Access-----
table(data$origin)
#data %<>% mutate(origin = fct_explicit_na(origin, na_level = "NA"))
nrow(data)
(olevels <- levels(data$origin))
olevels[length(olevels) + 1] <- "None"
data$origin <- factor(data$origin, levels = olevels)
data$origin[data$origin ==''] = 'None'

table(data$origin)
ggplot(data, aes(x=origin, y=..count.., fill=origin)) + geom_bar(stat='count') + geom_text(stat='count', aes(label=..count..), vjust=-.5) + labs(title='Summary of Activities in LMS', subtitle = 'Command Line, Restore, Web, Web Service, Others/NA') + scale_fill_discrete(name = "Origin of Access", breaks= c('cli','restore','web','ws','None'), labels = c("CLI",'Restore', "Web", "Web Service",'NotAvl'))

#------
names(data)
head(data$timecreated)
value <- 1372657859 ; as.Date(as.POSIXct(value, origin="1970-01-01"))  
data$timecreated <- as.Date(as.POSIXct(data$timecreated, origin="1970-01-01"))  
paste(as.character(range(data$timecreated)))
duration <- paste(duration, paste(as.character(range(data$timecreated))))
as.character(min(data$timecreated))

data %>% group_by(month=floor_date(timecreated, "month")) %>% summarize(count =n())  %>% ggplot(., aes(x = month, y = count)) +  geom_line(color = "#00AFBB", size = 2) + scale_x_date(date_breaks = '1 month', date_labels = "%b/%y") + theme(axis.text.x = element_text(angle = 30)) + labs(title='Number of Events : Month Wise', subtitle = paste('Duration', min(data$timecreated), ' to ', max(data$timecreated)))

#context level-----
table(data$contextlevel)
context_levels = c(10,30,40,50,70,80)
names(context_levels) = c('System','User','CourseCategory','Course','Module','Block')
context_levels
data$contextlevel = factor(data$contextlevel, ordered=T, labels=names(context_levels), levels=context_levels)
table(data$contextlevel)

ggplot(data, aes(x=contextlevel, y=..count.., fill=contextlevel)) + geom_bar(stat='count') + geom_text(stat='count', aes(label=..count..), vjust=-.5) + labs(title='Summary of Activities in LMS', subtitle = 'Context Levels - System, User, CourseCategory, Course, Module,Block') + scale_fill_discrete(name = "Context Level") +  theme(legend.position="top", legend.direction="horizontal", plot.title = element_text(hjust = 0.5)) + guides(fill = guide_legend(nrow = 1))


#------
names(data)
table(data$contextinstanceid)
table(data$courseid)
data %>% group_by(courseid)  %>% summarise(n=n())
ggplot(data, aes(x=courseid, y=..count.., fill=factor(courseid))) + geom_bar(stat='count') + geom_text(stat='count', aes(label=..count..), vjust=-.5) + labs(title='Summary of Activities in LMS', subtitle = 'Context Wise - Count of Events - (Course ID)')   + scale_fill_discrete(name = "Course ID")  +  theme(legend.position="top", legend.direction="horizontal", plot.title = element_text(hjust = 0.5))  + guides(fill = guide_legend(nrow = 2))

#+ guides(fill=FALSE) + theme(legend.position = 'top')
+ guides(fill = guide_legend(label.position = "bottom"))
#ggpubr::ggpie(., "n", label = "courseid", fill='courseid', legend=T)



#https://cedricscherer.netlify.com/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/#legends
