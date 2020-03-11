#analysing data from LA LMS
library(dplyr)
library(ggplot2)
library(tidyverse)  #load forcats
library(magrittr)  #<> function
library(lubridate)
library(wordcloud2)

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

#time frame------
names(data)
head(data$timecreated)
value <- 1372657859 ; as.POSIXct(value, origin="1970-01-01")  
data$timecreated <- as.POSIXct(data$timecreated, origin="1970-01-01")  
paste(as.character(range(data$timecreated)))
#duration <- paste(duration, paste(as.character(range(data$timecreated))))
(dur = paste(as.character(min(as.Date(data$timecreated))), as.character(max(as.Date(data$timecreated))), sep=' to '))

data %>% mutate(timecreated = as.Date(timecreated)) %>% group_by(month=floor_date(timecreated, "month")) %>% summarize(count =n())  %>% ggplot(., aes(x = month, y = count)) +  geom_line(color = "#00AFBB", size = 2) + scale_x_date(date_breaks = '1 month', date_labels = "%b/%y") + theme(axis.text.x = element_text(angle = 30)) + labs(title='Number of Events : Month Wise', subtitle = paste('Duration', dur))

#context level-----
table(data$contextlevel)
context_levels = c(10,30,40,50,70,80)
names(context_levels) = c('System','User','CourseCategory','Course','Module','Block')
context_levels
data$contextlevel = factor(data$contextlevel, ordered=T, labels=names(context_levels), levels=context_levels)
table(data$contextlevel)

ggplot(data, aes(x=contextlevel, y=..count.., fill=contextlevel)) + geom_bar(stat='count') + geom_text(stat='count', aes(label=..count..), vjust=-.5) + labs(title='Summary of Activities in LMS', subtitle = 'Context Levels - System, User, CourseCategory, Course, Module,Block') + scale_fill_discrete(name = "Context Level") +  theme(legend.position="top", legend.direction="horizontal", plot.title = element_text(hjust = 0.5)) + guides(fill = guide_legend(nrow = 1))

#course wise----
names(data)
table(data$contextinstanceid)
table(data$courseid)
data %>% group_by(courseid)  %>% summarise(n=n())
ggplot(data, aes(x=courseid, y=..count.., fill=factor(courseid))) + geom_bar(stat='count') + geom_text(stat='count', aes(label=..count..), vjust=-.5) + labs(title='Summary of Activities in LMS', subtitle = 'Context Wise - Count of Events - (Course ID)')   + scale_fill_discrete(name = "Course ID")  +  theme(legend.position="top", legend.direction="horizontal", plot.title = element_text(hjust = 0.5))  + guides(fill = guide_legend(nrow = 2))

par(mar=c(0,0,0,0))
data %>% group_by(courseid)  %>% summarise(n=n()) %>% mutate(courseid = factor(courseid)) %>% wordcloud2::wordcloud2(., minSize=4, rotateRatio = 0, size=2, shuffle = T, gridSize = 2)

(wcdata <- data %>% group_by(courseid)  %>% summarise(n=n()) %>% as.data.frame())
par(mar=c(0,0,0,0))
wordcloud::wordcloud(words=wcdata$courseid, freq=wcdata$n, scale=c(6,3), random.color = T, colors=1:5)


#-----10Mar20-----
head(data)
str(data)

names(data)
head(data[,c('eventname')])
table(data$eventname)
data %>% slice(1:5) %>% select(eventname)
data2 <- data %>% separate(eventname, '\\\\', remove=F, into=c('L1','L2','L3','L4')) %>% select(userid, timecreated, contextlevel, L2, L3, L4, origin, courseid,crud, edulevel, contextinstanceid )
names(data2)
table(data2$contextinstanceid)
table(data2$courseid)
activityList = c('attempt_submitted', 'attendance_taken_by_student', 'course_module_completion_updated','course_section_updated','user_graded', 'user_leveledup', 'user_graded')
data3 <- data2 %>% filter(timecreated > ymd_hms(20200201000000) & crud =='u' & courseid == '16' & L4 %in% activityList & userid > 2260)
table(data3$L4)
table(data3$crud)
table(data3$courseid)
head(data3)
#event <- bupaR::eventlog(eventlog=data2, case_id='userid', activity_id ='L4', activity_instance_id= 'contextinstanceid', lifecycle_id='crud', timestamp = 'timecreated', resource_id ='contextlevel')

events <- bupaR::simple_eventlog(eventlog = data3,   case_id = 'userid',  activity_id = 'L4', timestamp = 'timecreated' )

events %>% n_events()
events %>% trace_explorer()
events %>% n_traces()
events %>% n_activities()
events %>% activity_frequency()
events %>% activity_frequency() %>% plot()
events %>% activity_frequency(level='activity')
events %>% processmapR::process_map(sec=frequency('absolute'))
table(events$L4)
table(events$userid)
events %>% processmapR::process_map(sec=frequency('absolute'))
events %>% animate_process(duration=20)
events %>% dotted_chart(x = "absolute", sorted=T)
names(events)
events %>% dotted_chart(x = "relative")
events %>%   trace_explorer(coverage = 0.2)
events %>%   trace_explorer(coverage = 0.1)
events %>%   precedence_matrix(type = "relative") %>%   plot()
events %>%   precedence_matrix(type = "absolute") %>%   plot()
events %>%  size_of_selfloops ()
events %>%  number_of_repetitions()
events %>%  number_of_selfloops()
events %>% process_map(type=frequency('absolute'), rankdir='TB')
events %>% process_map(type=frequency('absolute'), rankdir='BT')
table(events$userid)
events %>% filter_case(cases = c(2400,2402)) %>% trace_explorer(show_labels = F, raw_data = F)
events %>% filter_activity_presence(activities = 'attempt_submitted') %>% trace_explorer(show_labels = T, raw_data = F)
events %>% filter_activity_presence(activities = 'user_leveledup') %>% trace_explorer(show_labels = T, raw_data = F)
events %>% animate_process(duration=20, mode='relative', repeat_count = 2, initial_state = 'paused', sec=frequency('relative'))


#https://cedricscherer.netlify.com/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/#legends
