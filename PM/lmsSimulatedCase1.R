#simulated LMS data for PM

library(processanimateR)
library(eventdataR)
library(bupaR)
library(dplyr)
library(processmapR)


(activities = c('Forum', 'PPT', 'Assignment','Quiz', 'Game' ,'Survey'))
(rollno = paste('S',1:50,sep='-'))
e1 <- expand.grid(rollno, activities)
head(e1)
names(e1) <- c('rollno','activity')
e1$timestamp = as.POSIXct(as.Date(paste('2020','2',sample(1:30, size=nrow(e1), replace=T),sep='-')))
head(e1)

#student Profile----
(rollno = paste('S',1:50,sep='-'))
(gender = sample(c('M','F'), size=50, replace=T, prob=c(.7,.3)))
(finalMarks = trunc(runif(50,min=60,max=500)))
(students = data.frame(rollno, gender, finalMarks, stringsAsFactors = F))
?merge
head(students)
names(students) ; names(e1)
(e2 <- merge(x=students, y=e1, all.y=T ))

  
#select fraction of rows randomly 
e3 <- e2 %>% sample_frac(.8)
#create bupaR object -----
events1 <- bupaR::simple_eventlog(eventlog = e3, case_id = 'rollno', activity_id = 'activity', timestamp = 'timestamp')

events1
#how many activities----
activity_frequency(events1)
#freq of participation in activities----
events1 %>% activity_frequency(level='activity')
events1 %>% activity_frequency(level='activity') %>% plot

#Process Map-----
processmapR::process_map(events1)

#Animate Process-----
ap1 <- animate_process(events1)
ap1
animate_process(events1, legend=T, mode='absolute')
animate_process(events1, legend=T, mode='relative')
animate_process(events1, legend=T, mode='relative', duration=10,  mapping = token_aes(color = token_scale("red"), size = token_scale(10)))
names(events1)
str(events1)
events1$gender = factor(events1$gender)
#color as per gender----
animate_process(events1,  legend = "color",   mapping = token_aes(color = token_scale("gender", scale='ordinal', range = c('red','blue'))), duration=10)

#color as per gender as size as per marks ----
animate_process(events1,  legend = "color",   mapping = token_aes(color = token_scale(attribute="gender", scale='ordinal', range = c('red','blue')), size=token_scale(12), opacity = token_scale('0.4')), duration=10)

animate_process(events1,  legend = "color",   mapping = token_aes(color = token_scale(attribute="finalmarks", scale='quantize', range = c('red','green','blue')), size=token_scale(12), opacity = token_scale('0.4')), duration=10)


animate_process(events1,  legend = "size",   mapping = token_aes(size = token_scale(12), shape='rect'), duration=10)
animate_process(events1, legend='size', mapping = token_aes(size = token_scale(15), shape = "rect"), duration=10)

animate_process(events1,  mapping = token_aes(co = token_scale(attribute='gender', range=c('red','blue'))), duration=10)



animate_process(events1, legend=T, mode='relative', duration=10,  mapping = token_aes(color = token_scale('marks'), scale = 'linear'))

htmlwidgets::saveWidget(ap1, file = "ap1.html")

#activity presence----
events1 %>% activity_presence() %>% plot
events1 %>% filter_activity(c("PPT", "Quiz")) %>% activities
events1 %>% activity_frequency(level = "activity")
events1 %>% activity_frequency(level = "activity") %>% plot()
#activities 80% of the path
events1 %>% filter_activity_frequency(percentile = 0.8)
events1 %>% filter_activity_presence(c("PPT","Quiz"), method = "all")



#resources----
events1
table(events1$resource_id)
events1 %>% resource_frequency("resource")
events1 %>% filter_resource_frequency(perc = 0.80) %>%   resources()
events1 %>% filter_activity_frequency(interval = c(50, 300)) %>% activities

#activities between T1 & T2
library(lubridate)
range(events1$timestamp, na.rm=T)
events1 %>% summary()
events1 %>%  filter_time_period(interval = ymd(c(20200201, 20200215)), filter_method = "trim") %>% summary()  #less events


#Trace Length-----
events1
events1 %>%  filter_trace_length(interval = c(2, 5)) %>% trace_length(units = "min")
events1 %>%  filter_trace_length(percentage = 0.5) %>%  trace_length()
#traces which have PPT
events1 %>%  filter_activity_presence("PPT") %>% traces
#traces which do not have PPT
events1 %>% filter_activity_presence("PPT", reverse = T) %>%  traces
events1 %>%  filter_activity_presence(c("PPT", "Quiz"), method = "all")  %>%   traces
events1 %>%  filter_activity_presence(c("PPT", "Quiz", reverse=T), method = "all")  %>% traces
#one of the activities
events1 %>%  filter_activity_presence(c("PPT", "Quiz"), method = "one_of")  %>% traces

#either PPT or Quiz was not there--
events1 %>%  filter_activity_presence(c("PPT", "Quiz", reverse=T), method = "one_of")  %>% traces

#none of them: students who did not take none of these
events1 %>% filter_activity_presence(c("PPT", "Game"), method = "none")  %>% traces


#start activities---
#student who started with PPT---
events1 %>% filter_endpoints(start_activities = "PPT") %>% traces
#students who did not start with PPT
events1 %>% filter_endpoints(start_activities = "PPT", reverse = T) %>% traces
#ended with
events1 %>%  filter_endpoints(end_activities = "Quiz") %>% traces()
#did not end with 
events1 %>%  filter_endpoints(end_activities = "Quiz", reverse=T) %>% traces()
#start and end with
events1 %>%  filter_endpoints(start_activities = "Quiz", end_activities = "Forum") %>% traces()
#map
events1 %>%  filter_endpoints(start_activities = "Quiz", end_activities = "Forum") %>% process_map()


#Precedence-----
events1 %>%  filter_precedence(antecedents = "PPT",  consequents = "Quiz",precedence_type = "directly_follows") %>% traces
#PPT follows Forum & Quiz - data/ traces
events1 %>%  filter_precedence(antecedents = "PPT", consequents = c("Forum", "Quiz"),  precedence_type = "eventually_follows", filter_method = "all") %>%  traces
#PPT follows Forum and Quiz both directly or later
events1 %>%  filter_precedence(antecedents = "PPT", consequents = c("Forum", "Quiz"),  precedence_type = "eventually_follows", filter_method = "all") %>%   process_map()

#PPT follows - Quiz, Assignment, or Game---
events1 %>%  filter_precedence(antecedents = "PPT", consequents = c("Quiz", "Assignment",'Game'),precedence_type = "eventually_follows", filter_method = "one_of") %>%  process_map()

#PPT does not follow Quiz, Assignment or Game
events1 %>%  filter_precedence(antecedents = "PPT", consequents = c("Quiz", "Assignment",'Game'),precedence_type = "eventually_follows", filter_method = "none") %>%  process_map()


#Select 80% of the cases that share the most common traces
events1 %>%  filter_trace_frequency(percentage = 0.8) %>% n_cases()
events1 %>%  filter_trace_frequency(percentage = 0.8) %>% process_map()
events1 %>%  filter_trace_frequency(percentage = 0.5) %>% n_cases()

#Or the cases of which the trace frequency is less than 25.
events1 %>%  filter_trace_frequency(interval = c(0,25)) %>%   n_cases()
#????

#Time Period---

events1 %>%  filter_time_period(interval = ymd(c(20200210, 20200220)), filter_method = "start") %>% dotted_chart
events1 %>%  filter_time_period(interval = ymd(c(20200210, 20200220)), filter_method = "complete") %>% dotted_chart
events1 %>%  filter_time_period(interval = ymd(c(20200210, 20200220)), filter_method = "contained") %>% dotted_chart
events1 %>%  filter_time_period(interval = ymd(c(20200210, 20200220)), filter_method = "intersecting") %>% dotted_chart 
events1 %>%  filter_time_period(interval = ymd(c(20200210, 20200220)), filter_method = "start") %>% dotted_chart(units='weeks')
events1 %>%  filter_time_period(interval = ymd(c(20200210, 20200220)), filter_method = "start") %>% dotted_chart(units='hours')
events1 %>%  filter_time_period(interval = ymd(c(20200210, 20200220)), filter_method = "start") %>% dotted_chart(units='hours', sort='end')

?dotted_chart.grouped_eventlog


#graphs-----
processmapR::resource_matrix(events1) %>% plot 
processmapR::resource_map(events1)
processmapR::trace_explorer(events1) 
processmapR::trace_explorer(events1, .abbreviate=FALSE) 
processmapR::trace_explorer(events1, coverage=.5, raw_data = TRUE)
processmapR::trace_explorer(events1, coverage=.5)
processmapR::trace_explorer(events1, n_traces=15)
processmapR::trace_explorer(events1, n_traces=15, type='frequent')
processmapR::trace_explorer(events1, n_traces=15, type='infrequent')

?trace_explorer
