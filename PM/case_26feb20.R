#sample Case - PM

library(gsheet)
library(bupaR)
library(dplyr)
library(edeaR)
library(processmapR)
library(processanimateR)

link='https://docs.google.com/spreadsheets/d/139d4RD4wnf4Qvp2mg2DCMWCOOemmiQM1NA9K7vn5Ak0/edit#gid=1506524608'

df = as.data.frame(gsheet2tbl(link))
head(df)
df
names(df)
str(df)
event <- bupaR::eventlog(eventlog=df, case_id='case', activity_id ='activity', activity_instance_id= 'act_inst', lifecycle_id='lifecycle', timestamp = 'time', resource_id ='resource')
event

event %>% process_map()
n_activities(event)
n_cases(event)
resource_map(event)
dotted_chart(event)
precedence_matrix(event)
precedence_matrix(event) %>% plot()
process_matrix(event)  %>% plot()
trace_explorer(event)
animate_process(event, duration=10)


#assigning activity_instance
head(df)
df1 = df
df2 <- df1 %>% group_by(case,activity) %>% summarise(count=n()) %>% tibble::rowid_to_column("act_inst") %>% select(-c(count)) %>% merge(df1)
df2

event %>% process_map(type = frequency("absolute"))
event %>% process_map(type = frequency("relative"))

#------------dfnew-----
link2='https://docs.google.com/spreadsheets/d/139d4RD4wnf4Qvp2mg2DCMWCOOemmiQM1NA9K7vn5Ak0/edit#gid=2038410307'

newdf = as.data.frame(gsheet2tbl(link2))
head(newdf)
newdf
names(newdf)
str(newdf)
newdf %>% arrange(time) %>% group_by(activity, case) %>% summarise(count=n(), ftime = first(time), ltime=last(time)) %>% arrange(ftime) %>% tibble::rowid_to_column("act_inst") %>% select(-c(count)) %>% merge(newdf) %>% arrange(act_inst)

newdf2 <- newdf %>% arrange(time) %>% group_by(activity, case) %>% summarise(count=n(), ftime = first(time), ltime=last(time)) %>% arrange(ftime) %>% tibble::rowid_to_column("act_inst") %>% select(-c(count)) %>% merge(newdf) %>% arrange(act_inst) %>% select(time, case, activity, act_inst, lifecycle, resource)
newdf2

event2 <- bupaR::eventlog(eventlog=newdf2, case_id='case', activity_id ='activity', activity_instance_id= 'act_inst', lifecycle_id='lifecycle', timestamp = 'time', resource_id ='resource')
event2

event2 %>% process_map()
n_activities(event2)
n_cases(event2)
resource_map(event2)
dotted_chart(event2)
precedence_matrix(event2)
precedence_matrix(event2) %>% plot()
process_matrix(event2)  %>% plot()
trace_explorer(event2)
animate_process(event2, duration=10)

event2 %>% process_map(type = frequency("absolute"))
event2 %>% process_map(type = frequency("relative"))

#activities during full time
event2 %>%  filter_processing_time(percentage = 1)
#activities which took 50% of time
event2 %>%  filter_processing_time(percentage = .5)  %>% processing_time(units = "hours")
#case where trace length between 1 to 3 activities
event2 %>%  filter_trace_length(interval = c(1, 3)) %>% process_map()
event2 %>%  filter_trace_length(interval = c(1, 3)) %>% trace_length(units = "hours")
event2 %>%  filter_trace_length(percentage = 1) %>%  trace_length()
event2 %>%   filter_time_period(interval = ymd(c(20170609, 20170630)), filter_method = "start") %>% n_cases

n_distinct(event2$activity)
activity_labels(event2)
activities(event2)
processing_time(event2, unit='hours')
processing_time(event2, level="log", units="days")
?processing_time
processing_time(event2, level="case", units="hours")
processing_time(event2, level="resource", units="hours")
processing_time(event2, level="activity", units="hours")
processing_time(event2, level="trace", units="hours")
processing_time(event2, level="resource-activity", units="hours")
processing_time(event2, level="activity", units="hours", append_column = T)



#28 Feb 20-----
event2
names(event2)
?distinct
event2 %>% select(case, activity) %>% head()
distinct(event2, case)
event2 %>% dplyr::distinct(case, activity) %>%  select(activity) %>%  dplyr::group_by_all %>%   summarize(metric=n())

#Activity - in how many cases
event2 %>% as.data.frame() %>% distinct(case, activity)  %>% select(activity) %>% group_by_all() %>% summarise(metric = n())
#Activity used in each case how many times
event2 %>% select(case, activity, time) %>%  group_by(case,activity) %>% summarize(metric=n())
#max time an activity was used
event2 %>% select(case, activity, time) %>%  group_by(case,activity) %>% summarize(metric=n()) %>% group_by(activity) %>% summarise(metric = max(metric))
#duration
duration(event2)
bupaR::durations(event2)
?bupaR
#activities reoccurred
number_of_repetitions(event2, level="activity", type="all")
#Which activities reoccurred consecutively in a case where the same resource repeated the activities?
number_of_repetitions(event2, level="activity", type="repeat")
#Which cases had activities duplicated? The duplicated activities were interrupted by other activities and these duplicated activities were redone by a different resource.
number_of_selfloops(event2, level="case", type = "redo")


#for each day, which resource was used how many times
event2 %>% as.data.frame() %>% mutate(Date= as.Date(time)) %>% mutate("ID_day" = group_indices_(., .dots = c("resource","Date"))) %>% group_by(ID_day) %>% arrange(ID_day, time) %>% select(resource, ID_day, case) %>% group_by(case, ID_day, resource) %>% summarise(metric = n())
#similar for activity
event2 %>% as.data.frame() %>% mutate(Date= as.Date(time)) %>% mutate("ID_day" = group_indices_(., .dots = c("activity","Date"))) %>% group_by(ID_day) %>% arrange(ID_day, time) %>% select(activity, ID_day, case) %>% group_by(case, ID_day, activity) %>% summarise(metric = n())

#count of each resource
event2 %>% as.data.frame() %>% add_count(resource)
event2 %>% as.data.frame() %>% add_count(activity)
event2 %>% as.data.frame() %>% group_by(case) %>% add_count(activity) %>% select(case, activity, time, n)

