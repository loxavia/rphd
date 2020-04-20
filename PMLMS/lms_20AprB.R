#activity Log
pacman::p_load(bupaR, processcheckR)
sepsis %>% check_rule(starts('ER Registration'), label='r1')

#cont'd from lms_20AprA
## activitylog(activitylog, case_id, activity_id, resource_id, lifecycle_ids)
# activitylog	 :The data object to be used as activity log. This can be a data.frame or tbl_df.
# case_id	:The case classifier of the activity log. A character vector containing variable names of length 1 or more.
# activity_id :The activity classifier of the activity log. A character vector containing variable names of length 1 or more.
# # resource_id	:The resource identifier of the activity log. A character vector containing variable names of length 1 or more.
# # lifecycle_ids :The columns with timestamps refering to different lifecycle events. A character vector of 1 or more. These should have one of the folliwing names: "schedule","assign","reassign","start","suspend","resume","abort_activity","abort_case","complete","manualskip","autoskip". These columns should be of the Date or POSIXct class.

events2
head(sepsis)





example_log_4
names(events2)
head(lmsData3)
head(lmsData3B)
table(lmsData3B$user) #case
table(lmsData3B$time) #timestamp
table(lmsData3B$activity) #activity - Names
table(lmsData3B$eventName) #activity Descp
table(lmsData3B$activityType) #activity Type
table(lmsData3B$resource) #resource
table(lmsData3B$lifecycle)  #lifecycle

lmsData3B
names(lmsData3B)
lmsData3B %>% mutate(activity = trimws(activity))  %>% group_by(user, activity, resource) %>% summarise(start=last(timestamp), end =first(timestamp)) %>% mutate(durActivity = difftime(end, start, units='mins')) %>% arrange(-durActivity) %>% select(c(user, activity, resource, start, end, durActivity)) 

lmsData3C <- lmsData3B %>% mutate(activity = trimws(activity)) %>% group_by(user, activity, resource) %>% summarise(start=last(timestamp), complete = first(timestamp)) %>% select(c(user, activity, resource, start, complete)) 
lmsData3C[1:5, c(1,2,3,4,5)]
nrow(lmsData3C)
head(lmsData3C)
lmsData3C$activity_instance = seq(1, nrow(lmsData3C))
lmsData3D <- lmsData3C %>% reshape2::melt(id.vars=c('user','activity','resource', 'activity_instance'), variable.name ='lifecycle', value.name ='timestamp')
table(lmsData3D$lifecycle)
head(lmsData3D)
class(lmsData3D)
str(lmsData3D)
lmsData3D$lifecycle = as.character(lmsData3D$lifecycle)
table(lmsData3D$lifecycle)
names(lmsData3D)

?eventlog
eventLog1 <- eventlog(eventlog = lmsData3D, case_id='user', activity_id='activity', resource_id = 'resource', lifecycle_id = 'lifecycle', activity_instance_id = 'activity_instance', timestamp = 'timestamp')
head(eventLog1)

?events_to_activitylog
?activitylog
activityLog1 <- events_to_activitylog(eventlog=eventLog1, case_id='user', activity_id='activity', activity_instance_id = 'activity_instance', lifecycle_id='lifecycle', timestamp='timestamp', resource_id='resource')
head(activityLog1)
head(activityLog1)

eventLog1 %>% activities()
eventLog1 %>% check_rule(starts('Page'), label='r1')
eventLog1 %>% check_rule(starts('Page'),label='r1') %>% check_rule(and('PPT','MindMaps'),label='r2') %>% group_by(r1, r2) %>% n_cases()

eventLog1 %>% check_rule(absent(activity=c("PPT"),n=0), label='PPT_0') %>% group_by(PPT_0) %>% n_cases() #no PPT

?detect_activity_frequency_violations(activitylog = eventLog1, 'PPT'=1)

eventLog1 %>% check_rule(contains("PPT",n=3), label='PPT_3') %>% group_by(PPT_3) %>% n_cases()
