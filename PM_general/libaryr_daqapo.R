#daqapo
#devtools::install_github("nielsmartin/daqapo")
library(daqapo)

data("hospital_actlog")
hospital_actlog

class(hospital_actlog)
names(hospital_actlog)
table(hospital_actlog$activity)
detect_activity_frequency_violations(activitylog = hospital_actlog,   "Registration" = 1, "Clinical exam" = 1)

#---
lmsActivityLog <- lmsData3 %>% rename('complete' = time) %>% activitylog(case_id='user', activity_id='context2', resource_id='component', lifecycle_ids='complete')
detect_activity_frequency_violations(activitylog = lmsActivityLog,   "PPT" = 10, "Quiz" = 20)
detect_activity_frequency_violations(activitylog = lmsActivityLog, "Quiz" = 40)

#------
detect_activity_order_violations(activitylog = hospital_actlog, activity_order = c( "Registration", "Triage", "Clinical exam", "Treatment", "Treatment evaluation"))
lmsActivityLog %>% precedence_matrix()  #not working
activity_labels(lmsActivityLog)

detect_activity_order_violations(activitylog = lmsActivityLog, activity_order = c( "PPT", "Assignment", "Quiz"), timestamp='complete')
#These did not follow the sequence above
?daqapo
#------
detect_attribute_dependencies(activitylog = hospital_actlog,  antecedent = activity == "Registration",consequent = startsWith(originator,"Clerk"))
names(hospital_actlog)
names(lmsActivityLog)
head(lmsActivityLog)
detect_attribute_dependencies(activitylog = lmsActivityLog,  antecedent = context2 == "Quiz",consequent = startsWith(eventName2,"Viewed"))
#!events with activity Quiz and lifecycle = Viewed

#------
?detect_case_id_sequence_gaps(activitylog = hospital_actlog)
detect_case_id_sequence_gaps(activitylog = lmsActivityLog)
#case id should be numeric
#------
names(hospital_actlog)
detect_conditional_activity_presence(activitylog = hospital_actlog,  condition = specialization == "TRAU",activities = "Clinical exam")
head(lmsActivityLog)
table(lmsActivityLog$context2, lmsActivityLog$eventName2)
lmsActivityLog %>% group_by(context2,eventName2) %>% summarise(n=n(), cases =length(unique(user))) %>% filter(context2 == 'Attendance') 

detect_conditional_activity_presence(activitylog = lmsActivityLog,  condition = eventName2 == "Updated", activities = "Attendance" )

#------
names(hospital_actlog)
detect_duration_outliers(activitylog = hospital_actlog,Treatment = duration_within(bound_sd = 1))
table(hospital_actlog$activity)
detect_duration_outliers(activitylog = lmsActivityLog, Quiz = duration_within(bound_sd = 1))
#probably start and end time
#------

detect_incomplete_cases(activitylog = hospital_actlog,  activities = c("Registration","Triage","Clinical exam","Treatment","Treatment evaluation"))
detect_incomplete_cases(activitylog = lmsActivityLog,  activities = c("PPT","Quiz","Assignment"))


#------

detect_incorrect_activity_names(activitylog = hospital_actlog,  allowed_activities = c( "Registration","Triage", "Clinical exam", "Treatment","Treatment evaluation"))
table(lmsActivityLog$context2)
detect_incorrect_activity_names(activitylog = lmsActivityLog,  allowed_activities = c("PPT","Quiz",'Attendance', "Assignment",'Checklist','Course','Folder','Forum','Glossary', 'Survey'))  #URL not put
#which events do not have the activities


#------
detect_missing_values(activitylog = hospital_actlog)
detect_missing_values(activitylog = lmsActivityLog) #none missing value

detect_missing_values(activitylog = hospital_actlog, level_of_aggregation = "activity")
summary(hospital_actlog)
detect_missing_values(activitylog = hospital_actlog, level_of_aggregation = "column",  column = "triagecode")
#------
detect_multiregistration(activitylog = hospital_actlog, threshold_in_seconds = 10)

detect_multiregistration(activitylog = lmsActivityLog, threshold_in_seconds = 5, timestamp = 'complete', filter_condition = eventName2 == "Updated", details=T)


#------
detect_overlaps(activitylog = hospital_actlog)
detect_overlaps(activitylog = lmsActivityLog) #duration

#------
detect_related_activities(activitylog = hospital_actlog,antecedent = "Treatment evaluation",  consequent = "Treatment")
detect_related_activities(activitylog = lmsActivityLog, antecedent = "PPT",  consequent = "Assignment") #ppt -> assignment
lmsActivityLog %>% filter( user =='Amit Sahgal') %>% as.data.frame()
events2 %>% filter( user =='Amit Sahgal') %>% trace_explorer()

hospital_actlog %>% filter_case(cases=529)
#------
detect_similar_labels(activitylog = hospital_actlog,column_labels = "activity", max_edit_distance = 3)

detect_similar_labels(activitylog = lmsActivityLog, column_labels = "context2", max_edit_distance = 4)

#------
detect_time_anomalies(activitylog = hospital_actlog)
detect_time_anomalies(activitylog = lmsActivityLog) #duration

#------
detect_unique_values(activitylog = hospital_actlog,column_labels = "activity")
detect_unique_values(activitylog = hospital_actlog,column_labels = c("activity", "originator"))
?detect_unique_values(activitylog = lmsActivityLog,column_labels = "context2")
#------------------------------------------

#names(events2)
names(events2[5]) = 'activity'
detect_activity_frequency_violations(activitylog = lmsActivityLog,  "PPT" = 1, "Quiz" = 1)


names(lmsData3)
head(lmsData3)
names(hospital_actlog)
lmsData4 <- lmsData3 %>% rename( activity= context2)
events3 <- lmsData4 %>% mutate(activity_instance_id = 1:nrow(.)) %>% eventlog(case_id = 'user', activity_id = 'activity', timestamp = 'time', activity_instance_id = 'activity_instance_id', lifecycle_id= 'eventName2', resource='component' )
events3
detect_activity_frequency_violations(activitylog = events3,  "PPT" = 1)  

detect_case_id_sequence_gaps(activitylog = hospital_actlog)

detect_conditional_activity_presence(activitylog = hospital_actlog,
                                     condition = specialization == "TRAU",
                                     activities = "Clinical exam")

detect_duration_outliers(activitylog = hospital_actlog,
                         Treatment = duration_within(bound_sd = 1))

detect_inactive_periods(activitylog = hospital_actlog,threshold = 30)
detect_incomplete_cases(activitylog = hospital_actlog,
                        activities = c("Registration","Triage","Clinical exam","Treatment","Treatment evaluation"))
detect_incorrect_activity_names(activitylog = hospital_actlog,
                                allowed_activities = c(
                                  "Registration",
                                  "Triage",
                                  "Clinical exam",
                                  "Treatment",
                                  "Treatment evaluation"))

detect_missing_values(activitylog = hospital_actlog)
detect_missing_values(activitylog = hospital_actlog, level_of_aggregation = "activity")
detect_missing_values(activitylog = hospital_actlog, level_of_aggregation = "column",    column = "triagecode")
detect_multiregistration(activitylog = hospital_actlog, threshold_in_seconds = 10)
detect_overlaps(activitylog = hospital_actlog)


detect_related_activities(activitylog = hospital_actlog,  antecedent = "Treatment evaluation",  consequent = "Treatment")
detect_similar_labels(activitylog = hospital_actlog,  column_labels = "activity",  max_edit_distance = 3)

detect_time_anomalies(activitylog = hospital_actlog)

detect_unique_values(activitylog = hospital_actlog, column_labels = "activity")
detect_unique_values(activitylog = hospital_actlog, column_labels = c("activity", "originator"))
detect_value_range_violations(activitylog = hospital_actlog,  triagecode = domain_numeric(from = 0, to = 5))

#domain_numeric(from, to)
#domain_time(from, to, format = ymd_hms)
#duration_within(bound_sd = 3, lower_bound = NA, upper_bound = NA)
#filter_anomalies(activity_log, anomaly_log)
#fix(detected_problems, ...)
#dataset
hospital #A data frame with 53 rows and 7 variables:
hospital_actlog #An activity log of 20 patients in a hospital (activity log object)
hospital_events #A data frame with 53 rows and 7 variables:

head(hospital)
head(hospital_actlog)
head(hospital_events)
detect_missing_values(activitylog = hospital_actlog)
detect_overlaps(activitylog = hospital)
#The activity log
str(hospital)
str(hospital_actlog)
