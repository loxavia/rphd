#PM 
# library 
pacman::p_load(gsheet, googlesheets4, ggplot2, dplyr, DiagrammeR,lubridate) 
pacman::p_load(bupaR, edeaR, processmapR, processanimateR, processcheckR) 
glink1 = "https://docs.google.com/spreadsheets/d/1Rv-8pDWMeonmjn7LCZh55MJtIh0XG5vHX-mIZpcGf1c"
googlesheets4::sheets_sheets(glink1)

df1 = as.data.frame(googlesheets4::read_sheet(glink1, sheet='E5'))
head(df1) ; dim(df1)
names(df1)

event1 <- events_to_activitylog(df1, case_id = 'case', activity_id = 'activity', activity_instance_id = 'activity', lifecycle_id = 'activity' , timestamp = , resource_id = 'activity')
#http://ceur-ws.org/Vol-1527/paper18.pdf
# Attribute Description
# case id The case to which the event belongs.
# activity id The activity the event refers to.
# activity instance id The activity instance the event belongs to.
# lifecycle id The stage in the transactional life cylce.
# timestamp The timestamp of the event.

#sheet-sample2

sample3 = as.data.frame(read_sheet(glink1, sheet='sample3'))
str(sample3)
sample3$timestamp = as.POSIXct(sample3$timestamp, format="%m/%d/%Y %H:%M:%S")
events3 <- eventlog(sample3,  case_id = "case", activity_id = "activity", activity_instance_id = "activity_instance", lifecycle_id = "activity_instance", timestamp = "timestamp", resource_id = "resource_id") 
#activity_instance_id = new incrementing integer, lifecycle_id #new column with Start/End strings: resource_id #new column with userID doing activity 
class(events3)
table(events3$lifecycle_transition)
n_events(events3)
n_activity_instances(events3)
table(events3$activity)
case_id(events3)
activity_id(events3)
activity_instance_id(events3)
lifecycle_id(events3)
timestamp(events3)


#import from csv file
sample4 = as.data.frame(read_sheet(glink1, sheet='sample4'))
sample4
sample4$activity_instance <- 1:nrow(sample4)
sample4
sample4b <- gather(sample4, lifecycle, timestamp , -case, - activity, -activity_instance)
head(sample4b)
table(sample4b$lifecycle)
sample4b$lifecycle <- factor(sample4b$lifecycle, labels = c("start","complete"))
str(sample4b)
sample4b$resource = 'web'
sample4b$timestamp <- dmy_hms(sample4b$timestamp)  #format

events4 <- eventlog(eventlog = sample4b,  case_id = "case",    activity_id = "activity",  activity_instance_id = "activity_instance",lifecycle_id = "lifecycle",     timestamp = "timestamp", resource_id = 'resource', order='case')
events4
events4 %>% mapping

events4 %>% number_of_selfloops("all")
events4 %>% summary()
number_of_selfloops(events4,'all')
number_of_selfloops(events4,'redo')  #repeat
number_of_selfloops(events4,'repeat')
events4 %>% number_of_selfloops("all") %>% ggplot() +  geom_histogram(aes(absolute))  #error
last_n(events4,3)
#for grouped events
events4 %>% group_by_case()
events4 %>% group_by_resource()
events4 %>% group_by_activity()
first_n(events4,2)  #of each case

#Computes the throughput times of each case. Throughput time is defined as the interval between the start of the first event and the completion of the last event
durations(events4)
durations(events4, units='hours')
case_list(events4)
#Retrieve a vector containing all unique case labels
case_labels(events4)
case_id(events4)
cases(events4)
act_collapse(events4)
activity_labels(events4)
lifecycle_id(events4)
lifecycles(events4)
lifecycle_ids(events4)
act_recode(events4, "AA"="A")
#Recode two or different more activity labels two a uniform activity label
act_unite(events4, "CD"= c("C","D"))  #temp
events4
add_end_activity(events4, label='Z')
add_start_activity(events4, label='start')
add_start_activity(events4, label='start') %>% as.data.frame %>% arrange(timestamp())
events4
events4 %>% eventlog(order='timestamp')
events4 %>% eventlog(order='case')

#a fine-grained summary of an event log with characteristics for each case: the number of events, the number of activity types, the timespan, the trace, the duration and the first and last event type.

cases(events4)
#Construct list of cases
case_list(events4)

#Function converting the timestamps in the data frame to the appropriate format.convert_timestamps(x, columns, format = ymd_hms)
#Function to detect inconsistencies in resource information between related events
detect_resource_inconsistencies(eventlog, filter_condition)


#sample data
## Not run:
data <- data.frame(case = rep("DU",6),   activity_id = c("PPT","Ebook","Assignment","Video","Quiz",'Quiz'), activity_instance_id = c(1:5,6),    lifecycle_id = c(rep("complete",5),'reattempt'),  timestamp = Sys.Date() + 1:6,  resource = c('T1','T2','T2','T4','T3','T6'))
data
events5 <- eventlog(data,case_id = "case", activity_id = "activity_id",
activity_instance_id = "activity_instance_id",lifecycle_id = "lifecycle_id",  timestamp = "timestamp", resource_id = "resource")
events5

#Select first n activity instances
first_n(events5, 3)
group_by_activity(events5)
group_by_case(events5)
group_by_resource(events5)
group_by_resource_activity(events5)
lifecycles(events5)
n_activities(events5)
n_activity_instances(events5) 
n_cases(events5)
n_events(events5)
n_resources(events5)
n_traces(events5)

#re_map(eventlog2, mapping) #mapping 
slice_activities(events5, 1)
slice_activities(events5, 2)
slice_activities(events5, 3)
slice_activities(events5, 4)
slice_activities(events5, 5)
slice_activities(events5, 6)
slice_activities(events5, 1:3)

slice_events(events5, 1:2)
slice_events(events5, 3)
traces(events5)

#links

