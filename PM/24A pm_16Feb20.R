#PM 
# library 
library(plyr)
library(tidyverse)
library(bupaR)
theme_set(theme_light())

library(gsheet)
link = "https://docs.google.com/spreadsheets/d/1Rv-8pDWMeonmjn7LCZh55MJtIh0XG5vHX-mIZpcGf1c/edit#gid=1225207469"

df1 = as.data.frame(gsheet2tbl(link))
df1

class(patients)
head(patients)
class(data)

library(edeaR)
#https://mran.microsoft.com/snapshot/2016-08-30/web/packages/edeaR/vignettes/preprocessing.html
data <- eventlog_from_xes()
?eventdataR
?events_to_activitylog

names(df1)
event1 <- events_to_activitylog(df1, case_id = 'case', activity_id = 'activity', activity_instance_id = 'activity', lifecycle_id = 'activity' , timestamp = nrow(df1), resource_id = 'activity')


#http://ceur-ws.org/Vol-1527/paper18.pdf

# Attribute Description
# case id The case to which the event belongs.
# activity id The activity the event refers to.
# activity instance id The activity instance the event belongs to.
# lifecycle id The stage in the transactional life cylce.
# timestamp The timestamp of the event.

link2 ="https://docs.google.com/spreadsheets/d/1Rv-8pDWMeonmjn7LCZh55MJtIh0XG5vHX-mIZpcGf1c/edit#gid=1747931820"

df2 = as.data.frame(gsheet2tbl(link2))
df2
write.csv(df2, 'eventlog.csv', row.names = F)
str(df2)

data <- eventlog_from_xes()
data <- read_xes()
?read_xes
#activities_to_eventlog(df2, case_id= , activity_id = , resource_id = , timestamps = )
head(df2)
df2$timestamp = as.POSIXct(df2$timestamp, format="%m/%d/%Y %H:%M:%S")
?eventlog
data <- eventlog(df2,
         case_id = "case",
         activity_id = "activity",
         activity_instance_id = "activity_instance", #new incrementing integer
         lifecycle_id = "activity_instance", #new column with Start/End strings
         timestamp = "timestamp",
         resource_id = "resource_id") #new column with userID doing activity 
class(data)
data
table(data$lifecycle_transition)
n_events(data)
n_activity_instances(data)
table(data$activity)

case_id(data)
activity_id(data)
activity_instance_id(data)
lifecycle_id(data)
timestamp(data)

library(lubridate)
data
data[1:4,timestamp(data)]


#import from csv file
link3 = "https://docs.google.com/spreadsheets/d/1Rv-8pDWMeonmjn7LCZh55MJtIh0XG5vHX-mIZpcGf1c/edit#gid=993472323"
df3 = as.data.frame(gsheet2tbl(link3))
df3
#https://mran.microsoft.com/snapshot/2016-08-30/web/packages/edeaR/vignettes/preprocessing.html
df3$activity_instance <- 1:nrow(df3)
df3
#reshape
library(tidyr)
df3b <- gather(df3, lifecycle, timestamp , -case, - activity, -activity_instance)
head(df3b)
df3b
df3b$lifecycle <- factor(df3b$lifecycle, labels = c("start","complete"))
str(df3b)
df3b$resource = 'web'
df3b$timestamp<- dmy_hms(df3b$timestamp)  #format
#A function to instantiate an object of class eventlog by specifying a data.frame or tbl_df and appropriate case, activity and timestamp classifiers.

log <- eventlog(eventlog = df3b,  case_id = "case",    activity_id = "activity",  activity_instance_id = "activity_instance",lifecycle_id = "lifecycle",     timestamp = "timestamp", resource_id = 'resource')
log
eventlog <- log
eventlog %>% mapping

eventlog %>% number_of_selfloops("log")
eventlog %>% summary()
number_of_selfloops(eventlog,'all')
number_of_selfloops(eventlog,'redo')  #repeat
number_of_selfloops(eventlog,'repeat')
eventlog %>% number_of_selfloops("all") %>% ggplot() +  geom_histogram(aes(absolute))  #error
write.csv(df3b, 'd3blog.csv')
last_n(eventlog,3)
df3b
tail(df3b)
eventlog %>% group_by_case()
eventlog %>% group_by_resource()
eventlog %>% group_by_activity()
first_n(eventlog,2)
#Computes the throughput times of each case. Throughput time is defined as the interval between the start of the first event and the completion of the last event
durations(eventlog)
durations(eventlog, units='hours')
case_list(eventlog)
#Retrieve a vector containing all unique case labels
case_labels(eventlog)
case_id(eventlog)
cases(eventlog)
act_collapse(eventlog)
activity_labels(eventlog, method='entry')
#data5 <- read_xes()  #error
lifecycle_id(eventlog)
lifecycles(eventlog)
lifecycle_ids(eventlog)
activity_labels(eventlog)
act_recode(eventlog, "AA"="A")
#Recode two or different more activity labels two a uniform activity label
act_unite(eventlog, "CD"= c("C","D"))
eventlog
add_end_activity(eventlog, label)
add_start_activity(eventlog, start="2015-03-01 01:23:45")
#a fine-grained summary of an event log with characteristics for each case: the number of events, the number of activity types, the timespan, the trace, the duration and the first and last event type.

cases(eventlog)
#Construct list of cases
case_list(eventlog)

#Function converting the timestamps in the data frame to the appropriate format.convert_timestamps(x, columns, format = ymd_hms)
#Function to detect inconsistencies in resource information between related events
detect_resource_inconsistencies(eventlog, filter_condition)


#sample data
## Not run:
data <- data.frame(case = rep("DU",6),   activity_id = c("PPT","Ebook","Assignment","Video","Quiz",'Quiz'), activity_instance_id = c(1:5,6),    lifecycle_id = c(rep("complete",5),'reattempt'),  timestamp = Sys.Date() + 1:6,  resource = rep("resource 1", 6))
data
eventlog2 <- eventlog(data,case_id = "case",
         activity_id = "activity_id",
         activity_instance_id = "activity_instance_id",
         lifecycle_id = "lifecycle_id",
         timestamp = "timestamp",
         resource_id = "resource")


#Select first n activity instances
first_n(eventlog2, 3)
group_by_activity(eventlog2)
group_by_case(eventlog2)
group_by_resource(eventlog2)
group_by_resource_activity(eventlog2)
lifecycles(eventlog2)
n_activities(eventlog2)
n_activity_instances(eventlog2) 
n_cases(eventlog2)
n_events(eventlog2)
n_resources(eventlog2)
n_traces(eventlog2)
#re_map(eventlog2, mapping) #mapping 
slice_activities(eventlog2, 1)
slice_activities(eventlog2, 2)
slice_activities(eventlog2, 3)
slice_activities(eventlog2, 4)
slice_activities(eventlog2, 5)
slice_activities(eventlog2, 6)
slice_events(eventlog2, 1:2)
slice_events(eventlog2, 3)
traces(eventlog2)



#links
https://mran.microsoft.com/snapshot/2016-08-30/web/packages/edeaR/vignettes/preprocessing.html
http://bupar.net/materials/20170904%20poster%20bupaR.pdf
https://www.bupar.net/subsetting.html
