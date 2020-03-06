#PM - Patient Case - Simple

#patients - John Doe

install.packages("bupaR","edeaR", "eventdataR","processmapR","processmonitR","xesreadR","petrinetR")
library(bupaR); library(eventdataR);
library(edeaR);library(processanimateR); library(processmapR) ; library(processmonitR) ;library(lubridate)
library(gsheet); library(dplyr)

link ='https://docs.google.com/spreadsheets/d/1Rv-8pDWMeonmjn7LCZh55MJtIh0XG5vHX-mIZpcGf1c/edit#gid=30764335'

data = as.data.frame(gsheet2tbl(link))
names(data)
head(data)
#dput(data)

eventlog <- data %>%
  eventlog(
    case_id = "patient",
    activity_id = "activity",
    activity_instance_id = "activity_instance",
    lifecycle_id = "status",
    timestamp = "timestamp",
    resource_id = "resource"
  )
#warning -ok
head(eventlog)
mapping(eventlog)
#summary
eventlog %>% summary()
activities(eventlog)
cases(eventlog)
resources(eventlog)
traces(eventlog)
case_id(eventlog)
lifecycles(eventlog)

#mapping
mapping(eventlog)
activity_id(eventlog)
activity_instance_id(eventlog)
case_id(eventlog)
lifecycle_id(eventlog)
resource_id(eventlog)
timestamp(eventlog)

#number/ count
n_activities(eventlog)
n_activity_instances(eventlog)
n_cases(eventlog)
n_events(eventlog)
n_resources(eventlog)
n_traces(eventlog)

#misc
slice(eventlog, 1:2) #case
sample_n(eventlog, 1)  #case wise
mutate(eventlog, newtime = timestamp + 120)
group_by(eventlog, status)  %>% summarise(count = n(), meanTime = mean(timestamp))

#EDA
activity_frequency(eventlog)
activity_frequency(eventlog, level='activity')
activity_presence(eventlog)
end_activities(eventlog)
start_activities (eventlog)
idle_time(eventlog)
number_of_repetitions(eventlog)
number_of_selfloops(eventlog)
number_of_traces(eventlog)
processing_time(eventlog)
resource_frequency(eventlog)
resource_specialisation(eventlog)
resource_involvement(eventlog)
size_of_repetitions (eventlog)
size_of_selfloops (eventlog)
throughput_time(eventlog)
trace_coverage(eventlog)
trace_length(eventlog)

#dottedchart
eventlog %>% dotted_chart()

#subsetting 
ifilter_activity(eventlog)
names(eventlog)


eventlog %>% process_matrix() %>% plot()
eventlog %>% process_matrix(type=performance()) %>% plot()
eventlog %>% precedence_matrix(type='absolute') %>% plot()
eventlog %>% precedence_matrix(type='relative') %>% plot()
eventlog %>% precedence_matrix(type='relative-antecedent') %>% plot()
eventlog %>% precedence_matrix(type='relative-consequent') %>% plot()
eventlog %>% precedence_matrix(type='relative-case') %>% plot()

#processmap
eventlog %>% process_map(type=frequency('relative'))
eventlog %>% process_map(type=frequency('absolute'))

eventlog %>% process_map(type=frequency('absolute'), rankdir='TB')
eventlog %>% process_map(type=frequency('absolute'), rankdir='BT')
eventlog %>% process_map(type=frequency('absolute'), rankdir='RL')


eventlog %>% process_map(type=frequency('absolute'),fixed_edge_width=T)
eventlog %>% process_map(type=frequency('absolute'),fixed_edge_width=F)

#resources-----
eventlog %>% resource_map(type = frequency("absolute"))
eventlog %>% resource_map(type = frequency("relative"))
eventlog %>% resource_matrix(type = "absolute") %>% plot()
eventlog %>% resource_matrix(type = "relative")     
eventlog %>% resource_matrix(type = "relative_antecedent")     
eventlog %>% resource_matrix(type = "relative_consequent")     

?resource_matrix
#trace explorere-----
trace_explorer(eventlog, coverage = NULL, n_traces = NULL,
type = c("frequent", "infrequent"), .abbreviate = T, show_labels = T, scale_fill = scale_fill_discrete(h = c(0, 360) + 15, l = 40),raw_data = F)
eventlog %>% trace_explorer()
eventlog %>% trace_explorer(.abbreviate = F)
eventlog %>% trace_explorer(n_traces = 1, type='frequent')
eventlog %>% trace_explorer(show_labels = F, raw_data = F)
?process_map
eventlog %>% animate_process(duration=10, mode='absolute', repeat_count = 1)
eventlog %>% animate_process(duration=5, mode='relative', repeat_count = 2, initial_state = 'paused')

?animate_process
#Generating Data----------------------------------
structure(list(patient = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = "John Doe", class = "factor"), activity = structure(c(1L, 3L, 3L, 3L, 4L, 4L, 3L, 3L, 2L), .Label = c("check-in", "check-out", "surgery", "treatment"), class = "factor"), timestamp = structure(1:9, .Label = c("2017-05-10 08:33:26","2017-05-10 08:38:21", "2017-05-10 08:53:16", "2017-05-10 09:25:19",    "2017-05-10 10:01:25", "2017-05-10 10:35:18", "2017-05-10 10:41:35",  "2017-05-10 11:05:56", "2017-05-11 14:52:36"), class = "factor"),  status = structure(c(1L, 2L, 3L, 1L, 3L, 1L, 3L, 1L, 1L), .Label = c("complete", "schedule", "start"), class = "factor"), activity_instance = c(1L, 2L, 2L, 2L, 3L, 3L, 4L, 4L, 5L), resource = structure(c(3L, 1L, 2L, 2L, 1L, 1L, 4L, 4L, 3L), .Label = c("Danny", "Richard", "Samantha", "William"), class = "factor")), class = "data.frame", row.names = c(NA, -9L))
