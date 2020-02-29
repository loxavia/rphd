#understanding terms
#Case, Event, Activity, Resource, Trace, Duration

library(gsheet)
library(bupaR)
library(dplyr)
library(edeaR)
library(processmapR)
library(processanimateR)
library(DiagrammeR)

link='https://docs.google.com/spreadsheets/d/1Rv-8pDWMeonmjn7LCZh55MJtIh0XG5vHX-mIZpcGf1c/edit#gid=792991636'

df = as.data.frame(gsheet2tbl(link))
head(df)
df
names(df)
str(df)
events1 <- bupaR::simple_eventlog(eventlog = df, case_id = 'case', activity_id = 'activity', timestamp = 'timestamp')
events1

events1 %>% process_map()
events1 %>% process_map(sec=frequency('relative'))

epm1 <- events1 %>% process_map(render=F, sec=frequency('relative'))
epm1
export_graph(epm1, 'epm1.png',file_type = 'png')

events1 %>% process_map(render = F) %>%  DiagrammeR::export_graph(file_name = 'E:/PMC/epm1.png', file_type = 'PNG')



events1 %>% activities()
#activity Freq
events1 %>% activity_frequency()
events1 %>% end_activities()
events1 %>% start_activities()

events1 %>% filter_activity(activities='surgery')
events1 %>% filter_activity_frequency(interval=c(1,2))
events1 %>% filter_activity_instance(activity_instances = c(1))
#Calculates for each activity type in what percentage of cases it is present.
events1 %>% filter_activity_presence(activities = c('surgery'))

#trace
?trace_coverage
events1 %>% trace_coverage(level='case')
events1 %>% trace_coverage(level='trace')
events1 %>% trace_coverage(level='log')
events1 %>% trace_coverage(level='log', append=T) 
events1 %>% trace_coverage(level='trace') %>% plot()

events1 %>% trace_length()
events1 %>% number_of_traces()
events1 %>% number_of_repetitions()
?number_of_repetitions
?number_of_selfloops

events1 %>% number_of_selfloops()

events1 %>% trace_explorer()
events1 %>% trace_explorer(coverage=.5)
events1 %>% trace_explorer(coverage=.2)
#frequent = .8
names(events1)
events1 %>% filter_trace_frequency(percentage = .80) %>% throughput_time(unit='hours')
events1 %>% filter_trace_frequency(percentage = .80) %>% throughput_time(unit='hours')
#group by ???

