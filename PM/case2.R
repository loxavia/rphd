#understanding terms
#Case, Event, Activity, Resource, Trace, Duration

library(gsheet)
library(bupaR)
library(dplyr)
library(edeaR)
library(processmapR)
library(processanimateR)
library(DiagrammeR)

link='https://docs.google.com/spreadsheets/d/1Rv-8pDWMeonmjn7LCZh55MJtIh0XG5vHX-mIZpcGf1c/edit#gid=609273766'

df2 = as.data.frame(gsheet2tbl(link))
head(df2)
df2
names(df2)
str(df)
events2 <- bupaR::simple_eventlog(eventlog = df2, case_id = 'case', activity_id = 'activity', timestamp = 'timestamp')
events2

events2 %>% process_map()
events2 %>% process_map(sec=frequency('relative'))

#epm2 <- events2 %>% process_map(render=F, sec=frequency('relative'))
#epm2
#export_graph(epm1, 'E:/PMC/epm2.png',file_type = 'PNG')
#events2 %>% process_map(render = F) %>%  DiagrammeR::export_graph(file_name = 'E:/PMC/epm2.png', file_type = 'PNG')


events2 %>% activities()
#activity Freq
events2 %>% activity_frequency()
events2 %>% end_activities()
events2 %>% start_activities()

events2 %>% filter_activity(activities='surgery')
events2 %>% filter_activity_frequency(interval=c(1,2))
events2 %>% filter_activity_frequency(interval=c(6,NA))

events2 %>% filter_activity_instance(activity_instances = c(1))
#Calculates for each activity type in what percentage of cases it is present.
events2 %>% filter_activity_presence(activities = c('surgery'))

#trace
?trace_coverage
events2 %>% trace_coverage(level='case')
events2 %>% trace_coverage(level='trace')
events2 %>% trace_coverage(level='log')
events2 %>% trace_coverage(level='log', append=T) 
events2 %>% trace_coverage(level='trace') %>% plot()

events2 %>% trace_length()
events2 %>% number_of_traces()
events2 %>% number_of_repetitions()
?number_of_repetitions
?number_of_selfloops

events2 %>% number_of_selfloops()

events2 %>% trace_explorer()
events2 %>% trace_explorer(coverage=1)

events2 %>% trace_explorer(coverage=.5)
events2 %>% trace_explorer(coverage=.2)
#frequent = .8
names(events2)
events2 %>% filter_trace_frequency(percentage = .80) %>% throughput_time(unit='hours')
events2 %>% filter_trace_frequency(percentage = .80) %>% throughput_time(unit='hours')
#group by ???
#events2 %>% filter_trace_frequency(percentage = .80) %>% group_by(`(case)_status`) %>% throughput_time(unit='hours')
events2 %>% precedence_matrix() %>% plot()
events2 %>% filter_activity_frequency(percentage = .8) %>% precedence_matrix() %>% plot()
events2 %>% filter_trace_frequency(percentage = .4) %>% precedence_matrix() %>% plot()
events2 %>% filter_trace_frequency(percentage = .5) %>% precedence_matrix() %>% plot()

events2 %>% animate_process(duration=10, units='seconds' )

#simple events, hence these will not work

events2 %>% processing_time()
