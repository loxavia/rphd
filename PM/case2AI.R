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

df3 = as.data.frame(gsheet2tbl(link))
head(df3)
df3
names(df3)
str(df3)
events3a <- bupaR::simple_eventlog(eventlog = df2, case_id = 'case', activity_id = 'activity', timestamp = 'timestamp')
events3a
events3a %>% process_map()

#activity instance
df3b <- df3 %>% group_by(case, activity) %>% summarise(count=n()) %>% tibble::rowid_to_column( "act_inst") %>% select(-c(count)) %>% merge(df3)

#there is no lifecycle
df3c <- df3b %>% mutate(lifecycle = NA)

events3b <- bupaR::eventlog(eventlog=df3c, case_id='case', activity_id ='activity', activity_instance_id= 'act_inst', lifecycle_id='lifecycle', timestamp = 'timestamp', resource_id ='resource')
#warning - one resource alloted to more than 1 activity. I can happen

events3b %>% process_map()
events3b %>% process_map(sec=frequency('relative'))

#epm2 <- events2 %>% process_map(render=F, sec=frequency('relative'))
#epm2
#export_graph(epm1, 'E:/PMC/epm2.png',file_type = 'PNG')
#events2 %>% process_map(render = F) %>%  DiagrammeR::export_graph(file_name = 'E:/PMC/epm2.png', file_type = 'PNG')

events3b %>% activities()
#activity Freq
events3b %>% activity_frequency()
events3b %>% end_activities()
events3b %>% start_activities()

events3b %>% filter_activity(activities='surgery')
events3b %>% filter_activity_frequency(interval=c(1,2))
events3b %>% filter_activity_frequency(interval=c(6,NA))

events3b %>% filter_activity_instance(activity_instances = c(1))
#Calculates for each activity type in what percentage of cases it is present.
events3b %>% filter_activity_presence(activities = c('surgery'))

#trace
events3b %>% trace_coverage(level='case')
events3b %>% trace_coverage(level='trace')
events3b %>% trace_coverage(level='log')
events3b %>% trace_coverage(level='log', append=T) 
events3b %>% trace_coverage(level='trace') %>% plot()

events3b %>% trace_length()
events3b %>% number_of_traces()
events3b %>% number_of_repetitions()

events3b %>% number_of_selfloops()

events3b %>% trace_explorer()
events3b %>% trace_explorer(coverage=1)

events3b %>% trace_explorer(coverage=.5)
events3b %>% trace_explorer(coverage=.2)
#frequent = .8
names(events3b)
events3b %>% filter_trace_frequency(percentage = .80) %>% throughput_time(unit='hours')
events3b %>% filter_trace_frequency(percentage = .80) %>% throughput_time(unit='hours')

events3b %>% precedence_matrix() %>% plot()
events3b %>% filter_activity_frequency(percentage = .8) %>% precedence_matrix() %>% plot()
events3b %>% filter_trace_frequency(percentage = .4) %>% precedence_matrix() %>% plot()
events3b %>% filter_trace_frequency(percentage = .5) %>% precedence_matrix() %>% plot()

events3b %>% animate_process(duration=10, units='seconds' )

#now work it work. from activity instance it finds time

events3b %>% processing_time()
events3b %>% trace_length()
events3b %>% filter_case(case='Dhiraj') %>% trace_length()
events3b %>% filter_case(case='Dhiraj') %>% process_map()

events3b %>% filter_case(case='Upen') %>% trace_length()
events3b %>% filter_case(case='Upen') %>% process_map()
events3b %>% filter_case(case='Upen', reverse=T) %>% process_map()

?filter_case_condition
events3b %>% throughput_time()

events3b %>% 
?renderer_graphviz
