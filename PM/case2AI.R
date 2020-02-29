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
df3b1 <- df3b %>% mutate(lifecycle = NA)

events3b <- bupaR::eventlog(eventlog=df3b1, case_id='case', activity_id ='activity', activity_instance_id= 'act_inst', lifecycle_id='lifecycle', timestamp = 'timestamp', resource_id ='resource')
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

events3b %>% group_by_resource() %>% n_cases()
events3b %>% group_by_case() %>% n_cases()
events3b %>% group_by(status) %>%  n_cases()
#group_by_case - group by cases
events3b %>% group_by_activity()  %>% n_cases() # - group by activity types
events3b %>% group_by_resource()  %>% n_cases()
#group_by_resource - group by resources

events3b %>% group_by_activity_resource()  %>% n_cases() #- group by activity resource pair % becos instance not calc correctly
events3b %>% group_by_activity_instance()  %>% n_cases()
#group_by_activity_instance - group by activity instances.

attributes(events3b)
events3b %>% select(activity) %>% group_by_activity() %>% throughput_time(units='hours')
?group_by_activity
#from first 5 events construct trace explorer
events3b %>% group_by_case() %>%  first_n(5) %>% trace_explorer(coverage = 0.95)
events3b %>% group_by_case() %>%  first_n(10) %>% trace_explorer(coverage = 0.95)


#group by activity - resource
#activity instance
df3c <- df3 %>% group_by(case, activity, resource) %>% summarise(count=n()) %>% tibble::rowid_to_column( "act_inst") %>% select(-c(count)) %>% merge(df3)

#there is no lifecycle
df3c1 <- df3c %>% mutate(lifecycle = NA)

events3c <- bupaR::eventlog(eventlog=df3c1, case_id='case', activity_id ='activity', activity_instance_id= 'act_inst', lifecycle_id='lifecycle', timestamp = 'timestamp', resource_id ='resource')

#---
events3b %>% group_by_activity_instance() %>% n_cases()
events3c %>% group_by_activity_resource()  %>% n_cases() #- group by activity resource pair % becos instance not calc correctly

events3c %>% resource_map()
events3c %>% resource_matrix()
events3c %>% process_matrix()
events3c %>% precedence_matrix()
events3c %>% dotted_chart(sort='start')
events3c %>% dotted_chart(sort='duration')

?dotted_chart

processing_time(events3c, level="case", units="hours")
processing_time(events3c, level="resource", units="hours")
processing_time(events3c, level="activity", units="hours")
processing_time(events3c, level="trace", units="hours")
processing_time(events3c, level="resource-activity", units="hours")
processing_time(events3c, level="activity", units="hours", append_column = T)

events3c %>% as.data.frame() %>% add_count(resource)
events3c %>% process_map(type = frequency("absolute"))
events3c %>% process_map(type = frequency("relative"))

animate_process(events3c, legend=T, mode='absolute', duration=10)
animate_process(events3c, legend=T, mode='relative', duration=10)
animate_process(events3c, legend=T, mode='relative', duration=10,mapping = token_aes(color = token_scale("red"), size = token_scale(10)))
#color as per gender----
animate_process(events3c,  legend = "color",   mapping = token_aes(color = token_scale("case", scale='ordinal', range = c('red','blue'))), duration=10)

animate_process(events3c,  legend = "color",   mapping = token_aes(color = token_scale(attribute="case", scale='ordinal', range = c('red','blue')), size=token_scale(12), opacity = token_scale('0.4')), duration=10)

animate_process(events3c, legend='size', mapping = token_aes(shape = "rect"), duration=10)
