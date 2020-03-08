#Simple Event


link = "https://docs.google.com/spreadsheets/d/1Rv-8pDWMeonmjn7LCZh55MJtIh0XG5vHX-mIZpcGf1c/edit#gid=2068730099"
library(edeaR)

library(gsheet)

data = as.data.frame(gsheet2tbl(link))
data
head(data)

data$timestamp = as.Date(data$timestamp, format='%d-%b-%Y')
event3 <- bupaR::simple_eventlog(eventlog = data,   case_id = 'case',  activity_id = 'activity', timestamp = 'timestamp')

activity_frequency(event3)
event3 %>% activity_frequency(level='activity')

event3 %>% activity_frequency(level = "activity") %>% plot()

edeaR::activity_frequency
edeaR::activity_presence
edeaR::end_activities
edeaR::idle_time
edeaR::number_of_repetitions
edeaR::number_of_selfloops
edeaR::number_of_traces
edeaR::processing_time
edeaR::resource_frequency
edeaR::resource_involvement
edeaR::resource_specialisation
edeaR::size_of_repetitions
edeaR::size_of_selfloops
edeaR::start_activities
edeaR::throughput_time
edeaR::trace_coverage
edeaR::trace_length



#----------------
event3 %>% filter_activity_frequency(percentage = 0.5)


event3
event3 %>% filter_activity_presence(c('A','B'), method='all')
#activity present in those traces
event3 %>% filter_activity_presence(c('F'), method='all')
#F was present with A only


event3 %>% filter_activity_frequency(interval = c(2,3)) %>% activities

event3 %>% filter_activity_frequency(interval = c(1,NA)) %>% activities


edeaR::ifilter_activity
edeaR::ifilter_activity_Frequency
edeaR::filter_attributes
edeaR::ifilter_resource
edeaR::ifilter_resource_frequency
edeaR::ifilter_time_period
edeaR::ifilter_trim
edeaR::ifilter_activity_presence
edeaR::ifilter_case
edeaR::ifilter_endpoints
edeaR::ifilter_precedence
edeaR::ifilter_processing_time
edeaR::ifilter_throughput_time
edeaR::ifilter_time_period
edeaR::ifilter_trace_frequency
edeaR::ifilter_trace_length 

event3
event3 %>%  process_map()
event3 %>% filter_activity_presence("B") %>%  traces
event3 %>% filter_activity_presence("B", reverse=T) %>%  traces #not there
event3 %>%  filter_activity_presence(c("B", "D"), method = "all")  %>% traces
event3 %>%  filter_activity_presence(c("F", "D"), method = "all")  %>% traces

#events which do not start with A
event3 %>%  filter_endpoints(start_activities = "A", reverse = T)

event3 %>%  filter_endpoints(start_activities = "A", end_activities = "F") %>% process_map()

event3 %>%  filter_activity_presence(c("A", "F"), method = "one_of")  %>%  traces

event3 %>%  filter_endpoints(start_activities = "A", end_activities = "E") %>% process_map()

event3 %>%  filter_endpoints(start_activities = "B", end_activities = "D") %>% process_map()


#Precedence
event3 %>%  filter_precedence(antecedents = "A",  consequents = "B",precedence_type = "directly_follows") %>% traces
event3 %>%  filter_precedence(antecedents = "A",  consequents = "D",precedence_type = "directly_follows") %>% traces

event3 %>% process_map()
event3 %>%  filter_precedence(antecedents = "A", consequents = c("B", "C"),  precedence_type = "eventually_follows", filter_method = "all") %>%  traces

#The next filter selects cases where Triage and Assessement is eventually followed by at least one the three antecedents, by changing the filter method to one_of.

event3 %>%  filter_precedence(antecedents = "B", consequents = c("C", "D", "E"),precedence_type = "eventually_follows", filter_method = "one_of") %>%  traces

event3 %>%  filter_precedence(antecedents = "B", consequents = c("C", "D", "E"),precedence_type = "eventually_follows", filter_method = "none") %>%  traces

event3 %>%  filter_trace_frequency(percentage = 0.2) %>% n_cases()
event3 %>%  filter_trace_frequency(interval = c(0,50)) %>%   n_cases()

event3 %>% dotted_chart()
event3 %>%  filter_time_period(interval = ymd(c(20200110, 20200118)), filter_method = "start") %>% dotted_chart
event3 %>%  filter_time_period(interval = ymd(c(20200110, 20200118)), filter_method = "complete") %>% dotted_chart
event3 %>%  filter_time_period(interval = ymd(c(20200110, 20200118)), filter_method = "contained") %>% dotted_chart
event3 %>%  filter_time_period(interval = ymd(c(20200110, 20200118)), filter_method = "intersecting") %>% dotted_chart


event3 %>%   filter_activity(c("A", "B")) %>%   activities
event3 %>%   filter_activity(c("A", "B","F")) %>%   activities
event3 %>%   filter_activity_frequency(percentage = 0.5 ) %>%  activities

event3 %>%   filter_activity_frequency(percentage = 0.5, reverse = T) %>%  activities
event3 %>%  filter_resource_frequency(perc = 0.80) %>% resources()

event3 %>%   filter_trim(start_activities = "A", end_activities =  c("D","F")) %>%   process_map(type = performance(units='days'))

#not working
event3 %>%  group_by(case, activity) %>%   throughput_time('log') %>%   plot 
event3 %>%  group_by(case, activity) %>% throughput_time('log')

processmonitR::activity_dashboard(event3) 
processmonitR::resource_dashboard(event3) 
processmonitR::rework_dashboard(event3)
processmonitR::performance_dashboard(event3) 
processmapR::process_map(event3) 
processmapR::precedence_matrix(event3) %>% plot
processmapR::trace_explorer(event3) 
processmapR::idotted_chart(event3) 
processmapR::resource_map(event3) 
processmapR::resource_matrix(event3) %>% plot 


animate_process(event3)
