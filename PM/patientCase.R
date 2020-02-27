#patient Case
head(patients,1) %>% as.data.frame()

patients %>%   filter_activity(c("X-Ray", "Blood test")) %>%   activities

patients %>% activity_frequency(level = "activity")

patients %>% activity_frequency(level = "activity") %>% plot() 

patients %>% filter_activity_frequency(percentile_cut_off = 0.8)
patients %>% filter_activity_presence(c("Registration","Check-out"), method = "all")

patients %>% filter_time_period(  start_point = ymd(20170101),
                                  end_point = ymd(20170131),  filter_method = "start", reverse = T)
patients %>% filter_time_period(interval = interval(ymd(20170101),ymd(20170131)),  filter_method = "start", reverse = T)
patients %>%   filter_activity_frequency(interval = c(300,500)) %>%
  activities
patients %>%   filter_resource(c("r1","r4")) %>%  resource_frequency("resource")
patients %>%   filter_resource_frequency(perc = 0.80) %>%   resources()


patients %>%   filter_activity_frequency(interval = c(300, NA)) %>% activities

#Trim------
#The trim filter is a special event filter, as it also take into account the notion of cases. In fact, it trim cases such that they start with a certain activities until they end with a certain activity. It requires two list: one for possible start activities and one for end activities. The cases will be trimmed from the first appearance of a start activity till the last appearance of an end activity. When reversed, these slices of the event log will be removed instead of preserved.

#Trim to time window
#Instead of triming cases to a particular start and/or end activity, we can also trim cases to a particular time window. For this we use the function filter_time_period with filter_method trim. This filter needs a time interval, which is a vector of length 2 containing data/datetime values. These can be created easily using lubridate function, e.g. ymd for year-month-day formats.

#This example takes only activity instances which happened (at least partly, i.e. some events) in December of 2017.

library(lubridate)
patients %>%  filter_time_period(interval = ymd(c(20171201, 20171231)), filter_method = "trim") %>% summary()
patients %>%  filter_trim(start_activities = "Registration", end_activities =  c("MRI SCAN","X-Ray")) %>%   process_map(type = performance())


#Throughput time----
#Filtering on throughput time can be done in an absolute and relative way, just as for many other filters.
#Absolute: specific a throughput time interval
#Relative: specific a percentage target
#For instance, we can filter cases with a throughput time between 50 and 100 hours. Notice that setting the time unit argument appropriately is important in this case.
patients %>%   filter_throughput_time(interval = c(50, 100), units = "hours") %>%  throughput_time(units = "hours")

#Alternatively, we can filter the 50% cases with the lowest throughput time.

patients %>%  filter_throughput_time(percentage = 0.5) %>% throughput_time(units = "hours")
#In both cases, the selection can be negated using the reverse argument. When using an interval, one of the limits can be set to NA to create an open interval

#Processing time----
#Filtering on processing time happens in exactly the same way as the filter on throughput time, as the examples below show.

patients %>% filter_processing_time(interval = c(50, 100), units = "hours") %>%  processing_time(units = "hours")

patients %>%  filter_processing_time(percentage = 0.5) %>%  processing_time(units = "hours")


#Trace length-----
#Filtering on trace length is similar to filters on processing or throughput time. Only the units argument is not needed here.

patients %>%  filter_trace_length(interval = c(2, 5)) %>%
  trace_length(units = "hours")

patients %>%  filter_trace_length(percentage = 0.5) %>%  trace_length()
#select cases that contain a specific activity, for instance a X-Ray scan.
patients %>%  filter_activity_presence("X-Ray") %>% traces

#Or that don’t have a specific activity.

patients %>% filter_activity_presence("X-Ray", reverse = T) %>%  traces

#We can also test more than one activity. In this case, we can require “all”, “one_of” or “none” of them to be present, through setting the argument method correctly.

#For example, there are no case that have both X-Ray and MRI-SCAN

patients %>%  filter_activity_presence(c("X-Ray", "MRI SCAN"), method = "all")  %>%   traces

#Almost all have on of them.

patients %>% filter_activity_presence(c("X-Ray", "MRI SCAN"), method = "one_of")  %>%  traces

#And 3 have none of them.

patients %>% filter_activity_presence(c("X-Ray", "MRI SCAN"), method = "none")  %>% traces

#End points
#Another way is to select cases with a specific start and or end activity. In case of the patients data set, all cases start with “Registration”. Filtering cases that don’t start with Registration gives an empty log.

patients %>% filter_endpoints(start_activities = "Registration", reverse = T)

#If we are interested to see the “completed” cases, those that start with Registration and end we “Check-out”, we can apply the following filter.

patients %>%  filter_endpoints(start_activities = "Registration", end_activities = "Check-out") %>% process_map()


#Precedence----
#Another control-flow filtering approach is to look at precedences between activities. The filter_precedence function uses 5 different inputs
#A list of (one or more) possible antecedent activities (“source”-activities)
#A list of (one or more) possible consequent activities (“target”-activities)
#A precedence_type
#directly_follows
#eventually_follows
#A filter_method: all, one_of or none of the precedence rules should hold.
#A reverse argument
#If there is more than one antecedent or consequent activity, the filter will test all possible pairs. The filter_method will tell the filter whether all of the rules should hold, at least one, or none are allowed.

#For example, take the patients data. The following filter takes only cases where “Triage and Assessment” is directly followed by “Blood test”.

patients %>%  filter_precedence(antecedents = "Triage and Assessment",        consequents = "Blood test",precedence_type = "directly_follows") %>% traces


#The following selects cases where Triage and Assessment is eventually followed by both Blood test and X-Ray, which never happens.


patients %>%  filter_precedence(antecedents = "Triage and Assessment", consequents = c("Blood test", "X-Ray"),  precedence_type = "eventually_follows", filter_method = "all") %>%  traces

patients %>%  filter_precedence(antecedents = "Triage and Assessment", consequents = c("Blood test", "X-Ray", "MRI SCAN"),precedence_type = "eventually_follows", filter_method = "one_of") %>%  traces

#This final example only retains cases where Triage and Assessment is not followed by any of the three consequent activities. The result is 2 incomplete cases where the last activity was Triage and Assessment.

patients %>%   filter_precedence(antecedents = "Triage and Assessment", consequents = c("Blood test", "X-Ray", "MRI SCAN"),   precedence_type = "eventually_follows", filter_method = "none") %>%   traces


#Trace Frequency
#Filtering on trace frequency is similar to the filters on activity/resource frequence and the performance filter: you can choose between a percentage target or between an frequency interval.

#Select 80% of the cases that share the most common traces.

sepsis %>%  filter_trace_frequency(percentage = 0.8) %>% n_cases()
## [1] 1050
#Or the 20% least common ones.
sepsis %>%  filter_trace_frequency(percentage = 0.2) %>% n_cases()

#Or the cases of which the trace frequency is less than 50.

sepsis %>%  filter_trace_frequency(interval = c(0,50)) %>%   n_cases()

#Time period
#Filtering cases by time period can be done using the filter_time_period introduced above. There are four different methods that result in case filters:
  
#start: all cases started in an interval
#complete: all cases completed in an interval
#contained: all cases contained in an interval
#intersecting: all cases with some activity in an interval
#The following four example dotted charts show the impact of the four different methods using the same interval.

sepsis %>%  filter_time_period(interval = ymd(c(20150101, 20150131)), filter_method = "start") %>% dotted_chart

sepsis %>%   filter_time_period(interval = ymd(c(20150101, 20150131)), filter_method = "complete") %>%   dotted_chart
sepsis %>%   filter_time_period(interval = ymd(c(20150101, 20150131)), filter_method = "contained") %>% dotted_chart

sepsis %>%   filter_time_period(interval = ymd(c(20150101, 20150131)), filter_method = "intersecting") %>% dotted_chart



#Graphs ----
processmapR::resource_matrix(sepsis) %>% plot 
processmapR::resource_map(patients)
processmapR::trace_explorer(patients) 


patients %>%  idle_time("resource", units = "days")
patients %>%   idle_time("resource", units = "days") %>%   plot()
patients %>%    processing_time("activity") %>%   plot()
#https://www.bupar.net/exploring.html


#animation
#https://www.bupar.net/processanimater.html
library(processanimateR)
library(eventdataR)
animate_process(patients)
