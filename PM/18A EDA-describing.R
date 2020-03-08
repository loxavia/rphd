#EDA - Describing
#http://bupar.net/exploring.html
#Exploring event data
#The metrics for exploring and describing event data which are available are based on literature in the field of operational excellence and are organized in the following (sub)categories

#Time perspective
#Organizational perspective
#Structuredness perspective
#Variance
#Rework
#Time perspective
#Three different time metrics can be computed:
# throughput time: the time between the very first event of the case and the very last
#processing time: the sum of the duration of all activity instances
#idle time: the time when no activity instance is active

#The duration of an activity instance is the time between the first and the last event related to that activity instance. In case several activity instances within a case overlap, processing time for that overlap will be counted twice. The figure below shows a schematic overview of different time metrics.



#Idle Time ----
#The idle time is the time that there is no activity in a case or for a resource. It can only be calculated when there are both start and end timestamps available for activity instances. It can be computed at the levels trace, resource, case and log, and using different time units.

patients %>%  idle_time("resource", units = "days")

#The idle time is the time that there is no activity in a case or for a resource. It can only be calculated when there are both start and end timestamps available for activity instances. It can be computed at the levels trace, resource, case and log, and using different time units.

#The output of all metrics in edeaR can be visualized by supplying it to the plot function.

patients %>%  idle_time("resource", units = "days") %>% plot()
patients %>%  idle_time("log", units = "days") %>% plot()
patients %>%  idle_time("case", units = "days") %>% plot(cols)

#Processing Time
#The processing time can be computed at the levels log, trace, case, activity and resource-activity. It can only be calculated when there are both start and end timestamps available for activity instances.

patients %>%  processing_time("activity") %>% plot

#Throughput Time
#The throughput time is the time form the very first event to the last event of a case. The levels at which it can be computed are log, trace, or case.

patients %>% throughput_time("log") %>% plot()


#Organizational Perspective
#Resource Frequency
#The resource frequency metric allows the computation of the number/frequency of resources at the levels of log, case, activity, resource, and resource-activity.

patients %>% resource_frequency("resource")

#Resource Involvement
#Resource involvement refers to the notion of the number of cases in which a resource is involved. It can be computed at levels case, resource, and resource-activity.

patients %>% resource_involvement("resource") %>% plot

#shows that only r1 and r2 are involved in all cases, r6 and r7 are involved in most of the cases, while the others are only involved in half of the cases, more or less.

#Resource Specialization
#The resource specalization metric shows whether resources are specialized in certain activities or not. It can be calculated at the levels log, case, resource and activity.

patients %>% resource_specialisation("resource")


#In the simple patients event log, each resource is performing exactly one activity, and is therefore 100% specialized.

#Structuredness
#Variance
#Activity Presence
#Activity presence shows in what percentage of cases an activity is present. It has no level-argument.

patients %>% activity_presence() %>% plot


#Activity Frequency
#The frequency of activities can be calculated using the activity_frequency function, at the levels log, trace and activity.

patients %>%  activity_frequency("activity")


#Start Activities
#The start of cases can be described using the start_activities function. Available levels are activity, case, log, resource and resource activity.

patients %>% start_activities("resource-activity")
patients %>% start_activities("activity")
patients %>% start_activities("log")
patients %>% start_activities("case")

#This shows that in this event log, all cases are started with the Registration by resource r1.

#End Activities
#Conversely, the end_activities functions describes the end of cases, using the same levels: log, case, activity, resource and resource-activity.
patients %>%   end_activities("resource-activity")
patients %>%   end_activities("case")

#In contract to the start of cases, the end of cases seems to differ more frequently, although it is mostly the Check-Out activity.

#Trace Coverage
#The trace coverage metric shows the relationship between the number of different activity sequences (i.e. traces) and the number of cases they cover.

patients %>% trace_coverage("trace") %>% plot()
#In the patients log, there are only 7 different traces, and 2 of them cover nearly 100% of the event log.

#Trace Length
#The trace length metric describes the length of traces, i.e. the number of activity instances for each case. It can be computed at the levels case, trace and log.

patients %>% trace_length("log") %>% plot

#It can be seen that in this simple event log, most cases have a trace length of 5 or 6, while a minority has a trace length lower than 5
