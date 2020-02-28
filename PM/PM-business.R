#BPR - PM

#https://businessinformatics.be/2017/09/29/bupar-business-process-analysis-with-r/

head(patients)
#write.csv(patients, 'data/patients.csv', row.names = F)
#The table shows an example event log. Each row is an event which belongs to a case (a patient). Different events together can form an activity instance, or execution (e.g. event 2-4 belong to surgery 2). Each event in such an execution will have a different transactional lifecycle status. Note that there can be different instances of a specific activity (e.g. there are two surgeries in the example). Furthermore, each event has a timestamp, indicating when it happened, and a resource, indicating who performed it.

#Trace Length
#The trace length metric describes the length of traces, i.e. the number of activity instances for each case. It can be computed at the levels case, trace and log.
patients %>%  trace_length("log") %>% plot

#Trace Coverage-----
#The trace coverage metric shows the relationship between the number of different activity sequences (i.e. traces) and the number of cases they cover.

patients %>% trace_coverage("trace") %>% plot()
#in the patients log, there are only 7 different traces, and 2 of them cover nearly 100% of the event log.


#Start Activities-----
#The start of cases can be described using the start_activities function. Available levels are activity, case, log, resource and resource activity.

patients %>%  start_activities("resource-activity")
#all cases are started with the Registration by resource r1

#End Activities
patients %>%  end_activities("resource-activity")
# the end of cases seems to differ more frequently, although it is mostly the Check-Out activity.  

#Available levels are activity, case, log, resource and resource activity.
patients %>%  start_activities("activity")
patients %>%  end_activities("activity")

patients %>%  start_activities("log")
patients %>%  end_activities("log")

patients %>%  start_activities("resource")
patients %>%  end_activities("resource")


#Activity Presence -----
#Activity presence shows in what percentage of cases an activity is present. It has no level-argument.

patients %>% activity_presence() %>% plot


#Resource Specialization----
#The resource specalization metric shows whether resources are specialized in certain activities or not. It can be calculated at the levels log, case, resource and activity.

patients %>% resource_specialisation("resource")
patients %>% resource_specialisation("log")
patients %>% resource_specialisation("case")
patients %>% resource_specialisation("activity")


#Resource Involvement-----
#Resource involvement refers to the notion of the number of cases in which a resource is involved. It can be computed at levels case, resource, and resource-activity.

patients %>% resource_involvement("resource") %>% plot
patients %>% resource_involvement("case") %>% plot
#resources used in each activity - count
patients %>% resource_involvement("resource-activity") %>% plot  #good

#Resource Frequency------
#The resource frequency metric allows the computation of the number/frequency of resources at the levels of log, case, activity, resource, and resource-activity.

patients %>% resource_frequency("resource")
patients %>% resource_frequency("log")
patients %>% resource_frequency("case")
patients %>% resource_frequency("resource-activity")
patients %>% resource_frequency("activity")


#Throughput Time ----
#The throughput time is the time form the very first event to the last event of a case. The levels at which it can be computed are log, trace, or case.

patients %>% throughput_time("log") %>% plot()  
patients %>% throughput_time("trace")
patients %>% throughput_time("case")
patients %>% throughput_time("log")
names(patients)

#Processing Time------
#The processing time can be computed at the levels log, trace, case, activity and resource-activity. It can only be calculated when there are both start and end timestamps available for activity instances.

patients %>% processing_time("activity") %>% plot
patients %>% processing_time("trace")
patients %>% processing_time("log")
patients %>% processing_time("activity")
patients %>% processing_time("resource-activity")


#Idle Time-----
#The idle time is the time that there is no activity in a case or for a resource. It can only be calculated when there are both start and end timestamps available for activity instances. It can be computed at the levels trace, resource, case and log, and using different time units.

patients %>% idle_time("resource", units = "days")

#The output of all metrics in edeaR can be visualized by supplying it to the plot function.

patients %>% idle_time("resource", units = "days") %>% plot()



#--------------
activity_frequency(patients, level = "activity") %>% arrange(relative)
# least common activity 
number_of_repetitions(patients, level="activity", type="all")

number_of_repetitions(sepsis, level="activity", type="all")
number_of_repetitions(sepsis, level="activity", type="repeat")
number_of_selfloops(sepsis, level="case", type = "redo")


#
patients %>% dotted_chart
patients %>% filter_trace_frequency(perc = 0.9) %>%   process_map()

#Frequent traces, i.e. activity sequences, can be explored with the trace_explorer.
patients %>% trace_explorer(coverage = 0.9)

patients %>% activity_presence %>% plot
#The example below shows in how many cases each of the activities is present. This shows that in the given event log, there is a set of very common activities, and a set of very rare activities.  

#https://gsverhoeven.github.io/post/exploring-process-mining/
patients %>% filter(patient == 1) %>% arrange(handling_id) #%>% 
#To visualize all traces, we set coverage to 1.0.

patients %>% processmapR::trace_explorer(type = "frequent", coverage = 1.0)
#So there are a few traces (0.6%) that do not end with a check-out. Ignoring these rare cases, we find that there are two types of cases:
#Cases that get an X-ray
#Cases that get a blood test followed by an MRI scan
#dotted plot
#It has two nice use cases. The first is when we plot actual time on the x-axis, and sort the cases by starting date.
library(ggplot2)
patients %>% dotted_chart(x = "absolute", sort = "start") + ggtitle("All cases") + theme_gray()
#The slope of this graphs learns us the rate of new cases, and if this changes over time. Here it appears constant, with 500 cases divided over five quarter years.
#The second is to align all cases relative to the first event, and sort on duration of the whole sequence of events.

patients %>% dotted_chart(x = "relative", sort = "duration") + ggtitle("All cases") +  theme_gray()
#A nice pattern emerges, where all cases start with registration, then quickly proceed to triage and assessment, after that, a time varying period of 1-10 days follows where either the blood test + MRI scan, or the X-ray is performed, followed by discussing the results. Finally, check out occurs.
#To conclude, the process mining approach to analyze time series event data appears highly promising. The dotted chart is a great addition to my data visualization repertoire, and the process mining folks appear to have at lot more goodies, such as Trace Alignment.
#
#https://stefanpeilweimar.blogspot.com/2019/01/a-short-case-study-overview-on-process.html
#https://github.com/gertjanssenswillen/processmapR/issues/33
#https://github.com/JesseVent/loan-app-process/blob/4bf9f206697d5fc05ec708db344c422fcbfcface/02-parallel-activities.R#L12

#loan------
ibrary(tidyverse)
library(xesreadR)
library(processanimateR)
library(processmapR)

#   ____________________________________________________________________________
#   Loan Application Event Logs                                             ####
#not workig
#loan  <- xesreadR::read_xes("https://solutiondesign.io/blog/data/loan_app_data.xes")
head(loan)

graph <- processmapR::process_map(loan, type_edges = performance(mean), render = F)
model <- DiagrammeR::add_global_graph_attrs(graph,  attr      = c("layout", "rankdir", "splines"),value     = c("dot", "LR", "ortho"),attr_type = c("graph", "graph", "graph"))

animate_process(loan, model, mode = "relative", duration = 240)

#case:https://rpubs.com/RohitP/production_mining



#data Let us consider the following event log of a telephone repair process? http://tinyurl.com/repairLogs

#links to read----
#https://courses.cs.ut.ee/MTAT.03.231/2018_spring/uploads/Main/2018Lecture12.pdf
#https://community.powerbi.com/t5/Community-Blog/Business-Process-Analysis-in-PowerBI-using-R-visuals/ba-p/659401

