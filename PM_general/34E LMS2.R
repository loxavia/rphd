#PM - LMS data
#actual Data - LA LMS Data - few rows

pacman::p_load(gsheet, googlesheets4, ggplot2, dplyr, DiagrammeR,lubridate) 
pacman::p_load(bupaR, edeaR, processmapR, processanimateR, processcheckR) 
glink1 = "https://docs.google.com/spreadsheets/d/1Rv-8pDWMeonmjn7LCZh55MJtIh0XG5vHX-mIZpcGf1c"
googlesheets4::sheets_sheets(glink1)

LMS3 = as.data.frame(googlesheets4::read_sheet(glink1, sheet='LMS3'))
head(LMS3) ; dim(LMS3)
#data summary
data = LMS3
names(data)
head(df)
data$Time #check data format
textdate = "14 February 2020, 4:28 PM"
strptime(textdate, format ='%d %B %Y, %I:%M %p')
as.POSIXct("16/02/20, 14:57", format='%d/%m/%y, %H:%M')
data$Time = as.POSIXct(data$Time, format='%d/%m/%y, %H:%M')
data$Time
str(data)
names(data)
#rename Columns----
cols1 = c('time','user','affectedUser', 'eventContext', 'component', 'eventName', 'Description','orgin','ipaddress')
names(data) = cols1
head(data)
data$time = as.POSIXct(data$time)
str(data$time)
#create events-----
events1 <- bupaR::simple_eventlog(eventlog = data, case_id = 'user', activity_id = 'eventContext', timestamp = 'time')
events1
activity_frequency(events1)
events1 %>% activity_frequency(level='activity')

events1 %>% activity_frequency(level = "activity") %>% plot()
events1 %>% process_map()

#how many activities
activity_frequency(events1)
#freq of participation in activities
events1 %>% activity_frequency(level='activity')

processmapR::process_map(events1)
animate_process(events1)
AP_events1 <- animate_process(events1)

htmlwidgets::saveWidget(AP_events1,file='E:/PMO/htmlV/lmsV1.html')
write.csv(events1,paste("E:/PMO/eventData/lms2events",Sys.Date(),".csv"), na='', row.names = F)

#more on events
events1 %>% activities
#combine activity into 1----
events1 %>% act_unite(PPT = c("Page: PPT - Analytics","Page: PPT - Data Handling")) %>% process_map() #2 ppts into 1

animate_process(events1, legend=T, mode='absolute')
animate_process(events1, legend=T, mode='relative')
animate_process(events1, legend=T, mode='relative', duration=10)

#Processmap
process_map(events1)
process_map(events1, rankdir = 'TB')
?act_collapse

events1 %>% act_collapse(newActivity = c('Page: PPT - Linear Regression', 'Page: PPT - Logistic Regression')) %>% process_map()
#diff between collapse and unit ??
events1 %>% throughput_time(level = "case")
events1 %>% throughput_time(level = "case",append = TRUE) %>% select(time, throughput_time_case)
events1 %>% trace_coverage(level = "case") %>% pull(trace) #order in which activity was performed
events1 %>% trace_coverage(level = "case", append=T) %>% select(user, absolute_case_trace_coverage)
events1 %>% trace_coverage(level = "case", append_column = "relative") 
#does every student learns in the same way
#http://www.bupar.net/enriching.html
events1$eventContext
#student who accesses Attendance Module - True
events1 %>% group_by_case %>% mutate(Attnd = any(eventContext == "Attendance: Online Attendance")) %>% ungroup_eventlog() %>% select(user, Attnd) %>% group_by(Attnd)  %>% n_cases()
#change to any to all
events1 %>% group_by_case %>% mutate(Attnd = any(eventContext == "Attendance: Online Attendance", eventContext == "Page: PPT - Logistic Regression") ) %>% ungroup_eventlog() %>% select(user, Attnd) %>% group_by(Attnd)  %>% n_cases()


#whether a case followed a frequent or infrequent path. 
events1 %>% trace_coverage(level = "case", append = T, append_column = "relative") %>% mutate(frequent = relative_case_trace_coverage > 0.9) %>% n_cases()
n_cases(events1)
#%>% filter(frequent=="FALSE") %>% select(user, frequent) 
names(events1)

#manipulation
events1 %>% group_by(eventContext) %>% n_cases()
events1 %>% group_by_resource %>% n_cases()
events1 %>% group_by_activity %>% n_cases() # group by activity types
events1 %>% group_by_case %>% n_activities() 
?group_by_resource_activity
events1 %>% group_by_resource_activity() %>% n_cases() #- group by activity resource pair
events1 %>% group_by_activity_instance %>% n_cases() #- group by activity instances. will be 1.
events1 %>% group_by_resource() %>% n_cases()  #since no resource was defined

events1 %>% bupaR::arrange(user)
events1 %>% bupaR::arrange(eventName)
events1 %>% bupaR::arrange(time, user) %>% head(5)
events1 %>% group_by_case  %>% dplyr::summarise(count=length(unique(eventContext)))
%>% mutate(CountID = dplyr::summarise(length(activity_instance_id)))
events %>% filter( grepl("PPT",eventContext)) %>% process_map()
events %>% select(user)
names(events)
events %>% select(user, time, eventName, force_df = TRUE)
events %>% arrange(desc(time))
events %>% slice(1:3) %>% select(user, time, eventName, force_df = TRUE)
events %>% slice_activities(1:5) #activity instances 1 to 5
#events %>% arrange(time)
events %>% slice_events(1:10)
events %>% first_n(n = 5)
events %>% arrange(desc(time)) %>%  first_n(n = 5)
#heads & tails of each cases explore the 95% most common first 10 activities in the  log.
events %>%  group_by_case() %>% first_n(10) %>%  trace_explorer(coverage = .95)

#
head(events$user)
#events %>%  eventlog(case_id = "Amit Sanpui")

mapping1 <- mapping(events)
mapping1

# percentage of cases an activity is present.
events %>% activity_presence() %>%  plot
#requency of activities can be calculated using the activity_frequency function, at the levels log, trace and activity.
events %>%  activity_frequency("activity")
#start of cases can be described using the start_activities function. Available levels are activity, case, log, resource and resource activity.
events %>%  start_activities("resource-activity")
events %>%  end_activities("resource-activity")

#trace coverage metric shows the relationship between the number of different activity sequences (i.e. traces) and the number of cases they cover.
events %>%  trace_coverage("trace") %>% plot()
#1 case covers all traces

# trace length metric describes the length of traces, i.e. the number of activity instances for each case. It can be computed at the levels case, trace and log.
events %>%  trace_length("log") %>%   plot
#most cases have a trace length of 15

#PRocess map
events %>% process_map()
#By default, the process map is annotated with frequencies of activities and flows. The is what is called the frequency profile, and can be created explicitly using the frequency function. This function has a value argument, which can be used to adjust the frequencies shown, for instance using relative frequencies instead of the default absolute ones.
events %>%   process_map(type = frequency("relative"))
events %>%   process_map(type = frequency("absolute"))
events %>%   process_map(type = frequency("absolute_case"))
events %>%   process_map(type = frequency("relative_case"))

#absolute frequency The absolute number of activity instances and flows
#absolute_case frequency The absolute number of cases behind each activity and flow
#relative frequency The relative number of instances per activity
#The relative outgoing flows for each activity
#relative_case frequency The relative number of cases per activity and flow

events %>% process_map(type = frequency("relative_case"), color_scale = "Purples")
events %>%  process_map(performance(median, "days"))
events %>%  process_map(performance(mean, "hours"))
events %>%  process_map(type_nodes = frequency("relative_case"),    type_edges = performance(mean))
  
m2 <- animate_process(events)
htmlwidgets::saveWidget(m2,file='lms.html')
