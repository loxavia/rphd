#simulated LMS data for PM

#libraries
pacman::p_load(gsheet, bupaR, dplyr, edeaR, processmapR, processanimateR, DiagrammeR,lubridate) 

#create some activities
(activities = c('Forum', 'PPT', 'Assignment','Quiz', 'Game' ,'Survey'))
(rollno = paste('S',1:50,sep='-'))

(e1 <- expand.grid(rollno, activities))
head(e1)
names(e1) <- c('rollno','activity')

#sample dates of month
e1$timestamp = as.POSIXct(as.Date(paste('2020','2',sample(1:30, size=nrow(e1), replace=T),sep='-')))
(e1$activityscores = round(rnorm(nrow(e1), mean=60, sd=8)))
head(e1)
dim(e1)

#student Profile----
(rollno = paste('S',1:50,sep='-'))
(sname = paste('Student-',1:50, sep=''))
(gender = sample(c('M','F'), size=50, replace=T, prob=c(.7,.3)))
(finalMarks = trunc(runif(50,min=200,max=500)))
(grades = sample(c('A','B','C'), size=50, replace=T, prob=c(.3,.3,.4)))

(students = data.frame(rollno, sname, gender, finalMarks, grades, stringsAsFactors = F))
head(students)
names(students) ; names(e1)
(e2 <- merge(x=students, y=e1, all.y=T ))
names(e2)
head(e2)
#select fraction of rows randomly 
e3 <- e2 %>% sample_frac(.8)

#create bupaR object -----
events <- bupaR::simple_eventlog(eventlog = e3, case_id = 'rollno', activity_id = 'activity', timestamp = 'timestamp')

events
head(events)
str(events)
summary(events)
events$rollno
events1 <- events %>% filter_case(cases = c('S-1','S-2','S-3','S-4','S-5'))

#how many activities----
activity_frequency(events1)
#freq of participation in activities----
events1 %>% activity_frequency(level='activity')
events1 %>% activity_frequency(level='activity') %>% plot

#Process Map-----
#only 1 at a time
events1 %>% processmapR::process_map(sec=frequency('absolute'))
events1 %>% processmapR::process_map(sec=frequency('relative'))
events1 %>% processmapR::process_map(sec=frequency('absolute-case'))
events1 %>% processmapR::process_map(sec=frequency('relative-case'))



#----
?process_map
#Animate Process-----
absam1 <- animate_process(events1, duration=10, mode='absolute', mapping = token_aes(color=token_scale('red')))
absam1

relam1 <- animate_process(events1, duration=10, mode='relative', mapping = token_aes(color=token_scale('red')))
relam1

#Color and size
animate_process(events1, legend=T, mode='absolute', duration=10, mapping = token_aes(color = token_scale("red"), size = token_scale(10)))
        
names(events1)
str(events1)
events1$gender = factor(events1$gender)
events1$grades = factor(events1$grades)

#color as per gender----
animate_process(events1,  legend = "color",   mapping = token_aes(color = token_scale("gender", scale='ordinal', range = c('red','blue'))), duration=10)

#color as per grades----
animate_process(events1,  legend = "color",   mapping = token_aes(color = token_scale("grades", scale='ordinal', range = c('green','yellow','red'))), duration=10)

g=c('A','B','C') ; gc =c('green','yellow','red')
i=3
events1 %>% filter(grades == g[i]) %>% animate_process(legend = "color", mode='relative', mapping = token_aes(color = token_scale(gc[i]), size = token_scale(10)), duration=10, initial_state = 'paused', jitter=2, epsilon_time = 1)


#color as per gender using opacity ----
animate_process(events1,  legend = "color",   mapping = token_aes(color = token_scale(attribute="gender", scale='ordinal', range = c('red','blue')), size=token_scale(12), opacity = token_scale('0.4')), duration=10)

str(events1$finalMarks)

animate_process(events1,  legend = "size",   mapping = token_aes(size = token_scale(attribute="finalmarks", scale='quantize', range=1:3), opacity = token_scale('0.4')), duration=10)

animate_process(events1,  legend = "color",   mapping = token_aes(color = token_scale(attribute="finalmarks", scale='quantize', range=c('red','yellow'))), duration=10)

str(patients$employee)
animate_process(patients, mode = "relative", jitter = 10, legend = "color",   mapping = token_aes(color = token_scale("employee", scale = "ordinal",   range = RColorBrewer::brewer.pal(7, "Paired"))))

animate_process(events1, legend='size', mapping = token_aes(size = token_scale(10), shape = "rect"), duration=10)

animate_process(events1, legend=T, mode='relative', duration=10,  mapping = token_aes(color = token_scale('finalmarks'), scale = 'linear'))

htmlwidgets::saveWidget(ap1, file = "ap1.html")

#activity presence----
events1 %>% activity_presence() %>% plot
events1 %>% filter_activity(c("PPT", "Quiz")) %>% activities
events1 %>% activity_frequency(level = "activity")
events1 %>% activity_frequency(level = "activity") %>% plot()
#activities 80% of the path
events1 %>% filter_activity_frequency(percentile = 0.8)
events1 %>% filter_activity_presence(c("PPT","Quiz"), method = "all")



#resources----
events1
table(events1$resource_id)
events1 %>% resource_frequency("resource")
events1 %>% filter_resource_frequency(perc = 0.80) %>%   resources()
events1 %>% filter_activity_frequency(interval = c(50, 300)) %>% activities

#activities between T1 & T2
library(lubridate)
range(events1$timestamp, na.rm=T)
events1 %>% summary()
events1 %>%  filter_time_period(interval = ymd(c(20200201, 20200215)), filter_method = "trim") %>% summary()  #less events


#Trace Length-----
events1
events1 %>%  filter_trace_length(interval = c(2, 5)) %>% trace_length(units = "min")
events1 %>%  filter_trace_length(percentage = 0.5) %>%  trace_length()
#traces which have PPT
events1 %>%  filter_activity_presence("PPT") %>% bupaR::traces()
#traces which do not have PPT
events1 %>% filter_activity_presence("PPT", reverse = T) %>%  bupaR::traces()
events1 %>%  filter_activity_presence(c("PPT", "Quiz"), method = "all")  %>%   bupaR::traces()
events1 %>%  filter_activity_presence(c("PPT", "Quiz", reverse=T), method = "all")  %>% bupaR::traces()
#one of the activities
events1 %>%  filter_activity_presence(c("PPT", "Quiz"), method = "one_of")  %>% bupaR::traces()
?traces
bupaR::traces(events1)
#either PPT or Quiz was not there--
events1 %>%  filter_activity_presence(c("PPT", "Quiz", reverse=T), method = "one_of")  %>% bupaR::traces()

#none of them: students who did not take none of these
events1 %>% filter_activity_presence(c("PPT", "Game"), method = "none")  %>% bupaR::traces()
events1 %>%  filter_activity_presence(c("PPT", "Quiz", reverse=T), method = "all")  %>% bupaR::traces()

events1 %>% filter_activity_presence('PPT')
#start activities---
#student who started with PPT---
events1 %>% filter_endpoints(start_activities = "PPT")
#students who did not start with PPT
events1 %>% filter_endpoints(start_activities = "PPT", reverse = T) %>% traces
#ended with
events1 %>%  filter_endpoints(end_activities = "Quiz") %>% traces()
#did not end with 
events1 %>%  filter_endpoints(end_activities = "Quiz", reverse=T) %>% traces()
#start and end with
events1 %>%  filter_endpoints(start_activities = "Quiz", end_activities = "Forum") %>% process_map()

#map
events1 %>%  filter_endpoints(start_activities = "Quiz", end_activities = "Forum") %>% process_map()
?process_map

#Precedence-----
events1 %>%  filter_precedence(antecedents = "PPT",  consequents = "Quiz",precedence_type = "directly_follows") %>% bupaR::traces()
#PPT follows Forum & Quiz - data/ traces
events1 %>%  filter_precedence(antecedents = "PPT", consequents = c("Forum", "Quiz"),  precedence_type = "eventually_follows", filter_method = "all") %>%  traces
#PPT follows Forum and Quiz both directly or later
events1 %>%  filter_precedence(antecedents = "PPT", consequents = c("Forum", "Quiz"),  precedence_type = "eventually_follows", filter_method = "all") %>%   process_map()

#PPT follows - Quiz, Assignment, or Game---
events1 %>%  filter_precedence(antecedents = "PPT", consequents = c("Quiz", "Assignment",'Game'),precedence_type = "eventually_follows", filter_method = "one_of") %>%  process_map()

#PPT does not follow Quiz, Assignment or Game
events1 %>%  filter_precedence(antecedents = "PPT", consequents = c("Quiz", "Assignment",'Game'),precedence_type = "eventually_follows", filter_method = "none") %>%  process_map()


#Select 80% of the cases that share the most common traces
events1 %>%  filter_trace_frequency(percentage = 0.8) %>% bupaR::n_cases()
events1 %>%  filter_trace_frequency(percentage = 0.8) %>% process_map()
events1 %>%  filter_trace_frequency(percentage = 0.5) %>% n_cases()

#Or the cases of which the trace frequency is less than 25.
events1 %>%  filter_trace_frequency(interval = c(3,5)) %>%   bupaR::n_cases()
#????
?filter_trace_frequency
#Time Period---
?dotted_chart
?filter_time_period
events1 %>%  filter_time_period(interval = ymd(c(20200215, 20200220)), filter_method = "start")
events1 %>%  filter_time_period(interval = ymd(c(20200210, 20200220)), filter_method = "complete") %>% bupaR::n_cases()
events1 %>%  filter_time_period(interval = ymd(c(20200210, 20200220)), filter_method = "contained") %>% dotted_chart
events1 %>%  filter_time_period(interval = ymd(c(20200210, 20200220)), filter_method = "intersecting") 
events1 %>%  filter_time_period(interval = ymd(c(20200210, 20200220)), filter_method = "start") %>% dotted_chart(units='weeks')
events1 %>%  filter_time_period(interval = ymd(c(20200210, 20200220)), filter_method = "start") %>% dotted_chart(units='hours')
events1 %>%  filter_time_period(interval = ymd(c(20200210, 20200220)), filter_method = "start") %>% dotted_chart(units='hours', sort='end')

?dotted_chart.grouped_eventlog


#graphs-----
processmapR::resource_matrix(events1) %>% plot 
processmapR::resource_map(events1)
processmapR::trace_explorer(events1) 
processmapR::trace_explorer(events1, .abbreviate=FALSE) 
processmapR::trace_explorer(events1, coverage=.5, raw_data = TRUE)
processmapR::trace_explorer(events1, coverage=.5)
processmapR::trace_explorer(events1, n_traces=15)
processmapR::trace_explorer(events1, n_traces=15, type='frequent')
processmapR::trace_explorer(events1, n_traces=15, type='infrequent')
plotly_trace_explorer(events1)
?trace_explorer


events1 %>%  filter_activity_frequency(percentage = 1.0) %>% 
  filter_trace_frequency(percentage = .80) %>%  process_map(render = F) %>% DiagrammeR::export_graph(file_name = 'E:/PMC/pm11.png',  file_type = 'PNG')

events1 %>%   filter_activity_frequency(percentage = 1.0) %>% 
  filter_trace_frequency(percentage = .80) %>%   precedence_matrix() %>%  plot()

events1 %>% bupaR::act_unite(PPTquiz= c("PPT","Quiz"))
events1 %>% bupaR::act_collapse(PPTquiz= c("PPT","Quiz"))
?bupaR::act_collapse
?bupaR::act_unite

events1 %>% trace_coverage(level = "case") 
events1 %>% trace_coverage(level = "case", append=T) %>% as.data.frame() %>% select(rollno, trace, absolute_case_trace_coverage)
events1 %>% trace_coverage(level = "case", append_column = "relative") 

library(bupaR)
?group_by_activity
events1 %>%  bupaR::group_by_case %>%   mutate(Attnd = any(eventContext == "PPT")) %>%   ungroup_eventlog()
events1 %>% bupaR::group_by_activity() %>% bupaR::n_cases()
events1 %>% filter( grepl("PP",activity)) %>% process_map()
names(events1)
events1 %>% arrange(desc(timestamp))
events1 %>% slice_activities(1:5)
events1 %>% slice_events(1:10)
events1 %>%  group_by_case() %>% first_n(10) %>%  trace_explorer(coverage = .8)
mapping(events1)
events1 %>% activity_presence() %>%  plot
events1 %>%  start_activities("resource-activity")
events1 %>%  end_activities("resource-activity")
events1 %>%   process_map(type = frequency("relative"))
events1 %>%   process_map(type = frequency("absolute"))
events1 %>%   process_map(type = frequency("absolute_case"))
events1 %>%   process_map(type = frequency("relative_case"))

events1 %>%   process_map(type = frequency("relative_case"), color_scale = "Purples")
events1 %>%  process_map(performance(median, "days"))
events1 %>%  process_map(performance(mean, "hours"))
events1 %>%  process_map(type_nodes = frequency("relative_case"),    type_edges = performance(mean))
events1 %>% group_by(activity) %>%   n_cases()
#whether a case followed a frequent or infrequent path. 
events1 %>% trace_coverage(level = "case", append = T, append_column = "relative") %>% mutate(frequent = relative_case_trace_coverage > 0.2) %>% filter(frequent=="FALSE") %>% select(sname, frequent)



#
activities(events1)
activity_labels(events1)
activity_id(events1)
activity_instance_id(events1)
arrange(events1)
case_list(events1)
?arrange
group_by_case(events1) %>% first_n(2)
group_by_case(events1) %>% last_n(1)
lifecycles(events1)
n_traces(events1)
trace_list(events1)
case_list(events1)
number_of_repetitions(events1, level="activity", type="all")
number_of_repetitions(events1, level="activity", type="repeat")
number_of_selfloops(events1, level="case", type = "redo")




#----egs------
summary(traffic_fines)
range(traffic_fines$amount, na.rm=T)
str(traffic_fines$amount)
animate_process(edeaR::filter_trace_frequency(bupaR::sample_n(traffic_fines,1000),percentage=0.5),   legend = "color", mode = "relative",  mapping = token_aes(color = token_scale("amount", scale = "linear",  range = c("yellow","red"))), duration=20)


head
animate_process(edeaR::filter_trace_frequency(bupaR::sample_n(traffic_fines,1000),percentage=0.5),   legend = "color", mode = "relative",  mapping = token_aes(color = token_scale("totalpaymentamount", scale = "linear",  range = c("yellow","red"))), duration=20)

range(events1$finalMarks)
mapping(traffic_fines)
summary(traffic_fines)
summary(events1)
events1$marks = as.character(events1$finalMarks)
events1 %>% sample_n(10) %>% filter_trace_frequency(percentage=.5) %>%  animate_process(legend = "color", mode = "relative",  mapping = token_aes(color = token_scale("marks", scale = "linear",  range = c("yellow","red"))), duration=20)

rn <- events1 %>% filter(finalMarks > 400) %>% pull(rollno)
rnlist <- unique(rn)
rnlength <- length(rnlist)
events1 %>% filter(rollno %in% rnlist) %>%  animate_process(legend = "color", mode = "relative",  mapping = token_aes(color = token_scale("rollno", scale = "ordinal", range = RColorBrewer::brewer.pal(rnlength, "Paired"))), duration=20)

#range = RColorBrewer::brewer.pal(rnlength, "Paired"))))



animate_process(events1,legend = "color", mode = "relative",  mapping = token_aes(color = token_scale("finalmarks", scale = "linear",  range = c("yellow","red"))), duration=10)
str(events1$finalMarks)

animate_process(traffic_fines[1:100,],legend = "color", mode = "relative",  mapping = token_aes(color = token_scale("finalamount", scale = "linear",  range = c("yellow","red"))), duration=10)

animate_process(edeaR::filter_trace_frequency(bupaR::sample_n(events1,25),percentage=1),   legend = "color", mode = "relative",  mapping = token_aes(color = token_scale("finalmarks", scale = "linear",  range = c("yellow","red"))), duration=10)
