#LMSDU - Python
pacman::p_load(dplyr, googlesheets4, ggplot2, bupaR, edeaR, lubridate, processmapR, processanimateR)
folder = 'E:/data/LMSDU/'
list.files(folder)
#(filePath = paste0(folder,'logs_CxAPPG_20200419-2043.csv')) #19Apr
(filePath = paste0(folder,'logs_CxAPPG_20200420-1049.csv')) #20Apr:1049h
lmsData = read.csv(filePath, stringsAsFactors = F)
courseName = 'Python Analytics in Moodle LMS'
#Summarise Data-----
dim(lmsData)
summary(lmsData)
head(lmsData)

#colNames-----
colLMS = c('time','user','affectedUser', 'eventContext', 'component', 'eventName', 'description','origin','ipaddress')
(names(lmsData) = colLMS)
head(lmsData)



#list of non-students-----
teachers = c('Dhiraj Upadhyaya','Kanika Tiwari','Pooja Gupta','-','Sharad Shah','Guest user')
names(lmsData)

lmsData %>% group_by(user) %>% summarise(n=n()) %>% ggplot(., aes(x='', y=n, fill=user)) + geom_bar(stat='identity')  + guides(fill=F) + labs(title='Users and their event Count', subtitle = paste(courseName,' Total Events ', nrow(lmsData))) + geom_text(aes(label=n, y=n), position = position_dodge2(.7)) + theme(strip.text.x=element_text(size=rel(.8))) + facet_wrap(labeller = labeller(user = label_wrap_gen(20)), ~ reorder(user, -n ))


#remove cols not required----
lmsData2 <- lmsData %>% select(-c(affectedUser, description, ipaddress, origin))
head(lmsData2)


#remove non-students
table(lmsData$user)
lmsData2B <- lmsData2 %>% filter(!user %in% teachers)
table(lmsData2B$user)

#remove teachers/managers Data-----
table(lmsData2B$user)
lmsData2B %>% group_by(user) %>% summarise(n=n()) %>% ggplot(., aes(x='', y=n, fill=user)) + geom_bar(stat='identity')  + guides(fill=F) + labs(title='Users and their event Count', subtitle = paste(courseName,' Total Events ', nrow(lmsData))) + geom_text(aes(label=n, y=n), position = position_dodge2(.7)) + theme(strip.text.x=element_text(size=rel(.8))) + facet_wrap(labeller = labeller(user = label_wrap_gen(20)), ~ reorder(user, -n ))


#structure the data----
str(lmsData2B)
lmsData2B$time = as.POSIXct(lmsData2B$time,format='%d/%m/%y, %H:%M')
range(lmsData2B$time)

table(lmsData2B$eventContext) #activity Type + Activity Name
table(lmsData2B$eventName) #lifecycle
table(lmsData2B$component)  #resource

lmsData2C <- lmsData2B %>% tidyr::separate(col='eventContext', into = c('activityType','activity'), sep = ':', remove=T)
head(lmsData2C)

#count of Events----
lmsData2C %>% group_by(eventName) %>% summarise(n=n()) %>% ggplot(., aes(x='', y=n, fill=eventName)) + geom_bar(stat='identity')+ guides(fill=F) + labs(title='Event Action and their Count', subtitle = paste(courseName,' Total Events- ', nrow(lmsData2B))) + geom_text(aes(label=n, y=n), position = position_dodge2(.7)) + theme(strip.text.x=element_text(size=rel(.8)))  + facet_wrap( labeller = labeller(user = label_wrap_gen(20)), ~ reorder(eventName, -n)) 

#count of Activity Type-----
lmsData2C %>% group_by(activity) %>% summarise(n=n()) %>% ggplot(., aes(x='', y=n, fill=activity)) + geom_bar(stat='identity')+ guides(fill=F) + labs(title='Activity and their Count', subtitle = paste(courseName,' Total Events - ', nrow(lmsData2B))) + geom_text(aes(label=n, y=n), position = position_dodge2(.7)) + theme(strip.text.x=element_text(size=rel(.8)))  + facet_wrap( labeller = labeller(user = label_wrap_gen(20)), ~ reorder(activity, -n)) 

names(lmsData2C)
#event Name & context---
lmsData2C %>% group_by(activityType, activity) %>% summarise(n=n()) %>% ggplot(., aes(x=activityType, y=activity, fill=n)) + geom_tile(color='black') + scale_fill_gradient2(low='blue',high='green') + labs(title='Activity Type & Name and their Count', subtitle = paste(courseName,' Total Events- ', nrow(lmsData2B))) + geom_text(aes(label=n), size=rel(2)) + theme(axis.text.x=element_text(angle=90, size=rel(.9)), axis.text.y=element_text(angle=0, size=rel(.9))) + scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) + scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 40))

#simplify the names of eventContext----
grep('Course', lmsData2B$eventContext, ignore.case = T)
lmsData2B %>% slice(grep('Course:', eventContext, ignore.case = T)) %>% select(eventContext) %>% head()
lmsData2B %>% slice(grep('Attendance:', eventContext, ignore.case = T)) %>% select(eventContext) %>% head()

#function to replace long events with small-----
ff = function(x, patterns, replacements = patterns, fill = NA, ...)
{  stopifnot(length(patterns) == length(replacements))
   ans = rep_len(as.character(fill), length(x))    
   empty = seq_along(x)
   for(i in seq_along(patterns)) {
    greps = grepl(patterns[[i]], x[empty], ...)
    ans[empty[greps]] = replacements[[i]]  
    empty = empty[!greps]
  }
    return(ans)
}

#--------------------
#have standard Names for activity Types
table(lmsData2C$activityType)
pattern1 = c('Course','Assignment','Page|PPT','Checklist','Questionnaire','Feedback','Quiz','URL','Glossary','Forum','Folder','Attendance', 'FlashCard| Quiz Venture| Crossword|Student Quiz')
replacement1 = c("Course",'Assignment','PPT','Checklist','Survey','Survey','Quiz','URL','Glossary','Forum','Folder','Attendance','Games')
cbind(pattern1, replacement1)
lmsData2C$activityType2 <- ff(lmsData2C$activityType, patterns=pattern1, replacements=replacement1, fill = 'OtherTypes', ignore.case = TRUE)
lmsData2C %>% filter(activityType2 == 'OtherTypes') %>% select(activityType)
#above should be nil
dim(lmsData2C)
#----------------------

table(lmsData2C$activityType)
lmsData2C %>% group_by(activityType2) %>% summarise(n=n()) %>% ggplot(., aes(x='', y=n, fill=activityType2)) + geom_bar(stat='identity') + guides(fill=F) + labs(title='Activty Type and their event Count', subtitle = paste(courseName,' Total Events ', nrow(lmsData2C))) + geom_text(aes(label=n, y=n), position = position_dodge2(.7)) + theme(strip.text.x=element_text(size=rel(.8)))  + facet_wrap( labeller = labeller(user = label_wrap_gen(20)), ~reorder(activityType2,-n))

lmsData2C %>% group_by(activityType, activity) %>% summarise(n=n()) %>% ggplot(., aes(x=activityType, y=activity, fill=n)) + geom_tile(color='black') + scale_fill_gradient2(low='blue',high='green') + labs(title='Activity Type & Name and their Count', subtitle = paste(courseName,' Total Events ', nrow(lmsData2C))) + geom_text(aes(label=n), size=rel(2)) + theme(axis.text.x=element_text(angle=90, size=rel(.9)), axis.text.y=element_text(angle=0, size=rel(.9))) + scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) + scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 40))

#-----------
table(lmsData2C$eventName)
pattern2 = c('submitted','uploaded','resume','taken','Viewed','reviewed','submitted','saved','shown','updated','created','ended', 'graded','up','downloaded', 'started')
replacement2 = c('Submitted','Uploaded','Resumed','Taken','Viewed','Reviewed','Submitted','Saved','Shown','Updated','Created','Ended', 'Graded','Up','downloaded','Started')
cbind(pattern2, replacement2)
lmsData2C$eventAction <- ff(lmsData2C$eventName, patterns=pattern2, replacements=replacement2, fill = 'OtherAction', ignore.case = TRUE)

table(lmsData2C$eventAction)
lmsData2C %>% filter(eventAction == 'OtherAction')  #nil is required

lmsData2C %>% group_by(activityType, eventAction) %>% summarise(n=n()) %>% ggplot(., aes(x=activityType, y=eventAction, fill=n)) + geom_tile(color='black') + scale_fill_gradient2(low='blue',high='green') + labs(title='Activity Type & Action and their Count', subtitle = paste(courseName,' Total Events ', nrow(lmsData2C))) + geom_text(aes(label=n), size=rel(2)) + theme(axis.text.x=element_text(angle=90, size=rel(.9)), axis.text.y=element_text(angle=0, size=rel(.9))) + scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) + scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 40))

lmsData2C %>% group_by(activityType, activity, eventAction) %>% summarise(n=n()) %>% ggplot(., aes(x=activityType, y=activity, fill=n)) + geom_tile(color='black') + scale_fill_gradient2(low='blue',high='green') + labs(title='Activity Type & Name and Action : Count', subtitle = paste(courseName,' Total Events ', nrow(lmsData2C))) + geom_text(aes(label=n), size=rel(2)) + theme(strip.text.x=element_text(size=rel(.8), angle=90), axis.text.x=element_text(angle=90, size=rel(.9),hjust=1, vjust=0), axis.text.y=element_text(angle=0, size=rel(.9))) + scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) + scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 40)) + facet_grid( . ~ eventAction, scales='free', space='free')

names(lmsData2C)
lmsData2C %>% group_by(activityType, user, eventAction) %>% summarise(n=n()) %>% ggplot(., aes(x=activityType, y=user, fill=n)) + geom_tile(color='black') + scale_fill_gradient2(low='blue',high='green') + labs(title=paste0('Activity Type & Name and their Count; ', courseName,'; Total Events ', nrow(lmsData2C))) + geom_text(aes(label=n), size=rel(2)) + theme(strip.text.x=element_text(size=rel(.8), angle=90), axis.text.x=element_text(angle=90, size=rel(.9), hjust=1, vjust=0), axis.text.y=element_text(angle=0, size=rel(.9))) + scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) + scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 40)) + facet_grid( . ~ eventAction, scales='free', space='free')

#users grades---
#Activity Completion
list.files(folder)
(filePath = paste0(folder,'progresscxappg.csv'))
actComp1 = read.csv(filePath, stringsAsFactors = F, skip=0)
dim(actComp1)
head(actComp1)
summary(actComp1)
actComp1 <- actComp1 %>% select (!starts_with('X')) %>% select(-Email.address)
actComp1 <- actComp1 %>% janitor::clean_names(case='all_caps')

summary(actComp1)
names(actComp1)
str(actComp1)
actComp2 <- janitor::remove_empty(actComp1, which = "cols")
summary(actComp2)
actComp2 %>% select(ASSIGNMENT_TWITTER) %>% head()
as.POSIXct(actComp2$ASSIGNMENT_TWITTER, '%A, %d %B %Y, %I:%M %p', tz='UTC')
names(actComp2)
actComp2[ , -1] <- lapply(actComp2[ , -1],as.POSIXct, format='%A, %d %B %Y, %I:%M %p', tz='UTC')
str(actComp2)

#grades
#http://learninganalytics.in/grade/export/xls/index.php?id=27
#http://learninganalytics.in/grade/export/txt/index.php?id=27
#replace - & (Real) with blank
list.files(folder)
(filePath = paste0(folder,'CxAPPG Grades-20200419_1423-comma_separated.csv'))
grades = read.csv(filePath)
dim(grades)
names(grades)
grades2 <- grades %>% select(-starts_with('Last')) %>% select(-c(Email.address, ID.number, Institution, Department)) %>% unite(col='user', c(First.name, Surname), sep=' ') %>% janitor::clean_names(case='all_caps')
head(grades2)
names(grades2)
str(grades2)
grades2[,-1] <- lapply(grades2[,-1], as.numeric)
grades2 %>% group_by(USER) %>% rowSums(na.rm=T)
grades2B <- grades2 %>% mutate(totalGrade = select(., 2:18) %>% apply(1, sum, na.rm=TRUE))
grades2C <- grades2B[,c("USER",'totalGrade')]
head(grades2C)

grades2C %>% select(USER)
unique(lmsData3B$user) %in%  unique(grades2C$USER)
#students who have done well so far---
(topUsers <- grades2C %>% top_frac(.2, totalGrade) %>% pull(USER))

#data ready for further processing----
#Students who have completed activity
#Top 20% students names
#Logs formatted for PM


#Summarise Data-----
#------------
table(lmsData2C$component)
names(lmsData2C)
head(lmsData2C)

#lmsData3-------
lmsData3 <- lmsData2C %>% select(time, user, activityType, activity, component, eventName, activityType2, eventAction)

head(lmsData3)
table(lmsData3$user) #case
table(lmsData3$time) #timestamp
table(lmsData3$activity) #activity - Names
table(lmsData3$eventName) #activity Descp
table(lmsData3$activityType) #activity Type
table(lmsData3$component) #resource
table(lmsData3$eventAction)  #lifecycle

lmsData3 %>% filter(component == 'System')  %>% select(activityType)
lmsData3 %>% filter(activityType == 'Course')  %>% select(activityType)
lmsData3 %>% filter(component == 'System')  %>% select(activityType)
lmsData3 %>% filter(component == 'System')  %>% group_by(activity, activityType, eventAction) %>% summarise(n=n())

lmsData3 %>% group_by(activityType, component, eventAction) %>% summarise(n=n()) %>% as.data.frame()
#course Types do not have useful work

#lmsData3B-----
lmsData3B <- lmsData3 %>% rename(activity = activity, resource=component, lifecycle = eventAction, timestamp=time) %>% filter( activityType != 'Course')
head(lmsData3B)

#create event object
names(lmsData3B)
head(lmsData3B)
str(lmsData3B)
events1 <- simple_eventlog(eventlog = lmsData3B, case_id = 'user', activity_id = 'activity', timestamp = 'timestamp')
events1

#summary------
events1 %>%  summary()
events1 %>%  process_map()
events1 %>%  n_activities()
events1 %>%  activities()
events1 %>%  activity_frequency (percentile=.6)
?
events1 %>%  cases()  #start and end time of each case with trace, length, activities participated
events1 %>%  resources()  # no resources...
events1 %>%  filter_case(cases = topUsers) %>% trace_explorer(raw_data = T, coverage=1)
events1 %>% activity_presence() %>% plot
#trace-----
events1 %>% trace_explorer(coverage=1)
events1 %>% trace_coverage(level = 'trace') 
events1 %>% trace_explorer(n_traces=3, type='frequent') #3 frequent first
events1 %>% trace_explorer(n_traces=3, show_labels = F)
events1 %>% trace_explorer(n_traces=3, show_labels = F, raw_data = T)
table(events1$activityType)
events1 %>% filter(activityType %in% c('Assignment', 'Quiz')) %>% filter_case(case = topUsers) %>% number_of_repetitions(type="all", level='log')
events1 %>% filter(activityType %in% c('Assignment', 'Quiz')) %>% filter_case(case = topUsers) %>% number_of_repetitions(type="all", level='log')

events1 %>% filter(activityType %in% c('Assignment', 'Quiz')) %>% filter_case(case = topUsers) %>% number_of_selfloops(append = T)
events1 %>% number_of_selfloops(level="case") %>% ggplot() +  geom_histogram(aes(absolute), bins=5)

events1 %>% filter(activityType %in% c('Assignment', 'Quiz')) %>% filter_case(case = topUsers) %>% animate_process(legend='color', repeat_count = 1, mode='absolute', duration=30, sec=frequency('relative'), mapping = token_aes( color = token_scale("user", scale = "ordinal",range = RColorBrewer::brewer.pal(7, "Paired"))))
names(events1)
table(events1$activityType, events1$lifecycle)
events1 %>% filter(activityType %in% c('Assignment', 'Quiz') ) %>% filter(lifecycle %in% c('Submitted','Started','Created','Uploaded')) %>% filter_case(case = topUsers) %>% animate_process(duration=20)



#create a new event type with resources ------
events2 <- lmsData3B %>% mutate(activity_instance_id = 1:nrow(.)) %>% eventlog(case_id = 'user', activity_id = 'activity', timestamp = 'timestamp', activity_instance_id = 'activity_instance_id', lifecycle_id= 'lifecycle', resource='resource')

events2 %>%  set_case_id("user")  #manually setting cases ID
#set_activity_id :
#mapping_events2 <- mapping(events2)
#events2 %>% re_map(mapping_events2)# undo changes

#summary-----
events2 %>% mapping()
events2 %>% summary()
events2 %>% n_activities()
events2 %>% activities()
events2 %>% cases()  #start and end time of each case with trace, length, activities participated
events2 %>% resources()  # resources
events2 %>% precedence_matrix() %>% plot()
events2 %>% activity_presence() %>% plot

#traces----
events2 %>% trace_coverage("trace") %>% plot()
#relationship between the number of different activity sequences (i.e. traces) and the number of cases they cover. : no predominant activity
events2 %>% trace_length("log") %>% plot
#the number of activity instances for each case
events2 %>% trace_explorer(coverage=1)
events2 %>% trace_coverage(level = 'trace') 
events2 %>% trace_explorer(n_traces=3, type='frequent') #3 frequent first
events2 %>% trace_explorer(n_traces=3, show_labels = F)
events2 %>% trace_explorer(n_traces=3, show_labels = F, raw_data = T)


#resources------
events2 %>% resource_involvement("resource") %>% plot
events2 %>% resource_matrix() %>% plot()
events2 %>% resource_labels()
events2 %>% resource_frequency()
events2 %>% group_by(resource) %>% summarise(n=n()) %>% ggplot(., aes(x=resource,y=n, fill=resource)) + geom_bar(stat='identity') + geom_text(aes(label=n, y=n)) + coord_flip() + guides(fill=F) + labs(title='Resources Used : Count')

events2 %>% animate_process(duration=10)  #very difficult to see


#activities----
#filter with type of activities
events2 %>% group_by(activityType, lifecycle) %>% summarise(n=n()) %>% as.data.frame()
events2 %>% filter((activityType %in% c('Assignment','Quiz')) & lifecycle %in% c('Submitted','Updated')) %>% n_cases()
events2 %>% filter((activityType %in% c('Assignment','Quiz')) & lifecycle %in% c('Submitted','Updated')) %>% animate_process(duration=10, mapping = token_aes(color=token_scale('red'), size=token_scale(10)), repeat_count = 2)

events2 %>%  activity_frequency(level="activity", sort=T, append_column=T)
events2 %>% start_activities(level="resource-activity")
events2 %>% start_activities(level= "log", append_column=T)
#first resource may be imp to motivate students
#events2 %>% start_activities(level="resource")


?start_activities
events2 %>% trace_list() %>% select(trace)

events2 %>% throughput_time(level = "case",append = TRUE)

head(events2)
events2 %>%  group_by_case %>% mutate(gave_Quiz_OR_Assign = any(activityType == "Quiz", activityType == 'Assignment')) %>% ungroup_eventlog() %>% group_by(gave_Quiz_OR_Assign) %>% n_cases()

events2 %>%  group_by_case %>% mutate(gave_Quiz_AND_Assign = all(activityType == "Quiz", activityType == 'Assignment')) %>% ungroup_eventlog() %>% group_by(gave_Quiz_AND_Assign) %>% n_cases()

#appending the relative trace coverage, we can create a variable that indicates whether a case followed a frequent or infrequent path. The following code adds a variable frequent whioch is TRUE if more than 20% of the cases share the same trace.
events2 %>% trace_coverage(level = "case",append = TRUE, append_column = "relative") %>% mutate(frequent = relative_case_trace_coverage > 0.2) %>% group_by(frequent) %>% processing_time() 


events2Time <- events2 %>% group_by(user, activity) %>% summarise(start=last(timestamp), end =first(timestamp)) %>% mutate(durActivity = difftime(end, start, units='mins')) %>% arrange(-durActivity) %>% select(c(user, activity, start, end, durActivity)) 
events2Time[1:5, c(1,2,3,4,5)]

events2 %>% trace_explorer()
events2 %>% resource_matrix() 
events2 %>% resource_involvement() #user- resource usage
events2 %>% resource_specialisation()
events2 %>% standardize_lifecycle()

events2 %>% processing_time()

?standardize_lifecycle
events1 %>% process_map()
events1 %>% filter_activity(activities =c('PPT','Assignment','Quiz')) %>% process_map()
events1 %>% n_cases()

names(lmsData3)
head(lmsData3)
events2 <- lmsData3 %>% mutate(activity_instance_id = 1:nrow(.)) %>% eventlog(case_id = 'user', activity_id = 'context2', timestamp = 'time', activity_instance_id = 'activity_instance_id', lifecycle_id= 'eventName2', resource='component' )

head(lmsData3)
lmsActivityLog <- lmsData3 %>% rename('complete' = time) %>% activitylog(case_id='user', activity_id='context2', resource_id='component', lifecycle_ids='complete')
  
events2

events2 %>% process_map()
events2 %>% resource_frequency() %>% plot()
events2 %>% filter_activity(activities =c('PPT','Assignment','Quiz')) %>% processmapR::process_map(sec=frequency('absolute'))
events2 %>% filter_activity(activities =c('PPT','Assignment','Quiz')) %>% processmapR::process_map(sec=frequency('relative'), rankdir='TB')
events2 %>% filter_activity(activities =c('PPT','Assignment','Quiz')) %>% processmapR::process_map(sec=frequency('absolute-case'), rankdir='BT')
events2 %>% filter_activity(activities =c('PPT','Assignment','Quiz')) %>% processmapR::process_map(sec=frequency('relative-case'), rankdir='RL')


events1  %>% filter_activity(activities =c('PPT','Assignment','Quiz')) %>% animate_process(renderer = renderer_graphviz(), repeat_count = 2)

events2 %>% resource_frequency("resource")
events2 %>% filter_resource_frequency(perc = 0.80) %>%   resources()
events2 %>% filter_activity_frequency(interval = c(3, NA)) %>%   resources()
events2 %>% activity_labels()

video2c <- events2 %>% filter_activity(activities = c('Course','Checklist','Attendance','Folder') ,reverse=T) %>%  animate_process(legend=T, mode='absolute', duration=20, sec=frequency('relative'), mapping = token_aes(color = token_scale("red"), size = token_scale(10)), repeat_count = 2)
video2c
animate_process(events2, legend=T, mode='absolute', duration=20, sec=frequency('relative'), mapping = token_aes(color = token_scale("red"), size = token_scale(10)), repeat_count = 2)

events2 %>% filter_activity(activities = c('Course','Checklist','Attendance','Folder') ,reverse=T) %>% process_map(sec=frequency('relative'))

V2D <- events2 %>% filter_activity(activities = c('Course','Checklist','Attendance','Folder') ,reverse=T) %>%  animate_process(legend=F, mode='absolute', duration=60, sec= frequency('relative'), mapping = token_aes(size = token_scale(15), color = token_scale("user", scale = "ordinal",range = RColorBrewer::brewer.pal(7, "Paired"))))

htmlwidgets::saveWidget(V2D,file='E:/PMO/PMV/V2D.html', libdir='E:/PMO/PMV/libdep', selfcontained = T)

events2
