#LMSDU - Python
pacman::p_load(dplyr, ggplot2, bupaR, edeaR, lubridate, processmapR, processanimateR)
folder = 'E:/data/LMSDU/'
list.files(folder)
(filePath = paste0(folder,'logs_CxAPPG_20200415-1510.csv'))
lmsData = read.csv(filePath, stringsAsFactors = F)
courseName = 'Python Analytics in Moodle LMS'
#Summarise Data-----
dim(lmsData)
summary(lmsData)

#remove teachers/managers Data-----
table(lmsData$user)
lmsData %>% group_by(user) %>% summarise(n=n()) %>% ggplot(., aes(x='', y=n, fill=user)) + geom_bar(stat='identity') + facet_wrap(user ~., labeller = labeller(user = label_wrap_gen(20))) + guides(fill=F) + labs(title='Users and their event Count', subtitle = paste(courseName,' Total Events ', nrow(lmsData))) + geom_text(aes(label=n, y=n), position = position_dodge2(.7)) + theme(strip.text.x=element_text(size=rel(.8)))

#list of non-students-----
teachers = c('Dhiraj Upadhyaya','Kanika Tiwari','Pooja Gupta','-','Sharad Shah','Guest user')
names(lmsData)
#colNames-----
colLMS = c('time','user','affectedUser', 'eventContext', 'component', 'eventName', 'description','origin','ipaddress')
(names(lmsData) = colLMS)
head(lmsData)

#remove cols not required----
lmsData2 <- lmsData %>% select(-c(affectedUser, description, ipaddress, origin))
head(lmsData2)

#remove non-students
table(lmsData$user)
lmsData2B <- lmsData2 %>% filter(!user %in% teachers)
table(lmsData2B$user)

#structure the data----
str(lmsData2B)
lmsData2B$time = as.POSIXct(lmsData2B$time,format='%d/%m/%y, %H:%M')
range(lmsData2B$time)

table(lmsData2B$eventContext)
table(lmsData2B$eventName)

lmsData2B %>% group_by(eventName) %>% summarise(n=n()) %>% ggplot(., aes(x='', y=n, fill=eventName)) + geom_bar(stat='identity') + facet_wrap(eventName ~., labeller = labeller(user = label_wrap_gen(20))) + guides(fill=F) + labs(title='Event Names and their Count', subtitle = paste(courseName,' Total Events ', nrow(lmsData2B))) + geom_text(aes(label=n, y=n), position = position_dodge2(.7)) + theme(strip.text.x=element_text(size=rel(.8)))

lmsData2B %>% group_by(eventContext) %>% summarise(n=n()) %>% ggplot(., aes(x='', y=n, fill=eventContext)) + geom_bar(stat='identity') + facet_wrap(eventContext ~., labeller = labeller(user = label_wrap_gen(20))) + guides(fill=F) + labs(title='Event Context and their Count', subtitle = paste(courseName,' Total Events ', nrow(lmsData2B))) + geom_text(aes(label=n, y=n), position = position_dodge2(.7)) + theme(strip.text.x=element_text(size=rel(.8)))

lmsData2B %>% group_by(eventName, eventContext) %>% summarise(n=n()) %>% ggplot(., aes(x=eventName, y=eventContext, fill=n)) + geom_tile(color='black') + scale_fill_gradient2(low='blue',high='green') + labs(title='Event Name & Context and their Count', subtitle = paste(courseName,' Total Events ', nrow(lmsData2B))) + geom_text(aes(label=n), size=rel(2)) + theme(axis.text.x=element_text(angle=90, size=rel(.9)), axis.text.y=element_text(angle=0, size=rel(.9))) + scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) + scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 40))

#+ scale_y_discrete(labels = abbreviate) 

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

table(lmsData2B$eventContext)
lmsData2C <- lmsData2B %>% tidyr::separate(col=eventContext, into=c('activity','actName'), sep=':', remove=F) %>% mutate(actName = gsub('"', '', actName))
#remove "" from actName
#--------------------
#pattern1 = c('Course:','Assignment:','Page:|PPT','Checklist:','Questionnaire:','Feedback:','Quiz:','URL:','Glossary:','Forum:','Folder:','Attendance:')
#replacement1 = c("Course",'Assignment','PPT','Checklist','Survey','Survey','Quiz','URL','Glossary','Forum','Folder','Attendance')
#cbind(pattern1, replacement1)
#lmsData2$context2 <- ff(lmsData2$eventContext, patterns=pattern1, replacements=replacement1, fill = 'OtherContext', ignore.case = TRUE)
#----------------------

table(lmsData2C$activity)
lmsData2C %>% group_by(activity) %>% summarise(n=n()) %>% ggplot(., aes(x='', y=n, fill=activity)) + geom_bar(stat='identity') + facet_wrap(activity ~., labeller = labeller(user = label_wrap_gen(20))) + guides(fill=F) + labs(title='Activty and their event Count', subtitle = paste(courseName,' Total Events ', nrow(lmsData2C))) + geom_text(aes(label=n, y=n), position = position_dodge2(.7)) + theme(strip.text.x=element_text(size=rel(.8)))

gsub('" ', '', lmsData2C$actName)
lmsData2C %>% group_by(activity, actName) %>% summarise(n=n()) %>% ggplot(., aes(x=activity, y=actName, fill=n)) + geom_tile(color='black') + scale_fill_gradient2(low='blue',high='green') + labs(title='Event Type & Name and their Count', subtitle = paste(courseName,' Total Events ', nrow(lmsData2C))) + geom_text(aes(label=n), size=rel(2)) + theme(axis.text.x=element_text(angle=90, size=rel(.9)), axis.text.y=element_text(angle=0, size=rel(.9))) + scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) + scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 40))

#-----------
table(lmsData2C$eventName)
pattern2 = c('submitted','uploaded','resume','taken','Viewed','reviewed','submitted','saved','shown','updated','created','ended', 'graded','up','downloaded')
replacement2 = c('Submitted','Uploaded','Resumed','Taken','Viewed','Reviewed','Submitted','Saved','Shown','Updated','Created','Ended', 'Graded','Up','downloaded')
cbind(pattern2, replacement2)
lmsData2C$eventAction <- ff(lmsData2C$eventName, patterns=pattern2, replacements=replacement2, fill = 'OtherEvent', ignore.case = TRUE)

table(lmsData2C$eventAction)

lmsData2C %>% group_by(activity, eventAction) %>% summarise(n=n()) %>% ggplot(., aes(x=activity, y=eventAction, fill=n)) + geom_tile(color='black') + scale_fill_gradient2(low='blue',high='green') + labs(title='Event Name & Action and their Count', subtitle = paste(courseName,' Total Events ', nrow(lmsData2C))) + geom_text(aes(label=n), size=rel(2)) + theme(axis.text.x=element_text(angle=90, size=rel(.9)), axis.text.y=element_text(angle=0, size=rel(.9))) + scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) + scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 40))

lmsData2C %>% group_by(activity, actName, eventAction) %>% summarise(n=n()) %>% ggplot(., aes(x=activity, y=actName, fill=n)) + geom_tile(color='black') + scale_fill_gradient2(low='blue',high='green') + labs(title='Event Type & Name and their Count', subtitle = paste(courseName,' Total Events ', nrow(lmsData2C))) + geom_text(aes(label=n), size=rel(2)) + theme(strip.text.x=element_text(size=rel(.8), angle=90), axis.text.x=element_text(angle=90, size=rel(.9)), axis.text.y=element_text(angle=0, size=rel(.9))) + scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) + scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 40)) + facet_grid( . ~ eventAction, scales='free', space='free')

lmsData2C %>% group_by(activity, user, eventAction) %>% summarise(n=n()) %>% ggplot(., aes(x=activity, y=user, fill=n)) + geom_tile(color='black') + scale_fill_gradient2(low='blue',high='green') + labs(title=paste0('Event Type & Name and their Count ', courseName,' Total Events ', nrow(lmsData2C))) + geom_text(aes(label=n), size=rel(2)) + theme(strip.text.x=element_text(size=rel(.8), angle=90), axis.text.x=element_text(angle=90, size=rel(.9)), axis.text.y=element_text(angle=0, size=rel(.9))) + scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 20)) + scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 40)) + facet_grid( . ~ eventAction, scales='free', space='free')

#users grades---


#------------
table(lmsData2$component)

names(lmsData2)
lmsData3 <- lmsData2 %>% select(-c(affectedUser, eventContext, eventName, description,ipaddress))
head(lmsData3)


lmsData2 %>% slice(grep('Questionnaire', lmsData2$eventContext, ignore.case = T)) %>% select(eventContext)
lmsData2 %>% select(eventContext)
table(lmsData2$eventContext)
lmsData2 %>% filter(component == 'System')  %>% select(eventContext)

lmsData3 %>% group_by(component, context2, eventName2) %>% summarise(n=n()) %>% as.data.frame()
lmsData3 %>% group_by(context2, eventName2) %>% summarise(n=n()) %>% as.data.frame()

#create event object
names(lmsData3)
head(lmsData3)
events1 <- simple_eventlog(eventlog = lmsData3, case_id = 'user', activity_id = 'context2', timestamp = 'time')
events1

events1 %>% animate_process()
events1 %>% trace_list()
events1 %>% trace_explorer()
#events1 %>% standardize_lifecycle()
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
