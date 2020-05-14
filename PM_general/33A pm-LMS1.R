#PM - LMS data

link = "https://docs.google.com/spreadsheets/d/1Rv-8pDWMeonmjn7LCZh55MJtIh0XG5vHX-mIZpcGf1c/edit#gid=513508550"
pacman::p_load(gsheet, bupaR, dplyr, edeaR, processmapR, processanimateR, DiagrammeR,lubridate) 

df = as.data.frame(gsheet2tbl(link))
data = df
names(data)
head(df)
data$Time  #check data format
textdate = "14 February 2020, 4:28 PM"
strptime(textdate, format ='%d %B %Y, %I:%M %p')
as.POSIXct("16/02/20, 14:57", format='%d/%m/%y, %H:%M')
data$Time = as.POSIXct(data$Time, format='%d/%m/%y, %H:%M')
data$Time
str(data)
names(data)
cols1 = c('time','user','affectedUser', 'eventContext', 'component', 'eventName', 'Description','orgin','ipaddress')
names(data) = cols1
head(data)
data$time = as.POSIXct(data$time)
str(data$time)
events <- bupaR::simple_eventlog(eventlog = data,   case_id = 'user',  activity_id = 'eventContext', timestamp = 'time')

events
activity_frequency(events)
events %>% activity_frequency(level='activity')

events %>% activity_frequency(level = "activity") %>% plot()
events %>%  process_map()


processmapR::process_map(events)
animate_process(events)
lm2 <- animate_process(events)

htmlwidgets::saveWidget(lm2,file='E:/lms2.html')



#-----lms3-----
link3 ='https://docs.google.com/spreadsheets/d/1Rv-8pDWMeonmjn7LCZh55MJtIh0XG5vHX-mIZpcGf1c/edit#gid=513508550'
df = as.data.frame(gsheet2tbl(link3))
df
data = df
names(data)
data$Time
textdate = "14/02/20, 13:28"
as.POSIXct (textdate, format ='%d/%m/%y, %H:%M')
data$Time = as.POSIXlt(data$Time, format ='%d/%m/%y, %H:%M')
data$Time
str(data)
names(data)
cols1 = c('time','user','affectedUser', 'eventContext', 'component', 'eventName', 'Description','orgin','ipaddress')
names(data) = cols1
head(data)
str(data$time)
events <- bupaR::simple_eventlog(eventlog = data,   case_id = 'user',  activity_id = 'eventContext', timestamp = 'time')

events
processmapR::process_map(events)
animate_process(events)

m2 <- animate_process(events, sec=frequency('relative'))
htmlwidgets::saveWidget(m2,file='lms.html')