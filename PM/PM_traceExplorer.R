#understanding Trace and Trace Explorer

#libraries
pacman::p_load(gsheet, bupaR, dplyr, edeaR, processmapR, processanimateR, DiagrammeR,lubridate, reshape2)

tlink = 'https://docs.google.com/spreadsheets/d/139d4RD4wnf4Qvp2mg2DCMWCOOemmiQM1NA9K7vn5Ak0/edit#gid=2054003498'

df = as.data.frame(gsheet2tbl(tlink)) 
head(df)

df2 <- reshape2::melt(df, id.vars=c('rollno','gender'), variable.name ='activity', value.name ='activityDate', na.rm=T)
head(df2)
df2$activityDate = as.POSIXct(as.Date(df2$activityDate, format='%d-%b-%y'))
str(df2)
dim(df2)

#create bupaR object -----
events <- bupaR::simple_eventlog(eventlog = df2, case_id = 'rollno', activity_id = 'activity', timestamp = 'activityDate')

events %>% trace_length()

#trace_explorer------
?trace_coverage
events %>% trace_coverage(append=T, level='log', append_column=T)
events %>% trace_coverage(append=T, level='trace', append_column=T)
events %>% trace_coverage(append=T, level='case')
events %>% group_by(gender) %>%  trace_coverage(level='trace', append_column=T)

#fullcoverage to least coverage
events %>% trace_explorer(.abbreviate = F)
events %>% trace_explorer(.abbreviate = F, coverage=1)
events %>% trace_explorer(.abbreviate = F, coverage=.9)
events %>% trace_explorer(.abbreviate = F, coverage=.8)
events %>% trace_explorer(.abbreviate = F, coverage=.5)
events %>% trace_explorer(.abbreviate = F, coverage=.5)
events %>% trace_explorer(.abbreviate = F, coverage=.4)
events %>% trace_explorer(.abbreviate = F, coverage=.3)
events %>% trace_explorer(.abbreviate = F, coverage=.2)
events %>% trace_explorer(.abbreviate = F, coverage=.1)
events %>% trace_explorer(.abbreviate = F, coverage=0)

events %>% trace_explorer(type='frequent')
events %>% trace_explorer(type='infrequent')
events %>% trace_explorer(n_traces=4)
events %>% trace_explorer(n_traces=4, type='infrequent')
events %>% trace_explorer(n_traces=4, type='frequent')


#coverage
?trace_coverage
events %>% trace_coverage(level='trace', append=T)
events %>% trace_coverage(level='log', append=T)
events %>% trace_coverage(level='case', append=T) %>% select(rollno, absolute_case_trace_coverage)
events %>% trace_coverage(level='case', append=T) %>% select(rollno, trace)

#trace_list
events %>% trace_list()
?trace_list

?trace_coverage.eventlog

#grouped events
?group_by_activity
events %>% group_by(gender) %>% trace_explorer(n_traces=10, type='infrequent')

group_by_activity(events) %>% trace_explorer(coverage=1)

events %>% processanimateR::animate_process(duration=30)
video2b <- animate_process(events, duration=20, mode='absolute', sec=frequency('absolute'), mapping = token_aes(color=token_scale('red'), size=token_scale(10)), repeat_count = 2, rankdir='LR', epsilon_time = 1)
video2b
#, initial_state = 'paused'
htmlwidgets::saveWidget(widget= video2b, file=paste('E:/PMO/V5/','video2b','.html',sep=''), selfcontained = T, libdir = 'E:/PMO/V5/libdir')
