#Process with https://www.dataminingapps.com/2017/11/a-process-mining-tour-in-r/

#libraries
pacman::p_load(gsheet, bupaR, dplyr, edeaR, processmapR, processanimateR, DiagrammeR,lubridate, htmlwidgets, googlesheets4, tidyverse) 

patients
dim(patients)
head(patients,5) %>% as.data.frame()
patients %>% process_map()
n_cases(patients)
pm1 <- patients %>% process_map()
get_activities(pm1)
get_flows(pm1)


#LMS ------
slink1 <- 'https://docs.google.com/spreadsheets/d/10yFqVzstMvVnhgUuwft6YoVGumy-d3VW-vJB2AJlr0o/edit#gid=1872143002'

sheets_browse(slink1)
sdata <- read_sheet(slink1, sheet='students')
head(sdata)
#eventStudents <- eventlog(sdata,case_id = "case",  activity_id = "activity", activity_instance_id = "activity_instance",  lifecycle_id = "activity_instance", timestamp = "timestamp", resource_id = "resource_id")
events1 <- bupaR::simple_eventlog(eventlog = sdata,   case_id = 'case',  activity_id = 'activity', timestamp = 'timestamp')
events1

custom1 = custom(FUN=mean, attribute = 'case', color_scale='PuBu',color_edges = 'dodgerblue4')
#custom2 = custom(FUN=sum, attribute = 'marks', units='Rs', color_scale='PuRd',color_edges = 'limegreen') #not working

#darkorange2
events1 %>% process_map()
events1 %>% process_map(custom = custom1)
events1 %>% process_map(custom = custom1, type=frequency('relative'))

(pm1 <- events1 %>% process_map())
pm1
#dotted chart----
(dc1a1 <- events1 %>% dotted_chart(x='absolute', y='start_day'))
(dc1a2 <- events1 %>% dotted_chart(x='absolute', y='start_week'))

(dc1b1 <- events1 %>% dotted_chart(x='relative', units='weeks'))
(dc1b2 <- events1 %>% dotted_chart(x='relative',units='days'))
(dc1b3 <- events1 %>% dotted_chart(x='relative',units='days', add_end_events =T))
(dc1c <- events1 %>% dotted_chart(x='relative_day', unit='days'))
(dc1d <- events1 %>% dotted_chart(x='relative_week', unit='weeks'))

dotted_chart(events1, x = "relative", y ="duration", color = NULL, units ="hours")
dotted_chart(events1, x = "absolute", y ="duration", color = NULL, units ="hours")
dotted_chart(events1, x = "relative", y ="start", color = NULL, units ="hours")
dotted_chart(events1, x = "absolute", y ="start", color = NULL, units ="hours")

#activities-----
get_activities(pm1)
get_flows(pm1)

# frequency
fa1 <- frequency(value = 'absolute', color_scale='PuBu', color_edges ='dodgerblue4')
fa2 <- frequency(value = 'absolute-case', color_scale='PuBu', color_edges ='dodgerblue4')
#https://www.graphviz.org/doc/info/colors.html
fr1 <- frequency(value = 'relative', color_scale='PuBu', color_edges ='dodgerblue4')
fr2 <- frequency(value = 'relative-case', color_scale='PuBu', color_edges ='dodgerblue4')

events1 %>% process_map(type=fa1)
events1 %>% process_map(type=fa2)
events1 %>% process_map(type=fr1)
events1 %>% process_map(type=fr2)

#layout-----
activities(events1)
(dfpositions = data.frame(act=c('PPT','Quiz', 'Assignment'), x=c(10, 20, 30), y=c(10,50,100)))
events1 %>% process_map( layout_pm = layout_pm(fixed_positions = dfposition))
events1 %>% process_map( layout_pm = layout_pm(weight=T))
events1 %>% process_map( layout_pm = layout_pm(edge_cutoff = 0))
#edge ?

#performance-----
?process_map
#no start and end times here---
events1 %>% process_map(type = performance())
events1 %>% process_map(type = performance(FUN=mean))
events1 %>% process_map(type = performance(FUN=max))
events1 %>% process_map(type = performance(units='days'))
events1 %>% process_map(type = performance(FUN=min), units='mins')
events1 %>% process_map(type = performance(FUN=max, color_edges = 'blue'))
events1 %>% process_map(type = performance(FUN=max, color_scale = 'Blues'))
events1 %>% process_map(type = performance(FUN=mean, flow_time='idle_time'))
events1 %>% process_map(type = performance(FUN=mean, flow_time='inter_start_time'))

#only 1 end activity----
x=end_activities(events1, level="activity")    
plot(x)

#resource Map-----
#no resources mapped---
resource_map(events1)
x=end_activities(events1, level="resource")    
plot(x)

#animate ----
events1 %>% processanimateR::animate_process()
events1 %>% processanimateR::animate_process(duration=10)
events1 %>% processanimateR::animate_process(duration=10, activity_callback_select = activity_select_decoration( stroke_width = "15", stroke='red'))

events1 %>% animate_process(renderer = renderer_graphviz(zoom_controls = T, svg_fit = T, svg_contain=F, zoom_initial=11), duration=10)
#zoom_initial ??
events1 %>% animate_process(timeline = F, duration=10)
events1 %>% animate_process(jitter = T, duration=10)
events1 %>% animate_process(mode = 'absolute', duration=10)
events1 %>% animate_process(mode = 'relative', duration=10)
events1 %>% animate_process(mode = 'relative', duration=10, legend='size')
events1 %>% animate_process(mode = 'relative', duration=10, legend='color')
events1 %>% animate_process(initial_state = 'paused', duration=10, legend='color')
events1 %>% animate_process(initial_time = 2, duration=10, legend='color', initial_state = 'paused')

events1 %>% animate_process(repeat_count = 2, duration=10)
events1 %>% animate_process(repeat_delay = 2, duration=10, repeat_count = 2)
events1 %>% animate_process(epsilon_time= 1, duration=10, repeat_count = 2)
events1 %>% animate_process(width=0, height=0,duration=10)
events1 %>% animate_process(width='100%', height='50%',duration=10)

events1 %>% animate_process(duration=10, legend='color', mapping = token_aes(color=token_scale('red')))
events1 %>% animate_process(duration=10, repeat_count = 2, legend='color', mapping = token_aes(color=token_scale('case', scale='ordinal', range=c('red','blue','green','orange','pink'))))

e1V1 <- events1 %>% animate_process(duration=10, sec=frequency('relative'),repeat_count = 2, legend='color', mapping = token_aes(color=token_scale('case', scale='ordinal', range=c('red','blue','green','orange','pink'))))

htmlwidgets::saveWidget(widget=e1V1, file='E:/PMO/VIDEOS/e1V1.html', title='Process Mining Video : Student Learning', libdir ='E:/PMO/VIDEOS/libdep', selfcontained = T)
