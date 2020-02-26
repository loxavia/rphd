#sample Case - PM

library(gsheet)
library(bupaR)
library(edeaR)
library(processmapR)

link='https://docs.google.com/spreadsheets/d/139d4RD4wnf4Qvp2mg2DCMWCOOemmiQM1NA9K7vn5Ak0/edit#gid=1506524608'

df = as.data.frame(gsheet2tbl(link))
head(df)
df
names(df)
str(df)
event <- bupaR::eventlog(eventlog=df, case_id='case', activity_id ='activity', activity_instance_id= 'activity_instance', lifecycle_id='lifecycle', timestamp = 'timestamp', resource_id ='resource')
event

event %>% process_map()
n_activities(event)
n_cases(event)
resource_map(event)
dotted_chart(event)
precedence_matrix(event)
precedence_matrix(event) %>% plot()
process_matrix(event)  %>% plot()
trace_explorer(event)
animate_process(event, duration=10)


#assigning activity_instance
head(df)
df1 = df
df2 <- df1 %>% group_by(activity, case) %>% summarise(count=n()) %>% tibble::rowid_to_column("activity_instance2") %>% select(-c(count)) %>% merge(df1)
df2
