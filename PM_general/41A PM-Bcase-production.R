
library(gsheet)
library(bupaR)
library(dplyr)
library(edeaR)
library(processmapR)
library(processanimateR)

link='https://docs.google.com/spreadsheets/d/139d4RD4wnf4Qvp2mg2DCMWCOOemmiQM1NA9K7vn5Ak0/edit#gid=2140056175'

df = as.data.frame(gsheet2tbl(link))
head(df)
dim(df)
names(df)
df$Start.Timestamp = as.POSIXct(df$Start.Timestamp)
df$Complete.Timestamp = as.POSIXct(df$Complete.Timestamp)
names(df)

df1 <- df %>% arrange(Start.Timestamp) %>% group_by(Activity, Case.ID) %>% summarise(count=n(), ftime = first(Start.Timestamp), ltime=last(Complete.Timestamp)) %>% arrange(ftime) %>% tibble::rowid_to_column("act_inst") %>% select(-c(count, ftime, ltime)) %>% merge(df) %>% arrange(act_inst)
head(df1)

prod_event = df1 %>%
  eventlog(
    case_id = "Case.ID",
    activity_id = "Activity",
    activity_instance_id = "act_inst",
    lifecycle_id = "Rework",
    timestamp = "Complete.Timestamp",
    resource_id = "Worker.ID"
  )
prod_event %>% n_activities

activity_data <- prod_event %>%   activity_frequency(level = "activity")
activity_data_reduced <- activity_data[activity_data$relative>0.02,]
(plot(activity_data_reduced))

event_reduced<- prod_event[prod_event$Case.ID %in% c("Case 1"),]
event_reduced %>% process_map(type = frequency("relative"))
event_reduced<- prod_event[prod_event$Case.ID %in% c("Case 1","Case 111","Case 104"),]
event_reduced %>% process_map(type = frequency("relative"))

event_reduced<- prod_event[prod_event$Case.ID %in% c("Case 1","Case 111","Case 104"),]
event_reduced %>% filter_trace_frequency(percentage = 0.2) %>% resource_map(type = frequency("absolute"))

prod_event %>%
  resource_specialisation("resource") %>% plot()
prod_event %>%
  resource_specialisation("activity") %>% plot()
prod_event %>% activity_presence() %>% plot()
prod_event %>% trace_length() %>% plot()
prod_event %>% trace_length()
start_activities(prod_event, level = "activity") %>% plot()

event_reduced <- prod_event[prod_event$Case.ID %in% c("Case 1","Case 111","Case 104"),]
event_reduced %>% filter_trace_frequency(percentage = 0.8) %>% process_map(type = frequency("absolute"))
prod_event %>% trace_explorer(coverage = 0.1, type = "infrequent")
