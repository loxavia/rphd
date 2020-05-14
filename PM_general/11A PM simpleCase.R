#very simple case of PM

pacman::p_load(bupaR,edeaR, eventdataR,processmapR,processmonitR,xesreadR,petrinetR) 

data <- data.frame(case = rep("Case1",5), activity_id = c("A","B","C","D","E"), activity_instance_id = 1:5,lifecycle_id = rep("complete",5),  timestamp = Sys.Date() + 1:5,resource = rep("resource 1", 5))
data
data$timestamp = as.POSIXct(data$timestamp)
eventsSample <- eventlog(data, case_id = "case", activity_id = "activity_id", activity_instance_id = "activity_instance_id", lifecycle_id = "lifecycle_id", timestamp = "timestamp", resource_id = "resource")

eventsSample %>% process_map(sec=frequency('relative'), rankdir='TB')
animate_process(eventsSample, sec=frequency('absolute'), duration=10)
