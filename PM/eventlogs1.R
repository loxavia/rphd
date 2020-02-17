#Process Mining - example
#https://www.bupar.net/creating_eventlogs.html
library(gsheet)
library(bupaR)
library(tidyr)

#The event log object--------

link1 = "https://docs.google.com/spreadsheets/d/1Rv-8pDWMeonmjn7LCZh55MJtIh0XG5vHX-mIZpcGf1c/edit#gid=792991636"
example_log_1 = as.data.frame(gsheet2tbl(link1))
example_log_1

#a data.frame with the information in the table above
example_log_1 %>%   eventlog(
    case_id = "patient",
    activity_id = "activity",
    activity_instance_id = "activity_instance",
    lifecycle_id = "status",
    timestamp = "timestamp",
    resource_id = "resource"
  )

#Lack of transitional lifecycle----
#It happens a lot that data is not recorded at the low level of transactions, but that only a single timestamp is recorded for each activity instance. In that case, an event is equivalent to a activity instance. For instance, consider the example above, but now we only have the following information.

link2 = "https://docs.google.com/spreadsheets/d/1Rv-8pDWMeonmjn7LCZh55MJtIh0XG5vHX-mIZpcGf1c/edit#gid=471397784"
example_log_2 = as.data.frame(gsheet2tbl(link2))
example_log_2
example_log_2 %>%
  mutate(status = "complete",  activity_instance = 1:nrow(.)) %>%
  eventlog(
    case_id = "patient",
    activity_id = "activity",
    activity_instance_id = "activity_instance",
    lifecycle_id = "status",
    timestamp = "timestamp",
    resource_id = "resource"
  )


#Lack of resources------------
#Since many of the functions in bupaR are targetted towards organizational and performance issues, they expect the presence of the resource attribute. However, in certain cases, this information will no be available, such as for the data in example_log_3
link3 = "https://docs.google.com/spreadsheets/d/1Rv-8pDWMeonmjn7LCZh55MJtIh0XG5vHX-mIZpcGf1c/edit#gid=2012909095"
example_log_3 = as.data.frame(gsheet2tbl(link3))
example_log_3
#In order to work around this problem, the easiest solution is to include an empty resource variable.
example_log_3 %>%  mutate(resource = NA) %>%
  eventlog(
    case_id = "patient",
    activity_id = "activity",
    activity_instance_id = "activity_instance",
    lifecycle_id = "status",
    timestamp = "timestamp",
    resource_id = "resource"
  )


#Activity log------
#Another possibity is that instead of a list of events, there is a list of activity instances available. T

link4 = "https://docs.google.com/spreadsheets/d/1Rv-8pDWMeonmjn7LCZh55MJtIh0XG5vHX-mIZpcGf1c/edit#gid=369936635"
example_log_4 = as.data.frame(gsheet2tbl(link4))
example_log_4
#first adding an unique id to define the activity instances, and subsequently by gathering the different timestamp columns using tidyr::gather
library(tidyr)
example_log_4 %>%  mutate(activity_instance = 1:nrow(.)) %>%
  gather(status, timestamp, schedule, start, complete)  %>%
  mutate(resource = NA) %>%
  filter(!is.na(timestamp)) %>%
  eventlog(
    case_id = "patient",
    activity_id = "activity",
    activity_instance_id = "activity_instance",
    lifecycle_id = "status",
    timestamp = "timestamp",
    resource_id = "resource"
  )

