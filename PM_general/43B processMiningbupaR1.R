#process Mining
#https://www.r-bloggers.com/process-mining-part-1-3-introduction-to-bupar-package/
#https://www.bupar.net/

library(plyr)
library(tidyverse)
library(bupaR)
theme_set(theme_light())

class(patients) # `eventlog` object noted 
#bupaR::mapping function identifies the information captured and the respective variable name in the event log.
head(patients)
dim(patients)
str(patients)
mapping(patients)

patients_df <- data.frame(patients)%>% select(- .order) %>%  spread(registration_type, time) 
#convert object and #remove this col as we don't need it and it messes with the spread function
head(patients_df)
dim(patients_df)
str(patients_df)
n_activities(patients)
n_distinct(patients$handling)
activity_labels(patients)
unique(patients$handling)
activities(patients)
#tidyway
patients_df %>% group_by(handling) %>% summarise(absolute=n()) %>% mutate(relative=absolute/sum(absolute))
#summary of duration
processing_time(patients,"activity",units="mins") 
# level of analysis, in this situation at level of activity #time units to be used
?bupaR

patients %>% filter_activity_presence("MRI SCAN") %>% processing_time(level="log", units="hours")

patients %>% filter_activity_presence("MRI SCAN",   method="none") %>%  processing_time(level="log", units="hours") # set arugment to "none" to for cases without the specific activity



