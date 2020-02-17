
#Installation
install.packages("bupaR")
install.packages("edeaR")
install.packages("eventdataR")
install.packages("processmapR")
install.packages("processmonitR")
install.packages("xesreadR")
install.packages("petrinetR")


library(bupaR)

patients %>%  filter_activity(c("X-Ray", "Blood test")) %>% activities
patients %>%  filter_activity_frequency(percentage = 0.5, reverse = T) %>%  activities
