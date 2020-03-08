
#Installation
pacman::p_load(gsheet, bupaR, dplyr, edeaR, processmapR, processanimateR, DiagrammeR) 


library(bupaR)

patients %>%  filter_activity(c("X-Ray", "Blood test")) %>% activities
patients %>%  filter_activity_frequency(percentage = 0.5, reverse = T) %>%  activities
