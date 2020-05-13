#libraries for PM
#https://github.com/bupaverse

pacman::p_load(gsheet, googlesheets4, ggplot2, dplyr, DiagrammeR,lubridate) 

pacman::p_load(bupaR, edeaR, processmapR, processanimateR, processcheckR) 

# Save/Read the eventobject
saveRDS(events, "eventsLMS.rds")
events1 <- readRDS("eventsLMS.rds")
events1


#install----
#devtools::install_github("DeveloperName/PackageName")  #restart R before
#devtools::install_github("bupaverse/processmapR")


#bupaR
#https://github.com/bupaverse/bupaR

#edeaR
#https://github.com/bupaverse/edeaR
#https://github.com/bupaverse/edeaR/issues/10

install.packages("pm4py")
#remotes::install_github("bupaverse/pm4py@dev")
devtools::install_github("bupaverse/pm4py")
pm4py::install_pm4py()
#env: C:\Users\du\AppData\Local\r-miniconda\envs\r-reticulate
library(pm4py)
#propro
#https://github.com/bupaverse/propro

#processcheckR
#https://github.com/bupaverse/processcheckR


#website
https://github.com/bupaverse/website


#logbuildR
#https://github.com/bupaverse/logbuildR


#docker
#https://github.com/bupaverse/docker


#pertrinet
#https://github.com/bupaverse/petrinetR
