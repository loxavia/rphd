#heuristic Miner
remotes::install_github("bupaverse/heuristicsmineR")
pacman::p_load(heuristicsmineR, eventdataR, bupaR, petrinetR)

data("patients")
dependency_matrix(patients)

dependency_matrix(events2)

causal_net(patients)
causal_net(events2)

m <- precedence_matrix_absolute(events2)
as.matrix(m)

dependency_matrix(events2, threshold = .7)
causal_net(events2, threshold = .7)


#petrinet-----
cn <- causal_net(events2, threshold = .7)
pn <- as.petrinet(cn)
render_PN(pn)

library(pm4py)
conformance_alignment(events2, pn, initial_marking = pn$marking, final_marking = c('p_in_6'))

patients_complete <- patients %>% filter_lifecycle('complete')
discovery_alpha(patients_complete)

install.packages('reticulate')
library(reticulate)
os <- import('os')
os$listdir('.')
remotes::install_github('bupaverse/pm4py@dev')
pm4py::install_pm4py(version='1.1.19')
Sys.which('python')

use_python('C:\\Users\\du\\AppData\\Local\\R-MINI~1\\envs\\R-RETI~1\\python.exe')

library(pm4py)

pm4py::install_pm4py(version=1.1.19)