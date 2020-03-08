
library(bupaR)
library(processmapR)
library(edeaR)

library(heuristicsmineR)
library(eventdataR)
data(patients)

#https://cran.r-project.org/web/packages/heuristicsmineR/readme/README.html
# Dependency graph / matrix
dependency_matrix(patients)
# Causal graph / Heuristics net
causal_net(patients)


# Efficient precedence matrix
m <- precedence_matrix_absolute(L_heur_1)
as.matrix(m)

# Example from Process mining book
dependency_matrix(L_heur_1, threshold = .7)
causal_net(L_heur_1, threshold = .7)

#The Causal net can be converted to a Petri net (note that there are some unnecessary invisible transition that are not yet removed):
  
# Convert to Petri net
library(petrinetR)
cn <- causal_net(L_heur_1, threshold = .7)
pn <- as.petrinet(cn)
render_PN(pn)


#The Petri net can be further used, for example for conformance checking through the pm4py package (Note that the final marking is currently not saved in petrinetR):
  
library(pm4py)
conformance_alignment(L_heur_1, pn, initial_marking = pn$marking, final_marking = c("p_in_6"))
remotes::install_github("bupaverse/pm4py@dev")
pm4py::install_pm4py()



#links
#https://github.com/snowplow/snowplow-web-data-model
#https://stuifbergen.com/2018/08/analyse-web-site-click-paths-as-processes/#source2