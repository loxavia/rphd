#Alpha Miner
#https://bupar.net/alpha_miner.html

#The alpha miner algorithm is provided through the pm4py package.

library(bupaR)
library(pm4py)
library(petrinetR)
#It can be executed through the discovery_alpha function.

#use only complete timestamp
patients_completes <- patients %>% filter_lifecycle("complete")
patients_completes

discovery_alpha(patients_completes) -> PN
#The resulting object consist of three elements, a net, an initial marking, and a final marking. The net can be visualized using petrinetR as follows.

PN %>% str




#Heuristics Miner
#The Heuristics miner algorithm is provided by the heuristicsmineR package.

library(bupaR)
library(heuristicsmineR)
library(petrinetR)
# Dependency graph / matrix
dependency_matrix(patients) %>% render_dependency_matrix()

# Causal graph / Heuristics net
causal_net(patients) %>% render_causal_net()

#This discovers the Causal net of the built-in L_heur_1 event log that was proposed in the Process Mining book:
  
# Efficient precedence matrix
m <- precedence_matrix_absolute(L_heur_1)
as.matrix(m)

## Example from Process mining book
dependency_matrix(L_heur_1, threshold = .7) %>% render_dependency_matrix()

causal_net(L_heur_1, threshold = .7) %>% render_causal_net()


#The Causal net can be converted to a Petri net (note that there are some unnecessary invisible transition that are not yet removed):
# Convert to Petri net
library(petrinetR)
cn <- causal_net(L_heur_1, threshold = .7)
pn <- as.petrinet(cn)
render_PN(pn)



#Inductive Miner
The Inductive miner algorithm is provided through the pm4py package.

library(bupaR)
library(pm4py)
library(petrinetR)
#It can be executed through the discovery_inductive function.

#use only complete timestamp
patients_completes <- patients %>% filter_lifecycle("complete")

discovery_inductive(patients_completes, variant = variant_inductive_only_dfg()) -> PN

#The resulting object consist of three elements, a net, an initial marking, and a final marking. The net can be visualized using petrinetR as follows.

PN %>% str
PN$petrinet %>% render_PN()