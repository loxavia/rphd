#alpha Miner

pacman::p_load(bupaR, pm4py, petrinetR)

patients_completes <- patients %>% filter_lifecycle('complete')
discovery_alpha (patients_completes) %>% PN

