#Alignments

#python

library(reticulate)
path_to_python <- "/anaconda/bin/python"
use_python(path_to_python)
Sys.setenv(RETICULATE_PYTHON = ".venv/bin/python")

#Alignments
#In order to compute alignments, we first discover a Petri Net from the patients_complete dataset.

patients_complete <- patients %>% filter_lifecycle("complete")
model <- discovery_inductive(patients_complete)
