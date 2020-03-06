
install.packages("pm4py")
#different ways
pm4py::install_pm4py()
pm4py::install_pm4py(method="auto", conda="C:\\programdata\\anaconda3")
#path of conda
#pip install pm4py
pm4py::install_pm4py(version = "1.2.7")
library(pm4py)

# Print the PM4PY version loaded
if (pm4py_available()) {
  print(pm4py$`__version__`)
}
# Most of the data structures are converted in their bupaR equivalents
library(bupaR)
?pm4py
# As Inductive Miner of PM4PY is not life-cycle aware, keep only `complete` events:
patients_completes <- patients[patients$registration_type == "complete", ]
patients_completes
# Discovery with Inductive Miner
pn <- pm4py::discovery_inductive(patients_completes)

# This results in an auto-converted bupaR Petri net and markings
str(pn)
class(pn$petrinet)

# Render with bupaR
pm4py::render_PN(pn$petrinet)

# Render with  PM4PY and DiagrammeR
library(DiagrammeR)
viz <- reticulate::import("pm4py.visualization.petrinet")

# Convert back to Python
py_pn <- r_to_py(pn$petrinet)
class(py_pn)

# Render to DOT with PMP4Y
dot <- viz$factory$apply(py_pn)$source
grViz(diagram = dot)

# Compute alignment
alignment <- conformance_alignment(patients_completes, pn$petrinet, pn$initial_marking, pn$final_marking)

# # Alignment is returned in long format as data frame
head(alignment)

# Evaluate model quality
quality <- evaluation_all(patients_completes, pn$petrinet, pn$initial_marking, pn$final_marking)

pm4py_available()
if (pm4py_available()) {
  library(eventdataR)
  data(patients)
  
  # As Inductive Miner of PM4Py is not life-cycle aware, keep only `complete` events:
  patients_completes <- patients[patients$registration_type == "complete", ]
  
  # Discover a Petri net
  net <- discovery_inductive(patients_completes)
  
  # Align event log and Petri net
  a <- conformance_alignment(patients_completes,
                             net$petrinet,
                             net$initial_marking,
                             net$final_marking)
  
  # Alignment is returned as data frame
  head(a)
}