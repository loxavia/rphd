#EDA1

#Process Maps
#Introduction The function process_map can be used very easily to create a process map of an event log. Below, an example of a process map for the patients event log can be found.

library(bupaR)
patients %>%  process_map()

#Map profiles
#Frequency profile By default, the process map is annotated with frequencies of activities and flows. The is what is called the frequency profile, and can be created explicitly using the frequency function. This function has a value argument, which can be used to adjust the frequencies shown, for instance using relative frequencies instead of the default absolute ones.

patients %>%  process_map(type = frequency("relative"))

#The frequency value displayed can be one of the following

# absolute frequency The absolute number of activity instances and flows
# absolute_case frequency The absolute number of cases behind each activity and flow
# relative frequency The relative number of instances per activity
# The relative outgoing flows for each activity
# relative_case frequency The relative number of cases per activity and flow

patients %>%  process_map(type = frequency("absolute"))

patients %>%  process_map(type = frequency("relative_case"))

#The colors can be modified through the color_scale argument. See RColorBrewer::brewer.pal.info for all options.

patients %>%  process_map(type = frequency("relative_case", color_scale = "Purples"))

#Performance profile
#Instead of a frequency profile, one can also use a performance profile, focussing on processing time of activities. The performance profile has two arguments: the FUN argument to specify the function to apply on the processing time (e.g. min, max, mean, median, etc.), and the units argument to specificy the time unit to be used.

patients %>%  process_map(performance(median, "days"))

patients %>%
  process_map(performance(mean, "hours"))

#here are two different duration types that can be displayed on the edges.

idle_time: the time between the end of the from-activity, and the start of the to-activity. Can be negative if the from-activity overleaps with the consecutive activity.
inter_start_time: the time between the start of the consecutive activity, including the duration of the from-activity.
The colors can be modified through the color_scale argument. See RColorBrewer::brewer.pal.info for all options.

#Custom profile
#Next to the specific profiles above, also custom numeric attributes can be projected. This can be achieved with the custom profile. It requires an aggregation function (mean, median, sum, min, etc.) and a numerical attribute. The units argument can be used to indicate how the values should be interpreted (e.g. “USD” for monetary values.) For edges, it will show the values related to the out-going activity.

traffic_fines %>%  process_map(type = custom(attribute = "amount", units = "EUR"))
#error

#Combining different profiles
#The profile used for edges and nodes can be differentiated using the type_edges and type_nodes attributes instead of the type argument. In this way, information about frequencies and performance, or any other value, can be combined in the same graph.

patients %>% process_map(type_nodes = frequency("relative_case"),  type_edges = performance(mean))

#Customizing the layout
# The layout of the process map can be further customized.
# 
# Instead of a left-right (LR) layout, the rankdir can be set to TB (top-bottom), BT (bottom-top) or RL (right-left).
# The varying edge width can be disabled.
# If render is set to FALSE, the function will return a unrendered graph, which can be further modified. See here for more information.
# Simplifying process maps
# When event logs get larger, they will also become more unstructured, making the process maps illegible and expensive to computate. In such cases, it is useful to apply them on a simplified version of the event log, using one or more of the subsetting methods provided by edeaR.

