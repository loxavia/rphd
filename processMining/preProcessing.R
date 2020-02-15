#Pre-Processing

#wrangling-----
#http://bupar.net/wrangling.html

library(bupaR)

head(traffic_fines)
dim(traffic_fines)
traffic_fines %>%  group_by(vehicleclass) %>%  n_cases()
# group_by_case - group by cases
# group_by_activity - group by activity types
# group_by_resource - group by resources
# group_by_activity_resource - group by activity resource pair
# group_by_activity_instance - group by activity instances.

sepsis %>%   group_by_resource %>%   n_cases
#http://www.bupar.net/exploring.html


#slice activities ----
patients %>%  slice_activities(1:10)
