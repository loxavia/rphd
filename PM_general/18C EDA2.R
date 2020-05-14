#EDA


#Precedence diagrams
#IntroductionA precendence diagrams is a two-dimensional matrix showing the flows between activities. It can be contain different type values, by adjusting the type argument.

#Absolute frequency of flows
#Relative frequency of flows
#Relative frequency of flows, for each antecendent
#I.e. given antecendent A, it is followed x% of the time by Consequent B
#Relative frequency of flows, for each consequent
#I.e. given consequent B, it is preceded x% of the time by Antecedent A
#The precedence diagrams can be visualized using the generic plot function. Below, an example of each of the different types is shown.

##Examples
#Absolute Frequencies
patients %>%   precedence_matrix(type = "absolute") 

patients %>%   precedence_matrix(type = "absolute") %>%   plot
R#elative Frequencies
patients %>%   precedence_matrix(type = "relative") %>%   plot

#Antecedent-wise Frequencies
patients %>%  precedence_matrix(type = "relative-antecedent") %>% plot
#Consequent-wise Frequencies
patients %>%   precedence_matrix(type = "relative-consequent") %>%   plot

#Dotted charts-----
#http://bupar.net/dotted_chart.html
#Dotted charts can be made with the dotted_chart function. A dotted chart is a graph in which each activity instance is displayed with a point. The x-axis referce to the time aspect, while the y-axis refers to cases. The dotted chart function has 3 arguments

#x: Either absolute (absolute time on x-axis) or relative (time difference since start case on x-axis)
#y: The ordering of the cases along the y-axis: by start, end, or duration. 
#color: The attribute used to color the activity instances. Default to the activity type.
#Static dotted charts
#Below, you can see some examples for dotted charts with various configurations

patients %>% dotted_chart(x = "absolute", y = "start")
patients %>% dotted_chart(x = "absolute", sorted=T)

patients %>% dotted_chart(x = "absolute", y = "start", color = "employee")
patients %>% dotted_chart(x = "relative", y = "duration", color = "employee")

#Interactive dotted charts
#The function idotted_chart opens a shiny app which can be used to modify the arguments of the dotted chart interactively.

#Trace explorer-----
#Different activity sequences in the event log can be visualized with the trace_explorer. It can be used to explore frequent as well as infrequent traces. The coverage argument specificies how much of the log you want to explore. By default it is set at 0.2, meaning that it will show the most (in)frequency traces covering 20% of the event log.

#Frequent traces
sepsis %>% trace_explorer()
sepsis %>%   trace_explorer(coverage = 0.2)
sepsis %>%   trace_explorer(coverage = 0.1)

#-----------------------
#http://bupar.net/social_networks.html
patients %>%  resource_map()

#Resource precedence matrix-----
#A more compact representation of hand-over-of-work is given by the resource_matrix function, which works the same as the precedence matrix functions.

patients %>% resource_matrix() %>% plot()
