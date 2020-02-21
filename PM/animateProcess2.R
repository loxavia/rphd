#animate process - color


library(eventdataR)
data(traffic_fines)
head(traffic_fines)
str(traffic_fines)
names(traffic_fines)
traffic_fines
range(traffic_fines$amount, na.rm=T)
animate_process(edeaR::filter_trace_frequency(bupaR::sample_n(traffic_fines,50),percentage=0.95), legend = "color", mode = "relative",  mapping = token_aes(color = token_scale("amount", scale = "linear", range = c("yellow","red"))), duration=10)

example_log$res
animate_process(example_log,  legend = "color",   mapping = token_aes(color = token_scale("res", scale = "ordinal",range = RColorBrewer::brewer.pal(8, "Paired"))))

animate_process(events1,  legend = "color",   mapping = token_aes(color = token_scale("gender", scale = "identity",range =  RColorBrewer::brewer.pal(3, "Paired"))))



# (2) Change token color based on second data frame
x <- data.frame(case = as.character(rep(c(1,2,3), 2)),    time = seq(from = as.POSIXct("2018-10-03 03:41:00"),     to = as.POSIXct("2018-10-03 06:00:00"), length.out = 6),   value = rep(c("orange", "green"), 3),  stringsAsFactors = FALSE)
x
animate_process(example_log,mode = "relative",  jitter = 10,  legend = "color",  mapping = token_aes(color = token_scale(x)), duration=10)
head(example_log)
events1
x <- data.frame(case = c('S-1','S-2','S-3'), time = seq(from = as.POSIXct("2020-01-03 03:41:00"), to = as.POSIXct("2020-02-20 06:00:00"), length.out = 3), value = c("orange", "green",'yellow'),  stringsAsFactors = FALSE)
x
animate_process(events1, mode = "relative",  jitter = 10,  legend = "color",  mapping = token_aes(color = token_scale(x)), duration=10)
#scale

animate_process(example_log, mapping = token_aes(size = token_scale(12), shape = "rect"))
#constant color
animate_process(example_log,  legend = "color",  mapping = token_aes(color = token_scale("red")), duration=10)
summary(example_log)
animate_process(example_log,  legend = "color",  mapping = token_aes(color = token_scale(attribute='lifecycle', scale='ordinal', range=c('red','blue'))), duration=10)
            



#scale----
animate_process(example_log, mapping = token_aes(size = token_scale(12), shape = "rect"), duration=10)

animate_process(example_log, mapping = token_aes(size = token_scale(8), shape = "rect"), duration=10)
