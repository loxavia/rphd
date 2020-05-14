#Case - Traffic Fines

#build in data sets
pacman::p_load(gsheet, bupaR, dplyr, edeaR, processmapR, processanimateR, DiagrammeR,lubridate) 

?eventdataR::traffic_fines

names(traffic_fines)

#animation-----
str(traffic_fines$amount)

#scale-linear
animate_process(edeaR::filter_trace_frequency(bupaR::sample_n(traffic_fines,1000),percentage=0.5),   legend = "color", mode = "relative",  mapping = token_aes(color = token_scale("amount", scale = "linear",  range = c("yellow","red"))), duration=20)

#scale-quantize
animate_process(edeaR::filter_trace_frequency(bupaR::sample_n(traffic_fines,1000),percentage=0.5),   legend = "color", mode = "relative",  mapping = token_aes(color = token_scale("amount", scale = "quantize",  range = c("yellow","red"))), duration=20)

#different Column - totalpayment

animate_process(filter_trace_frequency( bupaR::sample_n(traffic_fines,1000), percentage=0.5),   legend = "color", mode = "relative",  mapping = token_aes(color = token_scale("totalpaymentamount", scale = "linear",  range = c("yellow","red"))), duration=20)

range(traffic_fines$totalpaymentamount, na.rm=T)
animate_process( filter_trace_frequency(bupaR::sample_n(traffic_fines,1000),percentage=0.5),   legend = "color", mode = "relative",  mapping = token_aes(color = token_scale("totalpaymentamount", scale = "quantize",  range = c("yellow","red",'blue'))), duration=20)

#scale - ordinal
names(traffic_fines)
(caselist1 <- unique(traffic_fines$case_id)[1:5])
?filter_case
traffic_fines %>% filter_case(cases = c('A1','A100')) #single =
#traffic_fines %>% filter_case(cases %in% c('A1','A100'))

ifilter_case(traffic_fines)

traffic_fines %>% filter_case(cases = caselist1) %>%  animate_process(legend = "color", mode = "relative",  mapping = token_aes(color = token_scale("case_id", scale = "ordinal", range = RColorBrewer::brewer.pal(length(caselist), "Paired"))), duration=20)

traffic_fines %>% filter_case(cases = caselist1) %>%  animate_process(legend = "color", mode = "relative",  mapping = token_aes(color = token_scale("amount", scale = "quantize", range = RColorBrewer::brewer.pal(5, "Paired"))), duration=10)

traffic_fines %>% filter_case(cases = caselist1) %>%  animate_process(legend = "color", mode = "relative",  mapping = token_aes(color = token_scale("case_id", scale = "ordinal", range = RColorBrewer::brewer.pal(length(caselist), "Paired")) ,  size = token_scale("amount", scale = "linear", range=10:20)), duration=10)

range(traffic_fines$amount, na.rm=T)

#combine size and color
traffic_fines %>% filter_case(cases = caselist1) %>%  animate_process(legend = "color", mode = "relative", duration=20, mapping = mp1)
(msize <- "size = token_scale('amount', scale = 'quantize')")
(mcolor <- "color = token_scale('case_id', scale = 'ordinal', range = RColorBrewer::brewer.pal(length(caselist1), 'Paired'))")
(mp1 <- paste('token_aes( ', msize, ', ', mcolor, ' )', sep=''))
(msize1 <- paste('token_aes( ', msize, ' )', sep=''))

traffic_fines %>% filter_case(cases = caselist1) %>%  animate_process(legend = "size", mode = "relative", duration=20, mapping = token_aes( size = token_scale('amount', scale = 'quantize', range = 1:5)))

traffic_fines %>% filter_case(cases = caselist1) %>%  animate_process(legend = "color", mode = "relative", duration=20, mapping = token_aes( color = token_scale('amount', scale = 'quantize', range = c('red','green','yellow'))))
