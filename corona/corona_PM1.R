#PM in covid Cases of India
#india Corona
pacman::p_load(gsheet, bupaR, dplyr, edeaR, processmapR, processanimateR, DiagrammeR,lubridate, htmlwidgets, googlesheets4, tidyverse, reshape2, rvest)
#india gov site-----
(caption2 = paste('Compiled from https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_India', ' @Dhiraj ', ' :Status on', Sys.time()))

indWPcorona <- xml2::read_html("https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_India")
#table1 - today-----
#table no changed----

indWPcovid <- indWPcorona %>%  html_nodes("table") %>% .[[9]] %>%   html_table()
dim(indWPcovid)
indWPcovid
str(indWPcovid)
indWPcovid[1:10,1:3]
names(indWPcovid)
colNames = c('ser','dateDeath','age','gender','nationality','state','reason','ailments','source')
names(indWPcovid) = colNames
indWPcovid$dateDeath = as.POSIXct(as.Date(indWPcovid$dateDeath,'%B %d'))
head(indWPcovid)
colSums(is.na(indWPcovid))
indWPcovid <- tidyr::fill(data=indWPcovid,dateDeath, .direction = c("down"))
colSums(is.na(indWPcovid))
indWPcovid[is.na(indWPcovid$age), 'age'] = mean(indWPcovid$age, na.rm=T)
covidEvents <- bupaR::simple_eventlog(eventlog = indWPcovid,   case_id = 'nationality',  activity_id = 'state', timestamp = 'dateDeath')
covidEvents

covidEvents %>% animate_process(initial_time = 2, duration=10, legend='color', initial_state = 'paused')
(country = names(table(covidEvents$nationality)))

mapping1 = token_aes(color = token_scale("nationality", scale = "ordinal", range = RColorBrewer::brewer.pal(length(country), "Paired")), size = token_scale('age',scale='quantize',range=c(5,7,10)))
str(covidEvents)
table(covidEvents$ailments)

covidEvents %>% animate_process(legend = "color", mode = "absolute",  duration=20, mapping= mapping1)
covidEvents
(ailments= names(table(covidEvents$ailments)))
mapping1 = token_aes(color = token_scale("ailments", scale = "ordinal", range = RColorBrewer::brewer.pal(length(ailments), "Paired")), size = token_scale('age',scale='quantize',range=c(5,7,10)))
covidEvents %>% animate_process(legend = "color", mode = "absolute",  duration=60, mapping= mapping1)
covidEvents %>% process_map(sec=frequency('absolute'), rankdir='LR')



#world Process Mining of Corona
head(ctableau1)
str(ctableau1)
names(ctableau1)

range(ctableau1$date)
table(ctableau1$country_region)
country = c('India','Germany','Italy','Spain','India','US','China')
#country = c('India', 'China')

ctableau1 %>% filter(country_region %in% country) %>% arrange(country_region, case_type) %>% filter(country_region == 'India')

cCountries <- ctableau1 %>% filter(country_region %in% country)
cCountries
names(cCountries)
str(cCountries)
#cCountries$date = as.POSIXct(cCountries$date,'%m/%d/%Y')
cCountries$date
covidWorld <- bupaR::simple_eventlog(eventlog = cCountries,   case_id = 'case_type',  activity_id = 'country_region', timestamp = 'date')
covidWorld
covidWorld  %>% process_map()
covidWorld  %>% animate_process()
