#daily TS - Corona
#https://www.worldometers.info/coronavirus/coronavirus-death-toll/

library(pacman)  #loading multiple libraries
#library(ggplot2); library(dplyr); library(rvest); library(xml2)
pacman::p_load(ggplot2, dplyr, rvest, xml2, gridExtra, reshape2, tidyverse)
options(scipen = T)

#rename Columns
newcols = c('country', 'totalCases','newCases','totalDeaths','newDeaths', 'totalRecovered', 'activeCases','serious', 'casePerMillion')


coronaTS <- xml2::read_html("https://www.worldometers.info/coronavirus/coronavirus-death-toll/")

#table1 - today-----
cTS1 <- coronaTS %>%  html_nodes("table") %>% .[[1]] %>%   html_table()
head(cTS1)
names(cTS1) = c('date','Deaths', 'changeTotal','changePerc')
head(cTS1)
cTS1$date = as.Date(cTS1$date, '%b. %d')
str(cTS1)

gsub(pattern='%', x=cTS1$changePerc, replacement='')
gsub(pattern=',', x=cTS1$totalDeaths, replacement='')
gsub(pattern=",|%" , x=cTS1$totalDeaths, replacement='')
gsub(pattern=",|%" , x=cTS1$changePerc, replacement='')
(cTS1 <- cTS1 %>% mutate_at(vars(2:4), ~as.numeric(gsub(pattern=",|%" , x=., replacement=''))))

cTS2 <- coronaTS %>%  html_nodes("table") %>% .[[2]] %>%   html_table()
head(cTS2)
names(cTS2) = c('date','Deaths', 'changeTotal','changePerc')
head(cTS2)
cTS2$date = as.Date(cTS2$date, '%b. %d')
(cTS2 <- cTS2 %>% mutate_at(vars(2:4), ~as.numeric(gsub(pattern=",|%" , x=., replacement=''))))
head(cTS2)
cTS2$pn = ifelse(cTS2$changePerc > 0,'up','down')
head(cTS2)

#plot times Series
gLine2 <- ggplot(cTS2, aes(x=date, y=Deaths)) + geom_point(aes(size=abs(changePerc), colour=pn)) + geom_line() + ggrepel::geom_text_repel(aes(label=Deaths, y=Deaths, colour=pn), size=rel(3), hjust=-1) + theme(legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('Daily Deaths ', ' : Status of Corona : World'), subtitle='Rate of Change : Blue = UP, red-Down NULL' , caption = paste('Source - https://www.worldometers.info/coronavirus/coronavirus-death-toll/','Status on', Sys.time()), y='Numbers', x='Cases') + scale_size(range=c(2,4)) + guides(colour=F, fill=F, size=F)
gLine2



#cumulative Values -----
head(cTS1)
cTS1$pn = ifelse(cTS2$changePerc > 0,'red','blue')
#plot times Series
gLine1 <- ggplot(cTS1, aes(x=date, y=Deaths)) + geom_point(aes(size=abs(changePerc), shape=pn)) + geom_line() + ggrepel::geom_text_repel(aes(label=Deaths, y=Deaths, colour=pn), size=rel(3), hjust=-1) + theme(legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('Total Cumulative Deaths ', ' : Status of Corona : World'), subtitle='Blue- rate UP, Red- rate Down' , caption = paste('Source - https://www.worldometers.info/coronavirus/coronavirus-death-toll/','Status on', Sys.time()), y='Numbers', x='Cases') + scale_size(range=c(2,4)) + guides(colour=F, fill=F, size=F, shape=F) 
gLine1

lubridate::ceiling_date(cTS1$date)
(cTS2fn <- cTS2 %>% group_by(date = lubridate::ceiling_date(date,'15 days')) %>% summarise(total = sum(Deaths, na.rm=T)))
(cTS2mn <- cTS2 %>% group_by(date = lubridate::ceiling_date(date,'1 month')) %>% summarise(total = sum(Deaths, na.rm=T)))

gTSbar2 <- ggplot(cTS2fn, aes(x=date, y=total, fill=date)) + geom_bar(stat='identity') + geom_line() +  geom_text(aes(label=total, y=total), size=rel(3), hjust=0) + theme(legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('Daily Deaths ', ' : Fortnight Summary Status of Corona : World'), subtitle=NULL , caption = paste('Source - https://www.worldometers.info/coronavirus/coronavirus-death-toll/','Status on', Sys.time()), y='Numbers', x='Cases') + guides(fill=F)
gTSbar2


gridExtra::grid.arrange(gLine1, gLine2, ncol=2)
