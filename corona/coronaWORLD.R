#corona Status
#https://www.worldometers.info/world-population/population-by-country/
pacman::p_load(ggplot2, dplyr, rvest, xml2, gridExtra, reshape2)
options(scipen = T)

#rename Columns
newcols = c('country', 'Cases','newCases','Deaths','newDeaths', 'Recovered', 'activeCases','serious', 'casePerM','deathsPerM')
(today = Sys.Date())
(yesterday = today-1)
(caption1 = paste('Source -"https://www.worldometers.info/coronavirus/', ' : Compiled by @Dhiraj :', ' @ ', Sys.time()))

corona <- xml2::read_html("https://www.worldometers.info/coronavirus/")
#table1 - today-----
ctable1 <- corona %>%  html_nodes("table") %>% .[[1]] %>%   html_table()
head(ctable1)
names(ctable1) = newcols
names(ctable1)

#ctable$totalCases = as.numeric(gsub(",","",ctable1$totalCases))
ctable1 <- ctable1 %>% mutate_at(vars(2:9), ~as.numeric(gsub(',' , '', .)))
ctable1 %>% filter(country == 'India')
ctable1$status = today
names(ctable1)
(ctable1Total <- ctable1 %>% filter(country == 'Total:'))
(ctable1 <- ctable1 %>% filter(country != 'Total:'))
ctable1[,1:5]
dim(ctable1)
ctable1 %>% summarise(TotalCases = sum(Cases, na.rm=T), TotalDeaths= sum(Deaths, na.rm=T), TotalRecovered=sum(Recovered, na.rm=T))

#table2 - yesterday
ctable2 <- corona %>%  html_nodes("table") %>% .[[2]] %>%   html_table()
head(ctable2)
names(ctable2)
str(ctable2)
names(ctable2) = newcols

#remove commas from the data columns and convert to numeric
ctable2 <- ctable2 %>% mutate_at(vars(2:9), ~as.numeric(gsub(',' , '', .)))
ctable2 %>% filter(country == 'India')
ctable2$status =yesterday
head((ctable2))
tail(ctable2)
(ctable2Total <- ctable2 %>% filter(country == 'Total:'))
(ctable2 <- ctable2 %>% filter(country != 'Total:'))
ctable2[,1:5]
dim(ctable2)
ctable2 %>% group_by(status) %>% summarise(TotalCases = sum(Cases, na.rm=T), TotalDeaths= sum(Deaths, na.rm=T), TotalRecovered=sum(Recovered, na.rm=T))

#---------------

(countryOrder <- ctable1 %>% arrange(desc(Cases)) %>% pull(country))
#put in descreasing order : Total first

ctable1$country = factor(ctable1$country, ordered=T, levels=countryOrder)
ctable2$country = factor(ctable2$country, ordered=T, levels=countryOrder)

str(ctable2)

#reshape Data

#total cases > 10000 and India
ctable2Melt1 <- ctable2 %>% filter(Cases > 10000 | country=='India') %>% reshape2::melt(id.var=c('country','status'))
head(ctable2Melt1)
str(ctable2Melt1)

ctable1Melt1 <- ctable1 %>% filter(Cases > 10000 | country=='India') %>% reshape2::melt(id.var=c('country','status'))
head(ctable1Melt1)
str(ctable1Melt1)
ctable1 %>% group_by(status) %>% summarise(TotalCases = sum(Cases, na.rm=T), TotalDeaths= sum(Deaths, na.rm=T), TotalRecovered=sum(Recovered, na.rm=T))


#barPlot----
gbar <- function(df, status) {
  ggplot(df, aes(x=variable, y=value, fill=variable)) + geom_bar(stat='identity', position=position_dodge2(.7)) + facet_wrap(country ~., scale='free') + geom_text(aes(label=value, y=value), size=rel(2)) + theme(legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + coord_flip() + labs(title=paste('gbar: ', status, ' : Status of Corona : totalCases > 10000 and India'), subtitle=NULL , caption = caption1, y='Numbers', x='Cases') + guides(fill=F)
}
#top10 countries + India : Yesterday & Today -----
gbar2  = gbar(df=ctable2Melt1, status=yesterday )
gbar2
gbar1  = gbar(df=ctable1Melt1, status=today )
gbar1

#total cases > 1000 and India : Heat Map-----
ctable2Melt2 <- ctable2 %>% filter(Cases > 1000 | country=='India') %>% reshape2::melt(id.var=c('country','status'))
ctable1Melt2 <- ctable1 %>% filter(Cases > 1000 | country=='India') %>% reshape2::melt(id.var=c('country','status'))

head(ctable2Melt2)
head(ctable1Melt2)

#function - heatmap----
gheat <- function(df, status) {
  ggplot(df, aes(x=country, y=variable, fill=value)) + geom_tile(color='black') + geom_text(aes(label=value, size=value, angle=30)) + scale_fill_gradient2(low='blue', high='red') + scale_size(range=c(3,4)) +  theme(axis.text.x = element_text(angle=90, size=rel(1)), legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('gheat- : ', status, ': Status of Corona : totalCases > 1000 and India'), subtitle=NULL , caption =caption1, x='Country', y='Cases') + guides(fill=F, size=F)
}

gheat2 = gheat(df=ctable2Melt2, status=yesterday)
gheat2
gheat1 = gheat(df=ctable1Melt2, status=today)
gheat1

#combine-----
ctable2M <- ctable2 %>% reshape2::melt(id.var=c('country','status'))
ctable1M <- ctable1 %>% reshape2::melt(id.var=c('country','status'))
both1 = rbind(ctable1M, ctable2M)  #melted columns
head(both1)
both2 = rbind(ctable2, ctable1) #all columns
head(both2)
dim(both1)
dim(both2)
both2 %>% filter(country=='India')
table(both1$variable)
names(both2)

both1 %>% filter(variable != 'casePerM') %>% group_by(status, variable) %>% summarise(total = sum(value, na.rm=T)) %>% reshape2::dcast(status ~ variable, value.var='total')

gbarBoth1a <- both1 %>% filter(variable != 'casePerM') %>% group_by(status, variable) %>% summarise(value = sum(value, na.rm=T)) %>% ggplot(., aes(x=status, y=value, fill=factor(variable))) + geom_bar(stat='identity', position= position_dodge2(.7)) +  geom_text(aes(label=value, y=value), size=rel(3), position= position_dodge2(.7))  + theme(axis.text.x = element_text(angle=0, size=rel(1)), legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('gbarBoth1a : Compare : Status of Corona : Total Summary'), subtitle=NULL , caption =caption1, x='Date', y='Cases', fill='Case Type') + guides(fill=guide_legend(nrow=1,byrow=TRUE)) 
gbarBoth1a

gbarBoth1b <- both1 %>% filter(variable != 'casePerM') %>% group_by(status, variable) %>% summarise(value = sum(value, na.rm=T)) %>% ggplot(., aes(x=variable, y=value, fill=factor(status))) + geom_bar(stat='identity', position= position_dodge2(.7)) +  geom_text(aes(label=value, y=value), size=rel(3), position= position_dodge2(.7))  + theme(axis.text.x = element_text(angle=0, size=rel(1)), legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('gbarBoth1b : Compare : Status of Corona : Total Summary'), subtitle=NULL , caption =caption1, x='Date', y='Cases', fill='Dates') + guides(fill=guide_legend(nrow=1,byrow=TRUE)) 
gbarBoth1b

(both2Sum2 <- both2 %>% select(-country) %>% group_by(status) %>% summarise_all(sum, na.rm=T) )

names(both2)
#sumFun = 'Cases = sum(Cases, na.rm=T), newCases = sum(newCases, na.rm=T), Deaths = sum(Deaths, na.rm=T), newDeaths = sum(newDeaths, na.rm=T), Recovered = sum(Recovered, na.rm=T), activeCases = sum(activeCases, na.rm=T), serious = sum(serious, na.rm=T), casePerM = mean(casePerM, na.rm=T), countries = length(country) '

both2b <- both2 %>% group_by(status) %>% summarise(countries = length(country), Cases = sum(Cases, na.rm=T), newCases = sum(newCases, na.rm=T), Deaths = sum(Deaths, na.rm=T), newDeaths = sum(newDeaths, na.rm=T), Recovered = sum(Recovered, na.rm=T), activeCases = sum(activeCases, na.rm=T), serious = sum(serious, na.rm=T), casePerM = mean(casePerM, na.rm=T)) %>% reshape2::melt(id.vars='status')
both2b

(topCountry <- ctable1 %>% arrange(desc(Cases)) %>% slice(1:10) %>% pull(country))
(topCountry = c(as.character(topCountry),'India'))

selectCols = c('Cases','newCases','newDeaths','Recovered','serious')

gbarB1a <- ggplot(data=both1 %>% filter(variable %in% selectCols & country %in% topCountry), aes(x=variable, y=value, fill=factor(status))) + geom_bar(stat='identity', position=position_dodge2(.7)) + facet_wrap(country ~., scale='free') + geom_text(aes(label=value, y=value), size=rel(2.5), position=position_dodge2(.7), hjust=1) + theme(axis.text.x = element_text(angle=90, size=rel(.8)),legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('gbarB1a: ' ,'Today & Yesterday', ' : Status of Corona : Total, Top 10 countries : My Country(India) : Free Scaling'),caption = caption1, y='Numbers', x='Cases')
gbarB1a
both1
(selectCols = c('Cases','newCases','newDeaths','serious','Recovered','casePerM'))
(countries = topCountry)
gbarB1b <- ggplot(data=both1 %>% filter(variable %in% selectCols & country %in% countries), aes(x=country, y=value, fill=factor(status))) + geom_bar(stat='identity', position=position_dodge2(.7)) + facet_wrap(variable ~., scale='free') + geom_text(aes(label=value, y=value), size=rel(2.5), position=position_dodge2(.7), angle=0) + theme(axis.text.x = element_text(angle=0, size=rel(.8)),legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('gbarB1b ', today, ' & ', yesterday, ' : Status of Corona : Total, Top 10 countries : My Country(India) : Free Scaling'), caption = caption1, y='Numbers', x='Cases') + coord_flip()
gbarB1b

both2b
gbar2b <- ggplot(data=both2b, aes(x=status, y=value, fill=factor(status))) + geom_bar(stat='identity', position=position_dodge2(.7)) + facet_wrap(variable ~., scale='free') + geom_text(aes(label=round(value,2), y=value), size=rel(3), position=position_dodge2(.7), vjust=0.5,  angle=0) + theme(axis.text.x = element_text(angle=30, size=rel(.8)),legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('gbar2b :', today, ' & ', yesterday, ' : Status of Corona  : Free Scaling'), caption = caption1, y='Numbers', x='Date')
gbar2b

#daily run this code
(summary1 <- both %>% filter(variable != 'casePerM' & country != 'Total:') %>% group_by(status, variable) %>% summarise(total = sum(value, na.rm=T)) %>% reshape2::dcast(status ~ variable, value.var='total')) 
(summary2 <- both %>% filter(variable != 'casePerM' & country != 'Total:') %>% group_by(status, country,variable) %>% summarise(total = sum(value, na.rm=T)) %>% reshape2::dcast(status + country ~ variable, value.var='total') %>% arrange(country))
head(summary1)
head(summary2)
topCountry[-1]
countries
gCSum1 <- summary1 %>% melt(id.var='status') %>% ggplot(., aes(x=status, y=value, fill=factor(status))) + geom_bar(stat='identity') + facet_wrap(variable ~., scales='free') + geom_text(aes(label=value, y=value), size=rel(2.5), position=position_dodge2(.7), vjust=+1, angle=30) + theme(axis.text.x = element_text(angle=30, size=rel(.8)),legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('gCSum1 : ', today, ' & ', yesterday, ' : Status of Corona : All countries : Free Scale '), caption = caption1, y='Numbers', x='Dates')
gCSum1
gCSum2 <- summary2 %>% filter(country %in% countries) %>% melt(id.var=c('status','country')) %>% ggplot(., aes(x=status, y=value, fill=variable)) + geom_bar(stat='identity', position=position_stack()) + facet_wrap(. ~ country, scales='free') + ggrepel::geom_text_repel(aes(label=value, y=value), position = position_stack(), size=2) + theme(axis.text.x = element_text(angle=30, size=rel(.8)),legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('gCSum2 :', today, ' & ', yesterday, ' : Status of Corona : Top countries + India : Free Scale'), caption = caption1, y='Numbers', x='Dates') + guides(fill=guide_legend(nrow=1,byrow=TRUE))
gCSum2

both2


#end---
both2$compileDate = '2020-03-23'
write.csv(both2,'E:/data/both23mar.csv', na='')

#-------------------------------------------------------------------------
#daily TS - Corona
#https://www.worldometers.info/coronavirus/coronavirus-death-toll/

library(pacman)  #loading multiple libraries
#library(ggplot2); library(dplyr); library(rvest); library(xml2)
pacman::p_load(ggplot2, dplyr, rvest, xml2, gridExtra, reshape2, tidyverse)
options(scipen = T)
(caption3 = paste('Compiled by @Dhiraj from ', 'Source - https://www.worldometers.info/coronavirus/coronavirus-death-toll/','Status on', Sys.time()))
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
gLine2 <- ggplot(cTS2, aes(x=date, y=Deaths)) + geom_point(aes(size=abs(changePerc), colour=pn)) + geom_line() + ggrepel::geom_text_repel(aes(label=Deaths, y=Deaths, colour=pn), size=rel(3), hjust=-1) + theme(legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('gLine2 :', 'Daily Deaths ', ' : Status of Corona : World'), subtitle='Rate of Change : Blue = UP, red-Down NULL' , caption = caption3, y='Numbers', x='Cases') + scale_size(range=c(2,4)) + guides(colour=F, fill=F, size=F)
gLine2

#cumulative Values -----
head(cTS1)
cTS1$pn = ifelse(cTS2$changePerc > 0,'red','blue')
#plot times Series
gLine1 <- ggplot(cTS1, aes(x=date, y=Deaths)) + geom_point(aes(size=abs(changePerc), shape=pn)) + geom_line() + ggrepel::geom_text_repel(aes(label=Deaths, y=Deaths, colour=pn), size=rel(3), hjust=-1) + theme(legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('gLine1 :', 'Total Cumulative Deaths ', ' : Status of Corona : World'), subtitle='Blue- rate UP, Red- rate Down' , caption = caption3, y='Numbers', x='Cases') + scale_size(range=c(2,4)) + guides(colour=F, fill=F, size=F, shape=F) 
#gLine1

lubridate::ceiling_date(cTS1$date)
(cTS2fn <- cTS2 %>% group_by(date = lubridate::ceiling_date(date,'15 days')) %>% summarise(total = sum(Deaths, na.rm=T)))
(cTS2wk <- cTS2 %>% group_by(date = lubridate::ceiling_date(date,'1 week')) %>% summarise(total = sum(Deaths, na.rm=T)))

(cTS2mn <- cTS2 %>% group_by(date = lubridate::ceiling_date(date,'1 month')) %>% summarise(total = sum(Deaths, na.rm=T)))
cTS2fn
gTSbar1 <- ggplot(cTS2wk, aes(x=date, y=total, fill=date)) + geom_bar(stat='identity', position=position_dodge2(.8), width=1) + geom_line() +  geom_text(aes(label=total, y=total), size=rel(4), hjust=0) + theme(legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('gTSbar1 :', 'Daily Deaths ', ' : Week (End of Week Dates) Summary Status of Corona : World'), subtitle=NULL , caption = caption3 , y='Numbers', x='Cases') + guides(fill=F) 
gTSbar1 +  scale_x_date(date_breaks = seq(from=as.Date('2020-01-01'), by='1 week', length.out=10))
gTSbar1b <- gTSbar1 + scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 week",  date_labels = "%d/%b/%y")
#gTSbar1b

#gridExtra::grid.arrange(gLine1, gLine2, ncol=2)
gLine1
gLine2
gTSbar1b


#save plots-----
#some graphs from other sheets
#gbarIndia1, gbarIndia3
graphsList  = list(gbarBoth1a , gbarBoth1b, gbar2, gheat2, gbar1, gheat1, gbarB1, gbarB2,gCSum1, gCSum2, gLine1, gLine2, gTSbar1b,gbarIndia1, gbarIndia3)
graphArrange <- gridExtra::marrangeGrob(graphsList, nrow=1, ncol=1, top = quote(paste("Corona Plots : page", g, "of", npages)))
ggsave(paste0('E:/graphs/', "Corona",Sys.Date(),'-',lubridate::hour(Sys.time()) , ".pdf"), graphArrange, width=4, height=2, units="in", scale=3)

