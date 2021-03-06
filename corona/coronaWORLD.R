#corona Status
#https://www.worldometers.info/world-population/population-by-country/
pacman::p_load(ggplot2, dplyr, rvest, xml2, gridExtra, reshape2, lubridate)
options(scipen = T)

#rename Columns
newcols = c('country', 'Cases','newCases','Deaths','newDeaths', 'Recovered', 'activeCases','serious', 'casePerM','deathsPerM','totaltests','testsPerM')
length(newcols)
(today = Sys.Date())
(yesterday = today-1)
(caption1 = paste('Source -"https://www.worldometers.info/coronavirus/', ' : Compiled by @Dhiraj :', ' @ ', Sys.time()))

corona <- xml2::read_html("https://www.worldometers.info/coronavirus/")
#table1 - today-----
ctable1 <- corona %>%  html_nodes("table") %>% .[[1]] %>%   html_table()
head(ctable1); dim(ctable1)
length(newcols)
names(ctable1) = newcols
names(ctable1)

#ctable$totalCases = as.numeric(gsub(",","",ctable1$totalCases))
ctable1 <- ctable1 %>% mutate_at(vars(2:12), ~as.numeric(gsub(',' , '', .)))
(ctable1 %>% filter(country == 'India'))
ctable1$status = today
str(ctable1)
#ctable1$firstCase = as.Date(ctable1$firstCase,'%b %d')
names(ctable1)
head(ctable1)
ctable1 %>% filter(country == 'Total:')
(ctable1Total <- ctable1 %>% filter(country == 'Total:'))
(ctable1 <- ctable1 %>% filter(country != 'Total:'))
ctable1[,1:5]
dim(ctable1)
names(ctable1)
str(ctable1)
ctable1 %>% summarise(TotalCases = sum(Cases, na.rm=T), TotalDeaths= sum(Deaths, na.rm=T), TotalRecovered=sum(Recovered, na.rm=T), TotalNewCases = sum(newCases, na.rm=T), TotalActiveCases = sum(activeCases, na.rm=T), Totalserious = sum(serious, na.rm=T), MeanCasePerM = mean(casePerM, na.rm=T),MeanDeathsPerM = mean(deathsPerM, na.rm=T), TotalTests = sum(totaltests, na.rm=T), MeanTestsPerM = mean(testsPerM, na.rm=T) )
#firstCase = first(firstCase)
#table2 - yesterday
ctable2 <- corona %>%  html_nodes("table") %>% .[[2]] %>%   html_table()
head(ctable2)
names(ctable2)
str(ctable2)
names(ctable2) = newcols

#remove commas from the data columns and convert to numeric
ctable2 <- ctable2 %>% mutate_at(vars(2:12), ~as.numeric(gsub(',' , '', .)))
ctable2 %>% filter(country == 'India')
ctable2$status =yesterday
head((ctable2))
tail(ctable2)
(ctable2Total <- ctable2 %>% filter(country == 'Total:'))
(ctable2 <- ctable2 %>% filter(country != 'Total:'))
ctable2[,1:5]
dim(ctable2)
#ctable2$firstCase = as.Date(ctable2$firstCase,'%b %d')

ctable2 %>% summarise(TotalCases = sum(Cases, na.rm=T), TotalDeaths= sum(Deaths, na.rm=T), TotalRecovered=sum(Recovered, na.rm=T), TotalNewCases = sum(newCases, na.rm=T), TotalActiveCases = sum(activeCases, na.rm=T), Totalserious = sum(serious, na.rm=T), MeanCasePerM = mean(casePerM, na.rm=T),MeanDeathsPerM = mean(deathsPerM, na.rm=T))

#---------------

(countryOrder <- ctable1 %>% arrange(desc(Cases)) %>% pull(country))
#put in descreasing order : Total first

ctable1$country = factor(ctable1$country, ordered=T, levels=countryOrder)
ctable2$country = factor(ctable2$country, ordered=T, levels=countryOrder)

str(ctable2)
#reshape Data

#total cases > 10000 and India
ctable2Melt1 <- ctable2 %>% filter(Cases > 20000 | country=='India') %>% reshape2::melt(id.var=c('country','status'))
head(ctable2Melt1)
str(ctable2Melt1)
clist2 <- ctable2Melt1 %>% distinct(country) %>% pull(country)

ctable1Melt1 <- ctable1 %>% filter(Cases > 20000 | country=='India') %>% reshape2::melt(id.var=c('country','status'))
head(ctable1Melt1)
str(ctable1Melt1)
clist1 <- ctable1Melt1 %>% distinct(country) %>% pull(country)

ctable1 %>% group_by(status) %>% summarise(TotalCases = sum(Cases, na.rm=T), TotalDeaths= sum(Deaths, na.rm=T), TotalRecovered=sum(Recovered, na.rm=T))

#firstCase DF----
clist1
ctable2 %>% select(country) %>% filter(country %in% clist2)
ctable1 %>% select(country) %>% filter(country %in% clist1)

(case2 = ctable2 %>% select(country) %>% filter(country %in% clist2))
(case1 = ctable1 %>% select(country) %>% filter(country %in% clist1))
names(ctable1)
names(case1); head(case1)
case1$variable = 'deathsPerM'
case2$variable = 'deathsPerM'

#barPlot----
gbar <- function(df, status) {
  ggplot(df, aes(x=variable, y=value, fill=variable)) + geom_bar(stat='identity', position=position_dodge2(.7)) + facet_wrap(country ~., scale='free') + geom_text(aes(label=value, y=value), size=rel(2)) + theme(legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666"), axis.text.x = element_text(size=rel(.8))) + coord_flip() + labs(title=paste('gbar: : Date- ', status, ' : Status of Corona : totalCases > 20000 and India'), subtitle=NULL , caption = caption1, y='Numbers', x='Cases') + guides(fill=F)
}
#top10 countries + India : Yesterday & Today -----
gbar2  = gbar(df=ctable2Melt1, status=yesterday )
gbar2
#gbar2a <- gbar2 + geom_text(data=case2, x=9, y=100, aes(label = paste('First Case - ', firstCase)), size=rel(3), hjust = 0)
#gbar2a

gbar3  = gbar(df=ctable1Melt1, status=today )
gbar3
#gbar3a <- gbar3 + geom_text(data=case1, x=9, y=100, aes(label = paste('First Case - ', firstCase)), size=rel(3), hjust = 0)
#gbar3a

week(Sys.Date())
#total cases > 1000 and India : Heat Map-----
#(fcaseList2 <- ctable2 %>% filter(Cases > 1000 | country=='India') %>% mutate(value = week(firstCase), variable='firstCase') %>% select(country, status, variable, value))
#v1 = c('country','variable')
#fcaseList2[, v1 ] = lapply( fcaseList2[, v1], as.character)

(ctable2Melt2 <- ctable2 %>% filter(Cases > 5000 | country=='India') %>% reshape2::melt(id.var=c('country','status')))
head(ctable2Melt2)
#head(fcaseList2)
dim(ctable2Melt2)
#; dim(fcaseList2)
str(ctable2Melt2)
#str(fcaseList2)
#ctable2Melt2[, v1 ] = lapply( ctable2Melt2[, v1], as.character)
dim(ctable2Melt2)
#; dim(fcaseList2)
#ctable2Melt2a <- ctable2Melt2 %>% bind_rows(fcaseList2) %>% select(country, status, variable, value) 
head(ctable2Melt2a)
str(ctable2Melt2a)
table(ctable2Melt2a$country)
(vnames = names(table(ctable2Melt2$variable)))
(caseOrder = vnames[c(3,7,4,8,1,9,10,2, 5, 6,11)])
ctable2Melt2$variable = factor(ctable2Melt2$variable, ordered=T, level=caseOrder)
str(ctable2Melt2)
summary(ctable2Melt2)
ctable2Melt2 %>% group_by(country, variable)  %>% summarise(value=sum(value)) %>% dcast(country ~ variable, value.var='value') %>% arrange(desc(Cases))

#ctable1------
ctable1Melt2 <- ctable1 %>% filter(Cases > 5000 | country=='India') %>% reshape2::melt(id.var=c('country','status'))
head(ctable2Melt2)
head(ctable1Melt2)
#(fcaseList1 <- ctable1 %>% filter(Cases > 2000 | country=='India') %>% mutate(value = week(firstCase), variable='firstCase') %>% select(country, status, variable, value))
#ctable1Melt2a <- ctable1Melt2 %>% bind_rows(fcaseList1) %>% select(country, status, variable, value) 
ctable1Melt2$variable = factor(ctable1Melt2$variable, ordered=T, level=caseOrder)
ctable1Melt2$country = forcats::fct_relevel(ctable1Melt2$country,'India',after=1)

#function - heatmap----
gheat <- function(df, status) {
  ggplot(df, aes(x=country, y=variable, fill=value)) + geom_tile(color='black') + geom_text(aes(label=value, size=value, angle=30)) + scale_fill_gradient2(low='blue', high='red') + scale_size(range=c(3,3.5)) +  theme(axis.text.x = element_text(angle=30, size=rel(1)), legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('gheat- :  : Date- ', status, ': Status of Corona : totalCases > 5000 and India'), subtitle=paste('First Case : WeekNos (01Jan-1st week) & in India :') , caption =caption1, x='Country', y='Cases') + guides(fill=F, size=F)
}

gheat2 = gheat(df=ctable1Melt2, status=yesterday)
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

both1 %>% filter(variable != 'casePerM' | variable != 'deathsPerM' | variable !='firstCase') %>% group_by(status, variable) %>% summarise(total = sum(value, na.rm=T)) %>% reshape2::dcast(status ~ variable, value.var='total')

gbarBoth1a <- both1 %>% filter(variable != 'casePerM'  | variable != 'deathsPerM') %>% group_by(status, variable) %>% summarise(value = sum(value, na.rm=T)) %>% ggplot(., aes(x=status, y=value, fill=factor(variable))) + geom_bar(stat='identity', position= position_dodge2(.7)) +  geom_text(aes(label=value, y=value), size=rel(3), angle=30, position= position_dodge2(.7))  + theme(axis.text.x = element_text(angle=30, size=rel(1)), legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('gbarBoth1a : Compare : Status of Corona : Total Summary'), subtitle=NULL , caption =caption1, x='Date', y='Cases', fill='Case Type') + guides(fill=guide_legend(nrow=1,byrow=TRUE)) 
gbarBoth1a
both1
gbarBoth1b <- both1 %>% filter(variable != 'casePerM') %>% group_by(status, variable) %>% summarise(value = sum(value, na.rm=T)) %>% ggplot(., aes(x=variable, y=value, fill=factor(status))) + geom_bar(stat='identity', position= position_dodge2(.7)) +  geom_text(aes(label=value, y=value), size=rel(3), angle=30, position = position_dodge2(.7))  + theme(axis.text.x = element_text(angle=0, size=rel(1)), legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('gbarBoth1b : Compare : Status of Corona : Total Summary'), subtitle=NULL , caption =caption1, x='Date', y='Cases', fill='Dates') + guides(fill=guide_legend(nrow=1,byrow=TRUE)) 
gbarBoth1b

(both2Sum2 <- both2 %>% select(-country) %>% group_by(status) %>% summarise_all(sum, na.rm=T) )

names(both2)

both2b <- both2 %>% group_by(status) %>% summarise(countries = length(country), Cases = sum(Cases, na.rm=T), newCases = sum(newCases, na.rm=T), Deaths = sum(Deaths, na.rm=T), newDeaths = sum(newDeaths, na.rm=T), Recovered = sum(Recovered, na.rm=T), activeCases = sum(activeCases, na.rm=T), serious = sum(serious, na.rm=T), casePerM = mean(casePerM, na.rm=T), deathsPerM = mean(deathsPerM, na.rm=T)) %>% reshape2::melt(id.vars='status')
both2b

(topCountry <- ctable1 %>% arrange(desc(Cases)) %>% slice(1:10) %>% pull(country))
(topCountry = c(as.character(topCountry),'India'))
summary(ctable1)
selectCols1 = c('Cases','newCases','Deaths','newDeaths','Recovered','serious','activeCases', 'totaltests','testsPerM')
table(both1$variable)
?geom
gbarB1a <- ggplot(data=both1 %>% filter(variable %in% selectCols1 & country %in% topCountry), aes(x=variable, y=value, fill=factor(status))) + geom_bar(stat='identity', position=position_dodge2(.7)) + facet_wrap(country ~., scale='free') + geom_text(aes(label=value, y=value), angle=30,size=rel(2.5), position=position_dodge2(.7), vjust=0, hjust=0) + theme(axis.text.x = element_text(angle=90, size=rel(.8)),legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('gbarB1a: ' ,'Today & Yesterday', ' : Status of Corona : Total, Top 10 countries : My Country(India) : Free Scaling'),caption = caption1, y='Numbers', x='Cases')
gbarB1a
both1
(selectCols2 = c('Cases','newCases','Deaths','newDeaths','serious','Recovered','casePerM', 'deathsPerM' , 'totaltests','testsPerM'))
(countries = topCountry)
gbarB1b <- ggplot(data=both1 %>% filter(variable %in% selectCols2 & country %in% countries), aes(x=country, y=value, fill=factor(status))) + geom_bar(stat='identity', position=position_dodge2(.7)) + facet_wrap(variable ~., scale='free')  + theme(axis.text.y = element_text(angle=0, size=rel(.7)), axis.text.x = element_text(angle=0, size=rel(.6)),legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('gbarB1b ', today, ' & ', yesterday, ' : Status of Corona : Total, Top 10 countries : My Country(India) : Free Scaling'), caption = caption1, y='Numbers', x='Cases') + coord_flip()
gbarB1b
#+ geom_text(aes(label=value, y=value), size=rel(2.5), position=position_dodge2(.7), angle=0)

both2b #see sum above 
gbar2b <- ggplot(data=both2b, aes(x=status, y=value, fill=factor(status))) + geom_bar(stat='identity', position=position_dodge2(.7)) + facet_wrap(variable ~., scale='free') + geom_text(aes(label=round(value,2), y=value), size=rel(3), position=position_dodge2(.7), vjust=0.5,  angle=0) + theme(axis.text.x = element_text(angle=30, size=rel(.8)),legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('gbar2b :', today, ' & ', yesterday, ' : Status of Corona  : Free Scaling'), caption = caption1, y='Numbers', x='Date')
gbar2b

#daily run this code
both1
table(both1$variable)
'%ni%' <- Negate('%in%')

(summary1 <- both1 %>% filter( variable %ni% c('casePerM','deathsPerM','totaltests','testsPerM') & country != 'Total:') %>% group_by(status, variable) %>% summarise(total = sum(value, na.rm=T)) %>% reshape2::dcast(status ~ variable, value.var='total')) 

(summary2 <- both1 %>% filter(variable %ni% c('casePerM','deathsPerM','totaltests','testsPerM') & country != 'Total:') %>% group_by(status, country,variable) %>% summarise(total = sum(value, na.rm=T)) %>% reshape2::dcast(status + country ~ variable, value.var='total') %>% arrange(country))
head(summary1)
head(summary2)
topCountry[-1]
countries
gCSum1 <- summary1 %>% melt(id.var='status') %>% ggplot(., aes(x=status, y=value, fill=factor(status))) + geom_bar(stat='identity') + facet_wrap(variable ~., scales='free') + geom_text(aes(label=value, y=value), size=rel(2.5), position=position_dodge2(.7), vjust=+1, angle=30) + theme(axis.text.x = element_text(angle=30, size=rel(.8)),legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('gCSum1 : ', today, ' & ', yesterday, ' : Status of Corona : All countries : Free Scale '), caption = caption1, y='Numbers', x='Dates')
gCSum1
summary2
gCSum2 <- summary2 %>% filter(country %in% countries) %>% melt(id.var=c('status','country')) %>% ggplot(., aes(x=status, y=value, fill=variable)) + geom_bar(stat='identity', position=position_stack()) + facet_wrap(. ~ country, scales='free') + ggrepel::geom_text_repel(aes(label=value, y=value), position = position_stack(), size=2) + theme(axis.text.x = element_text(angle=30, size=rel(.8)),legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('gCSum2 :', today, ' & ', yesterday, ' : Status of Corona : Top countries + India : Free Scale'), caption = caption1, y='Numbers', x='Dates') + guides(fill=guide_legend(nrow=1,byrow=TRUE))
gCSum2

gCSum3 <- summary2 %>% filter(country %in% countries) %>% melt(id.var=c('status','country')) %>% ggplot(., aes(x=status, y=value, fill=variable)) + geom_bar(stat='identity', position=position_dodge2(.6)) + facet_wrap(. ~ country, scales='free') + ggrepel::geom_text_repel(aes(label=value, y=value), position = position_dodge2(.7), size=2) + theme(axis.text.x = element_text(angle=0, size=rel(.8)),legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('gCSum2 :', today, ' & ', yesterday, ' : Status of Corona : Top countries + India : Free Scale'), caption = caption1, y='Numbers', x='Dates') + guides(fill=guide_legend(nrow=1,byrow=TRUE))
gCSum3


both2

#end---
both2$compileDate =Sys.Date()
write.csv(both2,paste('E:/data/both',Sys.Date(),'.csv', na=''))

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

#gsub(pattern='%', x=cTS1$changePerc, replacement='')
#gsub(pattern=',', x=cTS1$totalDeaths, replacement='')
#gsub(pattern=",|%" , x=cTS1$totalDeaths, replacement='')
#gsub(pattern=",|%" , x=cTS1$changePerc, replacement='')
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
gLine2 <- ggplot(cTS2, aes(x=date, y=Deaths)) + geom_point(aes(size=abs(changePerc), colour=pn)) + geom_line() + ggrepel::geom_text_repel(aes(label=Deaths, y=Deaths, colour=pn), size=rel(3), hjust=-1) + theme(legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('gLine2 :', 'Daily Deaths ', ' : Status of Corona : World'), subtitle='Rate of Change : Blue = UP, red-Down NULL' , caption = caption3, y='Numbers', x='Cases') + scale_size(range=c(2,4)) + guides(colour=F, fill=F, size=F) + scale_x_date(date_breaks = "7 days", date_minor_breaks = "1 days",  date_labels = "%d/%m") + geom_hline(yintercept=c(500,1000,2000, 3000), color=c('green','yellow','orange','red')) 
gLine2

#cumulative Values -----
head(cTS1)
cTS1$pn = ifelse(cTS2$changePerc > 0,'red','blue')
#plot times Series
gLine1 <- ggplot(cTS1, aes(x=date, y=Deaths)) + geom_point(aes(size=abs(changePerc), shape=pn)) + geom_line() + ggrepel::geom_text_repel(aes(label=Deaths, y=Deaths, colour=pn), size=rel(3), hjust=-1) + theme(axis.text.x = element_text(angle=30, size=rel(.8)) ,legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('gLine1 :', 'Total Cumulative Deaths : Daily Changes ', ' : Status of Corona : World'), subtitle='Blue - rate UP, Red - rate Down' , caption = caption3, y='Numbers', x='Cases') + scale_size(range=c(2,4)) + guides(colour=F, fill=F, size=F, shape=F) + scale_x_date(date_breaks = "5 days", date_minor_breaks = "1 days",  date_labels = "%d/%m") + geom_hline(yintercept=c(5000,10000,20000, 30000), color=c('green','yellow','orange','red')) 
gLine1

lubridate::ceiling_date(cTS1$date)
(cTS2fn <- cTS2 %>% group_by(date = lubridate::ceiling_date(date,'15 days')) %>% summarise(total = sum(Deaths, na.rm=T)))
(cTS2wk <- cTS2 %>% group_by(date = lubridate::ceiling_date(date,'1 week')) %>% summarise(total = sum(Deaths, na.rm=T)))

(cTS2mn <- cTS2 %>% group_by(date = lubridate::ceiling_date(date,'1 month')) %>% summarise(total = sum(Deaths, na.rm=T)))
cTS2fn
gTSbar1 <- ggplot(cTS2wk, aes(x=date, y=total, fill=date)) + geom_bar(stat='identity', position=position_dodge2(.8), width=1) + geom_line() +  geom_text(aes(label=total, y=total), size=rel(4), hjust=0) + theme(legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('gTSbar1 :', 'Daily Deaths ', ' : Week (End of Week Dates) Summary Status of Corona : World'), subtitle=NULL , caption = caption3 , y='Numbers', x='Cases') + guides(fill=F) 
gTSbar1b <- gTSbar1 + scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day",  date_labels = "%d/%b/%y")
gTSbar1b
#gTSbar1b

#gridExtra::grid.arrange(gLine1, gLine2, ncol=2)
gLine1
gLine2
gTSbar1b

#save plots-----
#some graphs from other sheets
#gbarIndia1, gbarIndia3
gbar2b
graphsList  = list(gCSum2, gTSum3, gbarBoth1a , gbarBoth1b, gbar2, gheat2, gheat1, gLine1, gLine2, gTSbar1b,gbarIndia1A, gbarIndia1B, gbarIndia3,gTBLine1, gTBLine2, gTBArea2)
#gCSum2gbarIndia1A
graphArrange <- gridExtra::marrangeGrob(graphsList, nrow=1, ncol=1, top = quote(paste("Corona Plots : page", g, "of", npages)))
ggsave(paste0('E:/graphs/', "Corona",Sys.Date(),'-',lubridate::hour(Sys.time()) , ".pdf"), graphArrange, width=4, height=2, units="in", scale=3)

