#corona Status
library(pacman)  #loading multiple libraries
#library(ggplot2); library(dplyr); library(rvest); library(xml2)
pacman::p_load(ggplot2, dplyr, rvest, xml2, gridExtra, reshape2)
options(scipen = T)

#rename Columns
newcols = c('country', 'Cases','newCases','Deaths','newDeaths', 'Recovered', 'activeCases','serious', 'casePerM')


corona <- xml2::read_html("https://www.worldometers.info/coronavirus/")
#table1 - today-----
ctable1 <- corona %>%  html_nodes("table") %>% .[[1]] %>%   html_table()
head(ctable1)
names(ctable1) = newcols
names(ctable1)

#ctable$totalCases = as.numeric(gsub(",","",ctable1$totalCases))
ctable1 <- ctable1 %>% mutate_at(vars(2:9), ~as.numeric(gsub(',' , '', .)))
ctable1 %>% filter(country == 'India')
ctable1$status ='today'


#table2 - yesterday
ctable2 <- corona %>%  html_nodes("table") %>% .[[2]] %>%   html_table()
head(ctable2)
names(ctable2)
str(ctable2)
names(ctable2) = newcols

#remove commas from the data columns and convert to numeric
ctable2 <- ctable2 %>% mutate_at(vars(2:9), ~as.numeric(gsub(',' , '', .)))
ctable2 %>% filter(country == 'India')
ctable2$status ='yesterday'
head((ctable2))

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

#barPlot----
gbar <- function(df, status) {
  ggplot(df, aes(x=variable, y=value, fill=variable)) + geom_bar(stat='identity', position=position_dodge2(.7)) + facet_wrap(country ~., scale='free') + geom_text(aes(label=value, y=value), size=rel(2)) + theme(legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + coord_flip() + labs(title=paste(status, ' : Status of Corona : totalCases > 10000 and India'), subtitle=NULL , caption = paste('Source - https://www.worldometers.info/coronavirus/','Status on', Sys.time()), y='Numbers', x='Cases') + guides(fill=F)
}
gbar2  = gbar(df=ctable2Melt1, status='yesterday' )
gbar2
gbar1  = gbar(df=ctable1Melt1, status='today' )
gbar1

#total cases > 1000 and India : Heat Map-----
ctable2Melt2 <- ctable2 %>% filter(Cases > 1000 | country=='India') %>% reshape2::melt(id.var=c('country','status'))
ctable1Melt2 <- ctable1 %>% filter(Cases > 1000 | country=='India') %>% reshape2::melt(id.var=c('country','status'))

head(ctable2Melt2)
head(ctable1Melt2)
(df=ctable2Melt2)
gheat <- function(df, status) {
  ggplot(df, aes(x=country, y=variable, fill=value)) + geom_tile(color='black') + geom_text(aes(label=value, size=value, angle=30)) + scale_fill_gradient2(low='blue', high='red') + scale_size(range=c(3,4)) +  theme(axis.text.x = element_text(angle=20, size=rel(1)), legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste(status, ': Status of Corona : totalCases > 1000 and India'), subtitle=NULL , caption = paste('Source - https://www.worldometers.info/coronavirus/','Status on', Sys.time()), x='Country', y='Cases') + guides(fill=F, size=F)
}

gheat2 = gheat(ctable2Melt2, 'yesterday')
gheat2
gheat1 = gheat(ctable1Melt2, 'today')
gheat1

#combine-----
ctable2M <- ctable2 %>% reshape2::melt(id.var=c('country','status'))
ctable1M <- ctable1 %>% reshape2::melt(id.var=c('country','status'))
both = rbind(ctable1M, ctable2M)
head(both)
dim(both)

table(both$variable)
both %>% filter(variable != 'casePerM') %>% group_by(status, variable) %>% summarise(total = sum(value, na.rm=T)) %>% reshape2::dcast(status ~ variable, value.var='total')

(topCountry <- ctable1 %>% arrange(desc(Cases)) %>% slice(1:10) %>% pull(country))
(topCountry = c(as.character(topCountry),'India'))

selectCols = c('Cases','newCases','newDeaths','casePerM')
gbarB1 <- ggplot(data=both %>% filter(variable %in% selectCols & country %in% topCountry), aes(x=variable, y=value, fill=status)) + geom_bar(stat='identity', position=position_dodge2(.7)) + facet_wrap(country ~., scale='free') + geom_text(aes(label=value, y=value), size=rel(2.5), position=position_dodge2(.7)) + theme(axis.text.x = element_text(angle=30, size=rel(.8)),legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('Today & Yesterday', ' : Status of Corona : Total, Top 10 countries : My Country(India) : Free Scaling'),caption = paste('Source - https://www.worldometers.info/coronavirus/','Status on', Sys.time()), y='Numbers', x='Cases')

(countries = topCountry[-1])
gbarB1
gbarB2 <- ggplot(data=both %>% filter(variable %in% selectCols & country %in% countries), aes(x=country, y=value, fill=status)) + geom_bar(stat='identity', position=position_dodge2(.7)) + facet_wrap(variable ~., scale='free') + geom_text(aes(label=value, y=value), size=rel(2.5), position=position_dodge2(.7), angle=30) + theme(axis.text.x = element_text(angle=30, size=rel(.8)),legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666")) + labs(title=paste('Today & Yesterday', ' : Status of Corona : Total, Top 10 countries : My Country(India) : Free Scaling'), caption = paste('Source - https://www.worldometers.info/coronavirus/', 'Status on', Sys.time()), y='Numbers', x='Cases')
gBar2

#daily run this code
(summary1 <- both %>% filter(variable != 'casePerM' & country != 'Total:') %>% group_by(status, variable) %>% summarise(total = sum(value, na.rm=T)) %>% reshape2::dcast(status ~ variable, value.var='total'))
(summary2 <- both %>% filter(variable != 'casePerM' & country != 'Total:') %>% group_by(status, country,variable) %>% summarise(total = sum(value, na.rm=T)) %>% reshape2::dcast(status + country ~ variable, value.var='total') %>% arrange(country))
head(summary1)
head(summary2)
topCountry[-1]
countries
summary1 %>% melt(id.var='status') %>% ggplot(., aes(x=status, y=value, fill=status)) + geom_bar(stat='identity') + facet_wrap(variable ~., scales='free')
summary2 %>% filter(country %in% countries) %>% melt(id.var=c('status','country')) %>% ggplot(., aes(x=status, y=value, fill=variable)) + geom_bar(stat='identity', position=position_stack()) + facet_wrap(. ~ country, scales='free') + ggrepel::geom_text_repel(aes(label=value, y=value), position = position_stack(), size=2)
#end---

#-------------

#india gov site-----
indcorona <- xml2::read_html("https://www.mohfw.gov.in/")
#table1 - today-----
indcovid1 <- indcorona %>%  html_nodes("table") %>% .[[2]] %>%   html_table()
head(indcovid1)
newcolsIndia = c('ser','state', 'Ind','For','Rec','Death')
var = c('Indians', 'Foreign', 'recoveredAll', 'death')
names(indcovid1) = newcolsIndia
names(indcovid1)
head(indcovid1)
indcovid1$state = factor(indcovid1$state)
indcovid1Melt1 <- indcovid1 %>% select(-'ser') %>% filter(!grepl('Total', state)) %>% melt(id.vars='state')
head(indcovid1Melt1)

table(indcovid1Melt1$variable)
#indcovid1Melt1$variable = factor(indcovid1Melt1$variable, levels=c('Ind','For','Rec','Death'), labels= c('Indians', 'Foreign', 'RecoveredAll', 'Death'))
str(indcovid1Melt1)
gbarIndia1 <- ggplot(indcovid1Melt1, aes(x=variable, y=value, fill=variable)) + geom_bar(stat='identity', position=position_dodge2(.7)) + facet_wrap(state ~., scale='free') + theme(legend.position = 'top',  plot.title = element_text(hjust = 0.5, color = "#666666")) + geom_text(aes(label=value, y=value), position=position_dodge2(.7), size=rel(2.5)) + scale_fill_discrete(name='status', labels=var) +  labs(title=paste('Corona Status', 'My Country(India) : Free Scaling'), caption = paste('Source - https://www.mohfw.gov.in/', 'Status on', Sys.time()), x='State/Cases', y='Numbers')
gbarIndia1
str(indcovid1Melt1)
gbarIndia2 <- ggplot(indcovid1Melt1, aes(x=state, y=value, fill=state)) + geom_bar(stat='identity', position=position_dodge2(.7))  + theme(legend.position = 'top',  plot.title = element_text(hjust = 0.5, color = "#666666")) + geom_text(aes(label=value, y=value), position=position_dodge2(.7), size=rel(2.5))+  labs(title=paste('Corona Status', 'My Country(India) : Free Scaling'), caption = paste('Source - https://www.mohfw.gov.in/', 'Status on', Sys.time()), x='State/Cases', y='Numbers') + guides(fill=F) + coord_flip() + facet_grid(. ~ variable , scale='free')
gbarIndia2 

#var = c('Indians', 'Foreign', 'recoveredAll', 'death')
variable_names <- list('Ind'='Indian', 'For'='Foreign','Rec'='Recovered','Death'='Deaths')
variable_labeller <- function(variable, value){
  return(variable_names[value])
}
gbarIndia2
gbarIndia3 <- ggplot(indcovid1Melt1, aes(x=state, y=value, fill=state)) + geom_bar(stat='identity', position=position_dodge2(.7))  + theme(legend.position = 'top',  plot.title = element_text(hjust = 0.5, color = "#666666")) + geom_text(aes(label=value, y=value), position=position_dodge2(.7), size=rel(2.5))+  labs(title=paste('Corona Status', 'My Country(India) : Free Scaling'), caption = paste('Source - https://www.mohfw.gov.in/', 'Status on', Sys.time()), x='State/Cases', y='Numbers') + guides(fill=F) + coord_flip() + facet_grid(. ~ variable , scale='free', labeller=variable_labeller)
gbarIndia3



#save plots-----
graphsList  = list(gbar2, gheat2, gbar1, gheat1, gbarB1, gbarB2, gbarIndia1, gbarIndia3)
graphArrange <- gridExtra::marrangeGrob(graphsList, nrow=1, ncol=1, top = quote(paste("Corona Plots : page", g, "of", npages)))
ggsave(paste0('E:/graphs/', "Corona",Sys.Date(), ".pdf"), graphArrange, width=4, height=2, units="in", scale=3)

