#india Corona

#india gov site-----
(caption2 = paste('Compiled from https://www.mohfw.gov.in/', ' @Dhiraj ', ' :Status on', Sys.time()))

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
indcovid1B <- indcovid1 %>% select(-'ser') %>% filter(!grepl('Total', state))
indcovid1B$compileDate = Sys.Date()
indcovid1Melt1 <- indcovid1B %>% select(-compileDate) %>% melt(id.vars='state')
head(indcovid1Melt1)

table(indcovid1Melt1$variable)
#indcovid1Melt1$variable = factor(indcovid1Melt1$variable, levels=c('Ind','For','Rec','Death'), labels= c('Indians', 'Foreign', 'RecoveredAll', 'Death'))
str(indcovid1Melt1)
gbarIndia1 <- ggplot(indcovid1Melt1, aes(x=variable, y=value, fill=variable)) + geom_bar(stat='identity', position=position_dodge2(.7)) + facet_wrap(state ~., scale='free') + theme(legend.position = 'top',  plot.title = element_text(hjust = 0.5, color = "#666666")) + geom_text(aes(label=value, y=value), position=position_dodge2(.7), size=rel(2.5), vjust=-.5) + scale_fill_discrete(name='status', labels=var) +  labs(title=paste('gbarIndia1:', ' Corona Status', 'My Country(India) : Free Scaling'), caption = caption2, x='State/Cases', y='Numbers')
gbarIndia1
str(indcovid1Melt1)

#var = c('Indians', 'Foreign', 'recoveredAll', 'death')
variable_names <- list('Ind'='Indian', 'For'='Foreign','Rec'='Recovered','Death'='Deaths')
variable_labeller <- function(variable, value){
  return(variable_names[value])
}
str(indcovid1Melt1)
gbarIndia2 <- ggplot(indcovid1Melt1, aes(x=state, y=value, fill=state)) + geom_bar(stat='identity', position=position_dodge2(.7))  + theme(legend.position = 'top',  plot.title = element_text(hjust = 0.5, color = "#666666")) + geom_text(aes(label=value, y=value), position=position_dodge2(.7), size=rel(2.5))+  labs(title=paste('gbarIndia2: ', 'Corona Status', 'My Country(India) : Free Scaling'), caption = caption2, x='State/Cases', y='Numbers') + guides(fill=F) + coord_flip() + facet_grid(. ~ variable , scale='free', labeller=variable_labeller)
gbarIndia2 

indcovid1
gbarIndia2

gbarIndia3 <- ggplot(indcovid1Melt1, aes(x=state, y=value, fill=state)) + geom_bar(stat='identity', position=position_dodge2(.7))  + theme(legend.position = 'top',  plot.title = element_text(hjust = 0.5, color = "#666666")) + geom_text(aes(label=value, y=value), position=position_dodge2(.7), size=rel(2.5))+  labs(title=paste('gbarIndia3: ', 'Corona Status', 'My Country(India) : Free Scaling'), caption =caption2, x='State/Cases', y='Numbers') + guides(fill=F) + coord_flip() + facet_grid(. ~ variable , scale='free', labeller=variable_labeller)
gbarIndia3

#all graphs -----
gbarIndia2
gbarIndia1
gbarIndia3 #use this instead of gbarIndia2

write.csv(indcovid1B,'E:/data/indcove23mar.csv', na='', row.names=F)