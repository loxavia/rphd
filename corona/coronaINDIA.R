#india Corona
pacman::p_load(ggplot2, dplyr, rvest, xml2, gridExtra, reshape2,wesanderson)
#india gov site-----
(caption2 = paste('Compiled from https://www.mohfw.gov.in/', ' @Dhiraj ', ' :Status on', Sys.time()))

indcorona <- xml2::read_html("https://www.mohfw.gov.in/")
#table1 - today-----
#table no changed----

indcovid <- indcorona %>%  html_nodes("table") %>% .[[1]] %>%   html_table()
head(indcovid)
tail(indcovid)
dim(indcovid)
tail(indcovid,2)
indcovid1 <- indcovid %>% slice(1 : 1:(n()-2))
indcovid1
tail(indcovid1)
newcolsIndia = c('ser','state', 'Confirmed','Recovered','Death')
var = c('Indians', 'Foreign', 'recoveredAll', 'death')
head(indcovid1)
names(indcovid1) = newcolsIndia
names(indcovid1)
head(indcovid1)
indcovid1$state = factor(indcovid1$state)
indcovid1B <- indcovid1 %>% select(-'ser') %>% filter(!grepl('India', state) | !grepl('Total', state))
#indcovid1B$compileDate = Sys.Date()
indcovid1Melt1 <- indcovid1B %>% melt(id.vars='state')
head(indcovid1Melt1)
table(indcovid1Melt1$state)
table(indcovid1Melt1$variable)
#indcovid1Melt1$variable = factor(indcovid1Melt1$variable, levels=c('Ind','For','Rec','Death'), labels= c('Indians', 'Foreign', 'RecoveredAll', 'Death'))
str(indcovid1Melt1)
indcovid1Melt1$value = as.integer(indcovid1Melt1$value)
#+ scale_fill_discrete(name='status', labels=var)
gbarIndia1A <- ggplot(indcovid1Melt1, aes(x=variable, y=value, fill=variable)) + geom_bar(stat='identity', position=position_dodge2(.7), width=.7) + facet_wrap(state ~., scale='free') + theme(axis.text.x = element_text(angle=0, size=rel(.2)), legend.position = 'top',  plot.title = element_text(hjust = 0.5, color = "#666666"), axis.text.y = element_text(size=rel(.7))) + geom_text(aes(label=value, y=value), position=position_dodge2(.7), size=rel(2.5), vjust=0)  +  labs(title=paste('gbarIndia1A:', ' Corona Status', 'My Country(India) : Free Scaling'), caption = caption2, x='State/Cases', y='Numbers') + expand_limits(y = 0) + scale_y_continuous(name = "Numbers", breaks=c(5,10,15,20,50))
gbarIndia1A
str(indcovid1Melt1)
indcovid1Melt1$value = as.integer(indcovid1Melt1$value)
gbarIndia1B <- ggplot(indcovid1Melt1, aes(x=state, y=value, fill=state)) + geom_bar(stat='identity', position=position_dodge2(.7), width=.7) + facet_grid(variable ~., scale='free') + theme(legend.position = 'top',  plot.title = element_text(hjust = 0.5, color = "#666666"),axis.text.x = element_text(angle=90, size=rel(.9)), axis.text.y = element_text(size=rel(.7))) + geom_text(aes(label=value, y=value), size=rel(2.5), nudge_y = 0, nudge_x = 0.1, lineheight = 0.9) + scale_fill_discrete(name='status', labels=var) +  labs(title=paste('gbarIndia1B:', ' Corona Status', 'My Country(India) : Free Scaling'), caption = caption2, x='State/Cases', y='Numbers') + expand_limits(y = 0) + scale_y_continuous(name = "Numbers", breaks=c(5,10,15,20,50)) + guides(fill=F)
gbarIndia1B
str(indcovid1Melt1)

#var = c('Indians', 'Foreign', 'recoveredAll', 'death')
variable_names <- list('Ind'='Indian', 'For'='Foreign','Rec'='Recovered','Death'='Deaths')
variable_labeller <- function(variable, value){
  return(variable_names[value])
}
str(indcovid1Melt1)
gbarIndia2 <- ggplot(indcovid1Melt1, aes(x=state, y=value, fill=state)) + geom_bar(stat='identity', position=position_dodge2(.7))  + theme(legend.position = 'top',  plot.title = element_text(hjust = 0.5, color = "#666666")) + geom_text(aes(label=value, y=value), position=position_dodge2(.7), size=rel(2.5))+  labs(title=paste('gbarIndia2: ', 'Corona Status', 'My Country(India) : Free Scaling'), caption = caption2, x='State/Cases', y='Numbers') + guides(fill=F) + coord_flip() + facet_grid(. ~ variable , scale='free')
#+ facet_grid(. ~ variable , scale='free', labeller=variable_labeller)
gbarIndia2 
indcovid1
gbarIndia2

gbarIndia3 <- ggplot(indcovid1Melt1, aes(x=state, y=value, fill=state)) + geom_bar(stat='identity', position=position_dodge2(.7))  + theme(legend.position = 'top',  plot.title = element_text(hjust = 0.5, color = "#666666")) + geom_text(aes(label=value, y=value), position=position_dodge2(.7), size=rel(2.5))+  labs(title=paste('gbarIndia3: ', 'Corona Status', 'My Country(India) : Free Scaling'), caption =caption2, x='State/Cases', y='Numbers') + guides(fill=F) + coord_flip() + facet_grid(. ~ variable , scale='free')
#+ facet_grid(. ~ variable , scale='free', labeller=variable_labeller)
gbarIndia3

#all graphs -----
gbarIndia2
gbarIndia1
gbarIndia3 #use this instead of gbarIndia2

#pie
table(indcovid1Melt1$variable)
str(indcovid1Melt1)
indcovid1Melt1 %>% filter(variable=='Ind') %>% select(state, value) %>% mutate(level = ifelse(value >5, 'G10', 'L10')) %>% group_by(level) %>% summarise(n=length(level)) %>% ggpubr::ggpie(., "n", label = 'level', lab.pos = 'in', fill = "level", color='white')  + theme_classic() + guides(fill=F)

#scale_fill_gradient(low="blue", high="red")
#+ scale_fill_grey(start=0.8, end=0.2)
write.csv(indcovid1B,paste('E:/data/indcovid',Sys.Date(),'.csv'), na='', row.names=F)

