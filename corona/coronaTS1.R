#Hoppins data
pacman::p_load(ggplot2, dplyr, rvest, xml2, gridExtra, reshape2)
options(scipen = T)

hlink1='https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
hdata1 = read.csv(hlink1) 
head(hdata1)

(caption1 = paste('https://raw.githubusercontent.com/CSSEGISandData/COVID-19', ' : Compiled by @Dhiraj :', ' @ ', Sys.time()))
hdata1 %>% filter(Country.Region == 'India')
names(hdata1)
table(hdata1$Province.State)
hdata1Melt <- hdata1 %>% melt( id.vars=c('Province.State', 'Country.Region', 'Lat','Long'))
head(hdata1Melt)                               
gsub(x='X1.22.20',pattern='X', replacement='')
hdata1Melt$variable =  gsub(x=hdata1Melt$variable, pattern='X', replacement='')                              
head(hdata1Melt)
as.Date('1.22.20', format='%m.%d.%y')
hdata1Melt$variable = as.Date(hdata1Melt$variable, format='%m.%d.%y')
head(hdata1Melt)
str(hdata1Melt)
hdata1Melt2 <- hdata1Melt %>% filter(!value == 0 )
hsum1 <- hdata1Melt2 %>%  arrange(Country.Region, variable, value )
head(hsum1)
hsum1 %>% group_by(Country.Region) %>% top_n(1, value)
hsum1A <- hsum1 %>% group_by(Country.Region) %>% top_n(1, value) %>% filter(variable == Sys.Date()-1)

hsum1A %>% filter(Country.Region == 'India')
hsum1A %>% arrange(desc(value), Country.Region) %>% top_n(10, value)
(countries <- hsum1A %>% arrange(desc(value), Country.Region)  %>% mutate(country = as.character(Country.Region)) %>% pull(country))
(top10 = countries[1:10])
(top10ind = c(top10, 'India'))
hdata2 <- hdata1Melt2 %>% filter(Country.Region %in% top10ind)
hsum3 <- hdata1Melt2 %>% mutate(weekNo = week(variable)) %>% group_by(Country.Region, weekNo) %>% summarise( max = max(value))
hsum3

hdata1Melt2 %>% group_by(Country.Region, weekend = floor_date(variable)) %>% summarise( max = max(value))



names(hdata2)
ggplot(hdata2, aes(x=variable, y=value)) + geom_point(aes(size=value)) + facet_wrap(Country.Region ~., scales='free')+ guides(color=F) + ylim(1,NA) + theme(axis.text.x = element_text(angle=60, size=rel(.7))) +  scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 week",  date_labels = "%d/%b") + labs(title=paste('gTS :', 'Time Series Analysis Over Weeks : Selected Countries + India ', ' : Weekly Stats'), subtitle=NULL , caption = caption1 , y='Numbers', x='Dates') + scale_size(range=c(1,3))
# + geom_line(aes(color=Country.Region))  + geom_text(aes(label=value, y=value), size=rel(2)) 

names(hsum3)
gTSum3 <- ggplot(hsum3 %>% filter(Country.Region %in% top10ind), aes(x=weekNo, y=max)) + geom_point(aes(size=max)) + facet_wrap(Country.Region ~., scales='free') + ylim(1,NA) + theme(axis.text.x = element_text(angle=60, size=rel(.9))) + labs(title=paste('gTS :', 'Time Series Analysis Over Weeks : Confirmed Cases : Selected Countries + India ', ' : Weekly Stats'), subtitle=NULL , caption = caption1 , y='Numbers', x='Dates') + scale_size(range=c(1,3)) + geom_line(aes(color=Country.Region)) + geom_text(aes(label=max, y=max), size=rel(2), nudge_x = .5, nudge_y = .5) + scale_x_continuous() + guides(size=F, color=F) 
#+ geom_hline(yintercept=c(1000,5000,10000, 20000), color=c('green','yellow','orange','red'))
gTSum3

#------------
x=Sys.Date()
ceiling_date(x, "week")
floor_date(x, "week")

hsum2 <- hsum1 %>% group_by(Country.Region, weekend = ceiling_date(variable, "week")) %>% summarise(value = sum(value, na.rm=T))

hsum2 %>% filter(Country.Region == 'India')
hdata1 %>% filter(Country.Region == 'India')
