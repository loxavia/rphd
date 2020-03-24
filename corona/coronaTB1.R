#Corona from Tableau
#'https://data.world/covid-19-data-resource-hub/covid-19-case-counts/workspace/file?filename=COVID-19+Cases.csv'

link1 = 'https://query.data.world/s/vuxzmjfoaf2yeetwv4gbpgx625qi4d'
ctableau = read.csv(link1,  header=TRUE, stringsAsFactors=FALSE)
ctableau
(caption1 = paste(link1, ' : Compiled by @Dhiraj :', ' @ ', Sys.time()))

#DESCRIPTION-----
#date : date ; country_region : string ; province_state : string ; difference : integer ; prep_flow_runtime : datetime ;latest_date : date : case_type : string; cases : integer ; lat : decimal ;long : decimal ; location : geopoint

colT = c('date', 'country', 'state', 'diff', 'flowTime', 'latestDate', 'caseType', 'cases', 'latitude', 'longitude')
names(ctableau) = colT
head(ctableau)
dim(ctableau)
ctableau1 = ctableau
ctableau1$date = as.Date(ctableau1$date,'%m/%d/%Y')
head(ctableau1)
ctableau1$flowTime = lubridate::mdy_hms(ctableau1$flowTime)
head(ctableau1)
ctableau1$latestDate = as.Date(ctableau1$latestDate,'%m/%d/%Y')
head(ctableau1)
gTBLine1 <- ctableau1 %>% filter(country == 'India') %>% group_by(country, date) %>% summarise(cases=sum(cases, na.rm=T)) %>% select(date, country, cases) %>% ggplot(., aes(x=date, y=cases)) + geom_point(aes(size=cases)) + ggrepel::geom_text_repel(aes(label=cases, y=cases), hjust=2, vjust=1, size=rel(3)) + scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 week",  date_labels = "%d/%b/%y") +  labs(title=paste('gTSbar1 :', 'TS Status : India ', ' : Weekly Breaks'), subtitle=NULL , caption = caption1 , y='Numbers', x='Dates') + theme(legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666"), axis.text.x = element_text(angle=60)) + guides(size=guide_legend(nrow=1,byrow=TRUE)) 
gTBLine1

table(ctableau1$country)
countryList = c('India','China','Italy','US','Spain','Iran','Pakistan')
str(ctableau1)
ctableau$country = factor(ctableau$country)
gTBLine2 <- ctableau1 %>% filter(country %in% countryList) %>% group_by(country, date) %>% summarise(cases=sum(cases, na.rm=T)) %>% select(date, country, cases) %>% ggplot(., aes(x=date, y=cases, color=country)) + geom_point(aes(size=cases)) + scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 week",  date_labels = "%d/%b/%y") + geom_line(aes(group=country)) + labs(title=paste('gTBLine2 :', 'Status over Weeks : Selected Countries + India ', ' : Weekly Breaks'), subtitle=NULL , caption = caption1 , y='Numbers', x='Dates') + theme(legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666"), axis.text.x = element_text(angle=60)) + guides(size=guide_legend(nrow=2,byrow=TRUE), color=guide_legend(nrow=2,byrow=TRUE)) + scale_size(range=c(1,5))
gTBLine2


gTBArea2 <- ctableau1 %>% filter(country %in% countryList) %>% group_by(country, date) %>% summarise(cases=sum(cases, na.rm=T)) %>% select(date, country, cases) %>% ggplot(., aes(x=date, y=cases, fill=country)) + scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 week",  date_labels = "%d/%b/%y") + geom_area(alpha=.4) + labs(title=paste('gTBLine2 :', 'Status over Weeks : Selected Countries + India ', ' : Weekly Stats'), subtitle=NULL , caption = caption1 , y='Numbers', x='Dates') + theme(legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666"), axis.text.x = element_text(angle=60)) + guides(fill=guide_legend(nrow=1,byrow=TRUE)) + scale_fill_brewer(palette='Set1')
gTBArea2


