#Corona from Tableau
#'https://data.world/covid-19-data-resource-hub/covid-19-case-counts/workspace/file?filename=COVID-19+Cases.csv'

link1 = 'https://query.data.world/s/vuxzmjfoaf2yeetwv4gbpgx625qi4d'
ctableau = read.csv(link1,  header=TRUE, stringsAsFactors=FALSE)
ctableau
write.csv(ctableau, paste('E:/data/ctableau',Sys.Date(),'.csv', sep=''), row.names = F)
(caption1 = paste(link1, ' : Compiled by @Dhiraj :', ' @ ', Sys.time()))
dim(ctableau)
names(ctableau)
#DESCRIPTION-----
#date : date ; country_region : string ; province_state : string ; difference : integer ; prep_flow_runtime : datetime ;latest_date : date : case_type : string; cases : integer ; lat : decimal ;long : decimal ; location : geopoint
head(ctableau)
colT1 = c('table_names', 'case_type', 'cases', 'diff','date', 'country_region', 'province_state', 'admin2', 'fips', 'combined_key', 'lat', 'long', 'prep_flow_time', 'latest_date')
colT2 = c('case_type', 'cases','diff', 'table_names', 'combined_key', 'admin2','fips', 'prep_flow_time', 'latest_date', 'long', 'country_region', 'date','province_state', 'lat')
colT3 = c('table_names', 'date','case_type', 'cases','diff','country_region' ,'province_state', 'admin2', 'combined_key', 'fips', 'lat', 'long','latest_date', 'prep_flow_time' )
colT4 = c( 'case_type', 'cases', 'diff','date', 'country_region', 'province_state', 'admin2', 'combined_key','fips',  'lat', 'long', 'prep_flow_time','table_names')
colT5 = c( 'case_type', 'cases', 'diff','date', 'country_region', 'province_state', 'admin2', 'combined_key','fips',  'lat', 'long', 'table_names', 'prep_flow_time')
names(ctableau)
length(colT5); length(names(ctableau))
cbind(names(ctableau), colT5)
names(ctableau) = colT5
head(ctableau)
dim(ctableau)
ctableau1 = ctableau
head(ctableau1$date)

ctableau1$date = as.Date(ctableau1$date,'%m/%d/%Y')
str(ctableau1$date)
ctableau1$date = as.POSIXct(ctableau1$date, tz='GMT')

ctableau1$prep_flow_time = as.POSIXct(ctableau1$prep_flow_time,'%m/%d/%Y', tz='GMT')


#ctableau1$prep_flow_time = lubridate::mdy_hms(ctableau1$prep_flow_time)
head(ctableau1)
names(ctableau1)
str(ctableau1)
table(ctableau1$country_region)
ctbSum1 <- ctableau1 %>% group_by(country_region) %>% summarise(totalCases = sum(cases, na.rm=T)) %>% arrange(desc(totalCases))

ctbSum1  %>% filter(country_region == 'India')

gTBLine1 <- ctableau1 %>% filter(country_region == 'India') %>% group_by(country_region, date) %>% summarise(cases=sum(cases, na.rm=T)) %>% select(date, country_region, cases) %>% ggplot(., aes(x=date, y=cases)) + geom_point(aes(size=cases)) + ggrepel::geom_text_repel(aes(label=cases, y=cases), hjust=2, vjust=1, size=rel(3)) + scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 week",  date_labels = "%d/%b/%y") +  labs(title=paste('gTSbar1 :', 'TS Status : India ', ' : Weekly Breaks'), subtitle=NULL , caption = caption1 , y='Numbers', x='Dates') + theme(legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666"), axis.text.x = element_text(angle=60)) + guides(size=guide_legend(nrow=1,byrow=TRUE)) + geom_hline(yintercept=c(100,500,1000), color=c('green','orange','red'))
gTBLine1

table(ctableau1$country_region)
countryList = c('India','China','Italy','US','Spain','Iran','Pakistan')
str(ctableau1)
ctableau1$country_region = factor(ctableau1$country_region)
gTBLine2 <- ctableau1 %>% filter(country_region %in% countryList) %>% group_by(country_region, date) %>% summarise(cases=sum(cases, na.rm=T)) %>% select(date, country_region, cases) %>% ggplot(., aes(x=date, y=cases, color=country_region)) + geom_point(aes(size=cases)) + scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 week",  date_labels = "%d/%b/%y") + geom_line(aes(group=country_region)) + labs(title=paste('gTBLine2 :', 'Status over Weeks : Cases : Selected Countries + India ', ' : Weekly Breaks'), subtitle=NULL , caption = caption1 , y='Numbers', x='Dates') + theme(legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666"), axis.text.x = element_text(angle=60)) + guides(size=guide_legend(nrow=1,byrow=TRUE), color=guide_legend(nrow=2,byrow=TRUE)) + scale_size(range=c(1,3)) + geom_hline(yintercept=c(5000,10000,25000, 100000), color=c('green','yellow','orange','red')) 
gTBLine2

gTBArea2 <- ctableau1 %>% filter(country_region %in% countryList) %>% group_by(country_region, date) %>% summarise(cases=sum(cases, na.rm=T)) %>% select(date, country_region, cases) %>% ggplot(., aes(x=date, y=cases, fill=country_region)) + scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 week",  date_labels = "%d/%b/%y") + geom_area(alpha=.4) + labs(title=paste('gTBLine2 :', 'Status over Weeks : Selected Countries + India ', ' : Weekly Stats'), subtitle=NULL , caption = caption1 , y='Numbers', x='Dates') + theme(legend.position = 'top', plot.title = element_text(hjust = 0.5, color = "#666666"), axis.text.x = element_text(angle=60)) + guides(fill=guide_legend(nrow=1,byrow=TRUE)) + scale_fill_brewer(palette='Set1')
gTBArea2


#all graphs
gTBLine1
gTBLine2
gTBArea2

