#reading data


pacman::p_load(googlesheets4)

glink1= "https://docs.google.com/spreadsheets/d/15DF1e54g64R42nzvTSgOZqRJrjtdpUnFIyOA-MBppWo/edit#gid=1052786815"
moodleActivities <- googlesheets4::read_sheet(glink1, sheet='pyAnalytics')
head(moodleActivities)
#sheets_browse(glink1)


#remove empty row/columns
janitor::remove_empty(dat, which = c("rows", "cols"), quiet = TRUE)

startDate = as.Date('2020-02-01')
endDate = as.Date('2020-05-01')

lmsData %>% select(time)
df = lmsData[, c('user','time')]
df$campus ='Jaipur'
head(df)
df %>%  mutate(Date = as.Date(time,'%d/%m/%y')) %>% group_by(date1 = ceiling_date(Date, unit='day')) %>% summarise(n=n())

df %>%  mutate(Date = as.Date(time,'%d/%m/%y')) %>% tidyr::complete(Date = seq.Date(min(Date), max(Date), by="day")) %>% group_by(date1 = ceiling_date(Date, unit='day')) %>% summarise(n=n())

df %>%  mutate(Date = as.Date(time,'%d/%m/%y')) %>% tidyr::complete(Date = seq.Date(min(Date), max(Date), by="day"), campus='Others') %>% group_by(D1 = ceiling_date(Date, unit='day'), campus) %>% summarise(n=n()) 


df %>%  mutate(Date = as.Date(time,'%d/%m/%y'), campus=NA) %>% tidyr::complete(Date = seq.Date(min(Date), max(Date), by="day")) %>% fill(campus)


df$time = as.Date(df$time,'%d/%m/%y')
min(df$time); max(df$time)

difftime(range(df$time)[1],range(df$time)[2], units='days')
difftime(min(df$time, na.rm=T), max(df$time,na.rm=T), units='days')

tidyr::complete(Date = seq.Date(from=startDate, to=endDate, by="day"))


#https://blog.exploratory.io/populating-missing-dates-with-complete-and-fill-functions-in-r-and-exploratory-79f2a321e6b5