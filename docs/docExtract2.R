#extract data from google sheet
pacman::p_load(reshape2, dplyr, googlesheets4)

glink="https://docs.google.com/spreadsheets/d/1NE-np7z6wBuDgbsgkmQtnxPvPegQ8Z1LgSeLQwv6nUQ"

#sheets_browse(slink1)
sdata <- read_sheet(glink, sheet='journalcode')
sdata

names(sdata)
t(sdata[,-c('link')])
relig_income %>%
  pivot_longer(-religion, names_to = "income", values_to = "count")
head(relig_income)
sdata <- sdata %>% select(-link)
sdata <- lapply(sdata, as.character)
sdata <- as.data.frame(sdata)

sdata2 <- sdata %>% pivot_longer(-ser1, names_to='subject', values_to ='description')
head(sdata2)                                         
sdata %>% melt
?split
sdata3 <- split(sdata2, f=sdata2$ser1)
sdata2 %>% split(ser1)
sdata2 %>% write.excel(na='')
write.csv(sdata3, 'data3.csv', row.names = F, na='')
split(mtcars, cyl)


#------------
glink="https://docs.google.com/spreadsheets/d/1NE-np7z6wBuDgbsgkmQtnxPvPegQ8Z1LgSeLQwv6nUQ"
sdata <- read_sheet(glink, sheet='misclink')
sdata
names(sdata)
sdata <- lapply(sdata, as.character)
sdata <- as.data.frame(sdata)
sdata2 <- sdata  %>% pivot_longer(-ser, names_to='subject', values_to ='description')
head(sdata2)                                         
split(sdata2, f=sdata2$ser)
sdata2 %>% write.excel(na='')
#------
glink="https://docs.google.com/spreadsheets/d/1NE-np7z6wBuDgbsgkmQtnxPvPegQ8Z1LgSeLQwv6nUQ"
sdata <- read_sheet(glink, sheet='SumEDM')
sdata
names(sdata)
sdata <- lapply(sdata, as.character)
sdata <- as.data.frame(sdata)
head(sdata)
sdata <- sdata %>% select(-c(Site.Ref, My.Link))
sdata2 <- sdata  %>% pivot_longer(-ser, names_to='subject', values_to ='description')
head(sdata2)                                         
split(sdata2, f=sdata2$ser)
sdata2 %>% write.excel(na='')
write.csv(sdata2, 'data2.csv', row.names = F, na='')

#---------
#------
glink="https://docs.google.com/spreadsheets/d/1NE-np7z6wBuDgbsgkmQtnxPvPegQ8Z1LgSeLQwv6nUQ"
sdata <- read_sheet(glink, sheet='papers2')
sdata
names(sdata)
sdata <- lapply(sdata, as.character)
sdata <- as.data.frame(sdata)
head(sdata)
names(sdata)
sdata <- sdata %>% select(-c(My.Links))
sdata2 <- sdata  %>% pivot_longer(-ser, names_to='subject', values_to ='description')
head(sdata2)                                         
split(sdata2, f=sdata2$ser)
sdata2 %>% write.excel(na='')
write.csv(sdata2, 'data2.csv', row.names = F, na='')
