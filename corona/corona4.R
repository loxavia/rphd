#corona Status
pacman::p_load(ggplot2, dplyr, rvest, xml2, gridExtra, reshape2)
options(scipen = T)

link1 <- 'https://www.worldometers.info/coronavirus/coronavirus-age-sex-demographics/'
  
corona4 <- xml2::read_html(link1)

#table1 - today-----
ctable41 <- corona4 %>%  html_nodes("table") %>% .[[1]] %>%   html_table(header=T)
ctable41
cols41 = c('AgeLevel', 'deathRateConfirmed', 'deathRateAll')
names(ctable41) = cols41
ctable41
#Death Rate = (number of deaths / number of cases) = probability of dying if infected by the virus (%). The percentages do not have to add up to 100%, as they do NOT represent share of deaths by age group.

#--------------------------gender------
ctable42 <- corona4 %>%  html_nodes("table") %>% .[[2]] %>%   html_table(header=T)
ctable42
cols2 = c('sex', 'deathRateConfirmed', 'deathRateAll')
names(ctable42) = cols2
ctable42

#-------- PreExsiting--------------
#Pre-existing medical conditions (comorbidities)
ctable43 <- corona4 %>%  html_nodes("table") %>% .[[3]] %>%   html_table(header=T)
ctable43
cols3 = c('preCondition', 'deathRateConfirmed', 'deathRateAll')
names(ctable43) = cols3
ctable43
