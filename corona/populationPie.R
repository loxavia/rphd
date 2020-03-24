#population
pacman::p_load(ggplot2, dplyr, rvest, xml2, gridExtra, reshape2, ggpubr)
options(scipen = F)

popLink = 'https://www.worldometers.info/world-population/population-by-country/'

pop <- xml2::read_html(popLink)
pop2020 <- pop %>%  html_nodes("table") %>% .[[1]] %>%   html_table()
head(pop2020)

colP = c('ser','country','pop20','yrChange','netChange', 'densityKm2', 'landAreaKm2','migrants','fertilityRate','medAge', 'urbanPop', 'worldShare')
names(pop2020) = colP
head(pop2020)
names(pop2020)

pop2020A <- pop2020 %>% mutate_at(vars(c(3,5:10)), ~as.numeric(gsub(pattern=',' , replacement='', x=.)))
head(pop2020A)
pop2020A <- pop2020A %>% mutate_at(vars(c(4,11:12)), ~as.numeric(gsub(pattern='%' , replacement='', x=.)))
head(pop2020A)
str(pop2020A)

#countries with urban > 50----
(urbanD <- pop2020A %>% filter(! is.na(urbanPop)) %>% mutate(urban = ifelse(urbanPop > 50,'Urban','Rural')) %>% group_by(urban) %>% tally())
ggpubr::ggpie(urbanD, "n", label = "urban")
ggpubr::ggpie(urbanD, "n", label = "urban",  fill = "urban", color = "white",  palette = c('green','yellow') )
(labs <- paste0(urbanD$urban, " (", urbanD$n, "%)"))
ggpubr::ggpie(urbanD, "n", label = labs, lab.pos = "in", fill = "urban", color = "white",  palette = c('green','yellow') )


#pie
df <- data.frame( group = c("Male", "Female", "Child"), value = c(25, 25, 50))
ggpubr::ggpie(df, "value", label = "group")
ggpubr::ggpie(df, "value", label = "group",  fill = "group", color = "white",  palette = c("#00AFBB", "#E7B800", "#FC4E07") )
labs <- paste0(df$group, " (", df$value, "%)")
ggpubr::ggpie(df, "value", label = labs,fill = "group", color = "white", palette = c("#00AFBB", "#E7B800", "#FC4E07"))
# Change the position and font color of labels
ggpubr::ggpie(df, "value", label = labs,lab.pos = "in", lab.font = "white", fill = "group", color = "white",  palette = c("#00AFBB", "#E7B800", "#FC4E07"))






#https://rpkgs.datanovia.com/ggpubr/index.html