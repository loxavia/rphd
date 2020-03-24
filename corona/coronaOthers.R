#corona - other Sites---


link3 <- 'https://www.kff.org/global-health-policy/fact-sheet/coronavirus-tracker/'

cother1 <- xml2::read_html(link3)
(caption1 = paste(link3, ' : Compiled by @Dhiraj :', ' @ ', Sys.time()))
cother1A <- cother1 %>%  html_nodes("table") %>% .[[2]] %>%   html_table(header=T)
ctable41