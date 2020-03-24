#corona3

pacman::p_load(coronavirus)
# install.packages("devtools")
devtools::install_github("RamiKrispin/coronavirus")

data("coronavirus")

library(dplyr)

summary_df <- coronavirus %>% group_by(Country.Region, type) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases)

summary_df %>% head(20) 
coronavirus

table(coronavirus$Country.Region)
coronavirus %>% filter(Country.Region == 'India')

coronavirus::update_datasets(silence = TRUE)

#https://github.com/RamiKrispin/coronavirus



pacman::p_load(nCov2019)
nCov2019_set_country(country = 'Italy') 