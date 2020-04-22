


x <- c('Jim', 'Jim', 'Jim', 'Sue', 'Sue', 'Sue')
y <- c(100, 200, 150, 40, 50, 30)
z <- c(5, 6, 4, 3, 4, 4)
num <- c(1, 2, 3, 1, 2, 3)

df <- data.frame(x,y,z,num)
df
reshape(df, idvar = 'x', direction = 'wide', timevar = 'num') 
names(allPapers2)
reshape(allPapers2[,-c(1)], idvar = 'id', direction = 'long', timevar = 'subject') 


library(tidyr)
head(billboard)

billboard %>% 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank",
    values_drop_na = TRUE
  )

fish_encounters
fish_encounters %>% pivot_wider(names_from = station, values_from = seen)
allPapers2 %>% pivot_wider(names_from=subject, values_from = description)
