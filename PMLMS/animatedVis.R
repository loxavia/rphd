#animated Video

devtools::install_github("thomasp85/transformr")
pacman::p_load(ggplot2, dplyr, tidyr, lubridate, gganimate, transformr,av)

lmsData2
names(lmsData2B)
str(lmsData2B)
head(lmsData2B)
ceiling_date(lmsData2B$time, unit='month')
g1 <- lmsData2B %>% group_by(date = ceiling_date(time, unit='day'))  %>% summarise(count =n()) %>% mutate(cuml = cumsum(count)) %>% ggplot(., aes(x=date, y=cuml)) + geom_line(color='red') + geom_area(fill='blue') + geom_point(size=1.5) + labs(title ='Date Wise Count') 
g1

ag1 + transition_manual(gear)
animate(ag1, fps = 20, duration = 10)
#save animation----
anim_save(filename='aplot.mp4',animation=animate(ag1,duration=10), path='E:/graphs/')
?anim_save


data.frame(complete(mpg, cyl, fill=list(count=0)))

names(lmsData2C)
table(lmsData2C$activity)
g2 <- lmsData2C %>% group_by(activity, date = as.Date(ceiling_date(time, unit='week')))  %>% summarise(count =n()) %>% mutate(cuml = cumsum(count)) %>% ggplot(., aes(x=date, y=cuml)) + geom_line(color='red') + geom_area(fill='blue') + geom_point(size=1.5) + labs(title ='Date Wise Count')   + geom_text(aes(label=cuml, y=cuml)) + scale_x_date(date_breaks = "1 day", date_labels = "%d-%b ") + theme(axis.text.x = element_text(angle=30))
g2 + facet_wrap(. ~ activity, scale='free')

g2a <- g2+ transition_states( activity , transition_length = 1,state_length = 1) + enter_fade() +  exit_shrink() + labs(subtitle = 'Moving to {next_state}')
g2a
animate(g2a, fps = 20, duration = 10)

anim_save(filename='aplot2.gif',animation=animate(g2a, fps = 20, duration = 10), path='E:/graphs/')
anim_save(filename ="aplot2.mp4", animation=animate(g2a, fps = 20, duration = 10), path='E:/graphs/', renderer = av_renderer())
animate(g2a, renderer = av_renderer('E:/graphs/g2a.mp4'), width = 720*q, height = 480*q, res = 72*q, fps = 25, duration = 30)

#labs(subtitle = paste0('Moving to {next_state}', 'Frame Time{frame_time}', 'No of Layers {nlayers}'))
#,

?transition_manual
ggplot(mtcars) +  geom_boxplot(aes(factor(cyl), mpg)) + transition_manual(gear)



#av----

library(gganimate)
library(transformr)
p <- ggplot(airquality, aes(Day, Temp)) + 
  geom_line(size = 2, colour = 'steelblue') + 
  transition_states(Month, 4, 1) + 
  shadow_mark(size = 1, colour = 'grey')

# Render and show the video
q <- 2
animate(p, renderer = av_renderer('E:/graphs/animation.mp4'), width = 720*q, height = 480*q, res = 72*q, fps = 25)
utils::browseURL('E:/graphs/animation.mp4')
