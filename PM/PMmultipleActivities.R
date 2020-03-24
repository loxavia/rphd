#multiple Activities - PM

#libraries
pacman::p_load(gsheet, bupaR, dplyr, edeaR, processmapR, processanimateR, DiagrammeR,lubridate) 

#create some activities
(activities = c('Forum', 'PPT', 'Assignment','Quiz', 'Game' ,'Survey'))
n=5
(rollno = paste('S',1:n,sep='-'))

(e1 <- expand.grid(rollno, activities))
head(e1)
names(e1) <- c('rollno','activity')
head(e1)
e1$status ='complete'
head(e1)

#sample dates of month
set.seed(123)
e1$timestamp = as.POSIXct(as.Date(paste('2020','3',sample(1:25, size=nrow(e1), replace=T),sep='-')))

set.seed(124)
(e1$actscores = round(rnorm(nrow(e1), mean=60, sd=8)))
head(e1)
dim(e1)

#student Profile----
n
(rollno = paste('S',1:n,sep='-'))
(sname = paste('Student-',1:n, sep=''))
set.seed(123); (gender = sample(c('M','F'), size=n, replace=T, prob=c(.5,.5))); table(gender)
set.seed(124); (finalMarks = trunc(runif(n,min=200,max=500)))
set.seed(5); (grades = sample(c('A','B','C'), size=n, replace=T, prob=c(.3,.3,.4))); table(grades)

(students = data.frame(rollno, sname, gender, finalMarks, grades, stringsAsFactors = F))
head(students)
summary(students)
names(students) ; names(e1)
(e2 <- merge(x=students, y=e1, all.y=T ))
names(e2)
head(e2)
e2$gender = factor(e2$gender)
e2$grades = factor(e2$grades)
summary(e2)
unique(e2$rollno) # 50 cases - 300 events
#select fraction of rows randomly 
set.seed(12); e3 <- e2 %>% sample_frac(.8)

#create bupaR object -----
events1 <- bupaR::simple_eventlog(eventlog = e3, case_id = 'rollno', activity_id = 'activity', timestamp = 'timestamp')
events1

mapping(events1)
events1 %>% summary()
head(events1)
str(event1s)

#Process Map-----
events1 %>% processmapR::process_map(sec=frequency('absolute'))
events1 %>% processmapR::process_map(sec=frequency('relative'), rankdir='TB')

video1a <- animate_process(events1, duration=20, repeat_count = 1, mode='absolute', mapping = token_aes(color=token_scale('red')))
video1a

video1b <- animate_process(events1, duration=20, mode='relative', sec=frequency('absolute'), mapping = token_aes(color=token_scale('yellow'), size=token_scale(10)), repeat_count = 2)
video1b

#student Wise + Score
video1c <- events1 %>% animate_process(legend = "color", mode = "absolute", mapping = token_aes(color = token_scale("rollno", scale = "ordinal", range = RColorBrewer::brewer.pal(n_cases(events1), "Paired")) , size = token_scale("actscores", scale = "linear",range=c(10,20)), shape='rect'),  duration=20, repeat_count = 2)
video1c

#color as per gender----
video1d <- animate_process(events1,  legend = "color", repeat_count = 2,  mapping = token_aes(color = token_scale("gender", scale='ordinal', range = c('red','blue'))), duration=20)
video1d

#color as per grades : B to T----
video1e <- animate_process(events1,  legend = "color",   mapping = token_aes(color = token_scale("grades", scale='ordinal', range = c('green','yellow','red'))), duration=10, rankdir='BT', fixed_edge_width=T, repeat_count = 2)
video1e

#one by one- grades
g=c('A','B','C') ; gc =c('green','yellow','red')
i=3
table(events1$grades)
animateGrades <- function(i) {
  events1 %>% filter(grades == g[i]) %>% animate_process(legend = "color", mode='relative', mapping = token_aes(color = token_scale(gc[i]), size = token_scale(10)), duration=20, jitter=2, epsilon_time = 1, rankdir='LR', sec=frequency('relative'))
}

video1g <- animateGrades(i=1)
video1h <- animateGrades(i=2)
video1j <- animateGrades(i=3)

animateGrades2 <- function(i) {
  events1 %>% filter(grades == g[i]) %>% animate_process(legend = "size", mode='relative', mapping = token_aes(color = token_scale("gender", scale='ordinal', range = c('red','blue')), size = token_scale("actscores", scale = "quantize",range=c(1,2,3))), duration=10, initial_state = 'paused', jitter=2, epsilon_time = 1, rankdir='LR', sec=frequency('relative'))
}
video1k1 <- animateGrades2(i=1)
video1k2 <- animateGrades2(i=2)
video1k3 <- animateGrades2(i=3)

#color as per gender using opacity ----
video1m <- animate_process(events1,  legend = "color",   mapping = token_aes(color = token_scale(attribute="gender", scale='ordinal', range = c('red','blue')), size=token_scale(12), opacity = token_scale('0.4')), duration=10, initial_state = 'paused')
video1m 

video1n <- animate_process(events1,  legend = "color",   mapping = token_aes( color = token_scale(attribute="actscores", scale='quantize', range=c('red','green'))), duration=10)

video1o <- animate_process(events1,  legend = "color",   mapping = token_aes(color = token_scale(attribute="grades", scale='ordinal', range=c('red','yellow'))), duration=10)
video1o

video1p <- animate_process(events1, mode = "relative", jitter = 10, legend = "color",  mapping = token_aes(color = token_scale("rollno", scale = "ordinal",   range = RColorBrewer::brewer.pal(7, "Paired"))), duration=10)
video1p

video1q <- animate_process(events1, legend='size', mapping = token_aes(size = token_scale(10), shape = "rect"), duration=10)
head(events1)
video1q

video1r <- animate_process(events1,mode='relative', legend=NULL, duration=10,  mapping = token_aes(size = token_scale(attribute='actscores',scale = 'quantize', range=c(10,15))))
video1r
