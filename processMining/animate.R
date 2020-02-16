#Animate PM
#https://github.com/bupaverse/processanimateR
#https://bupaverse.github.io/processanimateR/reference/index.html
#https://www.dataminingapps.com/2017/11/a-process-mining-tour-in-r/

library(processanimateR)
library(eventdataR)

patients
patients2 = patients[1:1000,]
animate_process(patients2)
animate_process(patients2, mapping =token_aes(size=token_scale(12), shape='rect'))

animate_process(patients2, mode='relative', jitter=10, legend='color', mapping = token_aes(color = token_scale("employee", scale="ordinal", range=RColorBrewer::brewer.pal(7,'Paired'))))


#Tokens to images-----
animate_process(patients, mapping=token_aes(shape='image', size=token_scale(10), image= token_scale('https://upload.wikimedia.org/wikipedia/en/5/5f/Pacman.gif')))

#use external data---
library(processanimateR)
library(dplyr)
library(bupaR)

lactic <- sepsis %>% mutate(lacticacid = as.numeric(lacticacid))  %>% filter_activity(c('LacticAcid')) %>% as.data.frame()  %>% select("case" = case_id, 'time'= timestamp, value = lacticacid)
sepsisBase <- sepsis %>% filter_activity(c("LacticAcid","CRP", "Leucoytes", "ReturnER", "IV Liquid", "IV Antibiotics"), reverse=T) %>% filter_trace_frequency(percentage=.95)
#animate with secondary data
animate_process(sepsisBase, mode='relative', duration=200, legend='color', mapping=token_aes(color=token_scale(lactic, scale='linear', range=c("#fff5eb","#7f2704"))))



#Ordinal Scales
animate_process(patients, legend='color', mapping=token_aes(color=token_scale("employee", scale='ordinal', range=RColorBrewer::brewer.pal(8, "Paired"))))

#Linear Scales----
animate_process(sample_n(traffic_fines, 1000) %>% filter_trace_frequency(percentage=.95), mode='relative', legend='color', mapping=token_aes(color=token_scale("amount", scale='linear', range=c('yellow','red'))))


#time scales
animate_process(patients, mapping=token_aes(color=token_scale("time", scale='time', range=c('blue','red'))))
