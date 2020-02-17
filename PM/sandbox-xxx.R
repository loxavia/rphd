#PM - sandbox
#https://www.dataminingapps.com/2017/11/a-process-mining-tour-in-r/

# Load in libraries
library(tidyverse)
library(igraph)
#library(subprocess)
library(png)
library(grid)

# Returns the max/min of given sequence or a default value in case the sequence is empty
max.na <- function(..., def=NA, na.rm=FALSE)
  if(!is.infinite(x<-suppressWarnings(max(..., na.rm=na.rm)))) x else def

min.na <- function(..., def=NA, na.rm=FALSE)
  if(!is.infinite(x<-suppressWarnings(min(..., na.rm=na.rm)))) x else def

# Simple helper functions for time formatting
kSecond <- 1
kMinute <- kSecond * 60
kHour <- kMinute * 60

formatDurationHour <- function(s) {
  sprintf("%.0fh%s", floor(s/kHour), formatDurationMinute(s %% kHour))
}

formatDurationMinute <- function(s) {
  ifelse(s > kHour,
         formatDurationHour(s),
         sprintf("%.0fm%ss", floor(s/kMinute),
                 format((s %% kMinute) / kSecond, digits=9, scientific=F)))
}

formatSeconds <- function(s) {
  prefix <- ifelse(s < 0, "-", "")
  s <- abs(s)
  paste(prefix, formatDurationMinute(s), sep="")
}

link ="https://raw.githubusercontent.com/hortonworks/hortonworks-sandbox/master/apps/beeswax/data/sample_07.csv"



eventlog <- read.csv('c:/users/seppe/desktop/sandbox.csv', sep=';')
eventlog <- read.csv('link, sep=';')

#

eventlog$Start <- as.POSIXct(strptime(eventlog$Start.Timestamp, "%Y/%m/%d %H:%M:%OS"))
eventlog$Complete <- as.POSIXct(strptime(eventlog$Complete.Timestamp, "%Y/%m/%d %H:%M:%OS"))


eventlog %<>%
  mutate(RowNum=row_number()) %>% 
  arrange(Start, RowNum) %>% 
  mutate(RowNum=row_number()) %>% 
  rowwise %>%
  mutate(NextNum=min.na(.$RowNum[.$Case.ID == Case.ID & RowNum < .$RowNum & .$Start >= Complete])) %>%
  mutate(PrevNum=max.na(.$RowNum[.$Case.ID == Case.ID & RowNum > .$RowNum & .$Complete <= Start])) %>%
  ungroup



eventlog %>% 
  arrange(Start.Timestamp) %>% 
  group_by(Case.ID) %>% 
  summarize(Variant=paste(Activity, collapse='->', sep='')) %>% 
  ggplot(aes(x=reorder(Variant, -table(Variant)[Variant]) )) +
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  xlab('Variants') +
  geom_bar()


activities.basic <- eventlog %>%
  select(Case.ID, RowNum, Start, Complete, act=Activity) %>% 
  mutate(Duration=Complete-Start)


edges.basic <- bind_rows(
  eventlog %>% select(Case.ID, a=RowNum, b=NextNum),
  eventlog %>% select(Case.ID, a=PrevNum, b=RowNum)) %>%
  filter(!is.na(a), !is.na(b)) %>%
  distinct %>%
  left_join(eventlog, by=c("a" = "RowNum"), copy=T, suffix=c("", ".prev")) %>%
  left_join(eventlog, by=c("b" = "RowNum"), copy=T, suffix=c("", ".next")) %>%
  select(Case.ID, a, b, 
         a.act=Activity, b.act=Activity.next, 
         a.start=Start, b.start=Start.next,
         a.complete=Complete, b.complete=Complete.next) %>%
  mutate(Duration=b.start-a.complete)


# For the activities, we only need the activity name and the frequency of occurrence per activity
activities.counts <- activities.basic %>%
  select(act) %>% 
  group_by_all %>% 
  summarize(metric=n())

# For the arcs, we now only need the activity names of the endpoints and the frequency of occurrence per such pair
edges.counts <- edges.basic %>%
  select(a.act, b.act) %>%
  group_by_all %>%
  summarize(metric=n())


g <- graph_from_edgelist(edges.counts %>% select(a.act, b.act) %>% as.matrix(ncol=2))


dot.plot <- function(g) {
  tmpfile <- tempfile(pattern='dot', fileext='.dot')
  print(tmpfile)
  g %>% write_graph(tmpfile, "dot")
  handle <- spawn_process('c:/graphviz/bin/dot.exe', c('-Tpng', '-O', tmpfile))
  Sys.sleep(5) # Sleep to make sure dot has finished
  pngimg <- readPNG(paste(tmpfile, '.png', sep=''))
  grid.raster(pngimg)
}


g %<>%
  set_vertex_attr('fontsize', value='8') %>%
  set_vertex_attr('fontname', value='Arial') %>%
  set_vertex_attr('shape', value='box') %>%
  set_vertex_attr('label', value=paste(
    vertex_attr(., 'name'),
    activities.counts %>% .[match(vertex_attr(g, 'name'), .$act), ] %>% .$metric,
    sep='\n')) %>%
  set_edge_attr('label', value=edges.counts$metric)

dot.plot(g)



# Case frequencies: distinct frequencies per case-activity pair
activities.counts <- activities.basic %>%
  distinct(Case.ID, act) %>%
  select(act) %>% 
  group_by_all %>% 
  summarize(metric=n())

edges.counts <- edges.basic %>%
  distinct(Case.ID, a.act, b.act) %>%
  select(a.act, b.act) %>%
  group_by_all %>%
  summarize(metric=n())

# Max repetitions
activities.counts <- activities.basic %>%
  group_by(Case.ID, act) %>%
  summarise(metric=n()) %>%
  group_by(act) %>%
  summarise(metric=max(metric))

edges.counts <- edges.basic %>%
  group_by(Case.ID, a.act, b.act) %>%
  summarise(metric=n()) %>%
  group_by(a.act, b.act) %>%
  summarize(metric=max(metric))

# Median duration
activities.counts <- activities.basic %>%
  group_by(act) %>% 
  summarize(metric=formatSeconds(as.numeric(median(Duration), units='secs')))

edges.counts <- edges.basic %>%
  group_by(a.act, b.act) %>%
  summarize(metric=formatSeconds(as.numeric(median(Duration), units='secs')))
#Let us now try to make the median duration plot a bit more appealing. First, we’ll define two color gradients for the activities and edges respectively, as well as another small helper function:
  
col.box.red <- colorRampPalette(c('#FEF0D9', '#B30000'))(20)
col.arc.red <- colorRampPalette(c('#938D8D', '#B30000'))(20)

linMap <- function(x, from, to) (x - min(x)) / max(x - min(x)) * (to - from) + from
We can then use this function as follows:
  
  activities.counts <- activities.basic %>%
  group_by(act) %>% 
  summarize(metric=formatSeconds(as.numeric(median(Duration))),
            metric.s=as.numeric(median(Duration))) %>%
  ungroup %>%
  mutate(metric=ifelse(metric.s == 0, 'instant', metric),
         color=col.box.red[floor(linMap(metric.s, 1,20))])

edges.counts <- edges.basic %>%
  group_by(a.act, b.act) %>%
  summarize(metric=formatSeconds(as.numeric(median(Duration))),
            metric.s=as.numeric(median(Duration))) %>%
  ungroup %>%
  mutate(metric=ifelse(metric.s == 0, 'instant', metric),
         color=col.arc.red[floor(linMap(metric.s, 1, 20))],
         penwidth=floor(linMap(metric.s, 1, 5)))



g <- graph_from_edgelist(edges.counts %>% select(a.act, b.act) %>% as.matrix(ncol=2))
g %<>%
  set_graph_attr('dpi', value='300') %>%
  set_vertex_attr('fontsize', value='8') %>%
  set_vertex_attr('fontname', value='Arial') %>%
  set_vertex_attr('shape', value='box') %>%
  set_vertex_attr('style', value='rounded,filled') %>%
  set_vertex_attr('label', value=paste(
    vertex_attr(., 'name'),
    activities.counts %>% .[match(vertex_attr(g, 'name'), .$act), ] %>% .$metric,
    sep='\n')) %>%
  set_vertex_attr('fillcolor', value=activities.counts %>% 
                    .[match(vertex_attr(g, 'name'), .$act), ] %>% .$color) %>%
  set_edge_attr('fontsize', value='8') %>%
  set_edge_attr('fontname', value='Arial') %>%
  set_edge_attr('label', value=edges.counts$metric) %>%
  set_edge_attr('color', value=edges.counts$color) %>%
  set_edge_attr('penwidth', value=edges.counts$penwidth)


col.box.blue <- colorRampPalette(c('#DBD8E0', '#014477'))(20)
col.arc.blue <- colorRampPalette(c('#938D8D', '#292929'))(20)


activities.counts <- activities.basic %>%
  select(act) %>% 
  group_by_all %>% 
  summarize(metric=n()) %>%
  ungroup %>%
  mutate(fillcolor=col.box.blue[floor(linMap(metric, 1,20))])


edges.counts <- edges.basic %>%
  select(a.act, b.act) %>%
  group_by_all %>%
  summarize(metric=n()) %>%
  ungroup %>%
  mutate(color=col.arc.blue[floor(linMap(metric, 1,20))],
         penwidth=floor(linMap(metric, 1, 5)),
         metric.char=as.character(metric))


acts.res <- eventlog %>%
  filter(Activity == "Analyze Request for Quotation") %>%
  mutate(Duration=Complete-Start) %>%
  select(Duration, Resource, Activity) %>% 
  group_by(Resource, Activity) %>%
  summarize(metric.freq=n(), metric.perf=median(Duration)) %>% 
  ungroup %>%
  mutate(color='#75B779',
         penwidth=1,
         metric.char=formatSeconds(as.numeric(metric.perf)))


a <- bind_rows(activities.counts %>% select(name=act, metric=metric) %>% mutate(type='Activity'),
               acts.res %>% select(name=Resource, metric=metric.freq) %>% mutate(type='Resource')) %>%
  distinct %>%
  rowwise %>%
  mutate(fontsize=8,
         fontname='Arial',
         label=paste(name, metric, sep='\n'),
         shape=ifelse(type == 'Activity', 'box', 'ellipse'),
         style=ifelse(type == 'Activity', 'rounded,filled', 'solid,filled'),
         fillcolor=ifelse(type == 'Activity', activities.counts[activities.counts$act==name,]$fillcolor, '#75B779'))


e <- bind_rows(edges.counts, 
               acts.res %>% select(a.act=Resource, b.act=Activity, metric.char, color, penwidth)) %>%
  rename(label=metric.char) %>% 
  mutate(fontsize=8, fontname='Arial')


gh <- graph_from_data_frame(e, vertices=a, directed=T)

dot.plot(gh)
dot.plot(g)