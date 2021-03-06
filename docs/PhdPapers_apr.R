#extract from Research Papers----
pacman::p_load(docxtractr,tibble,dplyr, ggplot2, tidyr)

#install.packages("docxtractr")
folder ='E:/analytics/projects/rphd/docs/'
list.files(folder)
fileName = "PhdLATOC_2020.docx"
filePath = paste0(folder, fileName ) 

#Read in a Word document for table extraction
papers <- read_docx(filePath)

#Returns a description of all the tables in the Word document
docx_describe_tbls(papers)
# Get number of tables in a Word document
docx_tbl_count(papers)

#Extract all tables from a Word document (docx_extract_all is now deprecated)
tables1 <- docx_extract_all_tbls(papers, guess_header = T) 
tables1  
#?docx_extract_all_tbls(docx, guess_header = TRUE, preserve = FALSE, trim = TRUE)

#table1
tables1[[1]]
tables1[[22]]

#Extract a table from a Word document
#docx_extract_tbl(docx = papers, tbl_number = 1, header = TRUE, preserve = FALSE, trim = TRUEpapers, 1, header=T)
docx_tbl_count(papers)  #24 tables

#Extract comments from a Word document
glimpse(docx_extract_all_cmnts(papers))

#Returns a description of all the comments in the Word document
docx_describe_cmnts(papers) 

#Get number of comments in a Word document
docx_cmnt_count(papers)

#Make a specific row the column names for DF : assign_colnames(papers)
#Make column names great again : mcga
tables1[[1]]

# make table 1 better
assign_colnames(tables1[[1]], 2)

# make table 1's column names great again 
mcga(assign_colnames(tables1[[1]], 2))
tables1[[1]]

#now extract tables one by one
#tables
tables1

#table1-Phd Details ---
phdDetails <- mcga(assign_colnames(tables1[[1]], 1))
phdDetails

#table2- Research Objectives ---
resObj <- mcga(assign_colnames(tables1[[2]], 1))
resObj

#table3- Research Questions ---
resQues <- mcga(assign_colnames(tables1[[3]], 1))
resQues

#table4- Research Outcomes ---
resOut <- mcga(assign_colnames(tables1[[4]], 1))
resOut

#table5- Paper Plan ---
paperPlan <- mcga(assign_colnames(tables1[[5]], 1))
paperPlan

#table6- Techniques ---
techniques <- mcga(assign_colnames(tables1[[6]], 1))
head(techniques)

#table7- Definitions ---
definitions <- mcga(assign_colnames(tables1[[7]], 1))
head(definitions)

#table8- Template for Paper ---
paperTemplate <- mcga(assign_colnames(tables1[[8]], 1))
paperTemplate
paste(paperTemplate$ser, paperTemplate$subject)
paperTemplate[,c('ser','subject')] %>% as.data.frame()
#papers-----
docx_extract_tbl(papers,11, header=T)
tables1[[11]]
#nchar(P1$description)

#P1 - table11 ----
n=1 ; pn = 10+n
P <- mcga(assign_colnames(tables1[[pn]], n))
head(P); P$id = n 
P <- P %>% mutate_each( funs(ifelse(nchar(.)==0, NA, .) ))
P1=P
head(P1)
tables1
(K40 <- mcga(assign_colnames(tables1[[40]], 1)))
(K50 <- mcga(assign_colnames(tables1[[60]], 1)))


#function method-----
getTableData <- function(n) {
  (pn = 10+n)
  P <- mcga(assign_colnames(tables1[[pn]], 1))
  head(P) ; P$id = n 
  P <- P %>% mutate_each( funs(ifelse(nchar(.)==0, NA, .) ))
}
n=0;pid=1
(P1 = getTableData(n=pid)); pid=pid+1
(P2 = getTableData(n=pid)); pid=pid+1
(P3 = getTableData(n=pid)); pid=pid+1
(P4 = getTableData(n=pid)); pid=pid+1
(P5 = getTableData(n=pid)); pid=pid+1
(P6 = getTableData(n=pid)); pid=pid+1
(P7 = getTableData(n=pid)); pid=pid+1
(P8 = getTableData(n=pid)); pid=pid+1
(P9 = getTableData(n=pid)); pid=pid+1
(P10 = getTableData(n=pid)); pid=pid+1
(P11 = getTableData(n=pid)); pid=pid+1
(P12 = getTableData(n=pid)); pid=pid+1
(P13 = getTableData(n=pid)); pid=pid+1
(P14 = getTableData(n=pid)); pid=pid+1
(P15 = getTableData(n=pid)); pid=pid+1
(P16 = getTableData(n=pid)); pid=pid+1
(P17 = getTableData(n=pid)); pid=pid+1
(P18 = getTableData(n=pid)); pid=pid+1
(P19 = getTableData(n=pid)); pid=pid+1
(P20 = getTableData(n=pid)); pid=pid+1
(P21 = getTableData(n=pid)); pid=pid+1
(P22 = getTableData(n=pid)); pid=pid+1
(P23 = getTableData(n=pid)); pid=pid+1
(P24 = getTableData(n=pid)); pid=pid+1
(P25 = getTableData(n=pid)); pid=pid+1
(P26 = getTableData(n=pid)); pid=pid+1
(P27 = getTableData(n=pid)); pid=pid+1
(P28 = getTableData(n=pid)); pid=pid+1
(P29 = getTableData(n=pid)); pid=pid+1
(P30 = getTableData(n=pid)); pid=pid+1
(P31 = getTableData(n=pid)); pid=pid+1
(P32 = getTableData(n=pid)); pid=pid+1
(P33 = getTableData(n=pid)); pid=pid+1
(P34 = getTableData(n=pid)); pid=pid+1
(P35 = getTableData(n=pid)); pid=pid+1
(P36 = getTableData(n=pid)); pid=pid+1

rbind(P1,P2)
allPapers <- do.call("rbind", list(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17,P18, P19, P20, P21, P22, P23, P24, P25, P26, P27, P28,P29, P30,P31,P32,P33,P34,P35, P36))
dim(allPapers)

colSums(is.na(allPapers))
allPapers <- allPapers %>% filter(! is.na(description))
colSums(is.na(allPapers))

allPapers %>% group_by(subject) %>% summarise(n = n()) %>% as.data.frame()
allPapers %>% group_by(subject,id) %>% summarise(n = n()) %>% as.data.frame()
allPapers %>% filter(subject == 'area') %>% group_by(description) %>% summarise(n = n()) %>% as.data.frame()

#using list of DFs-----
docx_tbl_count(papers)
(paperscounts = docx_tbl_count(papers) - 10)
(paperscounts = paperscounts -3) #last few are different format
Q <- list() ;Q
(TT = 1:paperscounts)
#(TT = 1:80)

i=1
for (i in TT) {
Q[[i]] <- getTableData(n=i)
}
Q

allPapers2 = do.call("rbind", Q)
allPapers2
allPapers2 %>% filter(description =='Learning Style')
allPapers2 %>% group_by(id) %>% summarise(n = n()) %>% as.data.frame()

allPapers2 %>% group_by(subject) %>% summarise(n = n()) %>% as.data.frame()
allPapers2 %>% group_by(subject,id) %>% summarise(n = n()) %>% as.data.frame()

allPapers2 %>% filter(!is.na(description)) %>% filter(subject %in% c('area')) %>% select(subject, description, id) %>% group_by(subject, description) %>% summarise(n=n())
head(allPapers2)
allPapers2 <- allPapers2 %>% filter(!is.na(subject))
allPapers2 %>% group_by(id,subject) %>% summarise(n=n()) %>% filter(n>1)
allPapers2 <- allPapers2 %>% filter(!is.na(description)) 

#pivot https://tidyr.tidyverse.org/articles/pivot.html-----

allPapers3 <- allPapers2 %>% tidyr::pivot_wider(names_from=subject, values_from = description)
allPapers3[1:5, 1:10]
names(allPapers3)
allPapers3 %>% mutate_at(.vars=vars(area,year), .funs=list(as.character))  %>% select(area, year) %>% arrange(area)
names(allPapers2)
names(allPapers3)
allPapers3 %>% group_by(year) %>% summarise(n=n())
allPapers3 %>% arrange(year,id,RQ) %>% filter(!is.na(RQ)) %>%  select(id, RQ)
allPapers3 %>% arrange(year,id,RO) %>% filter(!is.na(RO)) %>%  select(id, RO)
allPapers3 %>% arrange(year,id,futurework) %>% filter(!is.na(futurework)) %>%  select(year,id, futurework)
allPapers3 %>% arrange(year,id,methods) %>% filter(!is.na(methods)) %>% select(year, id, methods)
allPapers3 %>% arrange(year,id,keywords) %>% filter(!is.na(keywords)) %>% select(year, id, keywords)
allPapers3 %>% arrange(year,id,title) %>% filter(!is.na(title)) %>% select(year, id, title)
allPapers3 %>% arrange(year,id,results) %>% filter(!is.na(results)) %>% select(year, id, results)
allPapers3 %>% arrange(year,id,conclusion) %>% filter(!is.na(conclusion)) %>% select(year, id, conclusion)
allPapers3 %>% arrange(year,id,area) %>% filter(!is.na(area)) %>% select(year, id, area)

allPapers3 %>% arrange(year,id,factors) %>% filter(!is.na(factors)) %>% select(year, id, factors)
allPapers3 %>% filter(area=='Learning Analytics')
allPapers3 %>% select(year,id,points, comments, highlights) %>% reshape2::melt(id.vars=c('year', 'id')) %>% filter(!is.na(value)) %>% select(year, id, variable, value)  %>% arrange(year, id, variable, value) %>% glimpse
#options(dplyr.width = Inf)


#------------
#allPapers2 %>% tidyr::pivot_wider(names_from=subject, values_from = description,  values_fill = list(description = NA)) 

#allPapers3 %>% mutate(area = as.character(area)) %>% group_by(area) %>% summarise(n=n())
as.character(allPapers3$rating)
(varNames = unique(allPapers2$subject))

allPapers4 <- allPapers2  %>% pivot_wider(names_from = c(subject), values_from = description) %>% mutate_at(.vars = vars(varNames), .funs=list(as.character))
which(duplicated(allPapers4))

allPapers4
allPapers4 %>% select(RQ) %>% filter(!is.null(RQ) & !is.na(RQ)) %>% filter(RQ != 'NULL')
allPapers4 %>% select(year) %>% filter(!is.null(year) & !is.na(year) & (year != 'NULL'))
allPapers4 %>%  mutate(area = sapply(area, toString))
str(allPapers4)
allPapers4 %>% purr::keep( ~ !is.null(.) )
?keep

allPapers4[!sapply(allPapers4, is.null)]


