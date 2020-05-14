#extract from Research Papers----
pacman::p_load(docxtractr,tibble,dplyr, ggplot2, tidyr)

#install.packages("docxtractr")
folder ='E:/analytics/projects/rphd/docs/'
list.files(folder, pattern='*.docx')
fileName = "PhdPapers2020.docx"
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

#Extract a table from a Word document
docx_tbl_count(papers)  #119 tables

#Comments from a Word document
glimpse(docx_extract_all_cmnts(papers))
docx_describe_cmnts(papers) 
docx_cmnt_count(papers)

#Make a specific row the column names for DF : assign_colnames(papers)
#Make column names great again : mcga
tables1[[1]]
assign_colnames(tables1[[1]], 1)
mcga(assign_colnames(tables1[[1]], 1)) #another method

#now extract tables one by one/ remove " 
(paper1 <- mcga(assign_colnames(tables1[[1]], 1)))
(paper1A <- paper1 %>% mutate_each( funs(ifelse(nchar(.)==0, NA, .) )))

#function method-----
getTableData <- function(n) {
  (pn = 0+n)
  P <- mcga(assign_colnames(tables1[[pn]], 1))
  head(P) ; P$id = paste0('P',n) 
  P <- P %>% mutate_each( funs(ifelse(nchar(.)==0, NA, .) ))
}
#-------------------------------------start here---------------------
#using list of DFs-----
docx_tbl_count(papers)
(paperscounts = docx_tbl_count(papers))
(paperscounts = paperscounts) # when some are diff
Q <- list() ;Q
(TT = 1:paperscounts)
#(TT = 1:80)

i=1
for (i in TT) { Q[[i]] <- getTableData(n=i) }
Q
#names(Q[[1]])
#for(i in TT){  print(paste0(i,names(Q[[i]])))}

allPapers2 = do.call("rbind", Q)
allPapers2
allPapers2 %>% filter(description %in% c('LS','EDM','LA','PM'))

allPapers2 %>% group_by(id) %>% summarise(n = n()) %>% as.data.frame() #metadata of each paper

allPapers2 %>% group_by(subject) %>% summarise(n = n()) %>% as.data.frame()
#check for errors
#searchList1 = c('links1')
#allPapers2 %>% filter( subject %in% searchList1)
#allPapers2 %>% filter( id %in% c(17))

allPapers2 %>% group_by(subject,id) %>% summarise(n = n()) %>% as.data.frame()

allPapers2 %>% filter(!is.na(description)) %>% filter(subject %in% c('area')) %>% select(subject, description, id) %>% group_by(subject, description) %>% summarise(n=n())
search = 'Learning analytics'
grep(search, allPapers2$description)
allPapers2 %>% slice(grep(search, description))

head(allPapers2)
allPapers2 <- allPapers2 %>% filter(!is.na(subject))
#check for duplicate subject value
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
allPapers3 %>% arrange(year,id,RO) %>% filter(!is.na(RO)) %>%  select(year,id, RO)
allPapers3 %>% arrange(year,id,futurework) %>% filter(!is.na(futurework)) %>%  select(year,id, futurework)
allPapers3 %>% arrange(year,id,methods) %>% filter(!is.na(methods)) %>% mutate_each( funs(gsub('"', '', .) )) %>% select(year, id, methods)
#gsub("[^[:alnum:]]", " ", x)

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

allPapers4[1:5,1:5]
names(allPapers4)
(dataExport1 <- allPapers4 %>% select(serial, area, year, title,dateA, dateR))
paperGLINK = '1NE-np7z6wBuDgbsgkmQtnxPvPegQ8Z1LgSeLQwv6nUQ'
library(googlesheets4)
dataExport1 %>% slice(51:nrow(dataExport1)) %>% sheet_write(paperGLINK, sheet='fromR')
