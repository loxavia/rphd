#extract from Research Papers----

#install.packages("docxtractr")

pacman::p_load(docxtractr,tibble,dplyr, ggplot2)

# Get number of tables in a Word document
docx_tbl_count(papers)

#Read in a Word document for table extraction
papers <- read_docx("docs/sampledocs.docx")
papers <- read_docx("https://docs.google.com/document/d/1-6gsNtG_xTIq6LdiR0iRVbnmzFLijFIS9wIyjjpvxkQ/edit?usp=sharing")

#Returns a description of all the tables in the Word document
docx_describe_tbls(papers)

#Extract all tables from a Word document (docx_extract_all is now deprecated)
tables1 <- docx_extract_all_tbls(papers) 
tables1  

#table1
tables1[[1]]

#Extract a table from a Word document
docx_extract_tbl(papers, header=T)

docx_tbl_count(papers)

#Extract comments from a Word document
docx_extract_cmnts(papers)
glimpse(docx_extract_all_cmnts(papers))

#Returns a description of all the comments in the Word document
docx_describe_cmnts(papers) 

#Get number of comments in a Word document
docx_cmnt_count(papers)

#Make a specific row the column names for the specified data.frame
assign_colnames(papers)

#Make column names great again : mcga
tables1[[1]]
# make table 1 better
assign_colnames(tables1[[1]], 2)

# make table 1's column names great again 
mcga(assign_colnames(tables1[[1]], 2))
tables1[[1]]
