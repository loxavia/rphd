#extract from word
#https://github.com/hrbrmstr/docxtractr
#install.packages("docxtractr")


library(docxtractr)
library(tibble)
library(dplyr)

# current version
packageVersion("docxtractr")
doc <- read_docx(system.file("examples/data.docx", package="docxtractr"))
docx_describe_tbls(doc)
docx_extract_tbl(doc, 1)
docx_extract_tbl(doc)
docx_extract_tbl(doc, header=FALSE)
#> NOTE: header=FALSE but table has a marked header row in the Word document

rp <- read_docx("docs/sampledocs.docx")
docx_describe_tbls(rp)
docx_extract_tbl(rp, header=T)

budget <- read_docx("http://rud.is/dl/1.DOCX")

docx_tbl_count(budget)
#> [1] 2

docx_describe_tbls(budget)
docx_extract_tbl(budget, 1)
docx_extract_tbl(budget, 2)
# three tables
doc3 <- read_docx(system.file("examples/data3.docx", package="docxtractr"))

docx_tbl_count(doc3)
#> [1] 3

docx_describe_tbls(doc3)
docx_extract_tbl(doc3, 3)
# no tables
none <- read_docx(system.file("examples/none.docx", package="docxtractr"))

docx_tbl_count(none)
#> [1] 0

# wrapping in try since it will return an error
# use docx_tbl_count before trying to extract in scripts/production
try(docx_describe_tbls(none))
#> No tables in document
try(docx_extract_tbl(none, 2))

# 5 tables, with two in sketchy formats
complx <- read_docx(system.file("examples/complex.docx", package="docxtractr"))

docx_tbl_count(complx)
#> [1] 5

docx_describe_tbls(complx)
docx_extract_tbl(complx, 3, header=TRUE)
docx_extract_tbl(complx, 4, header=TRUE)
docx_extract_tbl(complx, 5, header=TRUE)

# a "real" Word doc----
real_world <- read_docx(system.file("examples/realworld.docx", package="docxtractr"))

docx_tbl_count(real_world)
#> [1] 8

# get all the tables
tbls <- docx_extract_all_tbls(real_world)

# see table 1
tbls[[1]]
# make table 1 better
assign_colnames(tbls[[1]], 2)
# make table 1's column names great again 
mcga(assign_colnames(tbls[[1]], 2))
# see table 5
tbls[[5]]
# make table 5 better
assign_colnames(tbls[[5]], 2)
# preserve lines
intracell_whitespace <- read_docx(system.file("examples/preserve.docx", package="docxtractr"))
docx_extract_all_tbls(intracell_whitespace, preserve=TRUE)
docx_extract_all_tbls(intracell_whitespace)
# comments
cmnts <- read_docx(system.file("examples/comments.docx", package="docxtractr"))
print(cmnts)
glimpse(docx_extract_all_cmnts(cmnts))


# original
read_docx(  system.file("examples/trackchanges.docx", package="docxtractr")
) %>% docx_extract_all_tbls(guess_header = FALSE)

# accept
read_docx(   system.file("examples/trackchanges.docx", package="docxtractr"),
  track_changes = "accept" ) %>%  docx_extract_all_tbls(guess_header = FALSE)

# reject
read_docx(  system.file("examples/trackchanges.docx", package="docxtractr"),
  track_changes = "reject" ) %>%   docx_extract_all_tbls(guess_header = FALSE)


library(docxtractr)
library(testthat)
date()

test_dir("tests/")


#-------
# read_docx: Read in a Word document for table extraction
# docx_describe_tbls: Returns a description of all the tables in the Word document
# docx_describe_cmnts: Returns a description of all the comments in the Word document
# docx_extract_tbl: Extract a table from a Word document
# docx_extract_cmnts: Extract comments from a Word document
# docx_extract_all_tbls: Extract all tables from a Word document (docx_extract_all is now deprecated)
# docx_tbl_count: Get number of tables in a Word document
# docx_cmnt_count: Get number of comments in a Word document
# assign_colnames: Make a specific row the column names for the specified data.frame
# mcga : Make column names great again
# set_libreoffice_path: Point to Local soffice.exe File
# The following data file are included:
#   
#   system.file("examples/data.docx", package="docxtractr"): Word docx with 1 table
# system.file("examples/data3.docx", package="docxtractr"): Word docx with 3 tables
# system.file("examples/none.docx", package="docxtractr"): Word docx with 0 tables
# system.file("examples/complex.docx", package="docxtractr"): Word docx with non-uniform tables
# system.file("examples/comments.docx", package="docxtractr"): Word docx with comments
# system.file("examples/realworld.docx", package="docxtractr"): A “real world” Word docx file with tables of all shapes and sizes
# system.file("examples/trackchanges.docx", package="docxtractr"): Word docx with track changes in a table