#extract from pdf
#https://medium.com/@ketanrd.009/how-to-extract-pdf-tables-in-r-e994c0fe4e28
pacman::p_load(tidyverse,tabulizer)

#Extracting data from page 10 and 11
gpi_table <- extract_tables("http://visionofhumanity.org/app/uploads/2018/06/Global-Peace-Index-2018-2.pdf",  output = "data.frame",  pages = c(10,10,10,11,11), area = list( c(496, 38, 786, 169), c(496, 212, 786, 341),c(496, 380, 786, 508),c(496, 392, 738, 521),c(496, 225, 788, 353) ),guess = FALSE)
gpi_table

#clean
gpi_table_clean <- reduce(gpi_table, bind_rows)
gpi_table_clean
#rename the columns
names(gpi_table_clean)[1] <- 'gpi_rank'
names(gpi_table_clean)[2] <- 'gpi_country'
names(gpi_table_clean)[3] <- 'gpi_score'

gp_table_final <- gpi_table_clean