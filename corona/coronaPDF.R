
pacman::p_load(tidyverse,tabulizer)

# simple demo file
f <- system.file("examples", "data.pdf", package = "tabulizer")

# extract all tables
extract_tables(f)
 

link1 = "https://www.mohfw.gov.in/pdf/DistrictWiseList324.pdf"
district_table <- extract_tables(link, pages=c(1,2), output = "data.frame",area = list(c(400, 100, 212, 462)), guess=T)
district_table  
head(district_table)
names(district_table)
link2 = "https://www.mohfw.gov.in/pdf/coronvavirushelplinenumber.pdf"
helpline_table <- extract_tables(link2, output = "data.frame")
helpline_table      
?extract_tables               
