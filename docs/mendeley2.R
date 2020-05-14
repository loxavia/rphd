#papers : Mendeley -> CSV

#http://www.strategist.ie/exporting-endnote-mendelay-and-bibtex-databases-to-ms-excel/
#-------------
# Step4: Open BibTex file in Jabref
# # In Jabref (free download)
# # File – Open database
# Select the BibTex (*.bib) file from Step 3 and Open it
# # Step 5: Export from Jab ref to CSV
# # In Jabref
# # File -> Export
# Files of type: Open Office CSV (*.csv) [There is an option for MS Office XML – I had less success with this format]

biblio2 = read.csv(file='E:/data/papers/papers20A.csv')
head(biblio2)
dim(biblio2)
names(biblio2)
biblio2 %>% group_by(ISBN) %>% summarise(n=n())
biblio2 %>% group_by(Title) %>% summarise(n=n()) %>% filter(n > 1)
biblio2 %>% group_by(URL) %>% summarise(n=n())
biblio2 %>% group_by(Author) %>% summarise(n=n())  
biblio2 %>% slice(grep('Anchal',Author)) %>% select(Title)
