#mendeley and R
#https://www.mathieubray.com/2017/03/29/extract-notes-mendeley/
library(RSQLite) # Database 
library(dplyr) # This really should be loaded by default always...
library(tidyr) # 'spread' function to change table from 'long' to 'wide' format 
#We first need to connect to the database. Mendeley Support lists how to find the local database path for each system. In Windows, the mendeley.path, as we will refer to it in R, will look something like C:/Users/{username}/AppData/Local/Mendeley Ltd./Mendeley Desktop/{youremail}@{emailclient.com}@www.mendeley.com.sqlite.

mendeley.path1='C:/Users/du/AppData/Local/Mendeley Ltd/Mendeley Desktop/www.mendeley.com/78ebe61b-ba25-355b-aa5c-4efe659cd857-67c8/pages-index.sqlite'
mendeley.path2='C:/Users/du/AppData/Local/Mendeley Ltd/Mendeley Desktop/www.mendeley.com/78ebe61b-ba25-355b-aa5c-4efe659cd857-67c8/search-index.sqlite'
mendeley.path3='C:/Users/du/AppData/Local/Mendeley Ltd/Mendeley Desktop/www.mendeley.com/dup1966@gmail.com@www.mendeley.com.sqlite'
mendeley.path4='C:/Users/du/AppData/Local/Mendeley Ltd/Mendeley Desktop/78ebe61b-ba25-355b-aa5c-4efe659cd857@www.mendeley.com.sqlite'
#dup1966@gmail.com@www.mendeley.com.sqlite

# Connect to the database
mendeley.connection1 = dbConnect(RSQLite::SQLite(),mendeley.path1)
mendeley.connection2 = dbConnect(RSQLite::SQLite(),mendeley.path2)
mendeley.connection3 = dbConnect(RSQLite::SQLite(),mendeley.path3)
mendeley.connection4 = dbConnect(RSQLite::SQLite(),mendeley.path4)

# Some of the tables available in the Mendeley database
dbListTables(mendeley.connection1)
dbListTables(mendeley.connection2)
dbListTables(mendeley.connection3)
dbListTables(mendeley.connection4)

# The variables available in the 'Documents' table
dbListFields(mendeley.connection1,"Files")
dbListFields(mendeley.connection1,"Pages")
dbListFields(mendeley.connection1,"PageTerms")
dbListFields(mendeley.connection1,"PageFullText")
dbListFields(mendeley.connection,"PageFullText_docsize")

#----
dbListFields(mendeley.connection2,"Documents")
dbListFields(mendeley.connection2,"DocumentFullText")
dbListFields(mendeley.connection2,"DocumentFullText_content")
dbListFields(mendeley.connection2,"DocumentTerms")

t1 <- extract.table(mendeley.connection2, 'select id, docid, fieldNames from Documents')
t1
head(t1)

res <- dbSendQuery(conn=mendeley.connection, 'Files')
?dbSendQuery
#----
extract.table <- function(con,query){
   res <- dbSendQuery(con,query) # Send query
   table <- dbFetch(res) # Fetch table
   dbClearResult(res) # Free resources
   return(table)
}
bListFields(mendeley.connection2,"Documents")
res <- dbSendQuery(con=mendeley.connection2, 'Documents')





#-------------
# Step4: Open BibTex file in Jabref
# # In Jabref (free download)
# # File – Open database
# Select the BibTex (*.bib) file from Step 3 and Open it
# # Step 5: Export from Jab ref to CSV
# # In Jabref
# # File -> Export
# Files of type: Open Office CSV (*.csv) [There is an option for MS Office XML – I had less success with this format]
