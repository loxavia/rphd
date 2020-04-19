#reading data


pacman::p_load(googlesheets4)

glink1= "https://docs.google.com/spreadsheets/d/15DF1e54g64R42nzvTSgOZqRJrjtdpUnFIyOA-MBppWo/edit#gid=1052786815"
moodleActivities <- googlesheets4::read_sheet(glink1, sheet='pyAnalytics')
head(moodleActivities)
#sheets_browse(glink1)


#remove empty row/columns
janitor::remove_empty(dat, which = c("rows", "cols"), quiet = TRUE)