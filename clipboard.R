# read and write excel
#https://www.r-bloggers.com/copying-data-from-excel-to-r-and-back/

read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

dat=read.excel()
dat
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(dat)
write.excel(mtcars)

#devtools::install_github('cttobin/ggthemr')

df <- read.table(file = "clipboard", sep = "\t", header=TRUE)
df