#Traminer
#https://cran.r-project.org/web/packages/TraMineR/TraMineR.pdf

## load the mvad data
library(TraMineR)
data(mvad)
head(mvad)
mvad.seq <- seqdef(mvad[,17:86])
## distribution plot by sex (male)
seqdplot(mvad.seq, group=mvad$male, border=NA)

## compute the LCS pairwise distance matrix
## among the first 10 sequences
mvad.lcs <- seqdist(mvad.seq[1:10,], method="LCS")
mvad.lcs


data(actcal)
data(seqdata)
alphabet(seqdata)


data(actcal)
actcal.seq <- seqdef(actcal,13:24)
## Retrieving the alphabet
alphabet(actcal.seq)
## Setting the alphabet
alphabet(actcal.seq) <- c("FT", "PT", "LT", "NO")
## Event sequences
actcal.eseq <- seqecreate(actcal.seq)
alphabet(actcal.eseq)

