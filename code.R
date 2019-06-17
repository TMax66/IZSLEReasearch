######BIBLIOMETRIX##############
###############################
library(tidyverse)
library(bibliometrix)


D <- readFiles("izsler.bib")
M <- convert2df(D, dbsource = "scopus", format = "bibtex")

results <- biblioAnalysis(M, sep = ";")

NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ".  ")



plot(results)

topSO=sourceGrowth(d, top=10, cdf=TRUE)


library(reshape2)
library(ggplot2)
DF=melt(topSO, id='Year')
ggplot(DF,aes(Year,value, group=variable, color=variable))+geom_line()


########################################################
############PUBMED MINER################################
########################################################

library(pubmed.mineR)

abs<-xmlreadabs("pubmed_result.xml")
abs<-readabs("pubmed_result.txt")
abs<-cleanabs(abs)
wa<-word_atomizations(abs) 


library(adjutant)