######BIBLIOMETRIX##############
###############################
library(tidyverse)
library(bibliometrix)
library("Matrix")
library("stringr")
library("igraph")
library("FactoMineR")
library("factoextra")


D <- readFiles("izsler.bib")
M <- convert2df(D, dbsource = "scopus", format = "bibtex")

M<-M %>% 
  filter(PY>=1999)

results <- biblioAnalysis(M, sep = ";")

country<-as.data.frame(results[["Countries"]])
country %>% 
arrange(Freq) %>% 
mutate(Country = factor(Tab, unique(Tab))) %>% 
ggplot(aes(x=Freq, y=Country))+geom_point()






plot(x = results,  pause = FALSE)

DF<-dominance(results, k = 10)

NetMatrix<-biblioNetwork(M, analysis = "coupling", network = "authors", sep = ";")
net=networkPlot(NetMatrix, n = 20, Title = "Authors' Coupling", type = "fruchterman", size=FALSE, 
                remove.multiple=TRUE, vos.path = "/home/malou/git/IZSLEReasearch")



########################################################
############PUBMED MINER################################
########################################################

library(pubmed.mineR)

abs<-xmlreadabs("pubmed_result.xml")
abs<-readabs("pubmed_result.txt")
abs<-cleanabs(abs)
wa<-word_atomizations(abs) 


library(adjutant)