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

results <- biblioAnalysis(M, sep = ";")


plot(x = results, k = 10, pause = FALSE)

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