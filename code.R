######BIBLIOMETRIX##############
###############################
library(tidyverse)
library(bibliometrix)
library(Matrix)
library(stringr)
library(igraph)
library(FactoMineR)
library(factoextra)
library(scholar)

rm(list=ls())
izsler<- readFiles("izsler.bib")
izsler <- convert2df(izsler, dbsource = "scopus", format = "bibtex")

izsve<- readFiles("izsve.bib")
izsve <- convert2df(izsve, dbsource = "scopus", format = "bibtex")

izsam<- readFiles("izsam.bib")
izsam <- convert2df(izsam, dbsource = "scopus", format = "bibtex")

####produttività###

A=izsler %>% 
  group_by(PY) %>% 
  summarise(n=n()) %>% 
  filter(PY<2019 & PY>=2005)
A$Istituto<-rep("izsler", dim(A)[1]) 

B=izsve %>% 
  group_by(PY) %>% 
  summarise(n=n()) %>% 
  filter(PY<2019 & PY>=2005)
B$Istituto<-rep("izsve", dim(B)[1])

C=izsam %>% 
  group_by(PY) %>% 
  summarise(n=n()) %>% 
  filter(PY<2019 & PY>=2005) 

C$Istituto<-rep("izsam", dim(C)[1])

prod<-rbind(A,B,C)

prod %>% 
  ggplot(aes(x=PY, y=n, group=Istituto, color=Istituto))+geom_line()+
  labs(y="n.articoli", x="anno")+
  scale_x_continuous(breaks=c(2005:2018))


##################################
















izsler %>% 
  filter(PY>2005) %>% 
  group_by(SO) %>% 
  summarise(n=n()) %>% 
  top_n(10, n) %>% 
  arrange(n) %>% 
  mutate(SO = factor(SO, unique(SO))) %>% 
  ggplot(aes(x=SO, y=n))+geom_bar(stat = "identity")+coord_flip()

izsve %>% 
  filter(PY>2005) %>% 
  group_by(SO) %>% 
  summarise(n=n()) %>% 
  top_n(10, n) %>% 
  arrange(n) %>% 
  mutate(SO = factor(SO, unique(SO))) %>% 
  ggplot(aes(x=SO, y=n))+geom_bar(stat = "identity")+coord_flip()

izsam %>% 
  filter(PY>2005) %>% 
  group_by(SO) %>% 
  summarise(n=n()) %>% 
  top_n(10, n) %>% 
  arrange(n) %>% 
  mutate(SO = factor(SO, unique(SO))) %>% 
  ggplot(aes(x=SO, y=n))+geom_bar(stat = "identity")+coord_flip()





j<- %>% 
  filter(PY>2005) %>% 
  group_by(SO) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  top_n(10, n)

JIF<-get_impactfactor(j$SO, max.distance = 3)

JIF %>% 
  arrange(ImpactFactor) %>% 
  mutate(Journal = factor(Journal, unique(Journal))) %>% 
  ggplot(aes(x=Journal, y=ImpactFactor))+geom_bar(stat = "identity")+coord_flip()




######################################

izsler<-biblioAnalysis(M,sep=";")
S<-summary(object = izsler, k = 10, pause = FALSE)

DF<-dominance(izsler, k = 10)


authors<-gsub(","," ",names(izsler$Authors)[1:15])
indices<-Hindex(M, authors, sep = ";")
indices$H
