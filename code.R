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
D <- readFiles("izsler.bib")
M <- convert2df(D, dbsource = "scopus", format = "bibtex")


M %>% 
  #filter(PY>=2005) %>% 
  group_by(SO) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  filter(n>1) %>% 
  mutate(SO = factor(SO, unique(SO))) %>% 
  ggplot(aes(x=SO, y=n))+geom_bar(stat = "identity")+coord_flip()+
  facet_grid(~PY)


######################################

journals<-levels(as.factor(M$SO))
JIF<-get_impactfactor(journals)

JIF %>% 
arrange(ImpactFactor) %>% 
  mutate(Journal = factor(Journal, unique(Journal))) %>% 
  ggplot(aes(x=Journal, y=ImpactFactor))+geom_bar(stat = "identity")+coord_flip()
