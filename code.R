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
  filter(PY>2005) %>% 
  group_by(SO) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  top_n(10, n) %>% 
  mutate(SO = factor(SO, unique(SO))) %>% 
  ggplot(aes(x=SO, y=n))+geom_bar(stat = "identity")+coord_flip()


M %>% 
  group_by(PY) %>% 
  summarise(n=n()) %>% 
  filter(PY<2019 & PY>2005) %>% 
  ggplot(aes(PY, y=n))+geom_line()+geom_smooth(method = lm)


######################################

journals<-levels(as.factor(M$SO))
JIF<-get_impactfactor(journals, max.distance = 3)

JIF %>% 
arrange(ImpactFactor) %>% 
  mutate(Journal = factor(Journal, unique(Journal))) %>% 
  ggplot(aes(x=Journal, y=ImpactFactor))+geom_bar(stat = "identity")+coord_flip()

get_impactfactor("PLOSE ONE")
