library(tidyverse)
library(bibliometrix)
library(Matrix)
library(stringr)
library(igraph)
library(FactoMineR)
library(factoextra)
library(scholar)
library(topicmodels)
library(tidytext)
library(tm)
library(ggthemes)
library(DT)
library(knitr)
library(kableExtra)

izsler<- readFiles("wos1izsler.bib","wos2izsler.bib","wos3izsler.bib")
izsler <- convert2df(izsler, dbsource = "wos", format = "bibtex")
izsve<- readFiles("izve1.bib","izve2.bib","izve3.bib")
izsve <- convert2df(izsve, dbsource = "wos", format = "bibtex")

results <-biblioAnalysis(izsler, sep = ";")
S<-summary(results, k = 10, pause = FALSE)
kable(S$MainInformationDF) %>% 
  kable_styling(bootstrap_options = c("striped",full_width = F, position = "float_right"))



NetMatrix <- biblioNetwork(izsler, analysis = "co-citation", network = "references", sep = ";")


net=networkPlot(NetMatrix, n = 50, 
                Title = "Co-Citation Network", type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=FALSE, labelsize=0.7,edgesize =50,
                edges.min=0.3)


netStat<-networkStat(NetMatrix)
summary(netStat, k=10)


M=metaTagExtraction(izsler,"CR_SO",sep=";")
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "sources", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", 
                type = "auto", size.cex=TRUE, size=20, 
                remove.multiple=FALSE, labelsize=0.7,edgesize = 15, edges.min=0.1)




j<-izsler%>% 
  filter(PY>=2005) %>% 
  group_by(SO) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  top_n(30, n)




Jizsler<-get_impactfactor(j$SO, max.distance = 3)

Jizve<-get_impactfactor(jve$SO, max.distance = 3)

a<-Jizsler %>% 
  arrange(ImpactFactor) %>% 
  mutate(Journal = factor(Journal, unique(Journal))) %>% 
  ggplot(aes(x=Journal, y=ImpactFactor))+geom_bar(stat = "identity")+coord_flip()


x<-izsler %>% 
  filter(!is.na(FU)) %>%
  group_by(PY, FU) %>% 
  summarise(n=n()) #%>% 

#  ungroup() %>% 
  #mutate(tot=sum(n))
  
  
  arrange(desc(PY))

