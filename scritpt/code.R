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
library(topicmodels)
library(tidytext)
library(tm)
library(ggthemes)
library(here)



#rm(list=ls())

files<- c("izsler1.bib")#,"izsler2.bib","izsler3.bib")

izsler <- convert2df(files, dbsource = "wos", format = "bibtex")


#izsler<- readFiles("izsler.bib")
#izsler <- convert2df(izsler, dbsource = "scopus", format = "bibtex")


results <-biblioAnalysis(izsler, sep = ";")

S<-summary(results, k = 10, pause = FALSE)


####TEXT MINING#####
tryTolower<-function(x){
  y=NA
  try_error=tryCatch(tolower(x), error=function(e) e)
  if(!inherits(try_error, 'error'))
    y=tolower(x)
  return(y)
}

clean.corpus<-function(corpus){
  corpus<-tm_map(corpus, content_transformer(tryTolower))
  corpus<-tm_map(corpus, removeWords, custom.stopwords)
  corpus<-tm_map(corpus, removePunctuation)
  corpus<-tm_map(corpus, stripWhitespace)
  corpus<-tm_map(corpus,removeNumbers)
  return(corpus)
}

key<-data.frame(doc_id=seq(1:nrow(izsler)),text=izsler$ID)
key<-na.omit(key)
custom.stopwords<-c(stopwords('english'))
corpus <- VCorpus(DataframeSource(key))
corpus<-clean.corpus(corpus)

tdm<-TermDocumentMatrix(corpus, control=list(weighting=weightTf))
tdm<-removeSparseTerms(tdm,  sparse=0.99)
tdm.key.m<-as.matrix(tdm)

term.freq<-rowSums(tdm.key.m)
freq.df<-data.frame(word=names(term.freq), frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]
freq.df$word<-factor(freq.df$word, levels=unique(as.character(freq.df$word)))

ggplot(freq.df[1:50,], aes(x=word, y=frequency))+geom_bar(stat = "identity", fill='darkred')+
  coord_flip()+theme_gdocs()+geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)


izsve<- readFiles("izsve.bib")
izsve <- convert2df(izsve, dbsource = "scopus", format = "bibtex")

izsam<- readFiles("izsam.bib")
izsam <- convert2df(izsam, dbsource = "scopus", format = "bibtex")

izsto<- readFiles("izsto.bib")
izsto <- convert2df(izsto, dbsource = "scopus", format = "bibtex")

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


D=izsto %>% 
  group_by(PY) %>% 
  summarise(n=n()) %>% 
  filter(PY<2019 & PY>=2005) 

D$Istituto<-rep("izsto", dim(D)[1])



prod<-rbind(A,B,C,D)

prod %>% 
  ggplot(aes(x=PY, y=n, group=Istituto, color=Istituto))+geom_line()+
  labs(y="n.articoli", x="anno")+
  scale_x_continuous(breaks=c(2005:2018))


izsler %>% 
  filter(PY>=2005 & PY<2019) %>% 
  group_by(PY) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=PY, y=n))+geom_point(stat = "identity")+
  geom_line(stat="identity")+
  labs(x="Anno di pubblicazione", y="Numero articoli pubblicati")+
  scale_x_continuous(breaks=c(2005:2018))



  ##################################


izsler %>% 
  filter(PY>=2005 & PY<=2018) %>% 
  group_by(SO) %>% 
  summarise(n=n()) %>% 
  top_n(10, n) %>% 
  arrange(n) %>% 
  mutate(SO = factor(SO, unique(SO))) %>%
  ggplot(aes(x=SO, y=n))+geom_bar(stat = "identity", fill="red")+coord_flip()

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





j<-izsler%>% 
  filter(PY>2005) %>% 
  group_by(SO) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  top_n(10, n)


jve<-izsve%>% 
  filter(PY>2005) %>% 
  group_by(SO) %>% 
  summarise(n=n()) %>% 
  arrange(n) %>% 
  top_n(10, n)


Jizsler<-get_impactfactor(j$SO, max.distance = 3)

Jizve<-get_impactfactor(jve$SO, max.distance = 3)

a<-Jizsler %>% 
  arrange(ImpactFactor) %>% 
  mutate(Journal = factor(Journal, unique(Journal))) %>% 
  ggplot(aes(x=Journal, y=ImpactFactor))+geom_bar(stat = "identity")+coord_flip()

b<-Jizve %>% 
  arrange(ImpactFactor) %>% 
  mutate(Journal = factor(Journal, unique(Journal))) %>% 
  ggplot(aes(x=Journal, y=ImpactFactor))+geom_bar(stat = "identity")+coord_flip()



# 
# topAU <- authorProdOverTime(izsler, k = 10, graph = TRUE)
# 
# a<-biblioAnalysis(izsler)
# L <- lotka(a)
# 
# A<-cocMatrix(izsler, Field = "SO", sep = ";")
# sort(Matrix::colSums(A), decreasing = TRUE)[1:5]

##########CO-CITATION ANALYSIS#######
izsler<- readFiles("izsler.bib")
izsler <- convert2df(izsler, dbsource = "scopus", format = "bibtex")



net<-NetMatrix <- biblioNetwork(izsler, analysis = "co-citation", 
                           network = "references", sep = ";")

networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", 
                type = "fruchterman", size.cex=TRUE, size=20, 
                remove.multiple=FALSE, labelsize=0.7,edgesize = 10, 
                edges.min=5)


M=metaTagExtraction(izsler,"CR_SO",sep=";")
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "sources", sep = ";")
net=networkPlot(NetMatrix, n = 50, Title = "Co-Citation Network", type = "auto", size.cex=TRUE, size=15, remove.multiple=FALSE, labelsize=0.7,edgesize = 10, edges.min=5)

######################################

library(rAltmetric)
acuna <- altmetrics(doi = "10.1179/135100002125000406")


izsler<-biblioAnalysis(M,sep=";")
S<-summary(object = izsler, k = 10, pause = FALSE)

DF<-dominance(izsler, k = 10)


authors<-gsub(","," ",names(izsler$Authors)[1:15])
indices<-Hindex(M, authors, sep = ";")
indices$H
####################################################################


