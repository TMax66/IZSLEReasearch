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
library(googlesheets)
library(plyr)
library(kableExtra)
library(knitr)


source("score.R")


dati<-gs_title("Integrazione tecnico-scientifica alle attività amministrative a supporto della ricerca (Risposte)")
ds <-gs_read(dati, ws="Risposte del modulo 1" )
ds<-ds[-1,]

names(ds)[c(5,6,7,8)]<-c("Estero", "Ruolo", "Risposta", "Servizio")

########DESCRITTIVA#######################################
ds<- ds %>% drop_na(Risposta)
ds<-ds %>% 
  mutate(id=seq(1:dim(ds)[1]))
ds$Professione<-revalue(ds$Professione, c("Collaboratore tecnico professionale addetto ai servizi di laboratorio"="Tecnico di laboratorio", "Tecnico sanitario di laboratorio biomedico"="Tecnico di laboratorio"))

ds$Ruolo<-revalue(
  ds$Ruolo, c(
    "Tecnico" ="Tecnico di laboratorio" , 
    "Collaboratore tecnico professionale addetto ai servizi di laboratorio"=
      "Tecnico di laboratorio" , "professional e gestionale" ="Gestionale e tecnico/scientifico" , "gestionale e professional"  ="Gestionale e tecnico/scientifico" , "gestionale con mantenimento di attività tecnico-scientifica"="Gestionale e tecnico/scientifico" ))

ds$wordCount <- sapply(gregexpr("\\W+", ds$Risposta), length) 

ds %>% 
  ggplot(aes(x=Formazione,   y=wordCount))+geom_boxplot()+coord_flip()

ds %>% 
  ggplot(aes(x=Ruolo,   y=wordCount))+geom_boxplot()+coord_flip()



f<-ds %>% 
  group_by(Formazione) %>% 
  
  dplyr::summarise(Risposte=n(), "Età media"=mean(`Età`, na.rm=TRUE)," Servizio"=median(Servizio, na.rm=TRUE)) %>% 
  mutate_if(is.numeric, funs(round(.,digits = 2))) %>% 
  arrange(desc(Risposte))

kable(f,format='markdown')


p<-ds %>% 
  group_by(Professione) %>% 
  
  dplyr::summarise(Risposte=n(), "Età media"=mean(`Età`, na.rm=TRUE)," Servizio"=median(Servizio, na.rm=TRUE), 
                   "minS"=min(Servizio, na.rm=TRUE), "maxS"=max(Servizio, na.rm=TRUE)) %>% 
  mutate_if(is.numeric, funs(round(.,digits = 2))) %>% 
  arrange(desc(Risposte))

kable(p,format='markdown')



r<-ds %>% 
  group_by(Ruolo) %>% 
  
  dplyr::summarise(Risposte=n(), "Età media"=mean(`Età`, na.rm=TRUE)," Servizio"=median(Servizio, na.rm=TRUE), 
                   "minS"=min(Servizio, na.rm=TRUE), "maxS"=max(Servizio, na.rm=TRUE)) %>% 
  mutate_if(is.numeric, funs(round(.,digits = 2))) %>% 
  arrange(desc(Risposte))

kable(r,format='markdown')

####TEXT MINING##################################################à
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

risp<-data.frame(doc_id=seq(1:nrow(ds)),text=ds$Risposta)
risp<-na.omit(risp)
custom.stopwords<-c(stopwords('italian'), "ricerca","izsler","ricerche",
                    "altri","istituto","ricercatori", "progetto", "già",
                    "nazionali", "progetti", "modo")


corpus <- VCorpus(DataframeSource(risp))
corpus<-clean.corpus(corpus)

tdm<-TermDocumentMatrix(corpus, control=list(weighting=weightTf))
tdm<-removeSparseTerms(tdm,  sparse=0.99)
tdm.risp.m<-as.matrix(tdm)

term.freq<-rowSums(tdm.risp.m)
freq.df<-data.frame(word=names(term.freq), frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]
freq.df$word<-factor(freq.df$word, levels=unique(as.character(freq.df$word)))

ggplot(freq.df[1:50,], aes(x=word, y=frequency))+geom_bar(stat = "identity", fill='darkred')+
  coord_flip()+theme_gdocs()+geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)



freq.term<-findFreqTerms(tdm, lowfreq = 20)

plot(tdm, term=freq.term, corThreshold = 0.25,weighting=T)














