library(shiny)
library(shinydashboard)
library (DT)
library(tidyverse)
library(googlesheets)
library(topicmodels)
library(tidytext)
library(tm)
library(ggthemes)



dati<-gs_title("Integrazione tecnico-scientifica alle attività amministrative a supporto della ricerca (Risposte)")
ds <-gs_read(dati, ws="Risposte del modulo 1" )

  

####TEXT MINING#####
ds<-na.omit(ds)
names(ds)[7]<-"risposta"


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

risp<-data.frame(doc_id=seq(1:nrow(ds)),text=ds$risposta)
custom.stopwords<-c(stopwords('italian'), "ricerca", "progetti","attività","modo", "avere","dopo",
                    "argomenti","punto","altri", "altre", "finanziamenti", "rsr", "soprattutto", "parte",
                    "izsler")
corpus <- VCorpus(DataframeSource(risp))
corpus<-clean.corpus(corpus)
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "percorsi", replacement = "percorso")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "partecipare", replacement = "partecipazione")
tdm<-TermDocumentMatrix(corpus, control=list(weighting=weightTf))
tdm<-removeSparseTerms(tdm,  sparse=0.99)
tdm.risp.m<-as.matrix(tdm)

term.freq<-rowSums(tdm.risp.m)
freq.df<-data.frame(word=names(term.freq), frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]
freq.df$word<-factor(freq.df$word, levels=unique(as.character(freq.df$word)))

ggplot(freq.df[1:50,], aes(x=word, y=frequency))+geom_bar(stat = "identity", fill='darkred')+
  coord_flip()+theme_gdocs()+geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)


