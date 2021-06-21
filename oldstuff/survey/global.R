library(shiny)
library(shinydashboard)
library (DT)
library(tidyverse)
library(googlesheets)
library(topicmodels)
library(tidytext)
library(tm)
library(ggthemes)
library(wordcloud2)
library(wordcloud)



dati<-gs_title("Integrazione tecnico-scientifica alle attività amministrative a supporto della ricerca (Risposte)")
ds <-gs_read(dati, ws="Risposte del modulo 1" )


dati$ABprototipo<-
  revalue(dati$antibiotico, 
          "Lincomicina"="Clindamicina", "Pirlimicina"="Clindamicina", 
          "Enrofloxacin"="Danofloxacin", "Marbofloxacin"="Danofloxacin",
          "Flumequina"="Acido Nalidixico", "Amoxicillina"="Ampicillina", 
          "Apramicina"="Gentamicina","Cefalexina"="Cefalotina", "Cefoperazone"="Ceftiofur",
          "Cefquinome"="Ceftiofur", "Cloxacillina"="Oxacillina",  "Penetamato Iodidrato" = "Penicillina G",
          "Penicillina"="Penicillina G",  )

####TEXT MINING#####
names(ds)[7]<-"risposta"
ds<- ds %>% drop_na(risposta)



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
                    "izsler", "solo", "essere", "figure", "livello", "dovrebbe", 
"sempre", "potrebbe", "possibilità")
corpus <- VCorpus(DataframeSource(risp))
corpus<-clean.corpus(corpus)
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "percorsi", replacement = "percorso")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "partecipare", replacement = "partecipazione")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "borsista", replacement = "borsisti")
tdm<-TermDocumentMatrix(corpus, control=list(weighting=weightTf))
tdm<-removeSparseTerms(tdm,  sparse=0.99)
tdm.risp.m<-as.matrix(tdm)

term.freq<-rowSums(tdm.risp.m)
freq.df<-data.frame(word=names(term.freq), frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]
freq.df$word<-factor(freq.df$word, levels=unique(as.character(freq.df$word)))

freq.df %>% 
  top_n(30,frequency ) %>% 
  ggplot(aes(x=reorder(word, frequency), y=frequency))+geom_bar(stat = "identity", fill='steelblue3')+
  coord_flip()+geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)+
  theme(axis.text=element_text(size=12))+labs(x="termini", y="frequenza")

freq.term<-findFreqTerms(tdm, lowfreq = 15)

#plot(tdm, term=freq.term, corThreshold = 0.2,weighting=T)


# m<-tdm.risp.m
# v <- sort(rowSums(m),decreasing=TRUE)
# d <- data.frame(word = names(v),freq=v)
# 
# wordcloud2(d, size=0.5)
# 
# comparison.cloud(d, title.size=1,max.words=40,colors = brewer.pal(5,"Set1"))