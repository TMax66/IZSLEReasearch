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
library(wordcloud2)
library(wordcloud)
library(textmineR)
library(RWeka)
#source("score.R")


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
  ggplot(aes(x=Formazione,   y=wordCount))+geom_boxplot()+coord_flip()+
  geom_jitter(shape=16, position=position_jitter(0.2))+ facet_wrap(Professione~Ruolo)

# ds %>% 
#   ggplot(aes(x=Ruolo,   y=wordCount))+geom_boxplot()+coord_flip()



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
                    "nazionali", "progetti", "modo", "potrebbe", "dovrebbe", "attività", "essere", 
                    "potrebbero", "possibilità", "solo", "alcuni", "lungo", "far", "fare", "sempre",
                    "esempio", "diverse", "coinvolto", "personale", "avere", "supporto", "rapporti")


corpus <- VCorpus(DataframeSource(risp))
corpus<-clean.corpus(corpus)
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "borsista", replacement = "borsisti")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "borsa", replacement = "borse")
tdm<-TermDocumentMatrix(corpus, control=list(weighting=weightTf))
tdm<-removeSparseTerms(tdm,  sparse=0.99)
tdm.risp.m<-as.matrix(tdm)

term.freq<-rowSums(tdm.risp.m)
freq.df<-data.frame(word=names(term.freq), frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]
freq.df$word<-factor(freq.df$word, levels=unique(as.character(freq.df$word)))

ggplot(freq.df[1:50,], aes(x=word, y=frequency))+geom_bar(stat = "identity", fill='darkred')+
  coord_flip()+theme_gdocs()+geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)



freq.term<-findFreqTerms(tdm, lowfreq = 15)

plot(tdm, term=freq.term, corThreshold = 0.30,weighting=T)

########WORD CLOUD###
wordcloud2(freq.df)
#letterCloud(freq.df, word = "R")

####ASSOCIATION####

associations<-findAssocs(tdm,'formazione', 0.4)
associations<-as.data.frame(associations)
associations$terms<-row.names(associations)
associations$terms<-factor(associations$terms, levels = associations$terms)

ggplot(associations, aes(y=terms))+
  geom_point(aes(x=formazione), data=associations, size=1)+
  theme_gdocs()+geom_text(aes(x=formazione, label=formazione),
                          colour="darkred", hjust=-.25, size=3)+
  theme(text=element_text(size=8),
        axis.title.y = element_blank())

######bi-gram#####

risp<-data.frame(doc_id=seq(1:nrow(ds)),text=ds$Risposta)
risp<-na.omit(risp)
my_stop_words<-custom.stopwords<-c(stopwords('italian'), "ricerca","izsler","ricerche",
                                   "altri","istituto","ricercatori", "progetto", "già",
                                   "nazionali", "progetti", "modo", "potrebbe", "dovrebbe", "attività", "essere", 
                                   "potrebbero", "possibilità", "solo", "alcuni", "lungo", "far", "fare", "sempre",
                                   "esempio", "diverse", "coinvolto", "personale", "avere", "supporto",
                                   "rapporti")

bgr<-risp %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 2)

bgr %>%
  dplyr::count(ngram, sort = TRUE)

bsep<-bgr %>%
  separate(ngram, c("word1", "word2"), sep = " ")

bsepf <- bsep %>%
  filter(!word1 %in% my_stop_words) %>%
  filter(!word2 %in% my_stop_words) %>%
  dplyr::count(word1, word2, sort = TRUE)
bsepf<-na.omit(bsepf)


bigram_graph <- bsepf %>%
  filter(n>1) %>%
  graph_from_data_frame()



library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


###alternative bi-gram###

tokenBigram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
bigrams <- TermDocumentMatrix(corpus, control = list(tokenize = tokenBigram)) 

freqBigrams <- as.data.frame(rowSums(as.matrix(bigrams)))
freqBigrams$words <- row.names(freqBigrams)
row.names(freqBigrams) <- NULL
colnames(freqBigrams)[1] <- 'totalFreq'
freqBigrams <- freqBigrams %>% arrange(desc(totalFreq))
ggplot(data=freqBigrams[1:20,], aes(x=reorder(factor(words),-totalFreq), y=totalFreq)) + 
  geom_bar(stat='identity', fill="steelblue3", width=0.7) + 
  xlab('BiGrammi') + 
  ylab('Frequenza aasoluta') + 
  ggtitle('BiGrammi piu\' frequenti') +
  theme(axis.text.x=element_text(angle=60, hjust=1))

fb<-freqBigrams %>% 
separate(words, c("word1", "word2"), sep = " ")

fb<-na.omit(fb)

fb<-as_tibble(fb)

bigram_graph <- fb %>%
  filter(totalFreq>1) %>%
  graph_from_data_frame()

library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "gem") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))


ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = "totalFreq"), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

