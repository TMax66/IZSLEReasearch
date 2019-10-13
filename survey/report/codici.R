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
library(Rmpfr)
#library(RWeka)
#source("score.R")





#Sys.setenv(JAVA_HOME='C:/Program Files (x86)/Java/jre1.8.0_221')

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
  
  dplyr::summarise(Risposte=n(), "Età mediana"=mean(`Età`, na.rm=TRUE),"Mediana anni di servizio"=median(Servizio, na.rm=TRUE), 
                   "min-max"=(paste(  min(Servizio, na.rm=TRUE), "-",  max(Servizio, na.rm=TRUE)))) %>%
  mutate_if(is.numeric, funs(round(.,digits = 2))) %>% 
  arrange(desc(Risposte))


f %>%
  kable() %>%
  kable_styling( full_width = F,font_size = 18)


p<-ds %>% 
  group_by(Professione) %>% 
  
  dplyr::summarise(Risposte=n(), "Età mediana"=mean(`Età`, na.rm=TRUE),"Mediana anni di servizio"=median(Servizio, na.rm=TRUE), 
                   "min-max"=(paste(  min(Servizio, na.rm=TRUE), "-",  max(Servizio, na.rm=TRUE)))) %>%
   mutate_if(is.numeric, funs(round(.,digits = 2))) %>% 
  arrange(desc(Risposte))

p %>%
  kable() %>%
  kable_styling( full_width = F,font_size = 18)



r<-ds %>% 
  group_by(Ruolo) %>% 
  
  dplyr::summarise(Risposte=n(), "Età mediana"=mean(`Età`, na.rm=TRUE),"Mediana anni di servizio"=median(Servizio, na.rm=TRUE), 
                   "min-max"=(paste(  min(Servizio, na.rm=TRUE), "-",  max(Servizio, na.rm=TRUE)))) %>%
  mutate_if(is.numeric, funs(round(.,digits = 2))) %>% 
  arrange(desc(Risposte))

r %>%
  kable() %>%
  kable_styling(full_width = F,font_size = 18)

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
                    "esempio", "diverse", "coinvolto", "personale",  "rapporti","parte",
                    "figure", "livello", "tali", "necessario", "stesura", "gestione", "risultati", "dedicate", "dedicato",
                    "ecc", "fine", "soprattutto", "può", "avviene", "anni", "dopo", "ambito","corrente" )


corpus <- VCorpus(DataframeSource(risp))
corpus<-clean.corpus(corpus)
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "\\b(borsista|borse|borsa)\\b", replacement = "borsisti")
#corpus<-tm_map(corpus, content_transformer(gsub), pattern = "borsista", replacement = "borsisti")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "università", replacement = "universita'")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "percorsi", replacement = "percorso")
corpus<-tm_map(corpus, content_transformer(gsub), pattern = "dottorati", replacement = "dottorato")
tdm<-TermDocumentMatrix(corpus, control=list(weighting=weightTf))
tdm<-removeSparseTerms(tdm,  sparse=0.99)
tdm.risp.m<-as.matrix(tdm)

term.freq<-rowSums(tdm.risp.m)
freq.df<-data.frame(word=names(term.freq), frequency=term.freq)
freq.df<-freq.df[order(freq.df[,2], decreasing=T),]
freq.df$word<-factor(freq.df$word, levels=unique(as.character(freq.df$word)))

# ggplot(freq.df[1:30,], aes(x=word, y=frequency))+geom_bar(stat = "identity", fill='darkred')+
#   coord_flip()+theme_gdocs()+geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)


freq.df %>% 
  top_n(30,frequency ) %>% 
  ggplot(aes(x=reorder(word, frequency), y=frequency))+geom_bar(stat = "identity", fill='steelblue3')+
  coord_flip()+geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)+
  theme(axis.text=element_text(size=12))+labs(x="termini", y="frequenza")



freq.term<-findFreqTerms(tdm, lowfreq = 12)

plot(tdm, term=freq.term, corThreshold = 0.22)

########WORD CLOUD###
wordcloud2(freq.df, size=0.35)
#letterCloud(freq.df, word = "R")




m<-tdm.risp.m
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

dtm <- DocumentTermMatrix(corpus)
dtm_td <- tidy(dtm)
dtm_td %>%
  dplyr::count(term, sort = TRUE) %>%
  filter(n > 20) %>%
  mutate(word = reorder(term, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill="yellow",col="black") +
  xlab(NULL) +
  coord_flip() +

  theme(plot.title = element_text(size = 10, face = "bold")) + theme_bw()

pr_words <- dtm_td %>%
dplyr::count(term,sort=TRUE) %>%
filter(n >=5)


wordcloud2(pr_words, size=0.4)

####ASSOCIATION####



tdm<-removeSparseTerms(tdm, sparse=0.98)
associations<-findAssocs(tdm,'formazione', 0.40)
associations<-as.data.frame(associations)
associations$terms<-row.names(associations)
associations$terms<-factor(associations$terms, levels = associations$terms)

ggplot(associations, aes(y=terms))+
  geom_point(aes(x=formazione), data=associations, size=1)+
  theme_gdocs()+geom_text(aes(x=formazione, label=formazione),
                          colour="darkred", hjust=-.25, size=3)+
  theme(text=element_text(size=8),
        axis.title.y = element_blank())

tdm<-removeSparseTerms(tdm, sparse=0.98)
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
my_stop_words<-c(stopwords('italian'), "ricerca","izsler","ricerche",
                                   "altri","istituto","ricercatori", "progetto", "già",
                                   "nazionali", "progetti", "modo", "potrebbe", "dovrebbe", "attività", "essere", 
                                   "potrebbero", "possibilità", "solo", "alcuni", "lungo", "far", "fare", "sempre",
                                   "esempio", "diverse", "coinvolto", "personale",  "rapporti","parte",
                                   "figure", "livello", "tali", "necessario", "stesura", "gestione", "risultati", "dedicate", "dedicato",
                                   "ecc", "fine", "soprattutto", "può", "avviene", "anni", "dopo", "ambito","corrente" )
bgr<-risp %>%
  unnest_tokens(ngram, text, token = "ngrams", n =2)

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

# ######TOPICS MODEL#####
dtm<-DocumentTermMatrix(corpus, control=list(weighting=weightTf))
term_tfidf <- tapply(dtm$v/slam::row_sums(dtm)[dtm$i], dtm$j, mean) *
  log2(tm::nDocs(dtm)/slam::col_sums(dtm > 0))


dtm <- dtm[,term_tfidf >= 0.7015]
summary(slam::col_sums(dtm))


rowTotals<-apply(dtm,1,sum) #running this line takes time
empty.rows<-dtm[rowTotals==0,]$dimnames[1][[1]]
corpus<-corpus[-as.numeric(empty.rows)]
dtm <- DocumentTermMatrix(corpus)
# # determina il numero di topics###
burnin <- 1000
iter <- 1000
keep <- 50
# determining k (the number of topics)
seqk <- seq(2,100,1)
system.time(fitted_many <- lapply(seqk,function(k) topicmodels::LDA(dtm,k=k,
                                                                    method="Gibbs",control=list(burnin=burnin,iter=iter,keep=keep))))
# extract logliks from each topic
logLiks_many <- lapply(fitted_many, function(L) L@logLiks[-c(1:(burnin/keep))])
# compute harmonicMean
harmonicMean <- function(logLikelihoods, precision = 2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))
ldaplot <- ggplot(data.frame(seqk, hm_many), aes(x=seqk, y=hm_many)) + geom_path(lwd=1.5) +
  theme(text = element_text(family= NULL),
        axis.title.y=element_text(vjust=1, size=16),
        axis.title.x=element_text(vjust=-.5, size=16),
        axis.text=element_text(size=16),
        plot.title=element_text(size=20)) +
  xlab('Number of Topics') +
  ylab('Harmonic Mean') +
  ggtitle("Determining the number of topics")
k <- seqk[which.max(hm_many)]
#
k<-5

seedNum <- 50
system.time(ldaOut <- topicmodels::LDA(dtm,k = k,method="Gibbs",
                                       control=list(burnin=burnin,keep=keep,iter=2000,seed=seedNum)))

ldaOut.terms <- as.matrix(terms(ldaOut,5))
topics_beta <- tidy(ldaOut,matrix="beta")
top_terms <- topics_beta %>%
  group_by(topic) %>%
  top_n(5,beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms %>%
  mutate(term=reorder(term,beta)) %>%
  ggplot(aes(term,beta,fill=factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~ topic,scales="free") +
  coord_flip() +
  ggtitle("Probabilità dei primi 5 termini per topic")
