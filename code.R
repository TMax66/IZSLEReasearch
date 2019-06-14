













########################################################
############PUBMED MINER################################
########################################################

library(pubmed.mineR)

abs<-xmlreadabs("pubmed_result.xml")
abs<-readabs("pubmed_result.txt")
abs<-cleanabs(abs)
wa<-word_atomizations(abs) 


library(adjutant)