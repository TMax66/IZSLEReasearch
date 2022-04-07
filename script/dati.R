library(tidyverse)
library(bibliometrix)
library(here)

izsler <- here("data", c("izsler.bib"))
izsler <- convert2df(izsler, dbsource = "wos", format = "bibtex")


izsve <- here("data", c("izsve.bib"))
izsve <- convert2df(izsve, dbsource = "wos", format = "bibtex")


izsam <- here("data", c("izste.bib"))
izsam <- convert2df(izsam, dbsource = "wos", format = "bibtex")


izspiem <- here("data", c("izspiem.bib"))
izspiem <- convert2df(izspiem, dbsource = "wos", format = "bibtex")

izsum <- here("data", c("izsum.bib"))
izsum <- convert2df(izsum, dbsource = "wos", format = "bibtex")

izsic <- here("data", c("izssic.bib"))
izsic <- convert2df(izsic, dbsource = "wos", format = "bibtex")

izslt <- here("data", c("izslt.bib"))
izslt <- convert2df(izslt, dbsource = "wos", format = "bibtex")

izsmezz <- here("data", c("izsmezzg.bib"))
izsmezz <- convert2df(izsmezz, dbsource = "wos", format = "bibtex")

izspuglia <- here("data", c("izspuglia.bib"))
izspuglia <- convert2df(izspuglia, dbsource = "wos", format = "bibtex")

izssard <- here("data", c("izssard.bib"))
izssard <- convert2df(izssard, dbsource = "wos", format = "bibtex")







A <- izsler %>%
  mutate(PY = ifelse(is.na(PY), parse_number(earlyaccessdate), PY)) %>% 
  group_by(PY) %>%
  summarise(n=n())
A$Istituto<-rep("izsler", dim(A)[1])

B <- izsve %>%
  mutate(PY = ifelse(is.na(PY), parse_number(earlyaccessdate), PY)) %>% 
  group_by(PY) %>%
  summarise(n=n())
B$Istituto<-rep("izsve", dim(B)[1])

C <- izsam %>%
  mutate(PY = ifelse(is.na(PY), parse_number(earlyaccessdate), PY)) %>% 
  group_by(PY) %>%
  summarise(n=n())
C$Istituto<-rep("izsam", dim(C)[1])


D <- izspiem %>%
  mutate(PY = ifelse(is.na(PY), parse_number(earlyaccessdate), PY)) %>% 
  group_by(PY) %>%
  summarise(n=n())
D$Istituto<-rep("izspiem", dim(D)[1])


E <- izsic %>%
  mutate(PY = ifelse(is.na(PY), parse_number(earlyaccessdate), PY)) %>% 
  group_by(PY) %>%
  summarise(n = n())
E$Istituto<-rep("izsicilia", dim(E)[1])


F <- izslt %>%
  mutate(PY = ifelse(is.na(PY), parse_number(earlyaccessdate), PY)) %>% 
  group_by(PY) %>%
  summarise(n = n())
F$Istituto<-rep("izslt", dim(F)[1])


G <- izsmezz %>%
  mutate(PY = ifelse(is.na(PY), parse_number(earlyaccessdate), PY)) %>% 
  group_by(PY) %>%
  summarise(n = n())
G$Istituto<-rep("izsmezz", dim(G)[1])

H <- izssard %>%
  mutate(PY = ifelse(is.na(PY), parse_number(earlyaccessdate), PY)) %>% 
  group_by(PY) %>%
  summarise(n = n())
H$Istituto<-rep("izssard", dim(H)[1])

I <- izspuglia %>%
  mutate(PY = ifelse(is.na(PY), parse_number(earlyaccessdate), PY)) %>% 
  group_by(PY) %>%
  summarise(n = n())
I$Istituto<-rep("izspuglia", dim(I)[1])

L <- izsum%>%
  mutate(PY = ifelse(is.na(PY), parse_number(earlyaccessdate), PY)) %>% 
  group_by(PY) %>%
  summarise(n = n())
L$Istituto<-rep("izsumbmarche", dim(L)[1])

prod<-rbind(A,B,C,D, E, F, G, H, I, L)
