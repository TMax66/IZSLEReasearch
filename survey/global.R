library(shiny)
library(shinydashboard)
library (DT)
library(tidyverse)
library(googlesheets)



dati<-gs_title("Integrazione tecnico-scientifica alle attività amministrative a supporto della ricerca (Risposte)")
ds <-gs_read(dati, ws="Risposte del modulo 1" )






  
