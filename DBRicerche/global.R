library(shiny)
library(shinydashboard)
library (DT)
library(tidyverse)
library(googlesheets)
library(readxl)
ds <- read_excel("prizsler.xlsx")





ds %>% 
group_by(Tipologia) %>% 
filter(Tipologia=="Corrente") %>% 
  summarise(n=n())
