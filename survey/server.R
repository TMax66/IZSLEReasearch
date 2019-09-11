
server <- function(input, output, session) {
  
  
  output$Quest <- renderValueBox({
    valueBox(
      dim(ds)[1], "# Questionari", icon = icon("keyboard"),
      color = "yellow"
    )
  })
  
  output$age <- renderValueBox({
    valueBox(
      round(mean(ds$Età, na.rm=T),1), "Età media", icon = icon("gauss"),
      color = "blue"
    )
  })

  output$serv <- renderValueBox({
    valueBox(
      round(mean(ds$`Da quanti anni lavori in IZSLER?`),1),
      "Media anni di servizio", icon = icon("gauss"),
      color = "red"
    )
  })
  
  output$est <- renderValueBox({
    valueBox(
      ds %>% 
        group_by(`Esperienze formativo-professionali all'estero`) %>% 
        filter(`Esperienze formativo-professionali all'estero`=="Si") %>% 
        summarise(n=n()) %>% 
        select(n),
      "Esperienze all'estero", icon = icon("globe"),
      color = "red"
    )
  })
  
  
  
  
  output$prof<-renderPlot(
    ds %>% 
      group_by(Professione) %>% 
      summarise(n=n()) %>% 
      arrange(n) %>% 
      mutate(Professione = factor(Professione, unique(Professione))) %>%
      ggplot(aes(x=Professione, y=n))+ geom_bar(stat="identity", fill="steelblue3")+labs(x="")+
      coord_flip()+theme(axis.text=element_text(size=12))
      
  )
  
  
  output$form<-renderPlot(
    ds %>% 
      group_by(Formazione) %>% 
      summarise(n=n()) %>% 
      arrange(n) %>% 
      mutate(Formazione = factor(Formazione, unique(Formazione))) %>%
      ggplot(aes(x=Formazione, y=n))+ geom_bar(stat="identity",fill="steelblue3" )+labs(x="")+
      coord_flip()+theme(axis.text=element_text(size=12))
    
  )
  
  output$ruolo<-renderPlot(
    ds %>% 
      group_by(`Ruolo esclusivo o prevalente in Istituto`) %>% 
      summarise(n=n()) %>% 
      arrange(n) %>% 
      mutate(`Ruolo esclusivo o prevalente in Istituto` = factor(`Ruolo esclusivo o prevalente in Istituto`, unique(`Ruolo esclusivo o prevalente in Istituto`))) %>%
      ggplot(aes(x=`Ruolo esclusivo o prevalente in Istituto`, y=n))+ 
      geom_bar(stat="identity",fill="steelblue3")+labs(x="")+
      coord_flip()+theme(axis.text=element_text(size=12))
    
  )
  
  
  
  
  output$risp<- DT::renderDataTable(
    ds %>% 
      select(-1),
    server= FALSE,class = 'cell-border stripe',
    rownames = FALSE, options = list(
      dom = 'Bfrtip',paging = TRUE,autoWidth = TRUE,
      pageLength = 10)
  )
  
  
  
  
  
}
