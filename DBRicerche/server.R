
server <- function(input, output, session) {
  
  
  output$Quest <- renderValueBox({
    valueBox(
      dim(ds)[1], "# Progetti", icon = icon("keyboard"),
      color = "red"
    )
  })
  
  output$corr <- renderValueBox({
    valueBox(
      ds %>% 
        group_by(Tipologia) %>% 
        filter(Tipologia=="Corrente") %>% 
        summarise(n=n()) %>% 
        select(n), "# Ricerca Corrente", icon = icon("gauss"),
      color = "blue"
    )
  })

  output$fin <- renderValueBox({
    valueBox(
      ds %>%
        group_by(Tipologia) %>%
        filter(Tipologia=="Finalizzato") %>%
        summarise(n=n()) %>%
        select(n), "# Ricerca Finalizzata", icon = icon("gauss"),
      color = "blue"
    )
  })

  output$eu <- renderValueBox({
    valueBox(
      ds %>%
        group_by(Tipologia) %>%
        filter(Tipologia=="Europeo") %>%
        summarise(n=n()) %>%
        select(n), "# Progetti di Ricerca Europei", icon = icon("gauss"),
      color = "blue"
    )
  })
  
  output$reg <- renderValueBox({
    valueBox(
      ds %>%
        group_by(Tipologia) %>%
        filter(Tipologia=="Regionali") %>%
        summarise(n=n()) %>%
        select(n), "# Progetti di Ricerca Regionali ", icon = icon("gauss"),
      color = "orange"
    )
  })
  
  output$af <- renderValueBox({
    valueBox(
      ds %>%
        group_by(Tipologia) %>%
        filter(Tipologia=="Autofinanziato") %>%
        summarise(n=n()) %>%
        select(n), "# Progetti di Ricerca Autofinanziati ", icon = icon("gauss"),
      color = "purple"
    )
  })
  
  output$ccm <- renderValueBox({
    valueBox(
      ds %>%
        group_by(Tipologia) %>%
        filter(Tipologia=="CCM") %>%
        summarise(n=n()) %>%
        select(n), "# Progetti di Ricerca CCM ", icon = icon("gauss"),
      color = "aqua"
    )
  })
  
  output$at <- renderValueBox({
    valueBox(
      ds %>%
        group_by(Tipologia) %>%
        filter(Tipologia=="Altro tipo") %>%
        summarise(n=n()) %>%
        select(n), "# Altre tipologie ", icon = icon("gauss"),
      color = "navy"
    )
  })
  
  
  
  output$trend<-renderPlot(
    
    if(input$tipo=="Tutte")
    {    
      ds %>% 
        group_by(Anno) %>% 
        summarise(n=n()) %>% 
        ggplot(aes(x=Anno, y=n))+ geom_point(stat="identity", fill="steelblue3")+labs(x="")+
        geom_line()+
        theme(axis.text=element_text(size=12))
    }
    else
    
    {   
    
    ds %>% 
      group_by(Anno, Tipologia) %>% 
      summarise(n=n()) %>% 
      filter(Tipologia==input$tipo) %>% 
      ggplot(aes(x=Anno, y=n))+ geom_point(stat="identity", fill="steelblue3")+labs(x="")+
      geom_line()+
      theme(axis.text=element_text(size=12))
    }
  )
  
  
  output$db<- DT::renderDataTable(
    ds ,
    server= FALSE, filter= "top", class = 'cell-border stripe',
    rownames = FALSE, options = list(searching = FALSE,
      dom = 'Bfrtip',paging = TRUE,autoWidth = TRUE,
      pageLength = 10)
  )
  
  
}
