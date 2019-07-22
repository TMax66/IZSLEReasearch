server <- function(input, output,session) {
  
 #########CODE PER CONDIZIONARE IL BOTTONE SALVA AL RIEMPIMENTO DI ALCUNI CAMPI DEL TAB INTRODUZIONE DATI########### 
  observe({
    # check if all mandatory fields have a value
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    # enable/disable the submit button
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
#################################CODE PER CONDIZIONARE IL BOTTONE SALVA AL RIEMPIMENTO DI TUTTA LA SCHEDA RISCHIO####
  
  # observe({
  #   # check if all mandatory fields have a value
  #   rmandatoryFilled <-
  #     vapply(riskMandatory,
  #            function(x) {
  #              !is.null(input[[x]]) && input[[x]] != ""
  #            },
  #            logical(1))
  #   rmandatoryFilled <- all(rmandatoryFilled)
  # 
  #   # enable/disable the submit button
  #   shinyjs::toggleState(id = "rsubmit", condition = rmandatoryFilled)
  # })
  
  
 ##################################################################### 
  
  
 ###############Codice per popolare i campi nel dataframe della sezione Introduzione dati############# 
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    #data <- c(data, timestamp = epochTime())
    
    data <- t(data)
    data
  })
  ###############Codice per popolare i campi nel dataframe della sezione valutazione del rischio############# 
  
  # riskData <- reactive({
  #   rdata <- sapply(riskfields, function(x) input[[x]])
  #   #data <- c(data, timestamp = epochTime())
  #   rdata <- t(rdata)
  #   rdata
  # })
  # 
  # 
  
  #######Codice per salvare nella directory mydata il file con i dati inseriti nella sez introduzione dati#####
  saveData <- function(data) {
    # Grab the Google Sheet
    sheet <- gs_title("paratbc")
    # Add the data as a new row
    gs_add_row(sheet, input = data)
  }
  
  
  
  
  
  
  
  
  # saveData <- function(data) {
  #   fileName <- sprintf("%s_%s.csv",
  #                       humanTime(),
  #                       digest::digest(data))
  #   
  #   write.csv(x = data, file = file.path(responsesDir, fileName),
  #             row.names = FALSE, quote = FALSE)
  # }
  # 
  #######Codice per salvare nella directory mydata il file con i dati inseriti nella sez valutazione del rischio#####
  # saverData <- function(rdata) {
  #   fileName <- sprintf("%s_%s.csv",
  #                       humanTime(),
  #                       digest::digest(rdata))
  #   
  #   write.csv(x = rdata, file = file.path(riskdir, fileName),
  #             row.names = FALSE, quote = FALSE)
  # }
  # 
  # 
  
##############CODICI PER LE AZIONI LEGATE AL BOTTONE DELLA PARTE DI INTRODUZIONE DATI- PRIMA FORM#############
  observeEvent(input$submit, {
    saveData(formData())
    shinyjs::reset("form")
    output$responsesTable <- DT::renderDataTable(
      loadData(),
      rownames = FALSE,filter='top',
      options = list(searching = TRUE, lengthChange = FALSE)) 
    #shinyjs::hide("form")
    shinyjs::show("datainputed_msg")
  })
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("datainputed_msg")
    # write.csv(x = loadData(), file = file.path(getwd(), "backup.csv"),
    #             row.names = FALSE)
  })   
  
  session$onSessionEnded(function() {
    stopApp()
    #q("no")
    
  })
  
  #############CODICI PER LE AZIONI LEGATE AL BOTTONE DELLA PARTE DI VALUTAZIONE RISCHIO- SECONDA FORM#############
  
  # observeEvent(input$rsubmit, {
  #   saverData(riskData())
  #   shinyjs::reset("form2")
  #   output$tabrisk <- renderTable(
  #     loadRisk() %>% as.tibble %>% 
  #     group_by(riskazienda,settore)%>%
  #       summarise(punteggio=sum(risk)) %>% 
  #     head
  #   ) 
  #   #shinyjs::hide("form2")
  #   shinyjs::show("rdatainputed_msg")
  # })
  # observeEvent(input$rsubmit_another, {
  #   shinyjs::show("form2")
  #   shinyjs::hide("rdatainputed_msg")
  #   write.csv(x = loadRisk(), file = file.path(getwd(), "riskRis.csv"),
  #             row.names = FALSE)
  # 
  # })
  # 
  # session$onSessionEnded(function() {
  #   stopApp()
  #   #q("no")
  # 
  # })


  #############################################
  # observeEvent(input$Aggiorna, {
  #   shinyjs::reset("form3")
  #   output$riS<- renderTable({  
  #     loadRisk() %>% 
  #       filter(riskazienda==input$razienda) %>% 
  #       group_by(settore)%>%
  #       summarise(punteggio=sum(risk)) %>%
  #       mutate(max=c(80,60,35,25,16,60)) %>%
  #       select(settore,max,punteggio) %>% 
  #       mutate("% di settore"=(punteggio/max)*100) %>% 
  #       mutate("% sul totale"= (punteggio/276)*100)
  #     
  #   })
  #   #shinyjs::hide("form3")
  #   shinyjs::show("form3")
  # })
  # 
  
 
  
  #############CODICI DI OUTPUT VARI########################################
  output$responsesTable <- DT::renderDataTable(
    loadData(),
    rownames = FALSE,filter='top',
    options = list(searching = TRUE, lengthChange = FALSE))
  
  
  #dx<-reactive({
  #loadData()})
  
  output$distPlot <- renderPlot({
    loadData()%>%as.tibble()%>%
      filter(codaz==input$azienda)%>%
           ggplot(aes(anno, prev))+geom_line()+geom_point()
  })
   
  

  output$pvt <- renderPivottabler({
    pt <- PivotTable$new()
    pt$addData(loadData())
    pt$addColumnDataGroups("anno",addTotal=FALSE)
    pt$addRowDataGroups("pt")
    pt$defineCalculation(calculationName="n", summariseExpression="n()")
    pt$renderPivot()
 })
  
  
  output$summaryplot<-renderPlot({
    
    loadData()%>%as.tibble()%>%
      group_by(anno, pt)%>%
      summarise(totaz=n())%>%
      ggplot(aes(pt,totaz))+geom_bar(stat = "identity", fill="blue")+facet_grid(~anno)+coord_flip()
    
    
    
  })
  
  ###INFOBOX-ALLEVAMENTI ADERENTI AL PIANO####
  aderenti<- reactive({
    nALL<-length(unique(loadData()$codaz))
  })
  
  output$allad <- renderInfoBox({
    infoBox(
      "Allevamenti Aderenti",
      aderenti(),
      icon = icon("signal"),
      color = "purple"
      )
  })
 ####DATA AGGIORNAMENTO##########
  output$dataagg<-renderText({
    
    paste("data ultimo aggiornamento",
    as.character(as.Date(max(loadData()$dt, na.rm=TRUE), 
            origin=as.Date("1970-01-01"))))
  })
    
    

####TABELLA PER AZIENDA#####
    
    output$taball<-DT::renderDataTable(
      loadData()%>% 
        select(codaz,dt,numconf,vetlp,prev,pt)%>%
        filter(codaz==input$azienda),
      rownames = FALSE,
      options = list(searching = FALSE, lengthChange = FALSE)
      )

  
###################VALUTAZIONE DEL RISCHIO######

output$tabrisk<-renderTable(
  
  loadRisk() %>% as.tibble %>% 
    group_by(riskazienda,settore)%>%
    summarise(punteggio=sum(risk)) %>% 
    head
)

################RISULTATI VALUTAZIONE RISCHIO###########

   output$riS<- renderTable({
    saverData(riskData())
    loadRisk() %>%
    filter(riskazienda==input$razienda) %>%
    group_by(settore)%>%
    summarise(punteggio=sum(risk)) %>%
    mutate(max=c(80,60,35,25,16,60)) %>%
    select(settore,max,punteggio) %>%
    mutate("% di settore"=(punteggio/max)*100) %>%
    mutate("% sul totale"= (punteggio/276)*100)

})


}