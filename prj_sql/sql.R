Query<-function(fixed="SELECT COLUMN_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME=", tabella="'nometab'"){
  paste(fixed, tabella)
}
## tabella principale progetti
qpa<-Query(tabella = "'ProgettiAccordi'")
# 
myfun <- function(con, qpa, tabella)
{
  
  column.types <- dbGetQuery(con, qpa)
  
  ct <- column.types %>%
    mutate(cml = case_when(
      is.na(CHARACTER_MAXIMUM_LENGTH) ~ 10,
      CHARACTER_MAXIMUM_LENGTH == -1 ~ 100000,
      TRUE ~ as.double(CHARACTER_MAXIMUM_LENGTH)
    )
    ) %>%
    arrange(cml) %>%
    pull(COLUMN_NAME)
  fields <- paste(ct, collapse=", ")
  query <- paste("SELECT", fields, paste("FROM", tabella))
  return(query)
}

# 
queryPA <- myfun(con=con, qpa=qpa, tabella = "ProgettiAccordi")

### unità operative
qpuo<-Query(tabella = "'UOCoinvolte'")
# 
myfun <- function(con, qpuo, tabella)
{
  
  column.types <- dbGetQuery(con, qpuo)
  
  ct <- column.types %>%
    mutate(cml = case_when(
      is.na(CHARACTER_MAXIMUM_LENGTH) ~ 10,
      CHARACTER_MAXIMUM_LENGTH == -1 ~ 100000,
      TRUE ~ as.double(CHARACTER_MAXIMUM_LENGTH)
    )
    ) %>%
    arrange(cml) %>%
    pull(COLUMN_NAME)
  fields <- paste(ct, collapse=", ")
  query <- paste("SELECT", fields, paste("FROM", tabella))
  return(query)
}
# 
queryUO <- myfun(con=con, qpuo=qpuo, tabella = "UOCoinvolte")
