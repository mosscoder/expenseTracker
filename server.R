server <- function(input, output) {
  output$contents <- renderPlot({
    
    inFile <- input$files
    
    if (is.null(inFile))
      return(NULL)
    
    fs <- inFile$datapath
    
    xRead <- function(x) {
      raw <- read.xlsx(x, 1)
      col1 <- raw[, 1]
      row1 <- which(col1 == 'Date')
      endSheet <- which(startsWith(col1 %>% as.character(), '*'))
      clean <-
        read.xlsx(
          x,
          1,
          startRow = row1 + 1,
          endRow = endSheet,
          header = T
        ) %>%
        mutate(Date = as_date(Date)) %>%
        filter(!is.na(Date))
      
    }
    
    allRecords <- do.call(rbind, lapply(FUN = xRead, X = fs))
    
    acctSums <- allRecords %>% group_by(Expense.Account, Project) %>% summarize(Totals = sum(Total))
    
    ggplot(acctSums, aes(x = Expense.Account, y = Totals, fill = Expense.Account)) +
      geom_bar(stat = 'identity') +
      coord_flip() +
      facet_wrap(~ Project, ncol = 1) +
      theme_set(theme_gray(base_size = 28)) 
    
  })
}