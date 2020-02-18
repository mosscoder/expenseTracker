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
    
    acctSums <- allRecords %>% 
      mutate(Project = Project %>% as.character()) %>%
      mutate(Expense.Account = Expense.Account %>% as.character()) %>%
      group_by(Expense.Account, Project) %>% 
      summarize(Totals = sum(Total)) %>%
      ungroup() %>%
      mutate(Project = ifelse(is.na(Project), 'Project not reported', Project %>% as.character())) %>%
      mutate(Expense.Account = ifelse(is.na(Expense.Account), 'Account not reported', Expense.Account%>% as.character()))
    
    ggplot(acctSums, aes(x = Expense.Account, y = Totals, fill = Expense.Account)) +
      geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
      coord_flip() +
      facet_wrap(~ Project, ncol = 1, scales = 'free_y') +
      theme_set(theme_gray(base_size = 28)) 
    
  })
}