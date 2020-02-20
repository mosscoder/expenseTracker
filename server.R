server <- function(input, output) {
  dat <- reactive({
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
        read.xlsx(x,
                  1,
                  startRow = row1 + 1,
                  endRow = endSheet,
                  header = T) %>%
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
      mutate(Project = ifelse(
        is.na(Project),
        'Project not reported',
        Project %>% as.character()
      )) %>%
      mutate(
        Expense.Account = ifelse(
          is.na(Expense.Account),
          'Account not reported',
          Expense.Account %>% as.character()
        )
      )
    
    projectTotals <- acctSums %>%
      group_by(Project) %>%
      summarize(Totals = sum(Totals)) %>%
      mutate(Expense.Account = 'Project Total')
    
    toGG <- rbind(acctSums, projectTotals)
    
    levs <-
      toGG %>% filter(Expense.Account != 'Project Total') %>% select(Expense.Account) %>% unique() %>% unlist()
    toGG$Expense.Account <-
      factor(toGG$Expense.Account , levels = c(levs, 'Project Total'))
    toGG
  })
  
  catCount <- reactive({
    req(input$files)
    plotLength <-
      dat() %>% select(Project, Expense.Account) %>% unique() %>% nrow() %>% as.numeric()
  })
  
  plotWidth <- reactive(75 * catCount())
  
  output$contents <- renderPlot(height = 800, width = plotWidth,{
    inFile <- input$files
    if (is.null(inFile))
      return(NULL)
    
    dat <- dat()
    
    #### Code for palette ####
    palette.full <- c("#8B1117",
                      "#29D32A",
                      "#F743FB",
                      "#03ADC3",
                      "#F0A733",
                      "#6F1D68",
                      "#7B8BFA",
                      "#188D57",
                      "#1F3D46",
                      "#FA5B93",
                      "#C498C4",
                      "#F8651F",
                      "#4E5705",
                      "#B7755E",
                      "#283F85",
                      "#E028B0",
                      "#92C015",
                      "#0196DB",
                      "#A1AB60",
                      "#DC9AF8",
                      "#66BE9F",
                      "#317313",
                      "#B95D18",
                      "#A27811",
                      "#61410C",
                      "#107585",
                      "#9B005F",
                      "#E92725",
                      "#B62DC2",
                      "#6ECC4B",
                      "#F3606F",
                      "#CCB437",
                      "#622515",
                      "#7A2C8F",
                      "#98A008",
                      "#4CCBC4",
                      "#FF70B8",
                      "#7C9FF7",
                      "#393364",
                      "#C64704",
                      "#4F673D",
                      "#A96AD3",
                      "#CB533C",
                      "#8ED14A",
                      "#9FAEC1",
                      "#C9A551",
                      "#85D1A2",
                      "#C6548A",
                      "#524874",
                      "#653A35")
    
    #####
    
    pal <- c(palette.full[1:length(unique(dat$Expense.Account)) -1], '#ED4337')
    
    ggplot(dat,
           aes(x = Expense.Account, y = Totals, fill = Expense.Account)) +
      geom_col(color = 'black') +
      scale_fill_manual(values = pal) +
      geom_label(aes(
        label = paste0('$', Totals %>% round()),
        y = Totals + (max(Totals) * .05)),
      fill = 'white',
      fontface = 'bold',
      size = 4) +
      xlab('') +
      ylab('Amount') +
      facet_grid(. ~ Project , scales = "free", space = 'free') +
      theme_set(theme_gray(base_size = 16)) +
      guides(fill = guide_legend(nrow = 4, title = 'Account')) +
      theme(axis.text.x = element_text(angle = 50, hjust = 1),
            legend.position = "top",
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_rect('grey97'), axis.line = element_line(colour = "black"))
    
  })
  

}