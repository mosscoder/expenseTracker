ui <- fluidPage(
  fileInput("files", "Select one or more expense reports for account totals by project:",
            accept = c(
              "xlsx"),
            multiple = T
  ),
  uiOutput("plot.ui")
  #plotOutput("contents", height = '900px')
)