ui <- fluidPage(
  fileInput("files", "Select one or more expense reports for account totals by project:",
            accept = c(
              "xlsx"),
            multiple = T
  ),
  plotOutput("contents", height = '1200px')
)