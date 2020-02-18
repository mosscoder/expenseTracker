ui <- fluidPage(
  fileInput("files", "Choose Expense Reports",
            accept = c(
              "xlsx"),
            multiple = T
  ),
  plotOutput("contents")
)