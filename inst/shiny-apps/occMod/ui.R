ui <- shiny::fluidPage(
  
  # App title ----
  shiny::titlePanel("Run occupancy models on your rapid population assessment data"),
  
  # Sidebar layout with input and output definitions ----
  shiny::sidebarLayout(
    
    shiny::sidebarPanel(
      shinyFiles::shinyFilesButton('input_file', "Input file", title="Select the file containing your animal observation data. You can examine the example_input_file provided with this package for guidance.", multiple=FALSE),
      shiny::selectInput('input_file_type', 'What type of file is your Input file', c(
        "txt" = ".txt",
        "csv" = ".csv"
      )),
      shiny::textInput("nm1", "Column name of First column containing occupancy data", formals(occMod)[["nm1"]]),
      shiny::textInput("nmF", "Column name of Final column containing occupancy data", formals(occMod)[["nmF"]]),
      shiny::selectInput('parameter', 'Do you want to model the effect of a parameter (covariate) on occupancy', c(
        "No" = FALSE,
        "Yes" = TRUE
      )),
      shiny::textInput("nm_parameter", "If yes, enter the column name of the parameter", "removal"),#formals(occMod)[["nm_parameter"]]),
      shiny::selectInput('texty', 'Do you want a pretty printout? Answering No will be neater, but answering Yes will provide more information if you are including a parameter.', c(
        "Yes" = TRUE,
        "No" = FALSE
      )),
      shiny::actionButton("run_occMod", "Run occupancy model")
    ),
    
    shiny::mainPanel(
      shiny::helpText("Below is a summary of the results of your model:"),
      shiny::br(),
      shiny::verbatimTextOutput("print")
      )
  )
)

