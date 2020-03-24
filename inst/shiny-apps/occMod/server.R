server <- function(input, output, session) {
  
  # determine if Windows
  if(Sys.info()["sysname"] == "Windows"){
    Windows <- TRUE
  } else {
    Windows <- FALSE
  }
  os = ifelse(Windows, "Windows", "Mac")
  
  # base directory for fileChoose
  volumes = shinyFiles::getVolumes()
  
  # input_file
  shinyFiles::shinyFileChoose(input, "input_file", roots=volumes, session=session, filetypes=c('csv', 'txt'))
  filename_input_file <- shiny::reactive({shinyFiles::parseFilePaths(volumes, input$input_file)[length(shinyFiles::parseFilePaths(volumes, input$input_file))]})
  
  # run function
  shiny::observeEvent(input$run_occMod, {
    #req(input$input_file)
    inFile <<- input$input_file
    if(is.integer(inFile)){
      #if(is.integer(filename_input_file())){
      #if(is.null(inFile)){
      return(NULL)
      #input_file_collapse <- NULL
    } else{
      # on Windows deal with  issuefinding the right drive
      if(os == "Windows"){
        root <- inFile$root
        root1 <- gsub("\\(", "", root)
        root2 <- gsub("\\)", "", root1) # this gives [Drive]:
        input_file_collapse <- paste0(root2, paste0(inFile$files$`0`, collapse="/"))
      } else { # on not windows, we don't have to deal with this
        input_file_collapse <- paste0(inFile$files$`0`, collapse="/")
      }
    }
    output$print <- renderPrint({
      # putting this inside renderPrint to get the output to print
      oc <<- occMod(
        input_file = input_file_collapse,
        input_file_type = input$input_file_type,
        nm1 = input$nm1,
        nmF = input$nmF,
        parameter = input$parameter,
        nm_parameter = input$nm_parameter,
        shiny=TRUE
      )
      if(input$texty){
        if(input$parameter){
          printout <- paste0("The estimated occupancy (psi) is ", oc$occ_estimate, ". \n",
                             "The 95% confidence interval for occupancy is ", oc$occ_95CI[1], " - ", oc$occ_95CI[2], ".\n\n",
                             "The estimated detection probability (p) is ", oc$det_estimate, " \n",
                             "with a confidence interval of ", oc$det_95CI[1], " - ", oc$det_95CI[2], ".\n\n",
                             "The effect of ", input$nm_parameter, " on occupancy is ", round(oc$parameter_effects@estimates[2],3),".\n\n"
                             #"Here it the full output table for the model on occupancy probability:\n\n",
                             #oc$parameter_effects
          )
          #printout <- oc
          #oc$occ_estimate
        } else {
          printout <- paste0("The estimated occupancy (psi) is ", oc$occ_estimate, ". \n",
                             "The 95% confidence interval for occupancy is ", oc$occ_95CI[1], " - ", oc$occ_95CI[2], ".\n\n",
                             "The estimated detection probability (p) is ", oc$det_estimate, " \n",
                             "with a confidence interval of ", oc$det_95CI[1], " - ", oc$det_95CI[2], ".\n")
        }
        cat(printout)
      } else{
        print(oc)
      }
      
      
      
    }) # end renderPrint
    
    
    
  }) # end observe event
  
}
