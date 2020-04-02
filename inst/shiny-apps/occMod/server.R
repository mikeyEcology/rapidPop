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
  filename_input_file <- shiny::reactive({shinyFiles::parseFilePaths(volumes, input$input_file)})
  
  output$filepaths <- renderPrint({
    if (is.integer(input$input_file)) {
      cat("No files have been selected (shinyFileChoose)")
    } else {
      shinyFiles::parseFilePaths(volumes, input$input_file)
    }
  })
  
  # run function
  shiny::observeEvent(input$run_occMod, {
    #req(input$input_file)
    inFile <<- input$input_file
    # if(is.integer(inFile)){
    #  #if(is.integer(filename_input_file())){
    # #if(is.null(inFile)){
    #   return(NULL)
    #   #input_file_collapse <- NULL
    # } else{
    #   # on Windows deal with issue finding the right drive
    #   if(os == "Windows"){
    #     root <- inFile$root
    #     root1 <- gsub("\\(", "", root)
    #     root2 <- gsub("\\)", "", root1) # this gives [Drive]:
    #     input_file_collapse <- paste0(root2, paste0(inFile$files$`0`, collapse="/"))
    #   } else { # on not windows, we don't have to deal with this
    #     input_file_collapse <- paste0(inFile$files$`0`, collapse="/")
    #   }
    # }
    
    
    # output$file <- renderPrint({
    #   if (is.integer(input$input_file)) {
    #     cat("No files have been selected (shinyFileChoose)")
    #   } else {
    #     input_file_collapse <- parseFilePaths(volumes, input$input_file)
    #   }
    # })
    
    output$print <- renderPrint({
      # adding this to the same render print
      if (is.integer(input$input_file)) {
        cat("No files have been selected (shinyFileChoose)")
      } else {
        input_file_collapse <- shinyFiles::parseFilePaths(volumes, input$input_file)
      }
      
      # putting this inside renderPrint to get the output to print
      oc <<- occMod(
        input_file = as.character(input_file_collapse$datapath), #output$filepaths,  #parseFilePaths(volumes, input$input_file), #(filename_input_file()), #input_file_collapse,
        input_file_type = input$input_file_type,
        nm1 = input$nm1,
        nmF = input$nmF,
        parameter = input$parameter,
        nm_parameter = input$nm_parameter,
        shiny=TRUE
      )
      
      if(input$texty){
        if(input$parameter){
          # add if statement for if number of levels to adjust interpretation
          if(length(oc$levels) == 2){ # if there are two categories in which to compare occupancy
            
            # determine which level of the parameter was bigger
            greater_level <- ifelse(oc$parameter_effects@estimates[2] > 0, 2, 1) # if parameter is positive, the second level has a positive effect
            
            
            printout <- paste0("The overall estimated occupancy (psi) is ", oc$occ_estimate, ". \n",
                               "The 95% confidence interval for occupancy is ", oc$occ_95CI[1], " - ", oc$occ_95CI[2], ".\n\n",
                               "The estimated detection probability (p) is ", oc$det_estimate, " \n",
                               "with a confidence interval of ", oc$det_95CI[1], " - ", oc$det_95CI[2], ".\n\n",
                               "The effect of ", input$nm_parameter, " on occupancy is ", round(oc$parameter_effects@estimates[2],3),".\n",
                               "This means that '", oc$levels[greater_level], "' had a positive effect on occupancy;\n",
                               "the magnitude of this effect was ", round(abs(oc$parameter_effects@estimates[2]), 3), ".\n\n",
                               "Here is the estimated occupancy (and 95% confidence intervals) \n",
                               "in each level of ", input$nm_parameter, ".\n",
                               "Occupancy when ", input$nm_parameter, " = ", oc$levels[1], ": ", oc$occ_param_tbl[1,1], " (", oc$occ_param_tbl[2,1], ", ", oc$occ_param_tbl[3,1], ")\n",
                               "Occupancy when ", input$nm_parameter, " = ", oc$levels[2], ": ", oc$occ_param_tbl[1,2], " (", oc$occ_param_tbl[2,2], ", ", oc$occ_param_tbl[3,2], ")\n"
                               
            )
            
            
          } else {
           # if (length(oc$levels) > 2){
           # } else { # more than two categories (it is difficult to interpret), or numeric covariate
              printout <- paste0("The estimated overall occupancy (psi) is ", oc$occ_estimate, ". \n",
                                 "The 95% confidence interval for occupancy is ", oc$occ_95CI[1], " - ", oc$occ_95CI[2], ".\n\n",
                                 "The estimated detection probability (p) is ", oc$det_estimate, " \n",
                                 "with a confidence interval of ", oc$det_95CI[1], " - ", oc$det_95CI[2], ".\n\n",
                                 "The effect of ", input$nm_parameter, " on occupancy is ", round(oc$parameter_effects@estimates[2],3),".\n\n"
                                 #"Here it the full output table for the model on occupancy probability:\n\n",
                                 #oc$parameter_effects
              )
            }
          #}
          
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
