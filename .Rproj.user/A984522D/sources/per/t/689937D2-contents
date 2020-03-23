occMod <- function(
  input_file = NULL,
  input_file_type = ".txt", # txt of csv
  nm1 = "E01", # name of the first column containing occupancy data
  nmF = "E12", # name of the final column containing occupancy data
  parameter = FALSE, # do you want to model the effect of a parameter on occupancy
  nm_parameter, # if you are using an occupancy parameter, what is the name of its column
  n_round = 3,
  shiny=FALSE
){
  
  # read in input file
  if(input_file_type == ".txt"){
    input_tbl <- read.table(input_file, header = TRUE)
  } else {
    if(input_file_type == ".csv") {
      input_tbl <- read.csv(input_file)
    } else {
      stop("input_file_type must be either `.txt` or `.csv`")
    }
  } 
  
  if(parameter){
    # check to make sure that parameter has multiple values
    # otherwise R will shut down
    param_values <- eval(parse(text=paste0("input_tbl$", nm_parameter)))
    num_param_values <- length(unique(param_values))
    if(num_param_values < 2){
      stop(paste0("Your parameter (", nm_parameter, ") must have at least 2 values to model the effect of this parameter on occupancy. Either select a different parameter or run the model without a parameter (covariate)."))
    }
    
    # subset by relevant columns
    y1 <- input_tbl[, which(colnames(input_tbl)==nm1):which(colnames(input_tbl)==nmF)]
    # convert the observations to 1s and 0s
    y1 <- ifelse(y1>0, 1, 0)
    
    # convert parameter to numeric
    input_tbl[,nm_parameter] <- as.numeric(input_tbl[,nm_parameter])
    
    # set up unmarked data frame
    siteCovs <- data.frame(param= input_tbl[, c(nm_parameter)])
    umf <- unmarked::unmarkedFrameOccu(y = as.matrix(y1), siteCovs = siteCovs) # 
    
    # scale covariate
    unmarked::siteCovs(umf) <- scale(unmarked::siteCovs(umf))
    
    # run occupancy model
    oc1 <- unmarked::occu(~1 ~param, data = umf)
    
    # extract occupancy data
    oc1_st <- unmarked::backTransform(unmarked::linearComb(oc1, coefficients = c(1,0), type="state"))
    oc1_stCI <- unmarked::confint(unmarked::backTransform(unmarked::linearComb(oc1, coefficients = c(1,0), type="state")))
    
    # extract detection data
    oc1_det <- unmarked::backTransform(oc1, "det") # det is p: detection probability
    oc1_detCI <-unmarked::confint(unmarked::backTransform(oc1, type="det"))
    
    # extract parameter information
    param_estimate <- oc1@estimates@estimates$state@estimates[2]
    # rename the parameter in this table as the name given to function
    names(oc1@estimates@estimates$state@estimates)[2] <- nm_parameter
    
    # make a list of relevant information
    oc_list <- list(
      occ_estimate = round(oc1_st@estimate, n_round),
      occ_95CI = round(c(oc1_stCI[1], oc1_stCI[2]), n_round),
      det_estimate = round(oc1_det@estimate, n_round),
      det_95CI = round(c(oc1_detCI[1], oc1_detCI[2]), n_round),
      parameter_effects = oc1@estimates@estimates$state
    )
    
  } else { # no occupancy parameters
    #cat("running occupancy model without parameter\n")
    
    # subset by relevant columns
    y1 <- input_tbl[, which(colnames(input_tbl)==nm1):which(colnames(input_tbl)==nmF)]
  
    # convert these observations to 1s and 0s
    y1 <- ifelse(y1>0, 1, 0)
    
    # set up unmarked data frame
    umf <- unmarked::unmarkedFrameOccu(y = as.matrix(y1))
    
    # run occupancy model
    oc1 <- unmarked::occu(~1 ~1, data = umf)
    
    # extract occupancy data
    oc1_st <- unmarked::backTransform(oc1, "state") # state is Z: occupancy state
    oc1_stCI <- unmarked::confint(unmarked::backTransform(oc1, type="state"))
    
    # extract detection data
    oc1_det <- unmarked::backTransform(oc1, "det") # det is p: detection probability
    oc1_detCI <-unmarked::confint(unmarked::backTransform(oc1, type="det"))
    
    # make a list of relevant information
    oc_list <- list(
      occ_estimate = round(oc1_st@estimate, n_round),
      occ_95CI = round(c(oc1_stCI[1], oc1_stCI[2]), n_round),
      det_estimate = round(oc1_det@estimate, n_round),
      det_95CI = round(c(oc1_detCI[1], oc1_detCI[2]), n_round)
    )
    
  }
 
  # next steps: Add options to run multiple occupancy models for different time periods
  # or have season or before/after as parameter in the model
  
  
  
  # return this list
  if(shiny){
    return(oc_list)
    print(oc_list)
  } else {
    return(oc_list)
    #oc_list
  }

}

oc <- occMod(input_file = "/Users/mikeytabak/Desktop/qsc/projects/DIFS_shiny/example/Pigs_Occupancy_Input_CA_NE_Aug.txt",
       input_file_type = ".txt"
       #, parameter = FALSE,
       , parameter = TRUE,
       nm_parameter = "param")
