#' Run occupancy models in \code{rapidPop}.
#' 
#' @param input_file The path to and name of your input file
#' @param input_file_type The file type (or extenstion). Options are c(".txt", ".csv")
#' @param nm1 The name of the first column containing occupancy observation data. 
#'  Your \code{input_file} must have some columns contatining information about 
#'  whether there were animals in your observations. These columns can be either 
#'  0s and 1s or the number of animals in the image. \code{nm1} is the name (or header)
#'  of the first column containing these data.
#' @param nmF The name of the final column containing occupancy data.
#' @param parameter logical. Do you want to model the effect of a parameter on occupancy?
#'  For example, do you have a column indicating if these observations came from
#'  before or after control operations? If so, you could model the effect of this
#'  parameter on occupancy.
#' @param nm_parameter The name of the column for your \code{parameter}. If you
#'  are modeling a paramter (\code{parameter = TRUE}), this is the column name
#'  for the paramter you want to use. 
#' @param digits The number of digits to round to. This number is passed to the
#'  function \code{round}
#' 
#' @export
occMod <- function(
  input_file = NULL,
  input_file_type = ".txt", # txt of csv
  nm1 = "day01", # name of the first column containing occupancy data
  nmF = "day12", # name of the final column containing occupancy data
  parameter = FALSE, # do you want to model the effect of a parameter on occupancy
  nm_parameter, # if you are using an occupancy parameter, what is the name of its column
  digits = 3,
  shiny=FALSE
){
  
  # read in input file #*** add utils to all
  if(input_file_type == ".txt"){
    input_tbl <- utils::read.table(input_file, header = TRUE)
  } else {
    if(input_file_type == ".csv") {
      input_tbl <- utils::read.csv(input_file)
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
    
    # determine if parameter is a number or factor
    param_type <- ifelse(is.numeric(param_values), "numeric", "factor")
    
    # subset by relevant columns
    y1 <- input_tbl[, which(colnames(input_tbl)==nm1):which(colnames(input_tbl)==nmF)]
    # convert the observations to 1s and 0s
    y1 <- ifelse(y1>0, 1, 0)
    
    # convert parameter to numeric
    #input_tbl[,nm_parameter] <- as.numeric(input_tbl[,nm_parameter])
    
    # set up unmarked data frame
    siteCovs <- data.frame(param= input_tbl[, c(nm_parameter)])
    umf <- unmarked::unmarkedFrameOccu(y = as.matrix(y1), siteCovs = siteCovs) # 
    
    # scale covariate if numeric
    if(param_type == "numeric"){
      unmarked::siteCovs(umf) <- scale(unmarked::siteCovs(umf))
    } 
    
    # run occupancy model 
    #param <- eval(parse(text=paste0("input_tbl$", nm_parameter)))
    #oc1 <- unmarked::occu(~1 ~ eval(parse(text=paste0("input_tbl$", nm_parameter))), data = umf)
    oc1 <- unmarked::occu(~1 ~ param, data = umf)
    
    
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
      occ_estimate = round(oc1_st@estimate, digits),
      occ_95CI = round(c(oc1_stCI[1], oc1_stCI[2]), digits),
      det_estimate = round(oc1_det@estimate, digits),
      det_95CI = round(c(oc1_detCI[1], oc1_detCI[2]), digits),
      parameter_effects = oc1@estimates@estimates$state,
      levels=levels(param_values)
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
      occ_estimate = round(oc1_st@estimate, digits),
      occ_95CI = round(c(oc1_stCI[1], oc1_stCI[2]), digits),
      det_estimate = round(oc1_det@estimate, digits),
      det_95CI = round(c(oc1_detCI[1], oc1_detCI[2]), digits)
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

# oc <- occMod(input_file = "/Users/mikeytabak/Desktop/qsc/projects/DIFS_shiny/example/Pigs_Occupancy_Input_CA_NE_Aug.txt",
#        input_file_type = ".txt",
#        nm1 = "E01",
#        nmF="E12"
#        , parameter = TRUE,
#        #, parameter = TRUE,
#        nm_parameter = "param")
# 
# occMod(input_file = "/Users/mikeytabak/example_input_file.txt", 
#        #parameter=FALSE,
#        parameter=TRUE,
#        nm_parameter="removal")

