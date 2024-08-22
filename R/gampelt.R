#This function executes the main call to PELT to fit the GAM and detect changes in the parameters.
#This is now setup to run with the default settings of any GAM model that can go into mgcv.
#If df_in and formula are not supplied the function will report that they are missing - function needs both to run.
#' Title
#'
#' @param df_in A data frame containing the data within which you want to find a changepoint. This must contain
#'              the model response variable and covariates required by the formula. By default, expected covariates
#'              are the spatial locations of the data (U,V) and the time index (T). Other covariates can be used in
#'              the GAM model fit.
#' @param formula A GAM formula.
#' @param minseglen Positive Integer giving the minimum segment length (number of observations between changes).
#'                  Cannot be negative or longer than half the number of time points in the input dataset. Default
#'                  value is 5.
#' @param penalty   Choice of "None", "BIC/SIC", "MBIC", "AIC" and "Manual" penalties. If Manual is specified, the manual
#'                  penalty is contained in the pen.value parameter.
#' @param pen.value The value of the penalty when using the Manual penalty option. This is a numeric value and negative
#'                  values are not accepted.
#' @param verbose   Logical. If TRUE then verbose progress is printed to screen.
#' @inheritParams mgcv::gam
#'
#' @import stats
#'
#' @return An object of S4 class "GAM_PELT" is returned. The slot \code{cpts} contains the changepoints that are
#'         returned.
#' @export
#'
#' @author Michael Hollaway
#' @author Rebecca Killick
#'
#' @examples
#'
#' #Generate a simulation dataset.
#' #Set the seed for reproducibility
#' #100 timesteps with true cpt at 50.
#' #50 locations.
#' #Scenario 4b has change is both the spatial and temporal components of the time series.
#' ds_sim <- gen_SimData('4b',1234,n_ts=100,n_sites=50,true_cpts=c(50))
#'
#' #Run GAM.PELT - run with defaults.
#' #Turn off verbose mode.
#' cpts_out <- GAM.PELT(ds_sim$data, Y ~ s(U, V, bs = "tp", k = 5) + s(T, bs = "cr", k = 5) +
#'                      ti(U,V, T, d = c(2, 1), bs = c("tp", "cr"), k = c(5, 5)),
#'                      verbose=TRUE)
#'
#' #Access the changepoints.
#' cpts(cpts_out)
#'
#'
GAM.PELT <- function(df_in=list(), formula=list(), minseglen=5,penalty='BIC',pen.value=0,verbose=TRUE,family = gaussian(), weights = NULL, subset = NULL,
                     na.action=na.omit, offset = NULL, method = "GCV.Cp", optimizer = c("outer", "newton"), control = list(), scale = 0,
                     select = FALSE, knots = NULL, sp = NULL, min.sp = NULL, H = NULL, gamma = 1, fit = TRUE, paraPen = NULL, G = NULL, in.out = NULL,
                     drop.unused.levels = TRUE, drop.intercept = NULL, nei = NULL, discrete = FALSE){


  #Check to see if the user has supplied a GAM formula in the correct format.
  #Note this performs utilises the same call to terms.formula that is used in mgcv.
  #It only checks that a formula is provided in the correct format.
  form_check <- try(stats::terms.formula(formula, specials = c("s", "te", "ti", "t2")),silent = TRUE)

  #Run overall check to see if user has supplied a valid data.set and formula.
  #If form_check gives a correct formula class returned will be "terms" "formula so need to use all in check.
  if (!methods::is(df_in,"data.frame") & !methods::is(form_check,'try-error')){
    stop("Input data is not a data.frame")
  } else if (methods::is(df_in,"data.frame") & methods::is(form_check,'try-error')){
    stop("GAM formula provided is not correct format please check.")
  } else if (!methods::is(df_in,"data.frame") & methods::is(form_check,'try-error')){
    stop('Input data not a data.frame and invalid GAM formula provided.')
  }

  #Check to see if the user has supplied the correct data for the given GAM formula.
  #This is done by checking to see if the data frame supplied has corresponding columns for the GAM formula.
  #This will compare the variables requested by the formula with those available in the data.frame.
  #Error will be reported if there is a mis-match, either the formula is requesting a covariate not available.
  #Or, the labels in the data.frame are not correct (E.g. a type potentially.)
  if (!all(all.vars(formula) %in% names(df_in))){
    stop('Input data.set missing variables required by formula. Please check both formula and dataset.')
  }

  #Now check to see if the variables required by the GAM are all numerical data and if there are any missing data.
  #If there is missing data code will throw a warning telling the user to specify how to deal with missing data.
  #The current message will warn that currently the default of mgcv::gam will be used but to change as required.
  #If all data for a single variable in the formula is Nan also throw an error.
  #Only columns that are required by the GAM formula are checked.
  #n_missing in each column.
  n_missing_col <- apply(df_in[,all.vars(formula)], 2, function(data_col){length(which(is.na(data_col)))})
  #First check to see if any columns have fully missing data (I.e. Are all NaNs.)
  #mgcv runs a check in its setup that checks the number of missing rows in the input data and covariates.
  #In this case you need minimum of 2 full rows (across all covariates and response).
  #As part of the minseglen checks later another test will be run using the actual minseglen value supplied.
  #If so flag as error.
  if (length(which(n_missing_col > nrow(df_in) - 2)) > 0){
    stop("Not enough non-NA data to fit a meaningful GAM model.")
  }
  #If one or more columns has missing data send a message to the user to reminding them of it and to test
  #different ways of handling Nans in their GAM specification. Remind that default is na.omit which could
  #have implications for changepoint locations. Could remind to use full dataset.
  if (length(which((n_missing_col <= nrow(df_in) - 2) & (n_missing_col > 0))) > 0){
    message('NAs in dataset. Consider setting of na.action in GAM call or provide dataset with no missing data.')
  }
  non_numeric_cols <- names(df_in[,all.vars(formula)])[which(sapply(df_in[,all.vars(formula)], class) != "numeric")]
  if (length(non_numeric_cols) > 0){
    err_msg <- paste('Only numeric data is allowed. The following columns of the input data have non-numeric data: ',
                     paste(non_numeric_cols,collapse=','),sep='')
    stop(err_msg)
  }

  #Final check, if any of the covariates (not response variable) are all identical, throw an error that the
  #GAM fit will fail as will be unable to fit a smooth.
  #Drop the response variable in the columns check - is the first variable in all.vars return value.
  n_unique_col <- apply(df_in[,all.vars(formula)[-1]], 2, function(data_col){length(unique(data_col))})
  if (length(which(n_unique_col == 1)) > 0){
    stop('Some covariates in dataset contain same value for all rows, GAM unable to be fitted in current form.')
  }

  #Put a check on the minimum segment length.
  #If minseglen of <5 is supplied add warning saying it has been reset to 5.
  #If a minimum segment length of greater than half the number of timesteps is provided error.
  if (is.numeric(minseglen)){
    #Check to see if a decimal has been supplied by accident.
    if (minseglen%%1 != 0){
      stop('Minimum segment cannot be a decimal, please specify as an integer value.')
    } else {
      if (minseglen < 0){
        stop('Minimum segment length cannot be negative')
      } else if ((minseglen > 0) & (minseglen < 5)){
        minseglen <- 5
        warning('Minimum segment length for GAM.PELT is 5, automatically changed to 5.')
      } else if (minseglen > floor(length(unique(df_in$T))/2)){
        stop('Minimum segment length must be less than half the number of timepoints')
      }
    }
  } else {
    stop('Minimum segment length must be an integer value.')
  }

  #The verbose keyword can only take a logical value flag if anything else is supplied.
  if (!methods::is(verbose, 'logical')){
    stop('Value of verbose must be a logical and set to TRUE/FALSE.')
  }

  #As the GAM.PELT function can take all inputs a regular call to mgcv::gam can take, one option is
  #the setting to not fit the gam during setup. This will not work with GAM.PELT so report an error if
  #user tries to set this value to true.
  if (!methods::is(fit,'logical')){
    stop('Value of fit parameter must be a logical and set to TRUE/FALSE.')
  } else if (fit == FALSE){
    stop('Call to mgcv::gam has fit option set to FALSE. This will not work with GAM.PELT, please set fit=TRUE')
  }

  #Set up a list holding all the parameters to pass to the gam call at respective stages in the algorithm.
  #This will be used to update the dataset used in the call.
  #By default this will be the full dataset.
  GAM_call_list <- list(formula=formula, family = family, data = df_in, weights = weights,
                        subset = subset, na.action=na.action, offset = offset, method = method,
                        optimizer = optimizer, control = control, scale = scale, select = select,
                        knots = knots, sp = sp, min.sp = min.sp, H = H, gamma = gamma, fit = fit,
                        paraPen = paraPen, G = G, in.out = in.out, drop.unused.levels = drop.unused.levels,
                        drop.intercept = drop.intercept, nei = nei, discrete = discrete)

  #Build a call to the GAM model using the list of input parameters.
  #This will be passed through the algorithm to ensure that the same call is made for each segment split.
  GAM_call <- match.call(expand.dots=FALSE)
  #Update the call list to be the gam function from mgcv.
  GAM_call[[1]] <- quote(mgcv::gam)
  #Now add all the parameters.
  GAM_call[names(GAM_call_list)] <- GAM_call_list

  #Now determine the penalty - normally in the changepooint package n=length of time series, however for the spatio temporal data this will be n_ts by n_sites (I.e number of rows of the df to fit GAM to).
  #Get the penalty value based on the input to the function.
  #User defined penalty - can be supplied as either upper/lower case, code converts to upper case for output.
  if (toupper(penalty) == 'MANUAL'){
    #Check to see if the user has provided a numeric penalty value.
    if (is.numeric(pen.value) == FALSE){
      pen.value <- try(eval(parse(text=paste(pen.value))),silent=TRUE)
      if (inherits(pen.value,'try-error')){
        stop('Manual penalty could not be evaluated.')
      } else {
        pen.value <- pen.value
      }
    } else if (is.numeric(pen.value) == TRUE){
      pen.value <- pen.value
    }
    #No penalty
  } else if (toupper(penalty) == 'NONE'){
    pen.value <- 0.0
    #AIC/BIC/MBIC - defined from data.
  } else if (toupper(penalty) %in% c('AIC','BIC','MBIC','SIC')){
    #Call the gam function from mgcv
    GAM_full    <- eval(GAM_call)
    n_param     <- length(GAM_full$coefficients)
    pen.value   <- penalty_decision(toupper(penalty), nrow(GAM_call$data), n_param)
  } else {
    stop('Invalid penalty choice - please choose from AIC, BIC/SIC, MBIC, Manual or None. NOTE: If Manual penalty chosen please supply value at pen.value.')
  }

  #Final last check to catch any negative penalties.
  if (pen.value < 0){
    stop('pen.value cannot be negative, please change your penalty value.')
  }

  #Also get the number of timesteps for the PELT algortihm.
  n_ts <- length(unique(df_in$T))

  #Get the cpts locations, if any.
  ans <- PELT(df_in,n_ts,minseglen,GAM_call,pen.value,verbose)

  #Populate the S4 class that holds the output and return from the function.
  return(class_input(df_in,toupper(penalty),pen.value,minseglen,ans$cpt_locs,GAM_call_list$formula,GAM_call))

}
