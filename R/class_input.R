#File to input data to the S4 GAM.PELT Class

#Set up the function.
#' Class Input Function
#'
#' @description
#' This function helps to input all the necessary information into the correct format for
#' \code{GAM_PELT} class. This function is called by \code{GAM.PELT}. This is not intended for use
#' by regular users of the package.
#'
#' @description
#'
#' WARNING: No checks on arguments are performed!
#'
#'
#' @param data Data used in the changepoint analysis, see \code{\link{GAM.PELT}} for further details.
#' @param pen.type Penalty used as a text string, see \code{\link{GAM.PELT}} for further details.
#' @param pen.value Numerical value of the penalty used in the analysis (positive).
#' @param minseglen Minimum segment length used in the analysis (positive integer).
#' @param cpts Locations of changepoints identified by the analysis.
#' @param GAM.form GAM formula used in the changepoint analysis, see \code{\link{GAM.PELT}} for further details.
#' @param GAM_call Call to fit the GAM model used in the changepoint analysis. Internal variable used to output
#'                 the fitted GAM parameters for each segment based on the changepoint locations.
#'
#' @details This function takes all the input required for the \code{GAM_PELT} class and enters it into the object.
#'          This function is exported for developer use only.
#'
#' @return An object of class \code{GAM_PELT} filled with the given attributes.
#' @export
#'
#' @author Michael Hollaway
#' @author Rebecca Killick
#'
class_input <- function(data,pen.type,pen.value,minseglen,cpts,GAM.form,GAM_call){

  #First create the class.
  ans <- methods::new('GAM_PELT')

  #Populate the class.
  data.set(ans)  <- data
  pen.type(ans)  <- pen.type
  pen.value(ans) <- pen.value
  minseglen(ans) <- minseglen
  cpts(ans)      <- cpts
  GAM.form(ans)  <- GAM.form

  #Also add the parameter estimates for the GAM fit for each segment.
  #This needs to take the call to the GAM as well.
  ans <- param(ans,GAM_call)

  return(ans)

}
