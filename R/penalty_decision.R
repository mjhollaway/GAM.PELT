#Function for the penalty decision.
#' Penalty Decision Function
#'
#' Evaluates the arguments to return a numeric value for the penalty. This function is called by \code{GAM.PELT}.
#'
#'
#'
#' @param penalty Choice of "BIC/SIC", "AIC" or "MBIC". The predefined penalties listed do count the changepoint as a parameter.
#' @param n The length of the original data
#' @param diffparam The difference in the number of parameters (degrees of freedom) when a change is added. Changepoint is automatically added so should not be included in this number.
#'
#' @details
#' This function takes the text string input and converts it to a numerical value for the specific length of data specified by n.
#'
#'
#' @return The numeric value of the penalty
#'
#' @author Michael Hollaway
#' @author Rebecca Killick
#' @export
#'
penalty_decision <- function(penalty, n, diffparam){

  #Return the penalty value.
  if ((penalty == 'BIC') | (penalty == 'SIC')){
    pen.return=(diffparam+1)*log(n)
  } else if (penalty == 'AIC'){
    pen.return=2*(diffparam+1)
  } else if (penalty == 'MBIC'){
    pen.return=(diffparam+2)*log(n)
  }

  return(pen.return)
}
