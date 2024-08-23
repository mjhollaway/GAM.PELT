#' PELT (Pruned Exact Linear Time)
#'
#' @description
#' Implements the PELT method for identifying changepoints in a given set of summary statistics for a specified cost function and penalty.
#' This function is called by \code{GAM.PELT}.  This is not intended for use by regular users of the package.  It is exported for developers to call directly for speed increases or to fit alternative cost functions.
#'
#' @description
#' WARNING: No checks on arguments are performed!
#'
#'
#' @param df_in A data frame containing the data within which you want to find a changepoint. Currently assumes all variables to fit the formula set in GAM_call are present.
#' @param n The number of time points in the dataset
#' @param minseglen Positive Integer giving the minimum segment length (number of observations between changes).
#' @param GAM_call The call to provide to the \code{gam} function from \code{mgcv}
#' @param pen.value Penalty value, this should be set elsewhere as a numerical value
#' @param verbose Logical, print verbose progress output if TRUE
#'
#' @return A list is returned with elements:
#' @return \code{lastchangecpts} - Vector of length n containing the last changepoint prior to each timepoint
#' @return \code{checklist} - The PELT checklist
#' @return \code{cpt_locs} - Ordered list of the optimal number of changepoints
#' @return \code{lastchangelike} - Vector of length n containing the likelihood of the optimal segmentation up to each timepoint
#'
#' @references PELT Algorithm: Killick R, Fearnhead P, Eckley IA (2012) Optimal detection of changepoints with a linear computational cost, \emph{JASA} \bold{107(500)}, 1590--1598
#'
#' @author Michael Hollaway
#' @author Rebecca Killick
#'
#' @export
#'
PELT <- function(df_in,n,minseglen,GAM_call,pen.value,verbose){

  #Print a default message to let user know algorithm has started.
  #Only do this if verbose is set to TRUE (default)
  if (verbose == TRUE){
    message("Detecting spatio temporal changepoints")
  }

  #Set up a progress bar to keep track of cpt detection.
  #First determine the total number of iterations in the 2 PELT loops.
  #Only do this if verbose is set to TRUE (default)
  if (verbose == TRUE){
    nloop <- length((minseglen+1):(2*minseglen)) + length(((2*minseglen)+1):n)
    #Then setup the progress bar and a counter.
    pb    <- utils::txtProgressBar(min = 0, max = nloop, style = 3)
    done  <- 0
  }

  lastchangecpts <- NA
  lastchangelike <- NA
  checklist      <- NULL
  for(j in (minseglen+1):(2*minseglen)){
    lastchangelike[j] <- cost_function(1,(j+1),df_in,GAM_call,minseglen)   #Initialise cost of model from 1 to j for use later on.
    lastchangecpts[j] <- 0
    #Increase the progress loop if required.
    if (verbose == TRUE){
      done <- done+1
      utils::setTxtProgressBar(pb, done)
    }
  }
  noprune <- NULL
  for(tstar in ((2*minseglen)+1):n){
    tmplike <- NULL
    tmpt    <- c(checklist, tstar-minseglen)
    #Loop over the values of tmpt and determine the cost of segmenting between that value and tstar+1
    for (ii in 1:length(tmpt)){
      tmplike[ii] <- lastchangelike[tmpt[ii]] + cost_function((tmpt[ii]+1),tstar,df_in,GAM_call,minseglen)+pen.value
    }
    lastchangelike[tstar] <- min(c(tmplike,cost_function(1,tstar,df_in,GAM_call,minseglen)),na.rm=TRUE) # minimum at changepoint prior to tstar
    if(lastchangelike[tstar] == cost_function(1,tstar,df_in,GAM_call,minseglen)){
      lastchangecpts[tstar]=0
    }
    else{
      cpt=tmpt[tmplike==lastchangelike[tstar]][1]
      lastchangecpts[tstar] <- cpt
    }
    checklist=tmpt[tmplike<=lastchangelike[tstar]+pen.value]
    #Increase the progress loop if required.
    if (verbose == TRUE){
      done <- done+1
      utils::setTxtProgressBar(pb, done)
    }
  }

  #Extract the cpts.
  fcpt=NULL
  last=n
  while(last!=0){
    fcpt=c(fcpt,last)
    last=lastchangecpts[last]
  }

  #Sort and store the final cpt locations (this includes removing the last element of the tseries which is stored as the first cpt in fcpt)
  cpt_locs <- sort(fcpt[-1])

  #Print this message by default to let user know algorithm complete.
  #Only do this if verbose is set to TRUE (default)
  if (verbose == TRUE){
    message("COMPLETE")
  }

  return(list(cpt_locs=cpt_locs,checklist=checklist,lastchangecpts=lastchangecpts,lastchangelike=lastchangelike))

}
