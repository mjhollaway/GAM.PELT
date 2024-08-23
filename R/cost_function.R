#' Cost function for fitting GAM to current segment
#'
#' @param tloc_1 Time index of start of segment
#' @param tloc_2 Time index of end of segment
#' @param df_in  Input data frame to fit GAM to
#' @param GAM_call Object containing all the parameters of the GAM to evaluate.
#' @param minseglen Minimum segment length for GAM fit (still needed?)
#'
#' @return Cost of fitting the GAM to the segment indicated by tloc_1 and tloc_2
#'
#' @author Michael Hollaway
#' @author Rebecca Killick
#' @export
#'
cost_function <- function(tloc_1,tloc_2,df_in,GAM_call,minseglen){

  #First split the data according to the proposed segmentation.
  data_seg_ind <- which((df_in$T >= tloc_1) & (df_in$T <= tloc_2))

  #Fit using gam - update the call to use the latest data.
  GAM_call$data          <- df_in[data_seg_ind,]
  GAM_fit                <- eval(GAM_call)

  #NOTE: Could get the penalised fit here - use BIC. BIC(GAM_fit) - use diffparam=0 in penalty if useing this approach. in pen.value calculation ar starts.
  return(as.numeric(-2.0*mgcv::logLik.gam(GAM_fit)))

}
