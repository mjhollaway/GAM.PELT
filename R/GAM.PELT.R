#Wrapper function to call the main GAM PELT function.
#Function to run the changepoint detection algorithm.

#Load dependencies.
source("class_input.R")
source("GAM.PELT.class.R")

#Function for the penalty decision.
penalty_decision <- function(penalty, n, diffparam){

   #Return the penalty value.
   if (penalty == 'BIC'){
      pen.return=(diffparam+1)*log(n)
   } else if (penalty == 'AIC'){
      pen.return=2*(diffparam+1)
   } else if (penalty == 'MBIC'){
      pen.return=(diffparam+2)*log(n)
   } 
   
   return(pen.return)
}


#Create a function that evaluates the cost of fitting a GAM model to the segments.
cost_function <- function(tloc_1,tloc_2,df_in,GAM_call,resp_var,minseglen){

   #First split the data according to the proposed segmentation.
   data_seg_ind <- which((df_in$T >= tloc_1) & (df_in$T <= tloc_2))
   
   #Fit using gam - update the call to use the latest data.
   GAM_call$data          <- df_in[data_seg_ind,]
   GAM_fit                <- eval(GAM_call)
   
   #NOTE: Could get the penalised fit here - use BIC. BIC(GAM_fit) - use diffparam=0 in penalty if useing this approach. in pen.value calculation ar starts.
   return(as.numeric(-2.0*mgcv::logLik.gam(GAM_fit)))

}

#Define the PELT function to detect multiple changpoints in the data.
#This prunes the dataset as it goes along (See Killick et al 2012)
PELT <- function(df_in,n,minseglen,GAM_call,resp_var,pen.value,verbose){

   #Print a default message to let user know algorithm has started.
   message("Detecting spatio temporal changepoints")

   #Set up a progress bar to keep track of cpt detection.
   #First determine the total number of iterations in the 2 PELT loops.
   #Only do this if verbose is set to TRUE (default)
   if (verbose == TRUE){
      nloop <- length((minseglen+1):(2*minseglen)) + length(((2*minseglen)+1):n)
      #Then setup the progress bar and a counter.
      pb    <- txtProgressBar(min = 0, max = nloop, style = 3)
      done  <- 0
   }

   lastchangecpts <- NA
   lastchangelike <- NA
   checklist      <- NULL
   for(j in (minseglen+1):(2*minseglen)){
    lastchangelike[j] <- cost_function(1,(j+1),df_in,GAM_call,resp_var,minseglen)   #Initialise cost of model from 1 to j for use later on.
    lastchangecpts[j] <- 0
    #Increase the progress loop if required.
    if (verbose == TRUE){
       done <- done+1
       setTxtProgressBar(pb, done)
    }
   }
   noprune <- NULL
   for(tstar in ((2*minseglen)+1):n){
     tmplike <- NULL
     tmpt    <- c(checklist, tstar-minseglen)
     #Loop over the values of tmpt and determine the cost of segmenting between that value and tstar+1
     for (ii in 1:length(tmpt)){ 
       tmplike[ii] <- lastchangelike[tmpt[ii]] + cost_function((tmpt[ii]+1),tstar,df_in,GAM_call,resp_var,minseglen)+pen.value
     }
     lastchangelike[tstar] <- min(c(tmplike,cost_function(1,tstar,df_in,GAM_call,resp_var,minseglen)),na.rm=TRUE) # minimum at changepoint prior to tstar
     if(lastchangelike[tstar] == cost_function(1,tstar,df_in,GAM_call,resp_var,minseglen)){
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
        setTxtProgressBar(pb, done)
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
   message("COMPLETE")

   return(list(cpt_locs=cpt_locs,checklist=checklist,lastchangecpts=lastchangecpts,lastchangelike=lastchangelike))

}


#This function executes the main call to PELT to fit the GAM and detect changes in the parameters.
#This is now setup to run with the default settings of any GAM model that can go into mgcv.
#If df_in and formula are not supplied the function will report that they are missing - function needs both to run. 
GAM.PELT <- function(df_in, formula, minseglen=5,penalty='BIC',pen.value=0,verbose=TRUE,family = gaussian(), weights = NULL, subset = NULL, 
                     na.action=na.omit, offset = NULL, method = "GCV.Cp", optimizer = c("outer", "newton"), control = list(), scale = 0, 
                     select = FALSE, knots = NULL, sp = NULL, min.sp = NULL, H = NULL, gamma = 1, fit = TRUE, paraPen = NULL, G = NULL, in.out = NULL, 
                     drop.unused.levels = TRUE, drop.intercept = NULL, nei = NULL, discrete = FALSE){

    
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
    #User defined penalty
    if (penalty == 'Manual'){
      pen.value <- pen.value
    #No penalty
    } else if (penalty == 'NONE'){
      pen.value <- 0.0
    #AIC/BIC/MBIC - defined from data.
    } else if (penalty %in% c('AIC','BIC','MBIC')){
      #Call the gam function from mgcv
      GAM_full    <- eval(GAM_call)
      n_param     <- length(GAM_full$coefficients)
      pen.value   <- penalty_decision(penalty, nrow(GAM_call$data), n_param)
    } else {
      stop('Invalid penalty choice - please choose from AIC, BIC, MBIC, Manual or None. NOTE: If Manual penalty chosen please supply value at pen.value.')
    }

    #Also get the number of timesteps for the PELT algortihm.
    n_ts <- length(unique(df_in$T))

    #Get the cpts locations, if any.
    ans <- PELT(df_in,n_ts,minseglen,GAM_call,resp_var,pen.value,verbose)

    #Populate the S4 class that holds the output and return from the function.
    return(class_input(df_in,penalty,pen.value,minseglen,ans$cpt_locs,GAM_call_list$formula,GAM_call))

    #Return the changepoint locations and the max knots used to fit the gam, also the penalty value.
    #return(list(cpt_locs=ans$cpt_locs,minseglen=minseglen,penalty_value=pen.value,GAM_formula=GAM_formula,checklist=ans$checklist,lastchangecpts=ans$lastchangecpts,lastchangelike=ans$lastchangelike))

}



    

    