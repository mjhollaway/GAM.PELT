#' GAM_PELT-class
#'
#' @description
#' An S4 class to store the output of GAM.PELT
#'
#' @slot data.set Object of class \code{"data.frame"}, the original dataset.
#' @slot pen.type Object of class \code{"character"}, the penalty type specified in the analysis.
#' @slot pen.value Object of class \code{"numeric"}, the value of the penalty used in the analysis.
#' @slot minseglen Object of class \code{"numeric"}, the minimum segment length (number of observations between changepoints) used in the analysis.
#' @slot cpts Object of class \code{"numeric"}, vector of changepoints identified.
#' @slot GAM.form Object of class \code{"formula"}, the specified GAM formula.
#' @slot param.est Object of class \code{"list"}, a list where each element is a vector of GAM model parameter estimates.
#' @slot ncpts Object of class \code{"numeric"}, the total number of cpts identified.
#'
#'
#' @export

#Set up the output class for the cpt.spt method.
setClass("GAM_PELT",slots=list(data.set="data.frame",pen.type="character",pen.value="numeric",minseglen="numeric",cpts="numeric",GAM.form="formula",param.est="list",ncpts="integer"))

#Need to set generic values for the slots.
#' Generic Function - data.set
#' @description
#' Generic Function
#'
#' @details
#' Generic Function
#'
#'
#' @param object Object of class \code{GAM_PELT}
#'
#' @return A data frame containing the original dataset.
#'
#' @author Michael Hollaway
#' @author Rebecca Killick
#'
#' @examples
#' #Generate a new object of class GAM_PELT
#' x <- methods::new('GAM_PELT')
#' #Retrieve the data.set slot from x.
#' data.set(x)
#'
#' @export
#' @aliases data.set-methods
#' @rdname data.set-methods
#'
setGeneric("data.set", function(object) standardGeneric('data.set'))
#' Generic Function - pen.type
#' @description
#' Generic Function
#'
#' @details
#' Generic Function
#'
#'
#' @param object Object of class \code{GAM_PELT}
#'
#' @return The penalty type specified in the analysis.
#'
#' @author Michael Hollaway
#' @author Rebecca Killick
#'
#' @examples
#' #Generate a new object of class GAM_PELT
#' x <- methods::new('GAM_PELT')
#' #Retrieve the pen.type slot from x.
#' pen.type(x)
#'
#' @export
#' @aliases pen.type-methods
#' @rdname pen.type-methods
#'
setGeneric("pen.type", function(object) standardGeneric('pen.type'))
#' Generic Function - pen.value
#' @description
#' Generic Function
#'
#' @details
#' Generic Function
#'
#'
#' @param object Object of class \code{GAM_PELT}
#'
#' @return The value of the penalty used in the analysis.
#'
#' @author Michael Hollaway
#' @author Rebecca Killick
#'
#' @examples
#' #Generate a new object of class GAM_PELT
#' x <- methods::new('GAM_PELT')
#' #Retrieve the pen.value slot from x.
#' pen.value(x)
#'
#' @export
#' @aliases pen.value-methods
#' @rdname pen.value-methods
#'
setGeneric("pen.value", function(object) standardGeneric('pen.value'))
#' Generic Function - minseglen
#' @description
#' Generic Function
#'
#' @details
#' Generic Function
#'
#'
#' @param object Object of class \code{GAM_PELT}
#'
#' @return The minimum segment length (number of observations between changepoints) used in the analysis.
#'
#' @author Michael Hollaway
#' @author Rebecca Killick
#'
#' @examples
#' #Generate a new object of class GAM_PELT
#' x <- methods::new('GAM_PELT')
#' #Retrieve the minseglen slot from x.
#' minseglen(x)
#'
#' @export
#' @aliases minseglen-methods
#' @rdname minseglen-methods
#'
setGeneric("minseglen", function(object) standardGeneric('minseglen'))
#' Generic Function - cpts
#' @description
#' Generic Function
#'
#' @details
#' Generic Function
#'
#'
#' @param object Object of class \code{GAM_PELT}
#'
#' @return Returns an ordered vector of the optimal number of changepoints identified by the call to \code{GAM.PELT}
#'
#' @author Michael Hollaway
#' @author Rebecca Killick
#'
#' @examples
#' #Generate a new object of class GAM_PELT
#' x <- methods::new('GAM_PELT')
#' #Return the vector of changepoints - will be blank in new object.
#' cpts(x)
#'
#' @export
#' @aliases cpts-methods
#' @rdname cpts-methods
#'
setGeneric("cpts", function(object) standardGeneric('cpts'))
#' Generic Function - GAM.form
#' @description
#' Generic Function
#'
#' @details
#' Generic Function
#'
#'
#' @param object Object of class \code{GAM_PELT}
#'
#' @return The GAM formula used in the analysis.
#'
#' @author Michael Hollaway
#' @author Rebecca Killick
#'
#' @examples
#' #Generate a new object of class GAM_PELT
#' x <- methods::new('GAM_PELT')
#' #Retrieve the GAM.form slot from x.
#' GAM.form(x)
#'
#' @export
#' @aliases GAM.form-methods
#' @rdname GAM.form-methods
#'
setGeneric("GAM.form", function(object) standardGeneric('GAM.form'))
#' Generic Function - param.est
#' @description
#' Generic Function
#'
#' @details
#' Generic Function
#'
#'
#' @param object Object of class \code{GAM_PELT}
#'
#' @return A list where each element is a vector of GAM model parameter estimates.
#'
#' @author Michael Hollaway
#' @author Rebecca Killick
#'
#' @examples
#' #Generate a new object of class GAM_PELT
#' x <- methods::new('GAM_PELT')
#' #Retrieve the param.est slot from x.
#' param.est(x)
#'
#' @export
#' @aliases param.est-methods
#' @rdname param.est-methods
#'
setGeneric("param.est", function(object) standardGeneric('param.est'))
#' Generic Function - ncpts
#' @description
#' Generic Function
#'
#' @details
#' Generic Function
#'
#'
#' @param object Object of class \code{GAM_PELT}
#'
#' @return The total number of cpts identified.
#'
#' @author Michael Hollaway
#' @author Rebecca Killick
#'
#' @examples
#' #Generate a new object of class GAM_PELT
#' x <- methods::new('GAM_PELT')
#' #Retrieve the ncpts slot from x.
#' ncpts(x)
#'
#' @export
#' @aliases ncpts-methods
#' @rdname ncpts-methods
#'
setGeneric("ncpts", function(object) standardGeneric('ncpts'))

#Create the retrieval functions.
#Some of these will hold simple values and others will hold the function itself.
#' @docType methods
#' @rdname data.set-methods
#' @aliases data.set,GAM_PELT,data.set-methods
#' @export
setMethod("data.set","GAM_PELT",function(object) object@data.set)
#' @rdname pen.type-methods
#' @aliases pen.type,GAM_PELT,pen.type-methods
#' @export
setMethod("pen.type","GAM_PELT",function(object) object@pen.type)
#' @rdname pen.value-methods
#' @aliases pen.value,GAM_PELT,pen.value-methods
#' @export
setMethod("pen.value","GAM_PELT",function(object) object@pen.value)
#' @rdname minseglen-methods
#' @aliases minseglen,GAM_PELT,minseglen-methods
#' @export
setMethod("minseglen","GAM_PELT",function(object) object@minseglen)
#' @docType methods
#' @rdname cpts-methods
#' @aliases cpts,GAM_PELT,cpts-methods
#' @export
setMethod("cpts","GAM_PELT",function(object) object@cpts)  #GAM_PELT already removes the last cpt so no need to do here.
#' @rdname GAM.form-methods
#' @aliases GAM.form,GAM_PELT,GAM.form-methods
#' @export
setMethod("GAM.form","GAM_PELT",function(object) object@GAM.form)
#' @rdname param.est-methods
#' @aliases param.est,GAM_PELT,param.est-methods
#' @export
setMethod("param.est","GAM_PELT",function(object) object@param.est)
#' @rdname ncpts-methods
#' @aliases ncpts,GAM_PELT,ncpts-methods
#' @export
setMethod("ncpts","GAM_PELT",function(object) length(object@cpts))

#Set up a function to determine the GAM parameter coefficients for each segment.
setGeneric("param", function(object,...) standardGeneric("param"))
setMethod("param", "GAM_PELT", function(object,GAM_call) {

  #Set some local values of variables for use in parameter calculations.
  cpts    <- object@cpts
  data    <- object@data.set
  tmpcall <- GAM_call          #This will be modified depending on segment run.

  #First the segment start and end - remember the cpt/GAM routine also returns the end of the time series as a cpt - can remove this.
  #Set the segment start and ends from the cpt locations.
  seg_start <- c(1,cpts+1)
  seg_end   <- c(cpts,length(unique(data$T)))

  #Get the number of segments.
  nseg <- length(cpts)+1

  #Get the unique site ids - note some may be missing from the list due to incomplete datasets so need the ids themselves here.
  site_ids <- unique(data$SID)

  #Get the number of sites.
  n_sites <- length(site_ids)

  #Create some blank lists to hold the fitted coefficients.
  seg_gamcoeff <- list()

  #Loop over the segments and extract the fitted GAM info.
  for (ii in 1:nseg){

    #Define the current data segment.
    curr_seg_ind <- which((data$T >= seg_start[ii]) & (data$T <= seg_end[ii]))

    #Fit the GAM for the current segment.
    #Fit using gam - update the call to use the latest data.
    tmpcall$data <- data[curr_seg_ind,]
    GAM_seg_fit   <- eval(tmpcall)

    #Extract the required info.
    seg_gamcoeff[[ii]] <- GAM_seg_fit$coefficients
    #  seg_GAM[[ii]]      <- GAM_seg_fit
    #  #Loop over sites and get the individual residuals at each site.
    #  sites_resids <- list()
    #  for (jj in 1:n_sites){
    #     sites_resids[[jj]] <- resid(GAM_seg_fit)[which(data_fit$SID == site_ids[jj])]
    #  }


    #  seg_resid[[ii]]    <- sites_resids

  }

  #Add the segment names to the list for identification.
  names(seg_gamcoeff) <- paste('SEG',seq(1,nseg,1),sep='')

  #  #Pass out the output as a named list.
  #  return(list(coefs=seg_gamcoeff,resid=seg_resid,nseg=nseg,n_sites=n_sites,seg_GAM=seg_GAM))
  param.est(object) <- seg_gamcoeff

  return(object)

})

#Replacement functions for the slots - these are needed to replace values in the class.
#data.set.
setGeneric("data.set<-", function(object, value) standardGeneric("data.set<-"))
setReplaceMethod("data.set", "GAM_PELT", function(object, value) {
  object@data.set <- value
  return(object)
})
#pen.type.
setGeneric("pen.type<-", function(object, value) standardGeneric("pen.type<-"))
setReplaceMethod("pen.type", "GAM_PELT", function(object, value) {
  object@pen.type <- value
  return(object)
})
#pen.value
setGeneric("pen.value<-", function(object, value) standardGeneric("pen.value<-"))
setReplaceMethod("pen.value", "GAM_PELT", function(object, value) {
  object@pen.value <- value
  return(object)
})
#minseglen
setGeneric("minseglen<-", function(object, value) standardGeneric("minseglen<-"))
setReplaceMethod("minseglen", "GAM_PELT", function(object, value) {
  object@minseglen <- value
  return(object)
})
#cpts
setGeneric("cpts<-", function(object, value) standardGeneric("cpts<-"))
setReplaceMethod("cpts", "GAM_PELT", function(object, value) {
  object@cpts <- value
  return(object)
})
#GAM.form
setGeneric("GAM.form<-", function(object, value) standardGeneric("GAM.form<-"))
setReplaceMethod("GAM.form", "GAM_PELT", function(object, value) {
  object@GAM.form <- value
  return(object)
})
#param.est
setGeneric("param.est<-", function(object, value) standardGeneric("param.est<-"))
setReplaceMethod("param.est", "GAM_PELT", function(object, value) {
  object@param.est <- value
  return(object)
})
#ncpts
setGeneric("ncpts<-", function(object, value) standardGeneric("ncpts<-"))
setReplaceMethod("ncpts", "GAM_PELT", function(object, value) {
  object@ncpts <- value
  return(object)
})


#Set some functions to display the class neatly to the screen when it is shown.
# summary functions
setMethod("summary","GAM_PELT",function(object){
  cat("Type of penalty       :", pen.type(object), "with value,",pen.value(object),"\n")
  cat("Minimum Segment Length :", minseglen(object),"\n")
  if(length(cpts(object))<=20){cat("Changepoint Locations :",cpts(object),"\n")}
  else{cat("Number of changepoints:", ncpts(object),"\n")}
})

# show functions
#This function can be called with show(class_name) or just simply class name.
setMethod("show","GAM_PELT",function(object){
  cat("Class 'GAM_PELT' : Changepoint Object\n")
  cat("       ~~   : S4 class containing", length(attributes(object))-1, "slots with names\n")
  cat("             ", names(attributes(object))[1:(length(attributes(object))-1)], "\n\n")
  cat("summary(.)  :\n----------\n")
  summary(object)
})
