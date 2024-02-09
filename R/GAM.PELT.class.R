#Set up the output class for the cpt.spt method.
setClass("GAM_PELT",slots=list(data.set="data.frame",pen.type="character",pen.value="numeric",minseglen="numeric",cpts="numeric",GAM.form="formula",param.est="list"))

#Need to set generic values for the slots.
#data.set.
if(!isGeneric("data.set")) {
   if (is.function("data.set")){
   	fun <- data.set
   }
   else {fun <- function(object){
   		standardGeneric("data.set")
   	}
   }
   setGeneric("data.set", fun)
}
#pen.type.
if(!isGeneric("pen.type")) {
   if (is.function("pen.type")){
   	fun <- pen.type
   }
   else {fun <- function(object){
   		standardGeneric("pen.type")
   	}
   }
   setGeneric("pen.type", fun)
}
#pen.value.
if(!isGeneric("pen.value")) {
   if (is.function("pen.value")){
   	fun <- pen.value
   }
   else {fun <- function(object){
   		standardGeneric("pen.value")
   	}
   }
   setGeneric("pen.value", fun)
}
#minseglen.
if(!isGeneric("minseglen")) {
   if (is.function("minseglen")){
   	fun <- minseglen
   }
   else {fun <- function(object){
   		standardGeneric("minseglen")
   	}
   }
   setGeneric("minseglen", fun)
}
#cpts.
if(!isGeneric("cpts")) {
   if (is.function("cpts")){
   	fun <- cpts
   }
   else {fun <- function(object){
   		standardGeneric("cpts")
   	}
   }
   setGeneric("cpts", fun)
}
#GAM.form.
if(!isGeneric("GAM.form")) {
   if (is.function("GAM.form")){
   	fun <- GAM.form
   }
   else {fun <- function(object){
   		standardGeneric("GAM.form")
   	}
   }
   setGeneric("GAM.form", fun)
}
#param.est.
if(!isGeneric("param.est")) {
   if (is.function("param.est")){
   	fun <- param.est
   }
   else {fun <- function(object){
   		standardGeneric("param.est")
   	}
   }
   setGeneric("param.est", fun)
}


#Create the retrieval functions.
#Some of these will hold simple values and others will hold the function itself.
setMethod("data.set","GAM_PELT",function(object) object@data.set)
setMethod("pen.type","GAM_PELT",function(object) object@pen.type)
setMethod("pen.value","GAM_PELT",function(object) object@pen.value)
setMethod("minseglen","GAM_PELT",function(object) object@minseglen)
setMethod("cpts","GAM_PELT",function(object) object@cpts)  #GAM_PELT already removes the last cpt so no need to do here.
setMethod("GAM.form","GAM_PELT",function(object) object@GAM.form)
setMethod("param.est","GAM_PELT",function(object) object@param.est)

# ncpts function
if(!isGeneric("ncpts")) {
	if (is.function("ncpts")){
		fun <- ncpts
	}
	else {fun <- function(object){
			standardGeneric("ncpts")
		}
	}
	setGeneric("ncpts", fun)
}
setMethod("ncpts","GAM_PELT",function(object) length(cpts(object)))

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

# NEED TO FINISH SETTING UP THE CLASS WITH ALL THE REQUIRED FUNCTIONS! THEN CREATE A NEW CALLING ROUTINE THAT POPULATES THE CLASS AS BASED IN cpt r package.
# ALSO MAYBE SPLIT OUT SOME OF THE FUNCTIONS INTO SEPARATE R  SCRIPTS WITH A VIEW TO RUNNING AS AN R PACKAGE!