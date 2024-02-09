#File to input data to the S4 GAM.PELT Class

#Set up the function.
class_input <- function(data,pen.type,pen.value,minseglen,cpts,GAM.form,GAM_call){

    #First create the class.
    ans <- new('GAM_PELT')

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