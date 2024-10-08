% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gampelt.R
\name{GAM.PELT}
\alias{GAM.PELT}
\title{Title}
\usage{
GAM.PELT(
  df_in = list(),
  formula = list(),
  minseglen = 5,
  penalty = "BIC",
  pen.value = 0,
  verbose = TRUE,
  family = gaussian(),
  weights = NULL,
  subset = NULL,
  na.action = na.omit,
  offset = NULL,
  method = "GCV.Cp",
  optimizer = c("outer", "newton"),
  control = list(),
  scale = 0,
  select = FALSE,
  knots = NULL,
  sp = NULL,
  min.sp = NULL,
  H = NULL,
  gamma = 1,
  fit = TRUE,
  paraPen = NULL,
  G = NULL,
  in.out = NULL,
  drop.unused.levels = TRUE,
  drop.intercept = NULL,
  nei = NULL,
  discrete = FALSE
)
}
\arguments{
\item{df_in}{A data frame containing the data within which you want to find a changepoint. This must contain
the model response variable and covariates required by the formula. By default, expected covariates
are the spatial locations of the data (U,V) and the time index (T). Other covariates can be used in
the GAM model fit.}

\item{formula}{A GAM formula.}

\item{minseglen}{Positive Integer giving the minimum segment length (number of observations between changes).
Cannot be negative or longer than half the number of time points in the input dataset. Default
value is 5.}

\item{penalty}{Choice of "None", "BIC/SIC", "MBIC", "AIC" and "Manual" penalties. If Manual is specified, the manual
penalty is contained in the pen.value parameter.}

\item{pen.value}{The value of the penalty when using the Manual penalty option. This is a numeric value and negative
values are not accepted.}

\item{verbose}{Logical. If TRUE then verbose progress is printed to screen.}

\item{family}{
This is a family object specifying the distribution and link to use in
fitting etc (see \code{\link{glm}} and \code{\link{family}}). See 
\code{\link[mgcv]{family.mgcv}} for a full list of what is available, which goes well beyond exponential family.
Note that \code{quasi} families actually result in the use of extended quasi-likelihood 
if \code{method} is set to a RE/ML method (McCullagh and Nelder, 1989, 9.6).
}

\item{weights}{ prior weights on the contribution of the data to the log likelihood. Note that a weight of 2, for example, 
                is equivalent to having made exactly the same observation twice. If you want to re-weight the contributions 
                of each datum without changing the overall magnitude of the log likelihood, then you should normalize the weights
                (e.g. \code{weights <- weights/mean(weights)}). }

\item{subset}{ an optional vector specifying a subset of observations to be
          used in the fitting process.}

\item{na.action}{ a function which indicates what should happen when the data
          contain `NA's.  The default is set by the `na.action' setting
          of `options', and is `na.fail' if that is unset.  The
          ``factory-fresh'' default is `na.omit'.}

\item{offset}{Can be used to supply a model offset for use in fitting. Note
that this offset will always be completely ignored when predicting, unlike an offset 
included in \code{formula} (this used to conform to the behaviour of
\code{lm} and \code{glm}).}

\item{method}{The smoothing parameter estimation method. \code{"GCV.Cp"} to use GCV for unknown scale parameter and
Mallows' Cp/UBRE/AIC for known scale. \code{"GACV.Cp"} is equivalent, but using GACV in place of GCV. \code{"NCV"}
for neighbourhood cross-validation using the neighbourhood structure speficied by \code{nei} (\code{"QNCV"} for numerically more ribust version).  \code{"REML"} 
for REML estimation, including of unknown scale, \code{"P-REML"} for REML estimation, but using a Pearson estimate 
of the scale. \code{"ML"} and \code{"P-ML"} are similar, but using maximum likelihood in place of REML. Beyond the 
exponential family \code{"REML"} is the default, and the only other options are \code{"ML"}, \code{"NCV"} or \code{"QNCV"}.}

\item{optimizer}{An array specifying the numerical optimization method to use to optimize the smoothing 
parameter estimation criterion (given by \code{method}). \code{"outer"} 
for the direct nested optimization approach. \code{"outer"} can use several alternative optimizers, specified in the 
second element of \code{optimizer}: \code{"newton"} (default), \code{"bfgs"}, \code{"optim"} or \code{"nlm"}. \code{"efs"}
for the extended Fellner Schall method of Wood and Fasiolo (2017).}

\item{control}{A list of fit control parameters to replace defaults returned by 
\code{\link[mgcv]{gam.control}}. Values not set assume default values. }

\item{scale}{ If this is positive then it is taken as the known scale parameter. Negative signals that the 
scale parameter is unknown. 0 signals that the scale parameter is 1  for Poisson and binomial and unknown otherwise. 
Note that (RE)ML methods can only work with scale parameter 1 for the Poisson and binomial cases.    
}

\item{select}{ If this is \code{TRUE} then \code{gam} can add an extra penalty to each term so 
that it can be penalized to zero.  This means that the smoothing parameter estimation that is 
part of fitting can completely remove terms from the model. If the corresponding 
smoothing parameter is estimated as zero then the extra penalty has no effect. Use \code{gamma} to increase level of penalization.
}

\item{knots}{this is an optional list containing user specified knot values to be used for basis construction. 
For most bases the user simply supplies the knots to be used, which must match up with the \code{k} value
supplied (note that the number of knots is not always just \code{k}). 
See \code{\link[mgcv]{tprs}} for what happens in the \code{"tp"/"ts"} case. 
Different terms can use different numbers of knots, unless they share a covariate.
}

\item{sp}{A vector of smoothing parameters can be provided here.
 Smoothing parameters must be supplied in the order that the smooth terms appear in the model 
formula. Negative elements indicate that the parameter should be estimated, and hence a mixture 
of fixed and estimated parameters is possible. If smooths share smoothing parameters then \code{length(sp)} 
must correspond to the number of underlying smoothing parameters.}

\item{min.sp}{Lower bounds can be supplied for the smoothing parameters. Note
that if this option is used then the smoothing parameters \code{full.sp}, in the 
returned object, will need to be added to what is supplied here to get the 
 smoothing parameters actually multiplying the penalties. \code{length(min.sp)} should 
always be the same as the total number of penalties (so it may be longer than \code{sp},
if smooths share smoothing parameters).}

\item{H}{A user supplied fixed quadratic penalty on the parameters of the 
GAM can be supplied, with this as its coefficient matrix. A common use of this term is 
to add a ridge penalty to the parameters of the GAM in circumstances in which the model
is close to un-identifiable on the scale of the linear predictor, but perfectly well
defined on the response scale.}

\item{gamma}{Increase this beyond 1 to produce smoother models. \code{gamma} multiplies the effective degrees of freedom in the GCV or UBRE/AIC. \code{n/gamma} can be viewed as an effective sample size in the GCV score, and this also enables it to be used with REML/ML. Ignored with P-RE/ML or the \code{efs} optimizer. }

\item{fit}{If this argument is \code{TRUE} then \code{gam} sets up the model and fits it, but if it is
\code{FALSE} then the model is set up and an object \code{G} containing what
would be required to fit is returned is returned. See argument \code{G}.}

\item{paraPen}{optional list specifying any penalties to be applied to parametric model terms. 
\code{\link[mgcv]{gam.models}} explains more.}

\item{G}{Usually \code{NULL}, but may contain the object returned by a previous call to \code{gam} with 
\code{fit=FALSE}, in which case all other arguments are ignored except for
\code{sp}, \code{gamma}, \code{in.out}, \code{scale}, \code{control}, \code{method} \code{optimizer} and \code{fit}.}

\item{in.out}{optional list for initializing outer iteration. If supplied
then this must contain two elements: \code{sp} should be an array of
initialization values for all smoothing parameters (there must be a value for
all smoothing parameters, whether fixed or to be estimated, but those for
fixed s.p.s are not used); \code{scale} is the typical scale of the GCV/UBRE function,
for passing to the outer optimizer, or the the initial value of the scale parameter, if this is to
be estimated by RE/ML.}

\item{drop.unused.levels}{by default unused levels are dropped from factors before fitting. For some smooths 
involving factor variables you might want to turn this off. Only do so if you know what you are doing.}

\item{drop.intercept}{Set to \code{TRUE} to force the model to really not have a constant in the parametric model part,
even with factor variables present. Can be vector when \code{formula} is a list.}

\item{nei}{A list specifying the neighbourhood structure for \code{\link[mgcv]{NCV}}. \code{k} is the vector of indices to be dropped for each neighbourhood and \code{m} gives the end of each neighbourhood. So \code{nei$k[(nei$m[j-1]+1):nei$m[j]]} gives the points dropped for the neighbourhood \code{j}. \code{i} is the vector of indices of points to predict, with corresponding endpoints \code{mi}. So \code{nei$i[(nei$mi[j-1]+1):nei$mi[j]]} indexes the points to predict for neighbourhood j. If \code{nei==NULL} (or \code{k} or \code{m} are missing) then leave-one-out cross validation is obtained. If \code{jackknife} is supplied then \code{TRUE} indicates to use raw jackknife covariances estimator and \code{FALSE} to use the conventional Bayes estimate. If not supplied then the estimator accounting for neighbourhood structure is used. \code{jackknife} ignored when \code{method} is not NCV.}

\item{discrete}{experimental option for setting up models for use with discrete methods employed in \code{\link[mgcv]{bam}}. Do not modify.}
}
\value{
An object of S4 class "GAM_PELT" is returned. The slot \code{cpts} contains the changepoints that are
returned.
}
\description{
Title
}
\examples{

#Generate a simulation dataset.
#Set the seed for reproducibility
#100 timesteps with true cpt at 50.
#50 locations.
#Scenario 4b has change is both the spatial and temporal components of the time series.
ds_sim <- gen_SimData('4b',1234,n_ts=100,n_sites=50,true_cpts=c(50))

#Run GAM.PELT - run with defaults.
#Turn off verbose mode.
cpts_out <- GAM.PELT(ds_sim$data, Y ~ s(U, V, bs = "tp", k = 5) + s(T, bs = "cr", k = 5) +
                     ti(U,V, T, d = c(2, 1), bs = c("tp", "cr"), k = c(5, 5)),
                     verbose=TRUE)

#Access the changepoints.
cpts(cpts_out)


}
\author{
Michael Hollaway

Rebecca Killick
}
