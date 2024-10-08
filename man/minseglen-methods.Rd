% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_gampelt.R
\name{minseglen}
\alias{minseglen}
\alias{minseglen-methods}
\alias{minseglen,GAM_PELT-method}
\alias{minseglen,GAM_PELT,minseglen-methods}
\title{Generic Function - minseglen}
\usage{
minseglen(object)

\S4method{minseglen}{GAM_PELT}(object)
}
\arguments{
\item{object}{Object of class \code{GAM_PELT}}
}
\value{
The minimum segment length (number of observations between changepoints) used in the analysis.
}
\description{
Generic Function
}
\details{
Generic Function
}
\examples{
#Generate a new object of class GAM_PELT
x <- methods::new('GAM_PELT')
#Retrieve the minseglen slot from x.
minseglen(x)

}
\author{
Michael Hollaway

Rebecca Killick
}
