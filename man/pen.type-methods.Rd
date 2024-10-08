% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_gampelt.R
\name{pen.type}
\alias{pen.type}
\alias{pen.type-methods}
\alias{pen.type,GAM_PELT-method}
\alias{pen.type,GAM_PELT,pen.type-methods}
\title{Generic Function - pen.type}
\usage{
pen.type(object)

\S4method{pen.type}{GAM_PELT}(object)
}
\arguments{
\item{object}{Object of class \code{GAM_PELT}}
}
\value{
The penalty type specified in the analysis.
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
#Retrieve the pen.type slot from x.
pen.type(x)

}
\author{
Michael Hollaway

Rebecca Killick
}
