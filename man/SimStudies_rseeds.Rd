% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{SimStudies_rseeds}
\alias{SimStudies_rseeds}
\title{Random Seeds for simulation studies.}
\format{
\subsection{'SimStudies_rseeds'}{

A data frame with 170 rows and 12 columns:
\describe{
\item{SCEN}{Scenario}
\item{BATCH}{Batch}
\item{Replicate_1}{Random seed for 1st replicate in batch XX}
\item{Replicate_2}{Random seed for 2nd replicate in batch XX}
\item{Replicate_3}{Random seed for 3rd replicate in batch XX}
\item{Replicate_4}{Random seed for 4th replicate in batch XX}
\item{Replicate_5}{Random seed for 5th replicate in batch XX}
\item{Replicate_6}{Random seed for 6th replicate in batch XX}
\item{Replicate_7}{Random seed for 7th replicate in batch XX}
\item{Replicate_8}{Random seed for 8th replicate in batch XX}
\item{Replicate_9}{Random seed for 9th replicate in batch XX}
\item{Replicate_10}{Random seed for 10th replicate in batch XX}
}
}
}
\source{
This dataset was generated as part of the simulation studies to allow reproducibility of the simulations.
}
\usage{
SimStudies_rseeds
}
\description{
Random seeds required to reproduce the datasets used in the simulation studies for the GAM-PELT paper.
100 replicates were run for each scenario in batches of 10. E.g. To reproduce Scenario 4b replicate 4
extract the random seed from the following location in the data frame (SCEN='4b', BATCH=1, Replicate=Replicate_4).
Or to reproduce Scenario 1a replicate 24 extract the random seed as follows (SCEN='1a', BATCH=3, Replicate=Replicate_4).
}
\keyword{datasets}
