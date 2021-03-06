% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allGenerics.R, R/restartWorkflow.R
\name{restartWorkflow}
\alias{restartWorkflow}
\alias{restartWorkflow,Workflow-method}
\title{restartWorkflow}
\usage{
restartWorkflow(analysis)

\S4method{restartWorkflow}{Workflow}(analysis)
}
\arguments{
\item{analysis}{an S4 object of class Workflow}
}
\description{
Restart a workflow after initial analysis with \code{workflow} following a check point or error. 
Will resume the analysis from the break or failiure point.
}
