% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate.R
\name{generateWorkflow}
\alias{generateWorkflow}
\alias{generateWorkflow,Workflow-method}
\title{Generate a workflow project directory}
\usage{
generateWorkflow(workflow, start = TRUE)

\S4method{generateWorkflow}{Workflow}(workflow, start = TRUE)
}
\arguments{
\item{workflow}{S4 object of class \code{Workflow}}

\item{start}{TRUE/FALSE. Automatically activate the project in a new RStudio session after creation}
}
\description{
Generate a workflow project directory from a workflow definition.
}
\examples{
\dontrun{
file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')

workflow_input <- inputFilePath(file_paths,sample_information)

workflow_definition <- defineWorkflow(workflow_input,
                                      'FIE-HRMS fingerprinting',
                                      'Example project')
                                      
generateWorkflow(workflow_definition)
}
}
