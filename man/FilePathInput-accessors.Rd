% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input.R
\name{filePaths}
\alias{filePaths}
\alias{filePaths,FilePathInput-method}
\alias{filePaths<-}
\alias{filePaths<-,FilePathInput-method}
\alias{sampleInformation}
\alias{sampleInformation,FilePathInput-method}
\alias{sampleInformation<-}
\alias{sampleInformation<-,FilePathInput-method}
\title{\code{FilePathInput} get and set accessors}
\usage{
filePaths(x)

\S4method{filePaths}{FilePathInput}(x)

filePaths(x) <- value

\S4method{filePaths}{FilePathInput}(x) <- value

sampleInformation(x)

\S4method{sampleInformation}{FilePathInput}(x)

sampleInformation(x) <- value

\S4method{sampleInformation}{FilePathInput}(x) <- value
}
\arguments{
\item{x}{S4 object of class \code{FilePathInput}}

\item{value}{value to set}
}
\description{
Get and set methods for the \code{FilePathInput} S4 class.
}
\examples{
file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')

workflow_input <- inputFilePath(file_paths,sample_information)

## Return the input file paths
\dontrun{
filePaths(workflow_input)
}
## Set the input file paths
filePaths(workflow_input) <- c('a_file.mzML')

## Return the sample information 
sampleInformation(workflow_input)

## Set the sample information
sampleInformation(workflow_input) <- tibble::tibble(fileName = 'a_file.mzML')
}
