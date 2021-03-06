% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allClasses.R
\docType{class}
\name{WorkflowParameters-class}
\alias{WorkflowParameters-class}
\title{WorkflowParameters}
\description{
An S4 class to store workflow parameters
}
\section{Slots}{

\describe{
\item{\code{workflow}}{character containing the workflow to use}

\item{\code{files}}{file paths for sampels to process}

\item{\code{info}}{tibble containing sample information}

\item{\code{flags}}{character vector containing the workflow elements to analyse}

\item{\code{processing}}{Processing object containing processing parameters for the specified workflow}

\item{\code{analysis}}{AnalysisParameters object containing the analysis parameters}

\item{\code{annotation}}{AssignmentParameters object containing the annotation parameters}
}}

\seealso{
\code{\link[metabolyseR]{AnalysisParameters-class}} \code{\link[MFassign]{AssignmentParameters-class}} \code{\link[binneR]{BinParameters-class}} \code{\link[profilePro]{ProfileParameters-class}}
}
