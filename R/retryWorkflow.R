#' retryWorkflow
#' @description Retry a workflow after initial analysis with \code{workflow} following an error. 
#' Will resume the analysis from the point of error. 
#' @param analysis an S4 object of class Workflow
#' @export

retryWorkflow <- function(analysis){
  doWorkflow(analysis)
}