#' availableWorkflows
#' @description Get available workflows.
#' @param quiet show available workflows
#' @param return return character vector of available workflows
#' @examples 
#' availableWorkflows()
#' @export

availableWorkflows <- function(quiet = F, return = F){
  availWorkflows <- c('FIE-HRMS fingerprinting','NSI-HRMS fingerprinting','RP-LC-HRMS profiling','NP-LC-HRMS profiling','GC-MS profiling deconvolution')
  
  if (isFALSE(quiet)) {
    aw <- paste(availWorkflows,collapse = '\n\t\t\t')
    aw <- paste('\n\t\t\t',aw,sep = '')
    message('Available Workflows:',aw,sep = '')
    message("\nSee vignette('metaboWorkflows-usage') for details on these workflows.")
  }
    if (isTRUE(return)) {
      return(availWorkflows) 
    }
}