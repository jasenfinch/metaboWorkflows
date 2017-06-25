#' workflowParameters
#' @importFrom binneRlyse binParameters
#' @importFrom metabolyseR analysisParameters
#' @export

workflowParameters <- function(technique = NULL){
  
if (is.null(technique)) {
  cat('\nAvailable Techniques:','\t FIE')
}
  if (technique == 'FIE') {
    new('WorkflowParameters',
        technique = technique,
        processing = binParameters(),
        analysis = analysisParameters()
        )
  }
}