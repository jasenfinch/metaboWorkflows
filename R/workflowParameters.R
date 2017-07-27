#' workflowParameters
#' @description Initiate default workflow parameters for a selected workflow.
#' @param workflow the workflow analysis to use. NULL prints the available workflows.
#' @importFrom binneRlyse binParameters
#' @importFrom metabolyseR analysisParameters
#' @importFrom MFassign assignmentParameters
#' @export

workflowParameters <- function(workflow = NULL){
  availWorkflows <- c('FIE_HRMSfingerprinting','FIE2')
  if (is.null(workflow)) {
    availWorkflows <- paste(availWorkflows,collapse = '\n\t\t\t')
    availWorkflows <- paste('\n\t\t\t',availWorkflows,sep = '')
    cat('\nAvailable Workflows:',availWorkflows,sep = '')
  } else {
    if (workflow %in% availWorkflows) {
      if (grepl('FIE',workflow)) {
        w <- 'FIE'
      }
      new('WorkflowParameters',
          workflow = workflow,
          processing = binParameters(),
          analysis = analysisParameters(),
          annotation = assignmentParameters(w)
      )
    }
  }
}