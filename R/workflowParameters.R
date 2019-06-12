#' workflowParameters
#' @description Initiate default workflow parameters for a selected workflow.
#' @param workflow the workflow analysis to use. NULL prints the available workflows.
#' @importFrom binneR binParameters
#' @importFrom profilePro profileParameters
#' @importFrom metabolyseR analysisParameters changeParameter
#' @importFrom MFassign assignmentParameters
#' @export

workflowParameters <- function(workflow = NULL){
  availWorkflows <- c('FIE_HRMSfingerprinting','RP_LC_HRMSprofiling','NP_LC_HRMSprofiling','GC_MSprofilingDeconvolution')
  if (is.null(workflow)) {
    availWorkflows <- paste(availWorkflows,collapse = '\n\t\t\t')
    availWorkflows <- paste('\n\t\t\t',availWorkflows,sep = '')
    cat('\nAvailable Workflows:',availWorkflows,sep = '')
  } else {
    if (workflow %in% availWorkflows) {
      
      if (grepl('FIE',workflow)) {
        param <- new('WorkflowParameters',
                     workflow = workflow,
                     flags = workflowFlags(workflow),
                     processing = binParameters(),
                     analysis = analysisParameters(),
                     annotation = assignmentParameters('FIE'))
      }
      
      if (grepl('RP_LC_HRMS',workflow) | grepl('NP_LC_HRMS',workflow)) {
        w <- 'FIE'
        ap <- analysisParameters()
        ap <- changeParameter('RSDthresh', 0.25, ap)
        if (grepl('RP',workflow)) {
          p <- profileParameters('LCMS-RP')
        }
        if (grepl('NP',workflow)) {
          p <- profileParameters('LCMS-NP')
        }
        
        if (grepl('RP_LC_HRMS',workflow)) {
          m <- 'RP-LC'
        }
        if (grepl('NP_LC_HRMS',workflow)) {
          m <- 'NP-LC'
        }
        
        param <- new('WorkflowParameters',
                     workflow = workflow,
                     flags = workflowFlags(workflow),
                     processing = p,
                     analysis = ap,
                     annotation = assignmentParameters(m)
        )
      }
      
      if (grepl('GC_MSprofilingDeconvolution',workflow)) {
        w <- 'FIE'
        ap <- analysisParameters()
        ap <- changeParameter('RSDthresh', 0.30, ap)
        p <- profileParameters('GCMS-eRah')
        
        param <- new('WorkflowParameters',
                     workflow = workflow,
                     flags = workflowFlags(workflow),
                     processing = p,
                     analysis = ap,
                     annotation = assignmentParameters(w)
        )
      }
      
      return(param) 
    }
  }
}