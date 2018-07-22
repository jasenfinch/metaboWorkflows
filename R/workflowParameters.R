#' workflowParameters
#' @description Initiate default workflow parameters for a selected workflow.
#' @param workflow the workflow analysis to use. NULL prints the available workflows.
#' @importFrom binneR binParameters
#' @importFrom profilePro profileParameters
#' @importFrom metabolyseR analysisParameters changeParameter
#' @importFrom MFassign assignmentParameters
#' @export

workflowParameters <- function(workflow = NULL){
  availWorkflows <- c('FIE_HRMSfingerprinting','FIE_HRMSfingerprinting2','RP_LC_HRMSprofiling','NP_LC_HRMSprofiling','GC_MSprofilingDeconvolution')
  if (is.null(workflow)) {
    availWorkflows <- paste(availWorkflows,collapse = '\n\t\t\t')
    availWorkflows <- paste('\n\t\t\t',availWorkflows,sep = '')
    cat('\nAvailable Workflows:',availWorkflows,sep = '')
  } else {
    if (workflow %in% availWorkflows) {
      
      if (grepl('FIE',workflow) & grepl('2',workflow)) {
        param <- new('WorkflowParameters',
                     workflow = workflow,
                     processing = binParameters(),
                     analysis = analysisParameters(),
                     annotation = assignmentParameters('FIE'))
      } else {
        w <- 'FIE'
        param <- new('WorkflowParameters',
                     workflow = workflow,
                     processing = binParameters(),
                     analysis = analysisParameters(),
                     annotation = new('AssignmentParameters')
        )
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
        
        param <- new('WorkflowParameters',
                     workflow = workflow,
                     processing = p,
                     analysis = ap,
                     annotation = assignmentParameters(w)
        )
      }
      
      if (grepl('GC_MSprofilingDeconvolution',workflow)) {
        w <- 'FIE'
        ap <- analysisParameters()
        ap <- changeParameter('RSDthresh', 0.25, ap)
        p <- profileParameters('GCMS-eRah')
        
        param <- new('WorkflowParameters',
                     workflow = workflow,
                     processing = p,
                     analysis = ap,
                     annotation = assignmentParameters(w)
        )
      }
      
      return(param) 
    }
  }
}