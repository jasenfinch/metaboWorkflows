#' workflowParameters
#' @description Initiate default workflow parameters for a selected workflow.
#' @param workflow the workflow analysis to use. NULL prints the available workflows.
#' @param files supply vector of file paths for auto detection of parameters. Currently only enabled for the FIE_HRMSfingerprinting workflow
#' @importFrom binneR binParameters detectParameters
#' @importFrom profilePro profileParameters
#' @importFrom metabolyseR analysisParameters changeParameter getClusterType
#' @importFrom MFassign assignmentParameters
#' @importFrom parallel detectCores
#' @export

workflowParameters <- function(workflow = NULL, files = NULL){
  availWorkflows <- c('FIE_HRMSfingerprinting','RP_LC_HRMSprofiling','NP_LC_HRMSprofiling','GC_MSprofilingDeconvolution')
  if (is.null(workflow)) {
    availWorkflows <- paste(availWorkflows,collapse = '\n\t\t\t')
    availWorkflows <- paste('\n\t\t\t',availWorkflows,sep = '')
    cat('\nAvailable Workflows:',availWorkflows,sep = '')
  } else {
    if (workflow %in% availWorkflows) {
      
      ap <- analysisParameters()
      ap <- changeParameter('reps', 10, ap)
      ap <- changeParameter('perm', 1000, ap)
      ap <- changeParameter('clusterType', getClusterType(), ap)
      ap <- changeParameter('nCores', detectCores() * 0.75, ap)
      
      if (grepl('FIE',workflow)) {
        if (is.null(files)) {
          bp <- binParameters()
        } else {
          bp <- detectParameters(files)
        }
        param <- new('WorkflowParameters',
                     workflow = workflow,
                     flags = workflowFlags(workflow),
                     processing = bp,
                     analysis = ap,
                     annotation = assignmentParameters('FIE'))
      }
      
      if (grepl('RP_LC_HRMS',workflow) | grepl('NP_LC_HRMS',workflow)) {
        w <- 'FIE'
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