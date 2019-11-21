#' workflowParameters
#' @description Initiate default workflow parameters for a selected workflow.
#' @param workflow the workflow analysis to use. NULL prints the available workflows.
#' @param files supply vector of file paths for auto detection of parameters. Currently only enabled for the FIE_HRMSfingerprinting workflow
#' @param ... arguments to pass to binneR::detectParameters
#' @examples 
#' files <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes') 
#' info <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
#' 
#' wp <- workflowParameters('FIE-HRMS fingerprinting',files,info)
#' @importFrom binneR binParameters detectParameters
#' @importFrom profilePro profileParameters
#' @importFrom metabolyseR analysisParameters changeParameter getClusterType
#' @importFrom MFassign assignmentParameters
#' @importFrom parallel detectCores
#' @importFrom stringr str_detect
#' @importFrom tibble deframe
#' @importFrom dplyr select
#' @export

workflowParameters <- function(workflow, files, info, cls = 'class', ...){
  if (workflow %in% availableWorkflows(quiet = T,return = T)) {
    ap <- analysisParameters()
    ap <- changeParameter('reps', 10, ap)
    ap <- changeParameter('clusterType', getClusterType(), ap)
    ap <- changeParameter('nCores', detectCores() * 0.75, ap)
    
    i <- info %>%
      select(cls) %>%
      deframe()
    
    if (is.numeric(i)) {
      ap@preTreat <- list(QC = list(),
                          occupancyFilter = list(),
                          impute = list(),
                          transform = list())
    } else {
      
    }
    
    if (str_detect(workflow,'FIE-HRMS') | str_detect(workflow,'NSI-HRMS')) {
      if (is.null(files)) {
        bp <- binParameters()
      } else {
        bp <- detectParameters(files,...)
      }
      param <- new('WorkflowParameters',
                   workflow = workflow,
                   flags = workflowFlags(workflow),
                   processing = bp,
                   analysis = ap,
                   annotation = assignmentParameters('FIE'))
    }
    
    if (grepl('RP-LC-HRMS',workflow) | grepl('NP-LC-HRMS',workflow)) {
      w <- 'FIE'
      ap <- changeParameter('RSDthresh', 0.25, ap)
      if (grepl('RP',workflow)) {
        p <- profileParameters('LCMS-RP')
      }
      if (grepl('NP',workflow)) {
        p <- profileParameters('LCMS-NP')
      }
      
      if (grepl('RP-LC-HRMS',workflow)) {
        m <- 'RP-LC'
      }
      if (grepl('NP-LC-HRMS',workflow)) {
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
    
    if (grepl('GC-MS_profiling_deconvolution',workflow)) {
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
    
    files(param) <- files
    info(param) <- info
    
    return(param) 
  } else {
    stop('Workflow not recognised. Use availableWorkflows() to see available workflows.')
  }
}