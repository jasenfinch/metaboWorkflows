#' workflowParameters
#' @description Initiate default workflow parameters for a selected workflow.
#' @param workflow the workflow analysis to use. NULL prints the available workflows.
#' @param fp list of file paths to raw data
#' @param si tibble containing sample information
#' @param cls info column containing class information to use as default for pre-treatment and modelling
#' @param ... arguments to pass to binneR::detectParameters
#' @examples 
#' \dontrun{
#' fp <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes') 
#' si <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
#' 
#' wp <- workflowParameters('FIE-HRMS fingerprinting',fp,si)
#' }
#' @importFrom binneR binParameters detectParameters
#' @importFrom profilePro profileParameters
#' @importFrom metabolyseR analysisParameters changeParameter getClusterType
#' @importFrom MFassign assignmentParameters
#' @importFrom parallel detectCores
#' @importFrom stringr str_detect
#' @importFrom tibble deframe
#' @importFrom dplyr select
#' @export

workflowParameters <- function(workflow, fp, si, cls = 'class', ...){
  if (workflow %in% availableWorkflows(quiet = T,return = T)) {
    ap <- analysisParameters()
    
    i <- si %>%
      select(cls) %>%
      deframe()
    
    if (is.numeric(i)) {
      ap@preTreat <- list(QC = list(),
                          occupancyFilter = list(),
                          impute = list(),
                          transform = list())
    } else {
      
    }
    
    ap <- changeParameter('reps', 10, ap)
    ap <- changeParameter('clusterType', getClusterType(), ap)
    ap <- changeParameter('nCores', detectCores() * 0.75, ap)
    ap <- changeParameter('cls',cls,ap)
    
    if (str_detect(workflow,'FIE-HRMS') | str_detect(workflow,'NSI-HRMS')) {
      bp <- detectParameters(fp,...)
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
    
    files(param) <- fp
    info(param) <- si
    
    return(param) 
  } else {
    stop('Workflow not recognised. Use availableWorkflows() to see available workflows.')
  }
}