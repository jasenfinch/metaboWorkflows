#' workflowParameters
#' @description Initiate default workflow parameters for a selected workflow.
#' @param workflow the workflow analysis to use. NULL prints the available workflows.
#' @param fp list of file paths to raw data
#' @param si tibble containing sample information
#' @param cls info column containing class information to use as default for pre-treatment and modelling
#' @param QCidx QC sample label. QC processing skipped if label not present in column \code{cls}.
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
#' @importFrom metabolyseR analysisParameters changeParameter getClusterType modellingParameters
#' @importFrom MFassign assignmentParameters
#' @importFrom parallel detectCores
#' @importFrom stringr str_detect
#' @importFrom tibble deframe
#' @importFrom dplyr select mutate group_by_at summarise n filter
#' @export

workflowParameters <- function(workflow, fp, si, cls = 'class', QCidx = 'QC', ...){
  if (workflow %in% availableWorkflows(quiet = T,return = T)) {
    ap <- analysisParameters()
    ap <- changeParameter(ap,'cls',cls)
    
    i <- si %>%
      select(cls) %>%
      deframe()
    
    if (!(QCidx %in% i)) {
      ap@preTreat$QC <- NULL
    }
    
    if (is.numeric(i)) {
      si <- si %>%
        mutate(class1 = rep(1,nrow(si)))
      
      message('Created new sample information column "class1", suitable for pre-treatment with numeric meta data.')
      
      if (QCidx %in% i) {
        si$class1[(si[,cls] %>% deframe()) == QCidx] <- QCidx
      }
      
      ap <- changeParameter(ap,'cls','class1',elements = 'preTreat')
      ap@preTreat$impute <- list(
        all = list(
          occupancy = 2/3,
          parallel = "variables",
          nCores = detectCores() * 0.75,
          clusterType = getClusterType(),
          seed = 1234 
        )
      )
    } else {
      clsFreq <- si %>%
        group_by_at(cls) %>%
        summarise(Freq = n())
      
      if (nrow(clsFreq %>%
               filter(Freq > 5)) < (floor(length(unique(i)) / 2))) {
        message('Less than 50% of classes have > 5 replicates. Using ANOVA.')
        
        ap@modelling <- modellingParameters('anova')
      }
    }
    
    ap <- changeParameter(ap,'reps', 10)
    
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
      ap <- changeParameter(ap,'RSDthresh', 0.25)
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
      ap <- changeParameter(ap,'RSDthresh', 0.30)
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