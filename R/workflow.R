#' workflow
#' @description execute workflow based on specified worklow parameters and a file list.
#' @param files list of file paths to raw data
#' @param info tibble containing sample information
#' @param parameters S4 object of class WorkflowParameters containing the workflow parameters.
#' @importFrom magrittr %>%
#' @importFrom methods new
#' @importFrom tibble tibble
#' @importFrom utils packageVersion
#' @examples 
#' \dontrun{
#' library(metaboData)
#' library(binneR)
#' 
#' files <- filePaths('FIE-HRMS','BdistachyonEcotypes') 
#' info <- runinfo('FIE-HRMS','BdistachyonEcotypes')
#' 
#' wp <- workflowParameters('FIE_HRMSfingerprinting')
#' wp@processing <- detectParameters(files)
#' analysis <- workflow(files, info, wp)
#' }
#' @export

workflow <- function(files,info,parameters){
  if (grepl('BinParameters',class(parameters@processing))) {
    process <- new('Binalysis',
                   binLog = character(),
                   binParameters = parameters@processing,
                   files = character(),
                   info = tibble(),
                   binnedData = list(),
                   accurateMZ = tibble()
    )
  }
  if (grepl('ProfileParameters',class(parameters@processing))) {
   process <- new('MetaboProfile',
                  log = list(),
                  files = character(),
                  processingParameters = parameters@processing,
                  Info = tibble(),
                  Data = list(),
                  processingResults = list()
                  ) 
  }
  
  analysis <- new('Analysis',
                  log = list(),
                  parameters = parameters@analysis,
                  rawData = new('AnalysisData'),
                  preTreated = new('AnalysisData'),
                  classification = tibble(),
                  featureSelection = tibble(),
                  correlations = tibble()
  )
  annotation <-   new('Assignment',
                      parameters = parameters@annotation,
                      data = tibble(),
                      correlations = tibble(),
                      preparedCorrelations = tibble(),
                      relationships = tibble(),
                      addIsoAssign = list(),
                      transAssign = list(),
                      assignments  = tibble()
  )
  
  wf <- new('Workflow',
            logs = list(),
            flags = character(),
            files = files,
            info = info,
            workflowParameters = parameters,
            processed = process,
            analysed = analysis,
            annotated = annotation)
  
  wf@logs$packageVersion <- packageVersion('metaboWorkflows')
  wf@logs$initialisation <- date()
  
  wf <- wf %>% doWorkflow()
  return(wf)
}
