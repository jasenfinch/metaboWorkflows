#' workflow
#' @description execute workflow based on specified worklow parameters and a file list.
#' @param files character vector of file paths to raw data and info files
#' @param parameters S4 object of class WorkflowParameters containing the workflow parameters.
#' @importFrom magrittr %>%
#' @importFrom methods new
#' @importFrom tibble tibble
#' @examples 
#' \dontrun{
#' analysis <- workflow(
#'    list.files(
#'        system.file(
#'            'DataSets/FIE-HRMS/BdistachyonEcotypes',
#'            package = 'metaboData'),
#'        full.names = TRUE), 
#'        workflowParameters('FIE_HRMSfingerprinting')
#'    )}
#' @export

workflow <- function(files,parameters){
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
                  log = character(),
                  files = list(),
                  processingParameters = parameters@processing,
                  Info = tibble(),
                  Data = list(),
                  processingResults = list()
                  ) 
  }
  
  analysis <- new('Analysis',
                  log = list(),
                  parameters = parameters@analysis,
                  rawData = list(),
                  preTreated = list(),
                  classification = tibble(),
                  featureSelection = tibble(),
                  correlations = tibble()
  )
  annotation <-   new('Assignment',
                      parameters = parameters@annotation,
                      correlations = tibble(),
                      relationships = tibble(),
                      addIsoAssign = list(),
                      transAssign = list(),
                      assignments  = tibble()
  )
  
  if (!(is.list(files))) {
    files <- list(files)
  }
  
  wf <- new('Workflow',
            logs = list(),
            flags = character(),
            files = files,
            workflowParameters = parameters,
            processed = process,
            analysed = analysis,
            annotated = annotation)
  
  wf <- wf %>% doWorkflow()
  return(wf)
}