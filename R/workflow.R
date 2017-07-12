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
#'        workflowParameters('FIE')
#'    )}
#' @export

workflow <- function(files,parameters){
  bin <- new('Binalysis',
             binLog = character(),
             binParameters = parameters@processing,
             files = character(),
             info = tibble(),
             binnedData = list(),
             accurateMZ = tibble()
  )
  analysis <- new('Analysis',
                  log = list(),
                  parameters = parameters@analysis,
                  rawData = list(),
                  preTreated = list(),
                  classification = tibble(),
                  featureSelection = tibble(),
                  correlations = tibble()
  )
  annotation <-   new('Annotation',
                      parameters = parameters@annotation,
                      correlations = tibble(),
                      relationships = tibble(),
                      addIsoAssign = list(),
                      transAssign = list(),
                      assignments  = tibble()
  )
  
  wf <- new('Workflow',
            logs = list(),
            files = files,
            workflowParameters = parameters,
            processed = bin,
            analysed = analysis,
            annotated = annotation)
  workflow <- get(parameters@workflow)
  wf <- wf %>% workflow()
  return(wf)
}