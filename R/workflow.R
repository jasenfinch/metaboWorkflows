#' workflow
#' @importFrom dplyr tbl_df
#' @importFrom magrittr %>%
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
             info = tbl_df(data.frame()),
             binnedData = list(),
             accurateMZ = tbl_df(data.frame())
  )
  analysis <- new('Analysis',
                  log = list(),
                  parameters = parameters@analysis,
                  rawData = list(),
                  preTreated = list(),
                  classification = list(),
                  featureSelection = list(),
                  correlations = tbl_df(data.frame())
  )
  
  wf <- new('Workflow',
            logs = list(),
            files = files,
            workflowParameters = parameters,
            processed = bin,
            analysed = analysis)
  technique <- get(parameters@technique)
  wf <- wf %>% technique()
  return(wf)
}