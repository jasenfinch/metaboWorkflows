#' workflow
#' @rdname workflow
#' @description execute workflow based on specified worklow parameters and a file list.
#' @param parameters S4 object of class WorkflowParameters containing the workflow parameters.
#' @importFrom magrittr %>%
#' @importFrom methods new
#' @importFrom tibble tibble
#' @importFrom utils packageVersion
#' @examples 
#' \dontrun{
#' 
#' fp <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes') 
#' si <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
#' 
#' wp <- workflowParameters('FIE-HRMS fingerprinting',fp,si)
#' analysis <- workflow(wp)
#' }
#' @export

setMethod('workflow',signature = 'WorkflowParameters',
          function(parameters){
            if (grepl('BinParameters',class(parameters@processing))) {
              process <- new('Binalysis',
                             parameters@processing)
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
                            modelling = list(),
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
                      flags = character(),
                      files = files(parameters),
                      info = info(parameters),
                      workflowParameters = parameters,
                      processed = process,
                      analysed = analysis,
                      annotated = annotation)
            
            wf@logs$initialisation <- date()
            
            wf <- wf %>% doWorkflow()
            return(wf)
          }
)
