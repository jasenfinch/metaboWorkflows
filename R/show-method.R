#' show-WorkflowParameters
#' @description show method for WorkflowParameters class
#' @param object S4 object of class WorkflowParameters
#' @importFrom methods show
#' @export

setMethod('show',signature = 'WorkflowParameters',
          function(object){
            cat('Parameters for Workflow:',object@workflow)
          }
)

#' show-Workflow
#' @description show method for Workflows class
#' @param object S4 object of class Workflow
#' @importFrom methods show
#' @importFrom stringr str_c
#' @export

setMethod('show',signature = 'Workflow',
          function(object){
            cat('\n',object@logs[[1]],'\n',sep = '')
            cat('Workflow:',object@workflowParameters@workflow,'\n')
            cat('Completed Flags:',str_c(object@flags,collapse = ' '),'\n')
            cat('No. Samples:',length(object@files) - 1, '\n\n')
          }
)