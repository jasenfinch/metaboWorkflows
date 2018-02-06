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
#' @importFrom crayon bold red blue
#' @export

setMethod('show',signature = 'Workflow',
          function(object){
            cat('\nmetaboWorkflows Workflow\n')
            cat('Analysed by package version',bold(red(object@logs$packageVersion[1])))
            cat('\n',object@logs$initialisation,'\n',sep = '')
            cat('Workflow:',bold(blue(object@workflowParameters@workflow)),'\n')
            cat('Completed Flags:',str_c(object@flags,collapse = ' '),'\n')
            cat('No. Samples:',length(object@files[[1]]), '\n\n')
          }
)