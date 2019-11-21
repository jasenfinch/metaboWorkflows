#' show-WorkflowParameters
#' @description show method for WorkflowParameters class
#' @param object S4 object of class WorkflowParameters
#' @importFrom methods show
#' @export

setMethod('show',signature = 'WorkflowParameters',
          function(object){
            cat('Parameters for Workflow:',object@workflow,'\n')
            
            cat(blue('\nProcessing paramters:'))
            print(object@processing)
            
            cat(blue('\nAnalysis paramters:\n'))
            print(object@analysis)
            
            cat(blue('\nAnnotation paramters:'))
            print(object@annotation)
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
            cat(str_c('\n',blue('metaboWorkflows'),' ',red(str_c('v',object@logs$packageVersion %>% as.character())),'\n'))
            cat('\n',object@logs$initialisation,'\n',sep = '')
            cat('Workflow:',bold(blue(object@workflowParameters@workflow)),'\n')
            cat('Completed Flags:',str_c(object@flags,collapse = ' '),'\n')
            cat('No. Samples:',length(object@files), '\n\n')
          }
)