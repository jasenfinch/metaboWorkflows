#' parametersAnalysis
#' @rdname parametersAnalysis
#' @description modify or return the Analysis parameters for a given WorkflowParameters object.
#' @param parameters an object of class Workflow Parameters 
#' @param value new parameters value
#' @export

setMethod('parametersAnalysis',signature = 'WorkflowParameters',
          function(parameters) {
            return(parameters@analysis)
          }
)

#' @rdname parametersAnalysis
#' @export
`parametersAnalysis<-` <- function(parameters,value) {
  parameters@analysis <- value
  return(parameters)
}