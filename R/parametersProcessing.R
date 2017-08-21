#' parametersProcessing
#' @rdname parametersProcessing
#' @description modify or return the processing parameters for a given WorkflowParameters object.
#' @param parameters an object of class Workflow Parameters 
#' @param value new parameters value
#' @export

setMethod('parametersProcessing',signature = 'WorkflowParameters',
          function(parameters) {
            return(parameters@processing)
          }
)

#' @rdname parametersProcessing
#' @export
`parametersProcessing<-` <- function(parameters,value) {
  parameters@processing <- value
  return(parameters)
}