#' parametersAnnotation
#' @rdname parametersAnnotation
#' @description modify or return the Annotation parameters for a given WorkflowParameters object.
#' @param parameters an object of class Workflow Parameters 
#' @export

setMethod('parametersAnnotation',signature = 'WorkflowParameters',
          function(parameters) {
            return(parameters@annotation)
          }
)

#' @rdname parametersAnnotation
#' @export
`parametersAnnotation<-` <- function(parameters,value) {
  parameters@annotation <- value
  return(parameters)
}