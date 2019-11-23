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

setMethod('parametersProcessing<-',signature = 'WorkflowParameters',function(parameters,value) {
  parameters@processing <- value
  return(parameters)
})

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

setMethod('parametersAnalysis<-',signature = 'WorkflowParameters',function(parameters,value) {
  parameters@analysis <- value
  return(parameters)
})

#' parametersAnnotation
#' @rdname parametersAnnotation
#' @description modify or return the Annotation parameters for a given WorkflowParameters object.
#' @param parameters an object of class Workflow Parameters 
#' @param value new parameters value
#' @export

setMethod('parametersAnnotation',signature = 'WorkflowParameters',
          function(parameters) {
            return(parameters@annotation)
          }
)

#' @rdname parametersAnnotation
#' @export

setMethod('parametersAnnotation<-',signature = 'WorkflowParameters',function(parameters,value) {
  parameters@annotation <- value
  return(parameters)
})