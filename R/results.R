#' resultsParameters
#' @description return parameters from a Workflow object
#' @param object S4 object of class Workflow
#' @export

setMethod('resultsParameters', signature = 'Workflow',
          function(object) {
            return(object@workflowParameters)
          }
)

#' resultsProcessing
#' @description return processing results from a Workflow object
#' @param object S4 object of class Workflow
#' @export

setMethod('resultsProcessing', signature = 'Workflow',
          function(object) {
            return(object@processed)
          }
)

#' resultsAnalysis
#' @description return analysis results from a Workflow object
#' @param object S4 object of class Workflow
#' @export

setMethod('resultsAnalysis', signature = 'Workflow',
          function(object) {
            return(object@analysed)
          }
)

#' resultsAnnotation
#' @description return annotation results from a Workflow object
#' @param object S4 object of class Workflow
#' @export

setMethod('resultsAnnotation', signature = 'Workflow',
          function(object) {
            return(object@annotated)
          }
)