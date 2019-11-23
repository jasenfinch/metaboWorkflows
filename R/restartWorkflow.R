#' restartWorkflow
#' @rdname restartWorkflow
#' @description Restart a workflow after initial analysis with \code{workflow} following a check point or error. 
#' Will resume the analysis from the break or failiure point. 
#' @param analysis an S4 object of class Workflow
#' @export

setMethod('restartWorkflow',signature = 'Workflow',
          function(analysis){
            doWorkflow(analysis)
          }
)