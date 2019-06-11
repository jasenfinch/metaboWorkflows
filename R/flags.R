#' workflowFlags
#' @description Return possible flags for a given workflow.
#' @param w workflow name as returned by \code{workflowParameters()}
#' @examples 
#' workFlowFlags('FIE_HRMSfingerprinting')
#' @export

workflowFlags <- function(w){
  get(w) %>%
    {.()} %>%
    names()
}

#' flags
#' @rdname flags
#' @description methods for setting and returning workflow flags
#' @param x S4 object of class WorkflowParameters
#' @export

setMethod('flags',signature = 'WorkflowParameters',
          function(x){
            x@flags
          }
)

#' @rdname flags
#' @export

`flags<-` <- function(x,value){
  x@flags <- value
  return(x)
}