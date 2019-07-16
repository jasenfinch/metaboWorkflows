#' workflowFlags
#' @description Return possible flags for a given workflow.
#' @param w workflow name as returned by \code{workflowParameters()}
#' @examples 
#' workflowFlags('FIE_HRMSfingerprinting')
#' @export

workflowFlags <- function(w){
  get(w) %>%
    {.()} %>%
    names()
}

#' flags
#' @rdname flags
#' @description methods for setting and returning workflow flags
#' @param x S4 object of class WorkflowParameters or Workflow
#' @param value new flags
#' @export

setMethod('flags',signature = 'WorkflowParameters',
          function(x){
            x@flags
          }
)

#' @rdname flags
#' @export

setMethod('flags',signature = 'Workflow',
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