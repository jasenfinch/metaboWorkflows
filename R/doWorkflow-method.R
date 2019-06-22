#' @importFrom callr r

setMethod('doWorkflow',signature = 'Workflow',
          function(y){
            Workflow <- get(y@workflowParameters@workflow)
            elements <- y@workflowParameters@flags
            elements <- elements[!(elements %in% y@flags)]
            
            for (i in elements) {
              method <- Workflow(i)
              flag <- 'fail'
              try({
                y <- r(function(x,m){
                  suppressPackageStartupMessages(library(metaboWorkflows))
                  m(x)
                },args = list(x = y,m = method),show = TRUE)
                y@logs <- c(y@logs,list(date()))
                names(y@logs)[length(y@logs)] <- i
                y@flags <- c(y@flags,i)
                flag <- 'success'
              })
              if (flag == 'fail') {
                cat('Failed at workflow element',i)
                return(y)
              }
              if (grepl('CheckPoint',i)) {
                return(y)
              }
            }
            return(y)
          }
)