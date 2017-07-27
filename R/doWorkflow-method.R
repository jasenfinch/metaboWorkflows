
setMethod('doWorkflow',signature = 'Workflow',
          function(y){
            Workflow <- get(y@workflowParameters@workflow)
            elements <- names(Workflow())
            elements <- elements[!(elements %in% y@flags)]
            
            for (i in elements) {
              method <- Workflow(i)
              flag <- 'fail'
              try({
                y <- method(y)
                y@logs <- list(date())
                y@flags <- c(y@flags,i)
                flag <- 'success'
              })
              if (flag == 'fail') {
                cat('Failed at workflow element',i)
                return(y)
              # stop('Failed at workflow element ',i)
              }
            }
            return(y)
          }
)