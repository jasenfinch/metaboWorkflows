#' @importFrom callr r
#' @importFrom metaboMisc suitableParallelPlan

setMethod('doWorkflow',signature = 'Workflow',
          function(y){
            Workflow <- get(y@workflowParameters@workflow)
            elements <- y@workflowParameters@flags
            elements <- elements[!(elements %in% y@flags)]
            
            for (i in elements) {
              method <- Workflow(i)
              flag <- 'fail'
              try({
                res <- r(function(x,m){
                  requireNamespace('metaboWorkflows',quietly = TRUE)
                  metaboMisc::suitableParallelPlan()
                  try(m(x))
                },args = list(x = y,m = method),
                show = TRUE,
                spinner = FALSE)
                if (isS4(res)) {
                  y <- res
                  y@logs <- c(y@logs,list(date()))
                  names(y@logs)[length(y@logs)] <- i
                  y@flags <- c(y@flags,i)
                  flag <- 'success' 
                }
              })
              if (flag == 'fail') {
                cat('Failed at workflow element',i,'\n')
                return(y)
              }
              if (grepl('CheckPoint',i)) {
                return(y)
              }
            }
            return(y)
          }
)