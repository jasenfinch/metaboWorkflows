
checkWorkflow <- function(workflow){
  match.arg(workflow,
            choices = availableWorkflows())
}
