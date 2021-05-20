#' Define a metabolomics workflow
#' @rdname defineWorkflow
#' @description Define a metabolomics data analysis workflow for a given input type and workflow name.
#' @param input S4 object of class `GroverInput` or `FilePathInput`
#' @param workflow workflow name as returned by `availableWorkflows()`
#' @param project_name the project name
#' @param ... arguments to pass to `defineProject`
#' @return An S4 object of class `Workflow`
#' @examples 
#' file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
#' sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
#'
#' workflow_input <- filePathInput(file_paths,sample_information)
#' 
#' workflow_definition <- defineWorkflow(workflow_input,
#'                                       'FIE-HRMS fingerprinting',
#'                                       'Example project')
#' workflow_definition
#' @export

setGeneric('defineWorkflow',function(input,workflow,project_name,...)
  standardGeneric('defineWorkflow'))

#' @rdname defineWorkflow
#' @importFrom methods new

setMethod('defineWorkflow',signature = 'GroverInput',function(input,workflow,project_name,...){
  workflow <- checkWorkflow(workflow)
  
  workflow_project <- defineProject(project_name = project_name,
                                    ...)
  workflow <- new('Workflow',
                  workflow_project,
                  type = workflow,
                  input = input,
                  targets = list())
  
  targets(workflow) <- targetsWorkflow(workflow)
  
  return(workflow)
  
})

#' @rdname defineWorkflow

setMethod('defineWorkflow',signature = 'FilePathInput',function(input,workflow,project_name,...){
  workflow <- workflow <- checkWorkflow(workflow)
  
  workflow_project <- defineProject(project_name = project_name,
                                    ...)
  workflow <- new('Workflow',
                  workflow_project,
                  type = workflow,
                  input = input,
                  targets = list())
  
  targets(workflow) <- targetsWorkflow(workflow)
  
  return(workflow)
})
