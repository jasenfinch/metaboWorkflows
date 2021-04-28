#' Available workflows
#' @description Return the available metabolomics workflows.
#' @return A character vector of available workflow names.
#' @examples 
#' availableWorkflows()
#' @export

availableWorkflows <- function(){
  c('FIE-HRMS fingerprinting')
}

setClassUnion('Input',members = c('FilePathInput','GroverInput'))

setClass('Workflow',
         contains = c('Project','Input'),
         slots = list(
           type = 'character',
           input = 'Input',
           targets = 'list'
         ),
         prototype = list(
           type = availableWorkflows()[1],
           targets = list()
         ))

setValidity('Workflow',function(object){
  
  if (!(type(object) %in% availableWorkflows())) {
    return('Workflow not found, run availableWorkflows() to see the available workflows.') 
  }
  
  return(TRUE)
})

#' @importFrom purrr flatten

setMethod('show',signature = 'Workflow',
          function(object){
            cat('Workflow: ',type(object),'\n\n')
            as(object,'Project') %>% 
              print()
            cat('\n')
            input(object) %>% 
              print()
            cat('\n\n')
            cat('# targets:',object %>% 
                  targets() %>% 
                  flatten() %>% 
                  length())
          })

#' `Workflow` class get and set methods
#' @rdname Workflow-accessors
#' @description Get and set methods for the `Workflow` S4 class.
#' @param x S4 object of class `Workflow`
#' @param value value to set

setMethod('type',signature = 'Workflow',
          function(x){
            x@type
          })

#' @rdname Workflow-accessors

setMethod('type<-',signature = 'Workflow',
          function(x,value){
            x@type <- value
            validObject(x)
            return(x)
          })

#' @rdname Workflow-accessors
#' @export

setGeneric('input',function(x)
  standardGeneric('input'))

#' @rdname Workflow-accessors

setMethod('input',signature = 'Workflow',
          function(x){
            x@input
          })

#' @rdname Workflow-accessors
#' @export

setGeneric('input<-',function(x,value)
  standardGeneric('input<-'))

#' @rdname Workflow-accessors

setMethod('input<-',signature = 'Workflow',
          function(x,value){
            x@input <- value
            return(x)
          })

#' @rdname Workflow-accessors
#' @export

setGeneric('targets',function(x)
  standardGeneric('targets'))

#' @rdname Workflow-accessors

setMethod('targets',signature = 'Workflow',
          function(x){
            x@targets
          })

#' @rdname Workflow-accessors
#' @export

setGeneric('targets<-',function(x,value)
  standardGeneric('targets<-'))

#' @rdname Workflow-accessors

setMethod('targets<-',signature = 'Workflow',
          function(x,value){
            x@targets <- value
            return(x)
          })

#' @rdname Workflow-accessors

setMethod('filePaths',signature = 'Workflow',
          function(x){
            x %>% 
              input() %>% 
              filePaths()
          })

#' @rdname Workflow-accessors

setMethod('filePaths<-',signature = 'Workflow',
          function(x,value){
            workflow_input <- input(x)
            filePaths(workflow_input) <- value
            input(x) <- workflow_input
            return(x)
          })

#' @rdname Workflow-accessors

setMethod('sampleInformation',signature = 'Workflow',
          function(x){
            x %>% 
              input() %>% 
              sampleInformation()
          })

#' @rdname Workflow-accessors

setMethod('sampleInformation<-',signature = 'Workflow',
          function(x,value){
            workflow_input <- input(x)
            sampleInformation(workflow_input) <- value
            input(x) <- workflow_input
            return(x)
          })


#' @rdname Workflow-accessors

setMethod('instrument',signature = 'Workflow',
          function(x){
            x %>% 
              input() %>% 
              instrument()
          })

#' @rdname Workflow-accessors

setMethod('instrument<-',signature = 'Workflow',
          function(x,value){
            workflow_input <- input(x)
            instrument(workflow_input) <- value
            input(x) <- workflow_input
            return(x)
          })

#' @rdname Workflow-accessors

setMethod('directory',signature = 'Workflow',
          function(x){
            x %>% 
              input() %>% 
              directory()
          })

#' @rdname Workflow-accessors

setMethod('directory<-',signature = 'Workflow',
          function(x,value){
            workflow_input <- input(x)
            directory(workflow_input) <- value
            input(x) <- workflow_input
            return(x)
          })

#' @rdname Workflow-accessors

setMethod('host',signature = 'Workflow',
          function(x){
            x %>% 
              input() %>% 
              host()
          })

#' @rdname Workflow-accessors

setMethod('host<-',signature = 'Workflow',
          function(x,value){
            workflow_input <- input(x)
            host(workflow_input) <- value
            input(x) <- workflow_input
            return(x)
          })

#' @rdname Workflow-accessors

setMethod('port',signature = 'Workflow',
          function(x){
            x %>% 
              input() %>% 
              port()
          })

#' @rdname Workflow-accessors

setMethod('port<-',signature = 'Workflow',
          function(x,value){
            workflow_input <- input(x)
            port(workflow_input) <- value
            input(x) <- workflow_input
            return(x)
          })

#' @rdname Workflow-accessors

setMethod('auth',signature = 'Workflow',
          function(x){
            x %>% 
              input() %>% 
              auth()
          })

#' @rdname Workflow-accessors

setMethod('auth<-',signature = 'Workflow',
          function(x,value){
            workflow_input <- input(x)
            auth(workflow_input) <- value
            input(x) <- workflow_input
            return(x)
          })

#' Define a metabolomics workflow
#' @rdname defineWorkflow
#' @description Define a metabolomics data analysis workflow for a given input type and workflow name.
#' @param input S4 object of class `GroverInput` or `FilePathInput`
#' @param workflow 
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

setMethod('defineWorkflow',signature = 'GroverInput',function(input,workflow,project_name,...){
  workflow <- match.arg(workflow,
                        choices = availableWorkflows())
  
  workflow_project <- defineProject(project_name = project_name,
                                    ...)
  workflow <- new('Workflow',
                  workflow_project,
                  type = workflow,
                  input = input,
                  targets = list())
  
  targets(workflow) <- workflowTargets(workflow)
  
  return(workflow)
  
})

#' @rdname defineWorkflow

setMethod('defineWorkflow',signature = 'FilePathInput',function(input,workflow,project_name,...){
  workflow <- match.arg(workflow,
                        choices = availableWorkflows())
  
  workflow_project <- defineProject(project_name = project_name,
                                    ...)
  workflow <- new('Workflow',
                  workflow_project,
                  type = workflow,
                  input = input,
                  targets = list())
  
  targets(workflow) <- workflowTargets(workflow)
  
  return(workflow)
})
