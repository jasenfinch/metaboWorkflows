#' Available workflows
#' @description Return the available metabolomics workflows.
#' @return A character vector of available workflow names.
#' @examples 
#' availableWorkflows()
#' @export

availableWorkflows <- function(){
  c('FIE-HRMS fingerprinting',
    'NSI-HRMS fingerprinting',
    'RP-LC-HRMS profiling',
    'NP-LC-HRMS profiling',
    'GC-MS profiling')
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
#' @importFrom methods as

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
#' @examples 
#' ## Define a workflow with file path input
#' file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
#' sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
#'
#' workflow_input <- inputFilePath(file_paths,sample_information)
#' 
#' workflow_definition <- defineWorkflow(workflow_input,
#'                                       'FIE-HRMS fingerprinting',
#'                                       'Example project')
#' 
#' ## Return the workflow type
#' type(workflow_definition)
#' 
#' ## Set the workflow type
#' type(workflow_definition) <- "RP-LC-HRMS profiling" 
#' 
#' ## Return the workflow input
#' input(workflow_definition)
#' 
#' ## Set the workflow input
#' \dontrun{
#' input(workflow_definition) <- inputGrover(instrument = 'An_instrument',
#'                                           directory = 'Experiment_directory',
#'                                           host = 'a.grover.host',
#'                                           port = 80,
#'                                           auth = '1234')
#' }
#' 
#' ## Return the workflow targets for the input module
#' targets(workflow_definition)$input
#' 
#' ## Set the workflow targets
#' targets(workflow_definition) <- list(
#'                                      a_module = list(
#'                                      a_target =  target('a_target',
#'                                                         1 + 1,
#'                                                         args = list(memory = 'persistent'), 
#'                                                         comment = 'A target')
#'                                                )
#'                                      )
#' 
#' ## Return the workflow modules
#' modules(workflow_definition)
#' 
#' ## Return the workflow file paths
#' \dontrun{
#' filePaths(workflow_definition)
#' }
#' 
#' ## Set the workflow file paths
#' filePaths(workflow_definition) <- 'a_file.mzML'
#' 
#' ## Return the workflow sample information
#' sampleInformation(workflow_definition)
#' 
#' ## Set the workflow sample information
#' sampleInformation(workflow_definition) <- tibble::tibble(fileName = 'a_file.mzML')
#' 
#' ## Define a workflow with grover input
#' workflow_input <- inputGrover(instrument = 'An_instrument',
#'                               directory = 'Experiment_directory',
#'                               host = 'a.grover.host',
#'                               port = 80,
#'                               auth = '1234')
#' 
#' workflow_definition <- defineWorkflow(workflow_input,
#'                                       'FIE-HRMS fingerprinting',
#'                                       'Example project')
#' 
#' ## Return the workflow instrument
#' instrument(workflow_definition)
#' 
#' ## Set the workflow Instrument
#' instrument(workflow_definition) <- 'A_different_instrument'
#' 
#' ## Return the workflow directory
#' directory(workflow_definition)
#' 
#' ## Set the workflow directory
#' directory(workflow_definition) <- 'Another_experiment'
#' 
#' ## Return the workflow host
#' host(workflow_definition)
#' 
#' ## Set the workflow_host
#' host(workflow_definition) <- 'a.new.host'
#' 
#' ## Return the workflow port
#' port(workflow_definition)
#' 
#' ## Set the workflow port
#' port(workflow_definition) <- 81
#' 
#' ## Return the workflow auth
#' auth(workflow_definition)
#' 
#' ## Set the workflow auth
#' auth(workflow_definition) <- 'abcd'

setMethod('type',signature = 'Workflow',
          function(x){
            x@type
          })

#' @rdname Workflow-accessors
#' @importFrom methods validObject

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
#' @export

setGeneric('modules',function(x)
  standardGeneric('modules'))

#' @rdname Workflow-accessors

setMethod('modules',signature = 'Workflow',
          function(x){
            x %>% 
              targets() %>% 
              names()
          })

#' @rdname Workflow-accessors

setMethod('filePaths',signature = 'Workflow',
          function(x){
            workflow_input <- x %>% 
              input()
            
            if (class(workflow_input) == 'FilePathInput') {
              filePaths(workflow_input) 
            } else {
              stop('File paths only available for file path input.',call. = FALSE)
            }
          })

#' @rdname Workflow-accessors

setMethod('filePaths<-',signature = 'Workflow',
          function(x,value){
            workflow_input <- input(x)
            
            if (class(workflow_input) == 'FilePathInput') {
              filePaths(workflow_input) <- value
              input(x) <- workflow_input 
            } else {
              stop('File paths can only be set for file path input.',call. = FALSE)
            }
            
            return(x)
          })

#' @rdname Workflow-accessors

setMethod('sampleInformation',signature = 'Workflow',
          function(x){
            workflow_input <- x %>% 
              input()
            
            if (class(workflow_input) == 'FilePathInput') {
              sampleInformation(workflow_input) 
            } else {
              stop('Sample information only available for file path input.',call. = FALSE)
            }
          })

#' @rdname Workflow-accessors

setMethod('sampleInformation<-',signature = 'Workflow',
          function(x,value){
            
            workflow_input <- input(x)
            
            if (class(workflow_input) == 'FilePathInput') {
              sampleInformation(workflow_input) <- value
              input(x) <- workflow_input 
            } else {
              stop('Sample information can only be set for file path input.',call. = FALSE)
            }
            
            return(x)
          })


#' @rdname Workflow-accessors

setMethod('instrument',signature = 'Workflow',
          function(x){
            workflow_input <- x %>% 
              input()
            
            if (class(workflow_input) == 'GroverInput') {
              instrument(workflow_input) 
            } else {
              stop('Instrument only available for grover input.',call. = FALSE)
            }
          })

#' @rdname Workflow-accessors

setMethod('instrument<-',signature = 'Workflow',
          function(x,value){
            
            workflow_input <- input(x)
            
            if (class(workflow_input) == 'GroverInput') {
              instrument(workflow_input) <- value
              input(x) <- workflow_input 
            } else {
              stop('Instrument can only be set for grover input.',call. = FALSE)
            }
            
            return(x)
          })

#' @rdname Workflow-accessors

setMethod('directory',signature = 'Workflow',
          function(x){
            workflow_input <- x %>% 
              input()
            
            if (class(workflow_input) == 'GroverInput') {
              directory(workflow_input) 
            } else {
              stop('Directory only available for grover input.',call. = FALSE)
            }
          })

#' @rdname Workflow-accessors

setMethod('directory<-',signature = 'Workflow',
          function(x,value){
            workflow_input <- input(x)
            
            if (class(workflow_input) == 'GroverInput') {
              directory(workflow_input) <- value
              input(x) <- workflow_input 
            } else {
              stop('Directory can only be set for grover input.',call. = FALSE)
            }
            
            return(x)
          })

#' @rdname Workflow-accessors

setMethod('host',signature = 'Workflow',
          function(x){
            workflow_input <- x %>% 
              input()
            
            if (class(workflow_input) == 'GroverInput') {
              host(workflow_input) 
            } else {
              stop('Host only available for grover input.',call. = FALSE)
            }
          })

#' @rdname Workflow-accessors

setMethod('host<-',signature = 'Workflow',
          function(x,value){
            workflow_input <- input(x)
            
            if (class(workflow_input) == 'GroverInput') {
              host(workflow_input) <- value
              input(x) <- workflow_input 
            } else {
              stop('Host can only be set for grover input.',call. = FALSE)
            }
            
            return(x)
          })

#' @rdname Workflow-accessors

setMethod('port',signature = 'Workflow',
          function(x){
            workflow_input <- x %>% 
              input()
            
            if (class(workflow_input) == 'GroverInput') {
              port(workflow_input) 
            } else {
              stop('Port only available for grover input.',call. = FALSE)
            }
          })

#' @rdname Workflow-accessors

setMethod('port<-',signature = 'Workflow',
          function(x,value){
            workflow_input <- input(x)
            
            if (class(workflow_input) == 'GroverInput') {
              port(workflow_input) <- value
              input(x) <- workflow_input 
            } else {
              stop('Port can only be set for grover input.',call. = FALSE)
            }
            
            return(x)
          })

#' @rdname Workflow-accessors

setMethod('auth',signature = 'Workflow',
          function(x){
            workflow_input <- x %>% 
              input()
            
            if (class(workflow_input) == 'GroverInput') {
              auth(workflow_input) 
            } else {
              stop('Auth only available for grover input.',call. = FALSE)
            }
          })

#' @rdname Workflow-accessors

setMethod('auth<-',signature = 'Workflow',
          function(x,value){
            workflow_input <- input(x)
            
            if (class(workflow_input) == 'GroverInput') {
              auth(workflow_input) <- value
              input(x) <- workflow_input 
            } else {
              stop('Auth can only be set for grover input.',call. = FALSE)
            }
            
            return(x)
          })
