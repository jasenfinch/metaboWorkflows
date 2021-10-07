#' Edit workflow targets and modules
#' @rdname workflow-edit
#' @description Edit the targets of a workflow definition.
#' @param x S4 object of class `Workflow`
#' @param module the workflow module name
#' @param modules character vector of module names
#' @param target the workflow target name
#' @param replacement the replacement workflow module or target
#' @param addition the workflow module or target to add
#' @return S4 object of class `Workflow` with edited targets
#' @examples 
#' ## Define a workflow
#' file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
#' sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
#'
#' workflow_input <- inputFilePath(file_paths,sample_information)
#'
#' workflow_definition <- defineWorkflow(workflow_input,
#'                                      'FIE-HRMS fingerprinting',
#'                                      'Example project')
#'                                      
#' ## Add a module
#' workflow_definition <- moduleAdd(workflow_definition,
#'                                  'additional_module',
#'                                  list(new_target = target('new_target','1 + 1')))
#' ## Replace the additional module
#' workflow_definition <- moduleReplace(workflow_definition,
#'                                      'additional_module',
#'                                      list(replacement_target = target('replacement_target',
#'                                      '1 + 2')))
#' ## Remove the additional module
#' workflow_definition <- modulesRemove(workflow_definition,
#'                                     'additional_module')
#' 
#' ## Add a target to the input module
#' workflow_definition <- targetAdd(workflow_definition,
#'                                  'input',
#'                                  'new_target',
#'                                  target('new_target','1 + 1'))
#' ## Replace the additional target
#' workflow_definition <- targetReplace(workflow_definition,
#'                                  'input',
#'                                  'new_target',
#'                                  target('new_target','1 + 2')) 
#' ## Remove the additional target
#' workflow_definition <- targetRemove(workflow_definition,
#'                                  'input',
#'                                  'new_target')
#' @export

setGeneric('moduleReplace',function(x,module,replacement)
  standardGeneric('moduleReplace'))

#' @rdname workflow-edit

setMethod('moduleReplace',signature = 'Workflow',
          function(x,module,replacement){
            
            checkModule(x,module)
            checkModuleDefinition(replacement)
            
            targets(x)[[module]] <- replacement
            
            return(x)
          })

#' @rdname workflow-edit
#' @export

setGeneric('modulesRemove',function(x,modules)
  standardGeneric('modulesRemove'))

#' @rdname workflow-edit

setMethod('modulesRemove',signature = 'Workflow',
          function(x,modules){
            
            checkModule(x,modules)
            
            available_targets <- targets(x)
            
            targets(x) <- available_targets[!(names(available_targets) %in% modules)]
            
            return(x)
          })

#' @rdname workflow-edit
#' @export

setGeneric('modulesKeep',function(x,modules)
  standardGeneric('modulesKeep'))

#' @rdname workflow-edit

setMethod('modulesKeep',signature = 'Workflow',
          function(x,modules){
            
            checkModule(x,modules)
            
            available_targets <- targets(x)
            
            targets(x) <- available_targets[names(available_targets) %in% modules]
            
            return(x)
          })

#' @rdname workflow-edit
#' @export

setGeneric('moduleAdd',function(x,module,addition)
  standardGeneric('moduleAdd'))

#' @rdname workflow-edit

setMethod('moduleAdd',signature = 'Workflow',
          function(x,module,addition){
            
            checkModuleDefinition(addition)
            
            new_targets <- c(targets(x),
                             list(addition)) 
            
            names(new_targets)[length(new_targets)] <- module
            
            targets(x) <- new_targets 
            
            return(x)
          })

#' @rdname workflow-edit
#' @export

setGeneric('targetReplace',function(x,module,target,replacement)
  standardGeneric('targetReplace'))

#' @rdname workflow-edit

setMethod('targetReplace',signature = 'Workflow',
          function(x,module,target,replacement){
            
            checkModule(x,module)
            checkTarget(x,module,target)
            checkTargetDefinition(replacement)
            
            targets(x)[[module]][[target]] <- replacement
            
            return(x)
})

#' @rdname workflow-edit
#' @export

setGeneric('targetRemove',function(x,module,target)
  standardGeneric('targetRemove'))

#' @rdname workflow-edit

setMethod('targetRemove',signature = 'Workflow',
          function(x,module,target){
            checkModule(x,module)
            checkTarget(x,module,target)
            
            available_targets <- targets(x)
            
            module_targets <- available_targets[[module]]
            module_targets <- module_targets[names(module_targets) != target]
            
            available_targets[[module]] <- module_targets
            targets(x) <- available_targets
            
            return(x)
          })

#' @rdname workflow-edit
#' @export

setGeneric('targetAdd',function(x,module,target,addition)
  standardGeneric('targetAdd'))

#' @rdname workflow-edit

setMethod('targetAdd',signature = 'Workflow',
          function(x,module,target,addition){
            
            checkModule(x,module)
            checkTargetDefinition(addition)
            
            module_targets <- targets(x)
            
            new_targets <- c(module_targets[[module]],
                             list(addition)) 
            
            names(new_targets)[length(new_targets)] <- target
            
            module_targets[[module]] <- new_targets 
            
            targets(x) <- module_targets 
            
            return(x)
          })

isModule <- function(module){
  is.list(module)
}

isTarget <- function(target){
  if (class(target) == 'Target'){
    TRUE
  } else {
    FALSE
  }
}

checkModule <- function(x,module){
  if (all(!(module %in% modules(x)))){
    available_modules <- x %>% 
      modules() %>% 
      glue_collapse(sep = ', ')
    
    stop(glue('Module {module} not found. Argument module should be one of {available_modules}.'),
         call. = FALSE)
  }
}

checkModuleDefinition <- function(replacement){
  
  replacement_error <- 'The module should be a list containing objects of S4 class Target.'
  
  if (!isModule(replacement)) {
    stop(replacement_error,
         call. = FALSE)
  }
  
  replacement_targets <- replacement %>% 
    map_chr(class) %>% 
    {. == 'Target'}
  
  if (!all(replacement_targets)){
    stop(replacement_error,
         call. = FALSE)
  }
}

checkTarget <- function(x,module,target){
  if (!(target %in% names(targets(x)[[module]]))){
    available_targets <- x %>% 
      targets() %>%
      .[[module]] %>% 
      names() %>% 
      glue_collapse(sep = ', ')
    
    stop(glue('Target {target} not found. Argument target should be one of {available_targets} for module {module}.'),
         call. = FALSE)
  }
}

checkTargetDefinition <- function(replacement){
  if (!isTarget(replacement)){
    stop('The target should be an object of S4 class Target.',
         call. = FALSE)
  }
}