
setGeneric('workflowDependencies',function(x)
  standardGeneric('workflowDependencies'))

setMethod('workflowDependencies',signature = 'Workflow',
          function(x){
            workflow_deps <- list(
              `FIE-HRMS fingerprinting` = c(
                'jasenfinch/metaboMisc',
                'aberHRML/binneR',
                'jasenfinch/MFassign',
                'jasenfinch/metabolyseR'
              )
            )
            
            return(workflow_deps[[type(x)]])
          })

setGeneric('githubDependencies',function(x)
  standardGeneric('githubDependencies'))

setMethod('githubDependencies',signature = 'FilePathInput',
          function(x){
            character()
          })

setMethod('githubDependencies',signature = 'GroverInput',
          function(x){
            c('jasenfinch/grover',
              'jasenfinch/metaboMisc')
          })

setMethod('githubDependencies',signature = 'Workflow',
          function(x){
            
            input_dependencies <- x %>% 
              input() %>% 
              githubDependencies()
            
            workflow_deps <- workflowDependencies(x)
            
            c(input_dependencies,
              workflow_deps) %>% 
              unique() %>% 
              return()
          })
