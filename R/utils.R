
setGeneric('githubWorkflowDependencies',function(x)
  standardGeneric('githubWorkflowDependencies'))

setMethod('githubWorkflowDependencies',signature = 'Workflow',
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

setGeneric('githubInputDependencies',function(x)
  standardGeneric('githubInputDependencies'))

setMethod('githubInputDependencies',signature = 'FilePathInput',
          function(x){
            character()
          })

setMethod('githubInputDependencies',signature = 'GroverInput',
          function(x){
            c('jasenfinch/grover',
              'jasenfinch/metaboMisc')
          })

setGeneric('githubDependencies',function(x)
  standardGeneric('githubDependencies'))

setMethod('githubDependencies',signature = 'Workflow',
          function(x){
            
            input_dependencies <- x %>% 
              input() %>% 
              githubInputDependencies()
            
            workflow_deps <- githubWorkflowDependencies(x)
            
            c(input_dependencies,
              workflow_deps) %>% 
              unique() %>% 
              return()
          })

setGeneric('biocDependencies',function(x)
  standardGeneric('biocDependencies'))


setMethod('biocDependencies',signature = 'Workflow',
          function(x){
            c('mzR','BiocParallel','xcms','MSnbase')
          })