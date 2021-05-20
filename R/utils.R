
setGeneric('githubWorkflowDependencies',function(x)
  standardGeneric('githubWorkflowDependencies'))

setMethod('githubWorkflowDependencies',signature = 'Workflow',
          function(x){

            standard_deps <- c(
              'ropensci/tarchetypes',
              'jasenfinch/metaboMisc',
              'jasenfinch/MFassign',
              'jasenfinch/metabolyseR'
            )
            
            fingerprinting_deps <- 'aberHRML/binneR'
            
            profiling_deps <- 'jasenfinch/profilePro'
            
            workflow_deps <- switch(type(x),
              `FIE-HRMS fingerprinting` = c(standard_deps,
                                            fingerprinting_deps),
              `NSI-HRMS fingerprinting` = c(standard_deps,
                                            fingerprinting_deps),
              `RP-LC-HRMS profiling` = c(standard_deps,
                                            profiling_deps),
              `NP-LC-HRMS profiling` = c(standard_deps,
                                         profiling_deps)
            )
            
            return(workflow_deps)
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

setGeneric('otherInputDependencies',function(x)
  standardGeneric('otherInputDependencies'))

setMethod('otherInputDependencies',signature = 'FilePathInput',
          function(x){
            character()
          })

setMethod('otherInputDependencies',signature = 'GroverInput',
          function(x){
            'http://fgcz-ms.uzh.ch/~cpanse/rawrr_0.2.5.tar.gz'
          })

setGeneric('otherDependencies',function(x)
  standardGeneric('otherDependencies'))

setMethod('otherDependencies',signature = 'Workflow',
          function(x){
            
            input_dependencies <- x %>% 
              input() %>% 
              otherInputDependencies()
            
            input_dependencies %>% 
              return()
          })