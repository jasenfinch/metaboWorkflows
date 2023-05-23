
setGeneric('inputDependencies',function(x)
  standardGeneric('inputDependencies'))

setMethod('inputDependencies',signature = 'FilePathInput',
          function(x){
            character()
          })

setMethod('inputDependencies',signature = 'GroverInput',
          function(x){
            c('jasenfinch/grover',
              'jasenfinch/metaboMisc')
          })

setGeneric('workflowDependencies',function(x)
  standardGeneric('workflowDependencies'))

setMethod('workflowDependencies',signature = 'Workflow',
          function(x){

            standard_deps <- c(
              'bioc::ChemmineOB',
              'bioc::BiocParallel',
              'bioc::MSnbase',
              'bioc::xcms',
              'jasenfinch/metabolyseR',
              'aberHRML/assignments',
              'jasenfinch/metaboMisc',
              'jasenfinch/hrmtargets'
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
            
            input_dependencies <- x %>% 
              input() %>% 
              inputDependencies()
            
            c(input_dependencies,
              workflow_deps) %>% 
              unique() %>% 
              return()
          })
