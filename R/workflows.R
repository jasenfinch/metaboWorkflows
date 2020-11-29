
`FIE-HRMS fingerprinting` <- function(elements = NULL){
  methods <- list(
    `spectral binning` = workflow_elements$spectralBin,

    `detect batch differences` =  workflow_elements$detectBatchDiff,

    `detect missed injections` =  workflow_elements$detectMissInjections,

    `pre-treatment` =  workflow_elements$preTreat,
    
    `data quality check point` = workflow_elements$dataQualityCheckPoint,

    `molecular formula assignment` =  workflow_elements$MFassignment,

    `molecular formula assignment check point` =  workflow_elements$MFassignmentCheckPoint,

    `reduce isotopes` =  workflow_elements$reduceIsotopes,

    modelling =  workflow_elements$modelling,

    correlations =  workflow_elements$correlations
  )

  if (!is.null(elements)) {
    methods <- methods[[elements]]
  }

  return(methods)
}

`NSI-HRMS fingerprinting` <- function(elements = NULL){
  `FIE-HRMS fingerprinting`(elements)
}

`GC-MS profiling deconvolution` <- function(elements = NULL){
  methods <- list(
    
    deconvolve = workflow_elements$deconvolve,
    
    `detect batch differences` = workflow_elements$detectBatchDiff,
    
    `detect missed injections` = workflow_elements$detectMissInjections,
    
    `pre-treatment` = workflow_elements$preTreat_GC,
    
    `data quality check point` = workflow_elements$dataQualityCheckPoint,
    
    modelling = workflow_elements$modelling,
    
    correlations = workflow_elements$correlations
  )
  
  if (!is.null(elements)) {
    methods <- methods[[elements]]
  }
  
  return(methods)
}

`RP-LC-HRMS profiling` <- function(elements = NULL){
  methods <- list(
    
    `peak picking` = workflow_elements$peakPick(),
    
    `detect batch differences` = workflow_elements$detectBatchDiff(),
    
    `detect missed injections` = workflow_elements$detectMissInjections(),
    
    `pre-treatment` = workflow_elements$preTreat(),
    
    `data quality check point` = workflow_elements$dataQualityCheckPoint(),
    
    `molecular formula assignment` = workflow_elements$MFassignment(),
    
    `molecular formula assignment check point` = workflow_elements$MFassignmentCheckPoint(),
    
    modelling = workflow_elements$modelling(),
    
    correlations = workflow_elements$correlations()
  )
  
  if (!is.null(elements)) {
    methods <- methods[[elements]]
  }
  
  return(methods)
}


`NP-LC-HRMS profiling` <- function(elements = NULL){
  `RP-LC-HRMS profiling`(elements)
}