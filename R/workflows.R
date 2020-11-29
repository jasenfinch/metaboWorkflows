
`FIE-HRMS fingerprinting` <- function(elements = NULL){
  methods <- list(
    spectralBin = workflow_elements$spectralBin,

    detectBatchDiff =  workflow_elements$detectBatchDiff,

    detectMissInjections =  workflow_elements$detectMissInjections,

    preTreat =  workflow_elements$preTreat,

    MFassignment =  workflow_elements$MFassignment,

    MFassignmentCheckPoint =  workflow_elements$MFassignmentCheckPoint,

    reduceIsotopes =  workflow_elements$reduceIsotopes,

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
    
    detectBatchDiff = workflow_elements$detectBatchDiff,
    
    detectMissInjections = workflow_elements$detectMissInjections,
    
    preTreat = workflow_elements$preTreat_GC,
    
    dataQualityCheckPoint = workflow_elements$dataQualityCheckPoint,
    
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
    
    peakPick = workflow_elements$peakPick(),
    
    detectBatchDiff = workflow_elements$detectBatchDiff(),
    
    detectMissInjections = workflow_elements$detectMissInjections(),
    
    preTreat = workflow_elements$preTreat(),
    
    dataQualityCheckPoint = workflow_elements$dataQualityCheckPoint(),
    
    MFassignment = workflow_elements$MFassignment(),
    
    MFassignmentCheckPoint = workflow_elements$MFassignmentCheckPoint(),
    
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