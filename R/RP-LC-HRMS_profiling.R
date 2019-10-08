#' @importFrom profilePro profileProcess
#' @importFrom metabolyseR analysisParameters metabolyse reAnalyse
#' @importFrom dplyr bind_cols
#' @importFrom purrr map

`RP-LC-HRMS_profiling` <- function(elements = NULL){
  methods <- list(
    peakPick = function(x){
      cat('\nPeak picking',cli::symbol$continue,'\r')
      x@processed <- profileProcess(x@files,x@info,x@workflowParameters@processing)
      cat('\rPeak picking',green(cli::symbol$tick),'\n')
      return(x)
    },
    
    detectBatchDiff = `FIE-HRMS_fingerprinting`('detectBatchDiff'),
    
    detectMissInjections = `FIE-HRMS_fingerprinting`('detectMissInjections'),
    
    preTreat = `FIE-HRMS_fingerprinting`('preTreat'),
    
    dataQualityCheckPoint = `FIE-HRMS_fingerprinting`('dataQualityCheckPoint'),
    
    MFassignment = `FIE-HRMS_fingerprinting`('MFassignment'),
    
    MFassignmentCheckPoint = `FIE-HRMS_fingerprinting`('MFassignmentCheckPoint'),
    
    modelling = `FIE-HRMS_fingerprinting`('modelling'),
    
    correlations = `FIE-HRMS_fingerprinting`('correlations')
  )
  
  if (!is.null(elements)) {
    methods <- methods[[elements]]
  }
  
  return(methods)
}