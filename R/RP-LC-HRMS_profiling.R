#' @importFrom profilePro profileProcess
#' @importFrom metabolyseR analysisParameters metabolyse reAnalyse
#' @importFrom dplyr bind_cols
#' @importFrom purrr map

`RP-LC-HRMS profiling` <- function(elements = NULL){
  methods <- list(
    
    peakPick = function(x){
      eval(parse(text = 'suppressPackageStartupMessages(library(profilePro))'))
      message('\nPeak picking',cli::symbol$continue,'\r',appendLF = TRUE)
      x@processed <- profileProcess(x@files,x@info,x@workflowParameters@processing)
      message('\rPeak picking',green(cli::symbol$tick))
      return(x)
    },
    
    detectBatchDiff = `FIE-HRMS fingerprinting`('detectBatchDiff'),
    
    detectMissInjections = `FIE-HRMS fingerprinting`('detectMissInjections'),
    
    preTreat = `FIE-HRMS fingerprinting`('preTreat'),
    
    dataQualityCheckPoint = `FIE-HRMS fingerprinting`('dataQualityCheckPoint'),
    
    MFassignment = `FIE-HRMS fingerprinting`('MFassignment'),
    
    MFassignmentCheckPoint = `FIE-HRMS fingerprinting`('MFassignmentCheckPoint'),
    
    modelling = `FIE-HRMS fingerprinting`('modelling'),
    
    correlations = `FIE-HRMS fingerprinting`('correlations')
  )
  
  if (!is.null(elements)) {
    methods <- methods[[elements]]
  }
  
  return(methods)
}