#' @importFrom profilePro profileProcess
#' @importFrom metabolyseR analysisParameters metabolyse reAnalyse
#' @importFrom dplyr bind_cols
#' @importFrom purrr map

RP_LC_HRMSprofiling <- function(elements = NULL){
  methods <- list(
    peakPick = function(x){
      cat('\nPeak picking',cli::symbol$continue,'\r')
      x@processed <- profileProcess(x@files,x@info,x@workflowParameters@processing)
      cat('\rPeak picking',green(cli::symbol$tick),'\n')
      return(x)
    },
    
    detectBatchDiff = FIE_HRMSfingerprinting('detectBatchDiff'),
    
    detectMissInjections = FIE_HRMSfingerprinting('detectMissInjections'),
    
    preTreat = FIE_HRMSfingerprinting('preTreat'),
    
    dataQualityCheckPoint = FIE_HRMSfingerprinting('dataQualityCheckPoint'),
    
    MFassignment = FIE_HRMSfingerprinting('MFassignment'),
    
    MFassignmentCheckPoint = FIE_HRMSfingerprinting('MFassignmentCheckPoint'),
    
    classification = FIE_HRMSfingerprinting('classification'),
    
    featureSelection = FIE_HRMSfingerprinting('featureSelection'),
    
    correlations = FIE_HRMSfingerprinting('correlations')
  )
  
  if (!is.null(elements)) {
    methods <- methods[[elements]]
  }
  
  return(methods)
}