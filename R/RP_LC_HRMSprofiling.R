#' @importFrom profilePro profileProcess
#' @importFrom metabolyseR analysisParameters metabolyse reAnalyse
#' @importFrom dplyr bind_cols
#' @importFrom purrr map

RP_LC_HRMSprofiling <- function(elements = NULL){
  methods <- list(
    peakPick = function(x){
      x@processed <- profileProcess(x@files,x@info,x@workflowParameters@processing)
      return(x)
    },
    
    preTreat = FIE_HRMSfingerprinting('preTreat'),
    
    dataQualityCheckPoint = FIE_HRMSfingerprinting('dataQualityCheckPoint'),
    
    MFassignment = FIE_HRMSfingerprinting('MFassignment'),
    
    MFassignmentCheckPoint = FIE_HRMSfingerprinting('MFassignmentCheckPoint'),
    
    classification = function(x){
      p <- analysisParameters('classification')
      p@classification <- x@workflowParameters@analysis@classification
      x@analysed <- reAnalyse(x@analysed,p)
      x@analysed@parameters <- x@workflowParameters@analysis
      return(x)
    },
    
    featureSelection = FIE_HRMSfingerprinting('featureSelection'),
    
    correlations = FIE_HRMSfingerprinting('correlations')
  )
  
  if (!is.null(elements)) {
    methods <- methods[[elements]]
  }
  
  return(methods)
}