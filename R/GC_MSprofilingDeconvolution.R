
GC_MSprofilingDeconvolution <- function(elements = NULL){
  methods <- list(
    
    deconvolve = function(x){
      cat('\nDeconvolution',cli::symbol$continue,'\r')
      x@processed <- profileProcess(x@files,x@info,x@workflowParameters@processing)
      cat('\rDeconvolution',green(cli::symbol$tick),'\n')
      return(x)
    },
    
    detectBatchDiff = FIE_HRMSfingerprinting('detectBatchDiff'),
    
    detectMissInjections = FIE_HRMSfingerprinting('detectMissInjections'),
    
    preTreat = function(x){
      cat('\nPre-treatment',cli::symbol$continue,'\r')
      preTreatParameters <- analysisParameters('preTreat')
      preTreatParameters@preTreat <- x@workflowParameters@analysis@preTreat
      
      x@analysed <- metabolyse(x@processed@Data,x@processed@Info,preTreatParameters)
      
      cat('\rPre-treatment',green(cli::symbol$tick),'\n')
      return(x)
    },
    
    dataQualityCheckPoint = FIE_HRMSfingerprinting('dataQualityCheckPoint'),
    
    modelling = FIE_HRMSfingerprinting('modelling'),
    
    correlations = FIE_HRMSfingerprinting('correlations')
  )
  
  if (!is.null(elements)) {
    methods <- methods[[elements]]
  }
  
  return(methods)
}