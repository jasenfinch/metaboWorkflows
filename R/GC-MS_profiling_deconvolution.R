
`GC-MS profiling deconvolution` <- function(elements = NULL){
  methods <- list(
    
    deconvolve = function(x){
      cat('\nDeconvolution',cli::symbol$continue,'\r')
      x@processed <- profileProcess(x@files,x@info,x@workflowParameters@processing)
      cat('\rDeconvolution',green(cli::symbol$tick),'\n')
      return(x)
    },
    
    detectBatchDiff = `FIE-HRMS fingerprinting`('detectBatchDiff'),
    
    detectMissInjections = `FIE-HRMS fingerprinting`('detectMissInjections'),
    
    preTreat = function(x){
      cat('\nPre-treatment',cli::symbol$continue,'\r')
      preTreatParameters <- analysisParameters('preTreat')
      preTreatParameters@preTreat <- x@workflowParameters@analysis@preTreat
      
      x@analysed <- metabolyse(x@processed@Data,x@processed@Info,preTreatParameters)
      
      cat('\rPre-treatment',green(cli::symbol$tick),'\n')
      return(x)
    },
    
    dataQualityCheckPoint = `FIE-HRMS fingerprinting`('dataQualityCheckPoint'),
    
    modelling = `FIE-HRMS fingerprinting`('modelling'),
    
    correlations = `FIE-HRMS fingerprinting`('correlations')
  )
  
  if (!is.null(elements)) {
    methods <- methods[[elements]]
  }
  
  return(methods)
}