
`GC-MS profiling deconvolution` <- function(elements = NULL){
  methods <- list(
    
    deconvolve = function(x){
      message('\nDeconvolution',cli::symbol$continue,'\r',appendLF = TRUE)
      x@processed <- profileProcess(x@files,x@info,x@workflowParameters@processing)
      message('\rDeconvolution',green(cli::symbol$tick))
      return(x)
    },
    
    detectBatchDiff = `FIE-HRMS fingerprinting`('detectBatchDiff'),
    
    detectMissInjections = `FIE-HRMS fingerprinting`('detectMissInjections'),
    
    preTreat = function(x){
      message('\nPre-treatment',cli::symbol$continue,'\r',appendLF = TRUE)
      preTreatParameters <- analysisParameters('preTreat')
      preTreatParameters@preTreat <- x@workflowParameters@analysis@preTreat
      
      x@analysed <- metabolyse(x@processed@Data,x@processed@Info,preTreatParameters)
      
      message('\rPre-treatment',green(cli::symbol$tick))
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