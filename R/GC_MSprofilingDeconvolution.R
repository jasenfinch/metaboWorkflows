
GC_MSprofilingDeconvolution <- function(elements = NULL){
  methods <- list(
    
    deconvolve = function(x){
      x@processed <- profileProcess(x@files,x@info,x@workflowParameters@processing)
      return(x)
    },
    
    preTreat = function(x){
      preTreatParameters <- analysisParameters('preTreat')
      preTreatParameters@preTreat <- x@workflowParameters@analysis@preTreat
      
      modes <- names(x@processed@Data)
      
      pt <- metabolyse(x@processed@Data,x@processed@Info,preTreatParameters)
      
      dat <- pt@preTreated$Data
      info <- pt@preTreated$Info
      
      x@analysed <- new('Analysis',
                        log = list(analysis = date()),
                        parameters = x@workflowParameters@analysis,
                        rawData = list(),
                        preTreated = list(Data = dat,Info = info),
                        classification = tibble(),
                        featureSelection = tibble(),
                        correlations = tibble()
      )
      return(x)
    },
    
    dataQualityCheckPoint = function(x){
      cat('\nData pre-treatment complete. Break point for data quality check. Use restartWorkflow() to continue analysis.\n\n') 
      return(x)
    },
    
    modelling = FIE_HRMSfingerprinting('modelling'),
    
    correlations = function(x){
      p <- analysisParameters('correlations')
      p@correlations <- x@workflowParameters@analysis@correlations
      x@analysed <- reAnalyse(x@analysed,p)
      x@analysed@parameters <- x@workflowParameters@analysis
      return(x)
    }
  )
  
  if (!is.null(elements)) {
    methods <- methods[[elements]]
  }
  
  return(methods)
}