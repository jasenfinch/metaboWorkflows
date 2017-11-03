#' @importFrom binneRlyse binneRlyse binnedData
#' @importFrom metabolyseR analysisParameters metabolyse reAnalyse
#' @importFrom dplyr bind_cols

FIE_HRMSfingerprinting <- function(elements = NULL){
  methods <- list(
    spectralBin = function(x){
      binnedDat <- binneRlyse(files = unlist(x@files$Files),info = x@files$Info,parameters = x@workflowParameters@processing)
      x@processed <- binnedDat
      return(x)
    },
    
    preTreat = function(x){
      preTreatParameters <- analysisParameters('preTreat')
      preTreatParameters@preTreat <- x@workflowParameters@analysis@preTreat
      
      neg <- metabolyse(x@processed@binnedData$n,x@processed@info,preTreatParameters)
      pos <- metabolyse(x@processed@binnedData$p,x@processed@info,preTreatParameters)
      
      dat <- bind_cols(neg@preTreated$Data,pos@preTreated$Data)
      info <- neg@preTreated$Info
      x@analysed <- new('Analysis',
                        log = list(analysis = date()),
                        parameters = x@workflowParameters@analysis,
                        rawData = list(Data = bind_cols(binnedData(resultsProcessing(x))),Info = info(resultsProcessing(x))),
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
    
    classification = function(x){
      p <- analysisParameters('classification')
      p@classification <- x@workflowParameters@analysis@classification
      x@analysed <- reAnalyse(x@analysed,p) 
      x@analysed@parameters <- x@workflowParameters@analysis
      return(x)
    },
    
    featureSelection = function(x){
      p <- analysisParameters('featureSelection')
      p@featureSelection <- x@workflowParameters@analysis@featureSelection
      x@analysed <- reAnalyse(x@analysed,p) 
      x@analysed@parameters <- x@workflowParameters@analysis
      return(x)
    },
    
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