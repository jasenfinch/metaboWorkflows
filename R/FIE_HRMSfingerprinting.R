#' @importFrom binneR binneRlyse binnedData info
#' @importFrom metabolyseR analysisParameters metabolyse reAnalyse
#' @importFrom dplyr bind_cols
#' @importFrom cli symbol
#' @importFrom crayon green

FIE_HRMSfingerprinting <- function(elements = NULL){
  methods <- list(
    spectralBin = function(x){
      cat('\nSpectral binning',cli::symbol$continue,'\r')
      binnedDat <- binneRlyse(files = unlist(x@files),info = x@info,parameters = x@workflowParameters@processing)
      x@processed <- binnedDat
      cat('\rSpectral binning',green(cli::symbol$tick),'\n')
      return(x)
    },
    
    preTreat = function(x){
      cat('\nPre-treatment',cli::symbol$continue,'\r')
      preTreatParameters <- analysisParameters('preTreat')
      preTreatParameters@preTreat <- x@workflowParameters@analysis@preTreat
      
      neg <- metabolyse(x@processed@binnedData$n,x@processed@info,preTreatParameters,verbose = F)
      pos <- metabolyse(x@processed@binnedData$p,x@processed@info,preTreatParameters,verbose = F)
      
      dat <- bind_cols(neg@preTreated$Data,pos@preTreated$Data)
      info <- neg@preTreated$Info
      version <- packageVersion('metabolyseR')
      analysisStart <- date()
      x@analysed <- new('Analysis',
                        log = list(packageVersion = version,analysis = analysisStart,verbose = F),
                        parameters = x@workflowParameters@analysis,
                        rawData = list(Data = bind_cols(binnedData(resultsProcessing(x))),Info = info(resultsProcessing(x))),
                        preTreated = list(Data = dat,Info = info),
                        classification = tibble(),
                        featureSelection = tibble(),
                        correlations = tibble()
      )
      cat('\rPre-treatment',green(cli::symbol$tick),'\n')
      return(x)
    },
    
    dataQualityCheckPoint = function(x){
      cat(blue('\nBreak point for data quality check. Use restartWorkflow() to continue analysis.\n') )
      return(x)
    },
    
    classification = function(x){
      cat('\nClassification',cli::symbol$continue,'\r')
      p <- analysisParameters('classification')
      p@classification <- x@workflowParameters@analysis@classification
      x@analysed <- reAnalyse(x@analysed,p) 
      x@analysed@parameters <- x@workflowParameters@analysis
      cat('\rClassification',green(cli::symbol$tick),'\n')
      return(x)
    },
    
    featureSelection = function(x){
      cat('\nFeature selection',cli::symbol$continue,'\r')
      p <- analysisParameters('featureSelection')
      p@featureSelection <- x@workflowParameters@analysis@featureSelection
      x@analysed <- reAnalyse(x@analysed,p) 
      x@analysed@parameters <- x@workflowParameters@analysis
      cat('\rFeature selection',green(cli::symbol$tick),'\n')
      return(x)
    },
    
    correlations = function(x){
      cat('\nCorrelations',cli::symbol$continue,'\r')
      p <- analysisParameters('correlations')
      p@correlations <- x@workflowParameters@analysis@correlations
      x@analysed <- reAnalyse(x@analysed,p) 
      x@analysed@parameters <- x@workflowParameters@analysis
      cat('\rCorrelations',green(cli::symbol$tick),'\n')
      return(x)
    }
  )
  
  if (!is.null(elements)) {
    methods <- methods[[elements]]
  } 
  
  return(methods)
}