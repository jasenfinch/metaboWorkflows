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
    
    preTreat = function(x){
      preTreatParameters <- analysisParameters('preTreat')
      preTreatParameters@preTreat <- x@workflowParameters@analysis@preTreat
      
      modes <- names(x@processed@Data)
      
      pt <- map(modes,~{
        metabolyse(x@processed@Data[[.]],x@processed@Info,preTreatParameters)
      })
      names(pt) <- modes
      
      dat <- map(pt,~{
        .@preTreated$Data
      }) %>%
        bind_cols()
      
      info <- pt$neg@preTreated$Info
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

RP_LC_HRMSprofiling2 <- function(elements = NULL){
  methods <- list(
    peakPick = RP_LC_HRMSprofiling('peakPick'),
    
    preTreat = RP_LC_HRMSprofiling('preTreat'),
    
    dataQualityCheckPoint = RP_LC_HRMSprofiling('dataQualityCheckPoint'),
    
    correlations1 =  RP_LC_HRMSprofiling('correlations'),
    
    annotation = FIE_HRMSfingerprinting2('annotation'),
    
    MFassignmentCheckPoint = FIE_HRMSfingerprinting2('MFassignmentCheckPoint'),
    
    classification = RP_LC_HRMSprofiling('classification'),
    
    featureSelection = RP_LC_HRMSprofiling('featureSelection'),
    
    correlations = RP_LC_HRMSprofiling('correlations')
  )
  
  if (!is.null(elements)) {
    methods <- methods[[elements]]
  }
  
  return(methods)
}
