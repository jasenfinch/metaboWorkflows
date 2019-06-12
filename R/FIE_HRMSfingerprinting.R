#' @importFrom binneR binneRlyse binnedData info
#' @importFrom metabolyseR analysisParameters metabolyse reAnalyse
#' @importFrom dplyr bind_cols
#' @importFrom cli symbol
#' @importFrom crayon green
#' @importFrom metaboMisc addAssignments preTreatModes
#' @importFrom MFassign assignMFs

FIE_HRMSfingerprinting <- function(elements = NULL){
  methods <- list(
    spectralBin = function(x){
      cat('\nSpectral binning',cli::symbol$continue,'\r')
      binnedDat <- binneRlyse(files = x@files,info = x@info,parameters = x@workflowParameters@processing)
      x@processed <- binnedDat
      cat('\rSpectral binning',green(cli::symbol$tick),'\n')
      return(x)
    },
    
    preTreat = function(x){
      cat('\nPre-treatment',cli::symbol$continue,'\r')
      preTreatParameters <- analysisParameters('preTreat')
      preTreatParameters@preTreat <- x@workflowParameters@analysis@preTreat
      
      x@analysed <- preTreatModes(x@processed,x@workflowParameters@analysis)
      
      cat('\rPre-treatment',green(cli::symbol$tick),'\n')
      return(x)
    },
    
    dataQualityCheckPoint = function(x){
      cat(blue('\nBreak point for data quality check. Use restartWorkflow() to continue analysis.\n') )
      return(x)
    },
    
    MFassignment = function(x){
      cat('\nMolecular formula assignment',cli::symbol$continue,'\r')
      
      data('Adducts',package = 'mzAnnotation')
      data('Isotopes',package = 'mzAnnotation')
      data('Transformations',package = 'mzAnnotation')
      
      p <- analysisParameters('correlations')
      p@correlations <- x@workflowParameters@analysis@correlations
      x@analysed <- reAnalyse(x@analysed,p) 
      x@analysed@parameters <- x@workflowParameters@analysis
      
      x@annotated <- assignMFs(x@analysed@correlations,x@workflowParameters@annotation)
      
      x@analysed <- addAssignments(x@analysed,x@annotated)
      
      cat('\rMolecular formula assignment',green(cli::symbol$tick),'\n')
      return(x)
    },
    
    MFassignmentCheckPoint = function(x){
      cat(blue('\nBreak point to check MF assignments. Use restartWorkflow() to continue analysis.\n') )
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