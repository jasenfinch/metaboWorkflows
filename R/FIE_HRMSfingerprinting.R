#' @importFrom binneR binneRlyse binnedData info
#' @importFrom metabolyseR analysisParameters metabolyse reAnalyse
#' @importFrom dplyr bind_cols
#' @importFrom cli symbol
#' @importFrom crayon green
#' @importFrom metaboMisc addAssignments preTreatModes detectBatchDiff detectMissInjections reduce
#' @importFrom MFassign assignMFs
#' @importFrom utils data

FIE_HRMSfingerprinting <- function(elements = NULL){
  methods <- list(
    spectralBin = function(x){
      cat('\nSpectral binning',cli::symbol$continue,'\r')
      binnedDat <- binneRlyse(files = x@files,info = x@info,parameters = x@workflowParameters@processing)
      x@processed <- binnedDat
      cat('\rSpectral binning',green(cli::symbol$tick),'\n')
      return(x)
    },
    
    detectBatchDiff = function(x){
      cat('\nChecking if batch correction is needed',cli::symbol$continue,'\r')
      
      bd <- detectBatchDiff(x@processed)
      
      if (T %in% bd$`Correction needed`) {
        x@workflowParameters@analysis@preTreat <- c(
          list(
            correction = list(
              center = list(block = 'block',type = 'median',nCores = detectCores() * 0.75,clusterType = getClusterType())
            )),
          x@workflowParameters@analysis@preTreat
        )
      }
      
      cat('\rChecking if batch correction is needed',green(cli::symbol$tick),'\n')
      return(x)
    },
    
    detectMissInjections = function(x){
      cat('\nChecking for miss injections',cli::symbol$continue,'\r')
      
      mi <- detectMissInjections(x@processed)
      
      if (length(mi$missInjections) > 0) {
        x@processed@spectra$missInjections <- mi
        x@workflowParameters@analysis@preTreat <- c(
          list(
            remove = list(
              sample = list(idx = mi$idx,samples = mi$missInjections)
            )),
          x@workflowParameters@analysis@preTreat
        )  
      }
      
      cat('\rChecking for miss injections',green(cli::symbol$tick),'\n')
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
      
      x@annotated <- x %>%
        preTreatedData() %>%
        assignMFs(x@workflowParameters@annotation,verbose = TRUE)
      
      x@analysed <- addAssignments(x@analysed,x@annotated)
      
      cat('\rMolecular formula assignment',green(cli::symbol$tick),'\n')
      return(x)
    },
    
    MFassignmentCheckPoint = function(x){
      cat(blue('\nBreak point to check MF assignments. Use restartWorkflow() to continue analysis.\n') )
      return(x)
    },
    
    reduceIsotopes = function(x){
      cat('\nReducing isotopic features',cli::symbol$continue,'\r')
      x@analysed <- metaboMisc::reduce(x %>% resultsAnalysis(),adducts = F)
      cat('\rReducing isotopic features',green(cli::symbol$tick),'\n')
      return(x)
    },
    
    modelling = function(x){
      cat('\nModelling',cli::symbol$continue,'\r')
      p <- analysisParameters('modelling')
      p@modelling <- x@workflowParameters@analysis@modelling
      x@analysed <- reAnalyse(x@analysed,p) 
      x@analysed@parameters <- x@workflowParameters@analysis
      cat('\rModelling',green(cli::symbol$tick),'\n')
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