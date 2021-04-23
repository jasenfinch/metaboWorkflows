#' @importFrom binneR binneRlyse binnedData
#' @importFrom metabolyseR analysisParameters metabolyse reAnalyse dat<- dat parameters<- parameters
#' @importFrom dplyr bind_cols
#' @importFrom cli symbol
#' @importFrom crayon green
#' @importFrom metaboMisc preTreatModes detectBatchDiff detectMissInjections reduce
#' @importFrom MFassign assignMFs assignedData
#' @importFrom utils data

`FIE-HRMS fingerprinting` <- function(elements = NULL){
  methods <- list(
    spectralBin = function(x){
      message('\nSpectral binning ',cli::symbol$continue,'\r',appendLF = FALSE)
      binnedDat <- binneRlyse(files = as.character(x@files),info = x@info,parameters = x@workflowParameters@processing)
      x@processed <- binnedDat
      message('\rSpectral binning ',green(cli::symbol$tick))
      return(x)
    },
    
    detectBatchDiff = function(x){
      message('\nChecking if batch correction is needed ',cli::symbol$continue,'\r',appendLF = FALSE)
      
      bd <- detectBatchDiff(x@processed)
      
      if (TRUE %in% bd$`Correction needed`) {
        parameters(x@workflowParameters@analysis,'pre-treatment') <- c(
          list(
            correction = list(
              center = list(block = 'block',type = 'median')
            )),
          parameters(x@workflowParameters@analysis,'pre-treatment')
        )
      }
      
      message('\rChecking if batch correction is needed ',green(cli::symbol$tick))
      return(x)
    },
    
    detectMissInjections = function(x){
      message('\nChecking for miss injections ',cli::symbol$continue,'\r',appendLF = FALSE)
      
      mi <- detectMissInjections(x@processed)
      
      if (length(mi$missInjections) > 0) {
        x@processed@spectra$missInjections <- mi
        parameters(x@workflowParameters@analysis,'pre-treatment') <- c(
          list(
            remove = list(
              samples = list(idx = mi$idx,samples = mi$missInjections)
            )),
          parameters(x@workflowParameters@analysis,'pre-treatment')
        )  
      }
      
      message('\rChecking for miss injections ',green(cli::symbol$tick))
      return(x)
    },
    
    preTreat = function(x){
      message('\nPre-treatment ',cli::symbol$continue,'\r',appendLF = FALSE)
      preTreatParameters <- analysisParameters('pre-treatment')
      parameters(preTreatParameters,'pre-treatment') <- parameters(x@workflowParameters@analysis,
                                                                   'pre-treatment')
      
      x@analysed <- preTreatModes(x@processed,preTreatParameters)
      
      message('\rPre-treatment ',green(cli::symbol$tick))
      return(x)
    },
    
    dataQualityCheckPoint = function(x){
      message(blue('\nBreak point for data quality check. Use restartWorkflow() to continue analysis.'))
      return(x)
    },
    
    MFassignment = function(x){
      message('\nMolecular formula assignment ',cli::symbol$continue,'\r',appendLF = FALSE)
      
      x@annotated <- x %>%
        dat(type = 'pre-treated') %>%
        assignMFs(x@workflowParameters@annotation,verbose = TRUE)
      
      dat(x@analysed@preTreated) <- assignedData(x@annotated)
      
      message('\rMolecular formula assignment ',green(cli::symbol$tick))
      return(x)
    },
    
    MFassignmentCheckPoint = function(x){
      message(blue('\nBreak point to check MF assignments. Use restartWorkflow() to continue analysis.') )
      return(x)
    },
    
    reduceIsotopes = function(x){
      message('\nReducing isotopic features ',cli::symbol$continue,'\r',appendLF = FALSE)
      x@analysed <- metaboMisc::reduce(x %>% resultsAnalysis(),adducts = F)
      message('\rReducing isotopic features ',green(cli::symbol$tick))
      return(x)
    },
    
    modelling = function(x){
      message('\nModelling ',cli::symbol$continue,'\r',appendLF = FALSE)
      p <- analysisParameters('modelling')
      p@modelling <- x@workflowParameters@analysis@modelling
      x@analysed <- reAnalyse(x@analysed,p) 
      x@analysed@parameters <- x@workflowParameters@analysis
      message('\rModelling ',green(cli::symbol$tick))
      return(x)
    },
    
    correlations = function(x){
      message('\nCorrelations ',cli::symbol$continue,'\r',appendLF = FALSE)
      p <- analysisParameters('correlations')
      p@correlations <- x@workflowParameters@analysis@correlations
      x@analysed <- reAnalyse(x@analysed,p) 
      x@analysed@parameters <- x@workflowParameters@analysis
      message('\rCorrelations ',green(cli::symbol$tick))
      return(x)
    }
  )
  
  if (!is.null(elements)) {
    methods <- methods[[elements]]
  } 
  
  return(methods)
}