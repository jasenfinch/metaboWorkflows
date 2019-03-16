#' @importFrom metabolyseR analysisParameters metabolyse
#' @importFrom MFassign assignMFs
#' @importFrom dplyr left_join

FIE_HRMSfingerprinting2 <- function(elements = NULL){
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
      
      neg <- metabolyse(x@processed@binnedData$n,x@processed@info,preTreatParameters)
      pos <- metabolyse(x@processed@binnedData$p,x@processed@info,preTreatParameters)
      
      dat <- bind_cols(neg@preTreated$Data,pos@preTreated$Data)
      info <- neg@preTreated$Info
      x@analysed <- new('Analysis',
                        log = list(analysis = date()),
                        parameters = x@workflowParameters@analysis,
                        rawData = list(),
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
    
    correlations1 = function(x){
      cat('\nCorrelations',cli::symbol$continue,'\r')
      p <- analysisParameters('correlations')
      p@correlations <- x@workflowParameters@analysis@correlations
      x@analysed <- reAnalyse(x@analysed,p,verbose = T) 
      x@analysed@parameters <- x@workflowParameters@analysis
      cat('\rCorrelations',green(cli::symbol$tick),'\n')
      return(x)
    },
    
    annotation = function(x){
      cat('\nMolecular formula assignment',cli::symbol$continue,'\r')
      x@annotated <- assignMFs(x@analysed@correlations,x@workflowParameters@annotation)
      
      assignedFeats <- paste(x@annotated@assignments$Mode,x@annotated@assignments$`Measured m/z`,sep = '')
      isoNames <- x@annotated@assignments$Isotope 
      isoNames[is.na(isoNames)] <- ''
      assignNames <-  paste(paste(x@annotated@assignments$Mode,
                                  x@annotated@assignments$`Measured m/z`,sep = ''),
                            x@annotated@assignments$MF,
                            isoNames,
                            x@annotated@assignments$Adduct,
                            sep = ' ')
      assignedFeats <- tibble(Feature = assignedFeats, Name = assignNames)
      
      assignedFeats <- left_join(tibble(Feature = colnames(x@analysed@preTreated$Data)),assignedFeats)
      assignedFeats$Name[which(is.na(assignedFeats$Name))] <- assignedFeats$Feature[which(is.na(assignedFeats$Name))] 
      
      colnames(x@analysed@preTreated$Data) <- assignedFeats$Name 
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
    
    correlations2 = function(x){
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