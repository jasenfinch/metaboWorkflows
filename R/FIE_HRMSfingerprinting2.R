#' @importFrom binneRlyse binneRlyse
#' @importFrom metabolyseR analysisParameters metabolyse
#' @importFrom MFassign assignMFs
#' @importFrom dplyr left_join

FIE_HRMSfingerprinting2 <- function(elements = NULL){
  methods <- list(
    spectralBin = function(x){
      binnedDat <- binneRlyse(x@files,x@workflowParameters@processing)
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
                        rawData = list(),
                        preTreated = list(Data = dat,Info = info),
                        classification = tibble(),
                        featureSelection = tibble(),
                        correlations = tibble()
      )
      return(x)
    },
    
    correlations1 = function(x){
      p <- analysisParameters('correlations')
      p@correlations <- x@workflowParameters@analysis@correlations
      x@analysed <- reAnalyse(x@analysed,p) 
      x@analysed@parameters <- x@workflowParameters@analysis
      return(x)
    },
    
    annotation = function(x){
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
    
    correlations2 = function(x){
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