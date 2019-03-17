#' @importFrom metabolyseR analysisParameters metabolyse
#' @importFrom MFassign assignMFs
#' @importFrom dplyr left_join

FIE_HRMSfingerprinting2 <- function(elements = NULL){
  methods <- list(
    spectralBin = FIE_HRMSfingerprinting('spectralBin'),
    
    preTreat = FIE_HRMSfingerprinting('preTreat'),
    
    dataQualityCheckPoint = FIE_HRMSfingerprinting('dataQualityCheckPoint'),
    
    correlations1 = FIE_HRMSfingerprinting('correlations'),
    
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
    
    classification = FIE_HRMSfingerprinting('classification'),
    
    featureSelection = FIE_HRMSfingerprinting('featureSelection'),
    
    correlations2 = FIE_HRMSfingerprinting('correlations')
  )
  
  if (!is.null(elements)) {
    methods <- methods[[elements]]
  } 
  
  return(methods)
}