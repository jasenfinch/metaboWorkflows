#' @importFrom binneRlyse binneRlyse
#' @importFrom metabolyseR analysisParameters metabolyse
#' @importFrom MFassign assignMFs

setMethod('FIE2', signature = 'Workflow',
          function(x){
            binningParams <- x@workflowParameters@processing
            analysisParams <- x@workflowParameters@analysis
            annotationParams <- x@workflowParameters@annotation
            
            binnedDat <- binneRlyse(x@files,binningParams)
            info <- binnedDat@info
            
            preTreatParameters <- analysisParameters('preTreat')
            preTreatParameters@preTreat <- analysisParams@preTreat
            
            neg <- metabolyse(binnedDat@binnedData$n,info,preTreatParameters)
            pos <- metabolyse(binnedDat@binnedData$p,info,preTreatParameters)
            
            dat <- data.frame(neg@preTreated$Data,pos@preTreated$Data,stringsAsFactors = F)
            info <- neg@preTreated$Info
            
            correlationParams <- analysisParameters('correlations')
            correlationParams@correlations <- analysisParams@correlations
            correlations <- metabolyse(dat,info,correlationParams)
            
            assignments <- assignMFs(correlations@correlations,annotationParams)
            
            assignedFeats <- paste(assignments@assignments$Mode,assignments@assignments$`Measured m/z`,sep = '')
            isoNames <- assignments@assignments$Isotope 
            isoNames[is.na(isoNames)] <- ''
            assignNames <-  paste(paste(assignments@assignments$Mode,
                                  assignments@assignments$`Measured m/z`,sep = ''),
                                  assignments@assignments$MF,
                                  isoNames,
                                  assignments@assignments$Adduct,
                                  sep = ' ')
            assignedFeats <- tibble(Feature = assignedFeats, Name = assignNames)
            
            assignedFeats <- left_join(tibble(Feature = colnames(dat)),assignedFeats)
            assignedFeats$Name[which(is.na(assignedFeats$Name))] <- assignedFeats$Feature[which(is.na(assignedFeats$Name))] 
            
            colnames(dat) <- assignedFeats$Name
            
            analysisParams@preTreat <- list()
            analysis <- metabolyse(dat,info,analysisParams) 
            analysis@preTreated <- analysis@rawData
            analysis@rawData <- list()
            
            x@processed <- binnedDat
            x@analysed <- analysis
            x@annotated <- assignments
            x@logs <- list(date())
            return(x)
          })
          