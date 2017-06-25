#' @importFrom binneRlyse binneRlyse
#' @importFrom metabolyseR analysisParameters metabolyse

setMethod('FIE', signature = 'Workflow',
          function(x){
            binningParams <- x@workflowParameters@processing
            analysisParams <- x@workflowParameters@analysis
            
            binnedDat <- binneRlyse(x@files,binningParams)
            info <- binnedDat@info
            
            preTreatParameters <- analysisParameters('preTreat')
            preTreatParameters@preTreat <- analysisParams@preTreat
            
            neg <- metabolyse(binnedDat@binnedData$n,info,preTreatParameters)
            pos <- metabolyse(binnedDat@binnedData$p,info,preTreatParameters)
            
            dat <- data.frame(neg@preTreated$Data,pos@preTreated$Data,stringsAsFactors = F)
            info <- neg@preTreated$Info
            analysisParams@preTreat <- list()
            analysis <- metabolyse(dat,info,analysisParams) 
            analysis@preTreated <- analysis@rawData
            analysis@rawData <- list()
            
            x@processed <- binnedDat
            x@analysed <- analysis
            x@logs <- list(date())
            return(x)
          })