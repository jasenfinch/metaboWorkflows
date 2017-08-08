#' @importFrom profilePro profileProcess
#' @importFrom metabolyseR analysisParameters metabolyse reAnalyse
#' @importFrom dplyr bind_cols

RP_LC_HRMSprofiling <- function(elements = NULL){
  methods <- list(
    peakPick = function(x){
      x@processed <- profileProcess(x@files,x@workflowParameters@processing)
      return(x)
    },

    preTreat = function(x){
      preTreatParameters <- analysisParameters('preTreat')
      preTreatParameters@preTreat <- x@workflowParameters@analysis@preTreat

      neg <- metabolyse(x@processed@Data$neg,x@processed@Info,preTreatParameters)
      pos <- metabolyse(x@processed@Data$pos,x@processed@Info,preTreatParameters)

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