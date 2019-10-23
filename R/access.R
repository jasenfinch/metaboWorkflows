#' preTreatedData
#' @description return pre-treated data from a Workflow object.
#' @param x S4 object of class Workflow 
#' @importMethodsFrom metabolyseR preTreatedData
#' @export

setMethod('preTreatedData',signature = 'Workflow',function(x){
  x %>%
    resultsAnalysis() %>%
    preTreatedData()
})

#' preTreatedData
#' @description return pre-treated sample information from a Workflow object.
#' @param x S4 object of class Workflow 
#' @importMethodsFrom metabolyseR preTreatedInfo
#' @export

setMethod('preTreatedInfo',signature = 'Workflow',function(x){
  x %>%
    resultsAnalysis() %>%
    preTreatedInfo()
})

#' preTreated
#' @description return pre-treated AnalysisData object from a Workflow object.
#' @param x S4 object of class Workflow 
#' @importMethodsFrom metabolyseR preTreated
#' @export

setMethod('preTreated',signature = 'Workflow',function(x){
  x %>%
    resultsAnalysis() %>%
    preTreated()
})

#' processedData
#' @description Extract processed metabolomics data from an object of class Workflow
#' @param x S4 object of class Workflow
#' @importMethodsFrom profilePro processedData
#' @importFrom binneR binnedData
#' @export

setMethod('processedData',signature = 'Workflow',function(x){
  if (x %>% resultsProcessing() %>% class() == 'MetaboProfile') {
    x %>%
      resultsProcessing() %>%
      processedData()  
  } 
  if (x %>% resultsProcessing() %>% class() == 'Binalysis') {
    x %>%
      resultsProcessing() %>%
      binnedData()
  }
})

#' processedInfo
#' @rdname processedInfo
#' @description Extract processed sample info from an object of class Workflow.
#' @param x S4 object of class Workflow
#' @importFrom profilePro sampleInfo
#' @importFrom binneR info
#' @export

setMethod('processedInfo',signature = 'Workflow',function(x){
  if (x %>% resultsProcessing() %>% class() == 'MetaboProfile') {
    x %>%
      resultsProcessing() %>%
      sampleInfo()  
  } 
  if (x %>% resultsProcessing() %>% class() == 'Binalysis') {
    x %>%
      resultsProcessing() %>%
      info()
  }
})

