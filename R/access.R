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

#' binnedData
#' @description Extract binned data from a Workflow object.
#' @param x S4 object of class Workflow 
#' @importMethodsFrom binneR binnedData
#' @export

setMethod('binnedData',signature = 'Workflow',function(x){
  if (x %>% resultsProcessing() %>% class() == 'Binalysis') {
    x %>%
      resultsProcessing() %>%
      binnedData() 
  } else {
    stop('The processed data should be an S4 object of class Binalysis!')
  }
})

#' info
#' @description Extract raw binning runinfo data from a Workflow object.
#' @param x S4 object of class Workflow 
#' @importMethodsFrom binneR info
#' @export

setMethod('info',signature = 'Workflow',function(x){
  if (x %>% resultsProcessing() %>% class() == 'Binalysis') {
    x %>%
      resultsProcessing() %>%
      info()
  } else {
    stop('The processed data should be an S4 object of class Binalysis!')
  }
})

#' processedData
#' @description Extract processed metabolomics profiling data from an object of class Workflow
#' @param x S4 object of class Workflow
#' @importMethodsFrom profilePro processedData
#' @export

setMethod('processedData',signature = 'Workflow',function(x){
  if (x %>% resultsProcessing() %>% class() == 'MetaboProfile') {
    x %>%
      resultsProcessing() %>%
      processedData()  
  } else {
    stop('The processed data should be an S4 object of class MetaboProfile!')
  }
})

#' sampleInfo
#' @description Extract raw profiling sample info from an object of class Workflow.
#' @param x S4 object of class Workflow
#' @importMethodsFrom profilePro sampleInfo
#' @export

setMethod('info',signature = 'Workflow',function(x){
  if (x %>% resultsProcessing() %>% class() == 'MetaboProfile') {
    x %>%
      resultsProcessing() %>%
      sampleInfo()  
  } else {
    stop('The processed data should be an S4 object of class MetaboProfile!')
  }
})

