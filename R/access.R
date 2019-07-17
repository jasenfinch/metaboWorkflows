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