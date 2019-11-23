
setGeneric("doWorkflow", function(y) {
  standardGeneric("doWorkflow")
})

#' @rdname workflow
setGeneric("workflow", function(parameters) {
  standardGeneric("workflow")
})

#' @rdname restartWorkflow
setGeneric("restartWorkflow", function(analysis) {
  standardGeneric("restartWorkflow")
})

#' @rdname parametersProcessing
setGeneric("parametersProcessing", function(parameters) {
  standardGeneric("parametersProcessing")
})

#' @rdname parametersProcessing
setGeneric("parametersProcessing<-", function(parameters,value) {
  standardGeneric("parametersProcessing<-")
})

#' @rdname parametersAnalysis
setGeneric("parametersAnalysis", function(parameters) {
  standardGeneric("parametersAnalysis")
})

#' @rdname parametersAnalysis
setGeneric("parametersAnalysis<-", function(parameters,value) {
  standardGeneric("parametersAnalysis<-")
})

#' @rdname parametersAnnotation
setGeneric("parametersAnnotation", function(parameters) {
  standardGeneric("parametersAnnotation")
})

#' @rdname parametersAnnotation
setGeneric("parametersAnnotation<-", function(parameters,value) {
  standardGeneric("parametersAnnotation<-")
})

#' @rdname resultsParameters
setGeneric("resultsParameters", function(object) {
  standardGeneric("resultsParameters")
})

#' @rdname resultsProcessing
setGeneric("resultsProcessing", function(object) {
  standardGeneric("resultsProcessing")
})

#'  @rdname resultsAnalysis
setGeneric("resultsAnalysis", function(object) {
  standardGeneric("resultsAnalysis")
})

#'  @rdname resultsAnnotation
setGeneric("resultsAnnotation", function(object) {
  standardGeneric("resultsAnnotation")
})

#' @rdname flags
setGeneric("flags", function(x) {
  standardGeneric("flags")
})

#' @rdname plotTIC
setGeneric("plotTIC", function(x,...) {
  standardGeneric("plotTIC")
})

#' @rdname processedInfo
setGeneric("processedInfo", function(x) {
  standardGeneric("processedInfo")
})

#' @rdname files
setGeneric("files", function(x) {
  standardGeneric("files")
})

#' @rdname files
setGeneric("files<-", function(x,value) {
  standardGeneric("files<-")
})

#' @rdname info
setGeneric("info<-", function(x,value) {
  standardGeneric("info<-")
})
