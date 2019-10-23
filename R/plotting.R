#' plotPCA
#' @description Plot principle component analysis results of pre-treated data.
#' @param analysis object of class Workflow containing analysis results
#' @param cls info column to use for sample labelling
#' @param label info column to use for sample labels. Set to NULL for no labels.
#' @param scale scale the data
#' @param center center the data
#' @param xAxis principle component to plot on the x-axis
#' @param yAxis principle component to plot on the y-axis
#' @param ellipses should multivariate normal distribution 95\% confidence ellipses be plotted for each class?
#' @param title plot title
#' @param legendPosition legend position to pass to legend.position argument of \code{ggplot2::theme}
#' @param labelSize label size. Ignored if \code{label} is \code{NULL}
#' @importMethodsFrom metabolyseR plotPCA
#' @export

setMethod('plotPCA',signature = 'Workflow',
          function(analysis, cls = 'class', label = NULL, scale = T, center = T, xAxis = 'PC1', yAxis = 'PC2', ellipses = T, title = 'Principle Component Analysis (PCA)', legendPosition = 'bottom', labelSize = 2){
  analysis %>%
    resultsAnalysis() %>%
    plotPCA(cls = cls, label = label, scale = scale, center = center, xAxis = xAxis, yAxis = yAxis, ellipses = ellipses, title = title, legendPosition = legendPosition, labelSize = labelSize)
})

#' plotLDA
#' @description Plot linear discriminant analysis resultus of pre-treated data
#' @param analysis object of class Workflow containing analysis results
#' @param cls info column to use for sample labelling
#' @param label info column to use for sample labels. Set to NULL for no labels.
#' @param scale scale the data
#' @param center center the data
#' @param xAxis principle component to plot on the x-axis
#' @param yAxis principle component to plot on the y-axis
#' @param ellipses should multivariate normal distribution 95\% confidence ellipses be plotted for each class?
#' @param title plot title
#' @param legendPosition legend position to pass to legend.position argument of \code{ggplot2::theme}
#' @param labelSize label size. Ignored if \code{label} is \code{NULL}
#' @importMethodsFrom metabolyseR plotLDA
#' @export

setMethod('plotLDA',signature = 'Workflow',
          function(analysis, cls = 'class', label = NULL, scale = T, center = T, xAxis = 'DF1', yAxis = 'DF2', ellipses = T, title = 'Principle Component - Linear Discriminant Analysis (PC-LDA)', legendPosition = 'bottom', labelSize = 2){
            analysis %>%
              resultsAnalysis() %>%
              plotLDA(cls = cls, label = label, scale = scale, center = center, xAxis = xAxis, yAxis = yAxis, ellipses = ellipses, title = title, legendPosition = legendPosition, labelSize = labelSize)
          })

#' plotSupervisedRF
#' @param x object of class Workflow containing analysis results
#' @param cls info column to use for sample classes
#' @param rf list of additional parameters to pass to randomForest
#' @param label info column to use for sample labels. Set to NULL for no labels.
#' @param ellipses should multivariate normal distribution 95\% confidence ellipses be plotted for each class?
#' @param ROC should reciever-operator characteristics be plotted?
#' @param seed random number seed
#' @param title plot title
#' @param legendPosition legend position to pass to legend.position argument of \code{ggplot2::theme}
#' @param labelSize label size. Ignored if \code{label} is \code{NULL}
#' @importMethodsFrom  metabolyseR plotSupervisedRF
#' @export

setMethod('plotSupervisedRF',signature = 'Workflow',
          function(x, cls = 'class', rf = list(), label = NULL, ellipses = T, ROC = T, seed = 1234, title = '', legendPosition = 'bottom', labelSize = 2){
            x %>%
              resultsAnalysis() %>%
              plotSupervisedRF(cls = cls, rf = rf, label = label, ellipses = ellipses, ROC = ROC, seed = seed, title = title, legendPosition = legendPosition, labelSize = labelSize)
          })

#' plotUnsupervisedRF
#' @param x object of class Workflow containing analysis results
#' @param cls info column to use for sample labelling
#' @param rf list of additional parameters to pass to randomForest
#' @param label info column to use for sample labels. Set to NULL for no labels.
#' @param ellipses should multivariate normal distribution 95\% confidence ellipses be plotted for each class?
#' @param seed random number seed
#' @param title plot title
#' @param legendPosition legend position to pass to legend.position argument of \code{ggplot2::theme}
#' @param labelSize label size. Ignored if \code{label} is \code{NULL}
#' @importMethodsFrom metabolyseR plotUnsupervisedRF
#' @export

setMethod('plotUnsupervisedRF', signature = 'Workflow',
          function(x,cls = 'class', rf = list(), label = NULL, ellipses = T, seed = 1234, title = '', legendPosition = 'bottom', labelSize = 2){
            x %>%
              resultsAnalysis() %>%
              plotUnsupervisedRF(cls = cls, rf = rf, label = label, ellipses = ellipses, seed = seed, title = title, legendPosition = legendPosition, labelSize = labelSize)
          }
)

#' plotFeature
#' @description Plot a pre-treated feature trend.
#' @param analysis object of class Workflow containing analysis results
#' @param feature feature to plot
#' @param cls info column to use for class labels
#' @param label info column to use for sample labels
#' @param labelSize sample label size
#' @importMethodsFrom metabolyseR plotFeature
#' @export

setMethod('plotFeature',signature = 'Workflow',
          function(analysis, feature, cls = 'class', label = NULL, labelSize = 2){
            analysis %>%
              preTreated() %>%
              plotFeature(feature = feature,cls = cls,label = label,labelSize = labelSize)
          })

#' plotRSD
#' @description Plot RSD distributions of raw data in quality control samples.
#' @param analysis object of class Workflow containing analysis results
#' @param cls info column to use for class labels
#' @param QCidx QC sample label
#' @param QCparameters alternative parameters for QC sample pre-treatment. See details
#' @param modes split modes if present
#' @param histBins number of bins to use for histogram plotting
#' @param title plot title
#' @details If QCparameters is set as \code{NULL}, the default QC pre-treatment parameters are used as given by \code{analysisParameters('preTreat')}. Alternative pre-treatment routines can be used by specifying an \code{AnalysisParameters} object for \code{QCparameters}.
#' @importMethodsFrom metabolyseR plotRSD
#' @export

setMethod('plotRSD',signature = 'Workflow',
          function(analysis, cls = 'class', QCidx = 'QC', QCparameters = NULL, modes = T, histBins = 30, title = 'Relative standard deviation distributions'){
            analysis %>%
              resultsAnalysis() %>%
              plotRSD(cls = cls, QCidx = QCidx, QCparameters = QCparameters, modes = modes, histBins = histBins, title = title)
          })