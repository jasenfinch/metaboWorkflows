#' @importClassesFrom binneRlyse BinParameters Binalysis
#' @importClassesFrom metabolyseR AnalysisParameters Analysis
#' @importClassesFrom MFassign AssignmentParameters Assignment
#' @importClassesFrom profilePro ProfileParameters MetaboProfile 

setClassUnion('Processed',c('Binalysis','MetaboProfile'))
setClassUnion('Processing',c('BinParameters','ProfileParameters'))

#' WorkflowParameters
#' @description An S4 class to store workflow parameters
#' @slot workflow character containing the workflow to use
#' @slot processing Processing object containing processing parameters for the specified workflow
#' @slot analysis AnalysisParameters object containing the analysis parameters
#' @slot annotation AssignmentParameters object containing the annotation parameters
#' @seealso \code{\link[metabolyseR]{AnalysisParameters}} \code{\link[MFassign]{AssignmentParameters}} \code{\link[binneRlyse]{BinParameters}} \code{\link[profilePro]{ProfileParameters}}
#' @export
setClass('WorkflowParameters',
         slots = list(
           workflow = 'character',
           processing = 'Processing',
           analysis = 'AnalysisParameters',
           annotation = 'AssignmentParameters'
         )
)

#' Workflow
#' @description  An S4 class to store workflow results
#' @slot logs list containing workflow dates and times
#' @slot flags character containing workflow sections completed
#' @slot files list file names to process
#' @slot workflowParameters WorkflowParameters object containing parameters used 
#' @slot processed Processed object containing processing results
#' @slot analysed Analysis object containing analysis results
#' @slot annotated Assignment object containing annotation results
#' @seealso \code{\link[metabolyseR]{Analysis-class}} \code{\link[MFassign]{Assignment-class}} \code{\link[binneRlyse]{Binalysis-class}} \code{\link[profilePro]{MetaboProfile-class}}
#' @export
setClass('Workflow',
         slots = list(
           logs = 'list',
           flags = 'character',
           files = 'list',
           info = 'tbl_df',
           workflowParameters = 'WorkflowParameters',
           processed = 'Processed',
           analysed = 'Analysis',
           annotated = 'Assignment'
           )
)