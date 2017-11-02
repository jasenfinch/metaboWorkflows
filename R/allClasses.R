#' @importClassesFrom binneRlyse BinParameters Binalysis
#' @importClassesFrom metabolyseR AnalysisParameters Analysis
#' @importClassesFrom MFassign AssignmentParameters Assignment
#' @importClassesFrom profilePro ProfileParameters MetaboProfile 

setClassUnion('Processed',c('Binalysis','MetaboProfile'))
setClassUnion('Processing',c('BinParameters','ProfileParameters'))

#' @export
setClass('WorkflowParameters',
         slots = list(
           workflow = 'character',
           processing = 'Processing',
           analysis = 'AnalysisParameters',
           annotation = 'AssignmentParameters'
         )
)

#' @export
setClass('Workflow',
         slots = list(
           logs = 'list',
           flags = 'character',
           files = 'list',
           workflowParameters = 'WorkflowParameters',
           processed = 'Processed',
           analysed = 'Analysis',
           annotated = 'Assignment'
           )
)