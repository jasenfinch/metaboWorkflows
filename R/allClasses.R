#' @importClassesFrom binneRlyse BinParameters Binalysis
#' @importClassesFrom metabolyseR AnalysisParameters Analysis
#' @importClassesFrom MFassign AssignmentParameters Assignment

setClass('WorkflowParameters',
         slots = list(
           workflow = 'character',
           processing = 'BinParameters',
           analysis = 'AnalysisParameters',
           annotation = 'AssignmentParameters'
         )
)

setClass('Workflow',
         slots = list(
           logs = 'list',
           flags = 'character',
           files = 'character',
           workflowParameters = 'WorkflowParameters',
           processed = 'Binalysis',
           analysed = 'Analysis',
           annotated = 'Assignment'
           )
)