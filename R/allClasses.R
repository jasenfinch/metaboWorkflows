#' @importClassesFrom binneRlyse BinParameters Binalysis
#' @importClassesFrom metabolyseR AnalysisParameters Analysis

setClass('WorkflowParameters',
         slots = list(
           workflow = 'character',
           processing = 'BinParameters',
           analysis = 'AnalysisParameters'
         )
)

setClass('Workflow',
         slots = list(
           logs = 'list',
           flags = 'character',
           files = 'character',
           workflowParameters = 'WorkflowParameters',
           processed = 'Binalysis',
           analysed = 'Analysis')
)