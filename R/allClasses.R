#' @importClassesFrom binneRlyse BinParameters Binalysis
#' @importClassesFrom metabolyseR AnalysisParameters Analysis

setClass('WorkflowParameters',
         slots = list(
           technique = 'character',
           processing = 'BinParameters',
           analysis = 'Parameters'
         )
)

setClass('Workflow',
         slots = list(
           logs = 'list',
           files = 'character',
           workflowParameters = 'WorkflowParameters',
           processed = 'Binalysis',
           analysed = 'Analysis')
)