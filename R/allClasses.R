#' @importClassesFrom binneRlyse BinParameters Binalysis
#' @importClassesFrom metabolyseR AnalysisParameters Analysis
#' @importClassesFrom MFassign AnnotationParameters Annotation

setClass('WorkflowParameters',
         slots = list(
           workflow = 'character',
           processing = 'BinParameters',
           analysis = 'AnalysisParameters',
           annotation = 'AnnotationParameters'
         )
)

setClass('Workflow',
         slots = list(
           logs = 'list',
           files = 'character',
           workflowParameters = 'WorkflowParameters',
           processed = 'Binalysis',
           analysed = 'Analysis',
           annotated = 'Annotation'
           )
)