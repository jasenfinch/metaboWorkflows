availableWorkflows <- function(){
  c('FIE-HRMS fingerprinting')
}

setClassUnion('Input',members = c('FilePathInput','GroverInput'))

setClass('Workflow',
         contains = c('Project','Input'),
         slots = list(
           type = 'character',
           targets = 'list'
         ),
         prototype = list(
           type = availableWorkflows()[1],
           targets = workflowTargets(availableWorkflows()[1])
         ))

# setMethod('show',signature = 'Workflow',
#           function(object){
#   
# })