availableWorkflows <- function(){
  c('FIE-HRMS fingerprinting')
}

setClassUnion('Input',members = c('FilePathInput','GroverInput'))

setClass('Workflow',
         slots = list(
           type = 'character',
           input = 'Input', 
           targets = 'list'
         ),
         prototype = list(
           type = availableWorkflows()[1],
           input = groverInput('An_instrument',
                               'A_directory',
                               'A.host.address',
                               8000,
                               '1234'),
           targets = workflowTargets(availableWorkflows()[1])
         ))

# setMethod('show',signature = 'Workflow',
#           function(object){
#   
# })