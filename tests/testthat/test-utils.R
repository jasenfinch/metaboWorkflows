
file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')

workflow_input <- inputFilePath(file_paths,sample_information)

workflow_definition <- defineWorkflow(workflow_input,
                                      'FIE-HRMS fingerprinting',
                                      'Example project')

test_that("Workflow dependencies returned", {
  workflow_deps <- workflowDependencies(workflow_definition)
  
  expect_type(workflow_deps,'character')
})

test_that('Workflow dependencies returned for grover input',{
  grover_input <- inputGrover('an_instrument',
                              'a_directory',
                              'a_host_address',
                              8000,
                              '1234')
  
  input_deps <- inputDependencies(grover_input)
  
  expect_type(input_deps,'character')
})
