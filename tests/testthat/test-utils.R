
file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')

workflow_input <- inputFilePath(file_paths,sample_information)

workflow_definition <- defineWorkflow(workflow_input,
                                      'FIE-HRMS fingerprinting',
                                      'Example project')

test_that("GitHub dependencies returned", {
  gh_deps <- githubDependencies(workflow_definition)
  
  expect_type(gh_deps,'character')
})

test_that("Bioc dependencies returned", {
  bioc_deps <- biocDependencies(workflow_definition)
  
  expect_type(bioc_deps,'character')
})

test_that("other dependencies returned", {
  other_deps <- otherDependencies(workflow_definition)
  
  expect_type(other_deps,'character')
})

test_that('GitHub dependencies returned for grover input',{
  grover_input <- inputGrover('an_instrument',
                              'a_directory',
                              'a_host_address',
                              8000,
                              '1234')
  
  gh_deps <- githubInputDependencies(grover_input)
  
  expect_type(gh_deps,'character')
})

test_that('Other dependencies returned for grover input',{
  grover_input <- inputGrover('an_instrument',
                              'a_directory',
                              'a_host_address',
                              8000,
                              '1234')
  
  other_deps <- otherInputDependencies(grover_input)
  
  expect_type(other_deps,'character')
})