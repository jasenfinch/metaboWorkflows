
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
