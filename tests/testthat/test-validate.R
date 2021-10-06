test_that("validate works", {
  file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
  sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
  
  workflow_input <- inputFilePath(file_paths,sample_information)
  
  workflow_definition <- defineWorkflow(workflow_input,
                                        'FIE-HRMS fingerprinting',
                                        'Example project')
  
  expect_invisible(validate(workflow_definition))
})
