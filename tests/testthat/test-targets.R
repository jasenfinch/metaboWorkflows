test_that("glimpse works", {
  file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
  sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
  
  workflow_input <- filePathInput(file_paths,sample_information)
  
  workflow_definition <- defineWorkflow(workflow_input,
                                        'FIE-HRMS fingerprinting',
                                        'Example project')
  
  pl <- glimpse(workflow_definition)
  
  expect_s3_class(pl,'visNetwork')
})
