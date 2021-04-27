test_that("file path input works", {
  file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
  sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
  
  workflow_input <- filePathInput(file_paths,sample_information)
  
  expect_s4_class(workflow_input,'FilePathInput')
})

test_that('grover input works',{
  grover_input <- groverInput('an_instrument',
                              'a_directory',
                              'a_host_address',
                              '8000',
                              '1234')
  
  expect_s4_class(grover_input,'GroverInput')
})