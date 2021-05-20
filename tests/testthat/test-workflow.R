test_that("workflow definition works", {
  file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
  sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
  
  workflow_input <- inputFilePath(file_paths,sample_information)
  workflow_definition <- defineWorkflow(workflow_input,
                                        'FIE-HRMS fingerprinting',
                                        'Example project')
  
  expect_s4_class(workflow_definition,'Workflow')
})

test_that('Workflow slots can be returned',{
  file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
  sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
  
  file_path_input <- inputFilePath(file_paths,sample_information)
  grover_input <- inputGrover('Instrument','Experiment','a.grover.host',80,'1234')
  
  workflow_file_path <- defineWorkflow(file_path_input,
                                       'FIE-HRMS fingerprinting',
                                       'Example project')
  workflow_grover <- defineWorkflow(grover_input,
                                       'FIE-HRMS fingerprinting',
                                       'Example project')
  
  expect_identical(type(workflow_file_path),"FIE-HRMS fingerprinting")
  expect_s4_class(input(workflow_file_path),'FilePathInput')
  expect_type(targets(workflow_file_path),'list')
  
  expect_identical(filePaths(workflow_file_path),file_paths)
  expect_identical(sampleInformation(workflow_file_path),sample_information)
  
  expect_identical(instrument(workflow_grover),'Instrument')
  expect_identical(directory(workflow_grover),'Experiment')
  expect_identical(host(workflow_grover),'a.grover.host')
  expect_identical(port(workflow_grover),80)
  expect_identical(auth(workflow_grover),'1234')
})

test_that('Workflow slots can be set',{
  file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
  sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
  
  file_path_input <- inputFilePath(file_paths,sample_information)
  grover_input <- inputGrover('Instrument','Experiment','a.grover.host',80,'1234')
  
  workflow_file_path <- defineWorkflow(file_path_input,
                                       'FIE-HRMS fingerprinting',
                                       'Example project')
  workflow_grover <- defineWorkflow(grover_input,
                                    'FIE-HRMS fingerprinting',
                                    'Example project')
  
  input(workflow_file_path) <- inputFilePath(file_paths[1],sample_information[1,])
  targets(workflow_file_path) <- list('test')
  
  expect_error(type(workflow_file_path) <- 'test')
  expect_length(input(workflow_file_path) %>% 
                  filePaths(),1)
  expect_identical(targets(workflow_file_path),list('test'))
  
  filePaths(workflow_file_path) <- file_paths
  sampleInformation(workflow_file_path) <- sample_information
  
  expect_identical(filePaths(workflow_file_path),file_paths)
  expect_identical(sampleInformation(workflow_file_path),sample_information)
  
  instrument(workflow_grover) <- 'New Instrument'
  directory(workflow_grover) <- 'New Experiment'
  host(workflow_grover) <- 'new.grover.host'
  port(workflow_grover) <- 8000
  auth(workflow_grover) <- '4321'
  
  expect_identical(instrument(workflow_grover),'New Instrument')
  expect_identical(directory(workflow_grover),'New Experiment')
  expect_identical(host(workflow_grover),'new.grover.host')
  expect_identical(port(workflow_grover),8000)
  expect_identical(auth(workflow_grover),'4321')
})
