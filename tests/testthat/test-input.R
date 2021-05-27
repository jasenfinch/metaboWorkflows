test_that("file path input works", {
  file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
  sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
  
  workflow_input <- inputFilePath(file_paths,sample_information)
  
  expect_s4_class(workflow_input,'FilePathInput')
})

test_that('input parameters can be returned form FilePathInput class',{
  file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
  sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
  
  workflow_input <- inputFilePath(file_paths,sample_information)
  
  expect_identical(filePaths(workflow_input),file_paths)
  expect_identical(sampleInformation(workflow_input),sample_information)
})

test_that('input parameters can be set for FilePathInput class',{
  file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
  sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
  
  workflow_input <- inputFilePath(file_paths,sample_information)
  
  new_file_paths <- file_paths[1:10]
  new_sample_information <- sample_information[1:10,]
  
  filePaths(workflow_input) <- new_file_paths
  sampleInformation(workflow_input) <- new_sample_information
  
  expect_identical(filePaths(workflow_input),new_file_paths)
  expect_identical(sampleInformation(workflow_input),new_sample_information)
})

test_that('grover input works',{
  grover_input <- inputGrover('an_instrument',
                              'a_directory',
                              'a_host_address',
                              8000,
                              '1234')
  
  expect_s4_class(grover_input,'GroverInput')
  expect_output(print(grover_input),'Grover')
})

test_that('input parameters can be returned form GroverInput class',{
  instrument <- 'an_instrument'
  directory <- 'a_directory'
  host <- 'a_host_address'
  port <- 8000
  auth <- '1234'
  
  grover_input <- inputGrover(instrument,
                              directory,
                              host,
                              port,
                              auth)
  
  expect_identical(instrument(grover_input),instrument)
  expect_identical(directory(grover_input),directory)
  expect_identical(host(grover_input),host)
  expect_identical(port(grover_input),port)
  expect_identical(auth(grover_input),auth)
})

test_that('input parameters can be set for GroverInput class',{
  instrument <- 'an_instrument'
  directory <- 'a_directory'
  host <- 'a_host_address'
  port <- 8000
  auth <- '1234'
  
  grover_input <- inputGrover(instrument,
                              directory,
                              host,
                              port,
                              auth)
  
  new_instrument <-'new_instrument'
  new_directory <- 'new_directory'
  new_host <- 'new_host'
  new_port <- 9000
  new_auth <- '4321'
  
  instrument(grover_input) <- new_instrument
  directory(grover_input) <- new_directory
  host(grover_input) <- new_host
  port(grover_input) <- new_port
  auth(grover_input) <- new_auth

  expect_identical(instrument(grover_input),new_instrument)
  expect_identical(directory(grover_input),new_directory)
  expect_identical(host(grover_input),new_host)
  expect_identical(port(grover_input),new_port)
  expect_identical(auth(grover_input),new_auth)
})

