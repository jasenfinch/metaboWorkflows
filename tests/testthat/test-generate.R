test_that('generateWorkflow works for file path input',{
  temp_dir <- tempdir()
  
  file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
  sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
  
  workflow_input <- inputFilePath(file_paths,sample_information)
  
  workflow_definition <- defineWorkflow(workflow_input,
                                        'FIE-HRMS fingerprinting',
                                        'Example project',
                                        path = temp_dir,
                                        renv = FALSE
                                        )
  
  generateWorkflow(workflow_definition,start = FALSE)
  
  expect_true(dir.exists(paste0(temp_dir,'/Example_project')))
  
  unlink(paste0(temp_dir,'/Example_project'),recursive = TRUE)
})

test_that('generateWorkflow works for grover API input',{
  temp_dir <- tempdir()
  
  workflow_input <- inputGrover('an_instrument',
                                'a_directory',
                                'a.grover.host',
                                80,
                                '1234')
  
  workflow_definition <- defineWorkflow(workflow_input,
                                        'FIE-HRMS fingerprinting',
                                        'Example project',
                                        path = temp_dir,
                                        renv = FALSE
  )
  
  generateWorkflow(workflow_definition,start = FALSE)
  
  expect_true(dir.exists(paste0(temp_dir,'/Example_project')))
  
  unlink(paste0(temp_dir,'/Example_project'),recursive = TRUE)
})