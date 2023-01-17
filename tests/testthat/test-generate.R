test_that('generateWorkflow works for file path input',{
  temp_dir <- tempdir()
  
  if (dir.exists(paste0(temp_dir,'/Example_project'))){
    unlink(paste0(temp_dir,'/Example_project'),recursive = TRUE)
  }
  
  file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
  sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
  
  workflow_input <- inputFilePath(file_paths,sample_information)
  
  workflow_definition <- defineWorkflow(workflow_input,
                                        'FIE-HRMS fingerprinting',
                                        'Example project',
                                        path = temp_dir,
                                        renv = FALSE,
                                        force = TRUE
                                        )
  
  generateWorkflow(workflow_definition,start = FALSE)
  
  expect_true(dir.exists(paste0(temp_dir,'/Example_project')))
  
  unlink(paste0(temp_dir,'/Example_project'),recursive = TRUE)
})

test_that('generateWorkflow works for grover API input',{
  temp_dir <- tempdir()
  
  if (dir.exists(paste0(temp_dir,'/Example_project'))){
    unlink(paste0(temp_dir,'/Example_project'),recursive = TRUE)
  }
  
  workflow_input <- inputGrover('an_instrument',
                                'a_directory',
                                'a.grover.host',
                                80,
                                '1234')
  
  workflow_definition <- defineWorkflow(workflow_input,
                                        'FIE-HRMS fingerprinting',
                                        'Example project',
                                        path = temp_dir,
                                        renv = FALSE,
                                        force = TRUE
  )
  
  generateWorkflow(workflow_definition,start = FALSE)
  
  expect_true(dir.exists(paste0(temp_dir,'/Example_project')))
  
  unlink(paste0(temp_dir,'/Example_project'),recursive = TRUE)
})

test_that('aproject can be generated with Docker infrastructure but no GitHub API token',{
  temp_dir <- tempdir()
  gh_token <- Sys.getenv('GITHUB_PAT')
  Sys.setenv(GITHUB_PAT = '')
  
  if (dir.exists(paste0(temp_dir,'/Example_project'))){
    unlink(paste0(temp_dir,'/Example_project'),recursive = TRUE)
  }
  
  workflow_input <- inputGrover('an_instrument',
                                'a_directory',
                                'a.grover.host',
                                80,
                                '1234')
  
  workflow_definition <- defineWorkflow(workflow_input,
                                        'FIE-HRMS fingerprinting',
                                        'Example project',
                                        path = temp_dir,
                                        renv = FALSE,
                                        force = TRUE
  )
  
  expect_warning(generateWorkflow(workflow_definition,start = FALSE))
  
  expect_true(dir.exists(paste0(temp_dir,'/Example_project')))
  expect_true(grepl('latest',readLines(paste0(temp_dir,'/Example_project/misc/docker/Dockerfile'))[3]))
  
  unlink(paste0(temp_dir,'/Example_project'),recursive = TRUE)
  Sys.setenv(GITHUB_PAT = gh_token)
})