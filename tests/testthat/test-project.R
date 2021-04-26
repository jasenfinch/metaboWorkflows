
test_that('project creation works',{
  
  skip('Skipping project creation test')
  
  createProject('test project',path = tempdir())
  
  project_files <- list.files(paste0(tempdir(),'/test_project'),all.files = TRUE)
  
  expect_length(project_files,15)
  
  unlink(paste0(tempdir(),'/test_project'))
})
