test_that("target defintion works", {
  workflow_target <- target('a_target','1 + 1')
  
  expect_s4_class(workflow_target,'Target')
  expect_output(print(workflow_target),'tar_target')
})

test_that('invalid targets are correctly detected',{
  expect_error(target('a target','1 + 1'))
  expect_error(target('a_target','1 + 1',type = 'wrong'))
})

test_that('target elements can be returned',{
  workflow_target <- target('a_target','1 + 1')
  
  expect_identical(name(workflow_target),'a_target')
  expect_identical(command(workflow_target),'1 + 1')
  expect_identical(type(workflow_target),'tar_target')
  expect_identical(args(workflow_target),list())
  expect_identical(comment(workflow_target),character())
  expect_type(code(workflow_target),'character')
})

test_that('target elements can be set',{
  workflow_target <- target('a_target','1 + 1')
  
  name(workflow_target) <- 'new_target'
  command(workflow_target) <- '2 + 2'
  type(workflow_target) <- 'tar_file'
  args(workflow_target) <- list(format = 'file')
  comment(workflow_target) <- 'a target'
  
  expect_identical(name(workflow_target),'new_target')
  expect_identical(command(workflow_target),'2 + 2')
  expect_identical(type(workflow_target),'tar_file')
  expect_identical(args(workflow_target),list(format = 'file'))
  expect_identical(comment(workflow_target),'a target')
  expect_type(code(workflow_target),'character') 
})