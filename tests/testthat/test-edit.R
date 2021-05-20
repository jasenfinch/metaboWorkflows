
file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')

workflow_input <- inputFilePath(file_paths,sample_information)

workflow_definition <- defineWorkflow(workflow_input,
                                      'FIE-HRMS fingerprinting',
                                      'Example project')

replacement_target <- target('replacement','1 + 1')

test_that("module replacement works", {
  workflow_definition <- moduleReplace(workflow_definition,
                                       'input',
                                       list(replacement = replacement_target))
  
  expect_identical(targets(workflow_definition)$input$replacement,replacement_target)
})

test_that("module removal works", {
  workflow_definition <- moduleRemove(workflow_definition,'input')
  
  expect_null(targets(workflow_definition)$input)
})

test_that("module removal errors when incorrect module specified", {
  expect_error(moduleRemove(workflow_definition,'wrong'))
})


test_that('module addition works',{
  workflow_definition <- moduleAdd(workflow_definition,
                                   'additional_target',
                                   list(new_target = replacement_target))
  
  expect_true('additional_target' %in% modules(workflow_definition))
})

test_that('module addition errors when non-module specified',{
  expect_error(moduleAdd(workflow_definition,
                                   'additional_target',
                                   1))
})

test_that('module addition errors when non-target specified',{
  expect_error(moduleAdd(workflow_definition,
                         'additional_target',
                         list(new_target = 1)))
})

test_that("target replacement works", {
  workflow_definition <- targetReplace(workflow_definition,
                                       'input',
                                       'file_paths_list',
                                       replacement_target)
  
  expect_identical(targets(workflow_definition)$input$file_paths_list,
                   replacement_target)
})

test_that("target removal works", {
  workflow_definition <- targetRemove(workflow_definition,'input','file_paths_list')
  
  expect_null(targets(workflow_definition)$input$file_paths_list)
})

test_that("target removal errors when incorrect target specified", {
  expect_error(targetRemove(workflow_definition,'input','wrong'))
})

test_that('target addition works',{
  workflow_definition <- targetAdd(workflow_definition,
                                   'input',
                                   'additional_target',
                                   replacement_target)
  
  expect_true('additional_target' %in% names(targets(workflow_definition)[['input']]))
})
  
test_that('target addition errors when non-target specified',{
  expect_error(targetAdd(workflow_definition,
                                   'input',
                                   'additional_target',
                                   list()))
})
