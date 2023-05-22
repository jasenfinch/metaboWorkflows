test_that('Project class definition works',{
  workflow_project <- defineProject('A metabolomics project')
  
  expect_s4_class(workflow_project,'Project')
})

test_that('Project elements can be returned',{
  workflow_project <- defineProject('A metabolomics project')
  
  expect_identical(projectName(workflow_project),
                   'A metabolomics project')
  expect_identical(path(workflow_project),'.')
  expect_identical(renv(workflow_project),
                   TRUE)
  expect_identical(docker(workflow_project),
                   TRUE)
  expect_identical(github(workflow_project),
                   FALSE)
  expect_identical(private(workflow_project),
                   FALSE)
  expect_identical(githubActions(workflow_project),
                   FALSE)
  expect_identical(parallelPlan(workflow_project),
                   rlang::expr(jfmisc::suitableParallelPlan()))
  expect_identical(force(workflow_project),
                   FALSE)
})

test_that('Project elements can be set',{
  workflow_project <- defineProject('A metabolomics project')
  
  projectName(workflow_project) <- 'A new name'
  path(workflow_project) <- '../'
  renv(workflow_project) <- FALSE
  docker(workflow_project) <- FALSE
  github(workflow_project) <- TRUE
  private(workflow_project) <- TRUE
  githubActions(workflow_project) <- TRUE
  parallelPlan(workflow_project) <- rlang::expr(future::plan())
  force(workflow_project) <- TRUE
  
  expect_identical(projectName(workflow_project),
                   'A new name')
  expect_identical(path(workflow_project),'../')
  expect_identical(renv(workflow_project),
                   FALSE)
  expect_identical(docker(workflow_project),
                   FALSE)
  expect_identical(github(workflow_project),
                   TRUE)
  expect_identical(private(workflow_project),
                   TRUE)
  expect_identical(githubActions(workflow_project),
                   TRUE)
  expect_identical(parallelPlan(workflow_project),
                   rlang::expr(future::plan()))
  expect_identical(force(workflow_project),
                   TRUE)
})
