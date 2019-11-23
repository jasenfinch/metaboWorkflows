library(metaboWorkflows)

si <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
fp <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')[c(which(si$class == 'ABR6')[1:3],which(si$class == 'BD21')[1:3])] 
si <- si[c(which(si$class == 'ABR6')[1:3],which(si$class == 'BD21')[1:3]),] %>%
  mutate(block = c(rep(1,3),rep(2,3)))

context('workflow')

test_that('workflow works',{
  wp <- workflowParameters('FIE-HRMS fingerprinting',fp,si,nCores = 2)  
  parametersProcessing(wp)@nCores <- 2
  flags(wp) <- flags(wp)[1]
  analysis <- workflow(wp)
  expect_s4_class(wp,'WorkflowParameters')
  expect_s4_class(analysis,'Workflow')
})
