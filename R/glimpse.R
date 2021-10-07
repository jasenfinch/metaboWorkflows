#' Visualise the workflow targets
#' @rdname glimpse
#' @description Visualise the directed acyclic graph for a workflow's targets.
#' @param x S4 object of class Workflow
#' @examples 
#' file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
#' sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
#'
#' workflow_input <- inputFilePath(file_paths,sample_information)
#' 
#' workflow_definition <- defineWorkflow(workflow_input,
#'                                       'FIE-HRMS fingerprinting',
#'                                       'Example project')
#'                  
#' glimpse(workflow_definition)
#' @export

setGeneric('glimpse',function(x)
  standardGeneric('glimpse'))

#' @rdname glimpse
#' @importFrom targets tar_dir tar_script tar_glimpse
#' @importFrom rlang parse_exprs eval_tidy

setMethod('glimpse',signature = 'Workflow',
          function(x){
            
            graph <-  glue('targets::tar_dir({{
                              targets::tar_script({{
                                library(tarchetypes)
                                
                                dir.create("./report")
                                writeLines(\'{rmd(x)}\',"./report/report.Rmd")
                                
                                {x %>%
                                  targets() %>% 
                                  targetsList()}
                                }}, ask = FALSE)
                              targets::tar_glimpse()
                            }})') %>% 
              parse_exprs() %>% 
              map(eval_tidy)
            
            graph[[1]]
          })