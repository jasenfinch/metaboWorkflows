#' Validate a workflow definition
#' @rdname validate
#' @description Validate a workflow definition, checking for issues.
#' An error or a warning will be returned if a problem is detected.
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
#' validate(workflow_definition)
#' @export

setGeneric('validate',function(x)
  standardGeneric('validate'))

#' @rdname validate
#' @importFrom targets tar_validate
#' @importFrom rlang parse_exprs eval_tidy

setMethod('validate',signature = 'Workflow',
          function(x){
            
            validation <-  glue('targets::tar_dir({{
                              targets::tar_script({{
                                library(tarchetypes)
                                
                                dir.create("./report")
                                writeLines(\'{rmd(x)}\',"./report/report.Rmd")
                                
                                {x %>%
                                  targets() %>% 
                                  targetsList()}
                                }}, ask = FALSE)
                              targets::tar_validate()
                            }})') %>% 
              parse_exprs() %>% 
              map(eval_tidy)
            
            invisible(validation)
          })