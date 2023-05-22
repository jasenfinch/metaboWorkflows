#' Workflow definition target information
#' @rdname manifest
#' @description Return a tibble of information about the targets in a workflow definition.
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
#' manifest(workflow_definition)
#' @export

setGeneric('manifest',function(x)
  standardGeneric('manifest'))

#' @rdname manifest
#' @importFrom targets tar_manifest

setMethod('manifest',signature = 'Workflow',
          function(x){
            
            
            project_directory <- projectDirectory(
              projectName(x),
              path(x)
            )
            
            manifest <-  glue('targets::tar_dir({{
                              targets::tar_script({{
                                
                                dir.create("./report")
                               writeLines(\'{rmd(x)}\',"./report/{basename(project_directory)}_report.Rmd") 
                                {x %>%
                                  targets() %>% 
                                  targetsList()}
                                }}, ask = FALSE)
                              targets::tar_manifest()
                            }})') %>% 
              parse_exprs() %>% 
              map(eval_tidy)
            
            return(manifest[[1]])
          })