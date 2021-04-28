#' Generate a workflow project directory
#' @rdname generateWorkflow
#' @description Generate a workflow project directory from a workflow definition.
#' @param workflow S4 object of class `Workflow`
#' @examples 
#' \dontrun{
#' file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
#' sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
#'
#' workflow_input <- filePathInput(file_paths,sample_information)
#' 
#' workflow_definition <- defineWorkflow(workflow_input,
#'                                       'FIE-HRMS fingerprinting',
#'                                       'Example project')
#'                                       
#' generateWorkflow(workflow_definition)
#' }
#' @export

setGeneric('generateWorkflow',function(workflow)
  standardGeneric('generateWorkflow'))

#' @rdname generateWorkflow
#' @importFrom projecttemplates projectDirectory projectSkeleton targetsScript targetsRun utils renvInitialise docker createGit githubActions createGithub
#' @importFrom cli symbol
#' @importFrom crayon green

setMethod('generateWorkflow',signature = 'Workflow',
          function(workflow){
            project_directory <- projectDirectory(projectName(workflow),
                                                  path(workflow))
            
            projectSkeleton(project_directory,
                            force = force(workflow))
            readme(projectName(workflow),
                   type(workflow),
                   path(workflow))
            
            message('Adding targets infrastructure')
            targetsScript(project_directory,type = 'report')
            targetsRun(project_directory)
            writeTargets(targets(workflow),paste0(project_directory,'/_targets.R'))
            
            utils(glue('{project_directory}/R'),
                  cran = c('purrr','targets','tarchetypes'),
                  github = githubDependencies(workflow))
            
            inputPrep(workflow)
            
            output(project_directory)
            
            renvInitialise(project_directory,
                           github = githubDependencies(workflow), 
                           rebuild = rebuild(workflow))
            
            projecttemplates::docker(projectName(workflow),
                                     path(workflow))
            
            if (all(github(workflow), githubActions(workflow))) {
              projecttemplates::githubActions(projectName(workflow), 
                                              path(workflow))
            }
            
            createGit(project_directory,type = 'report')
            
            if (isTRUE(github)) {
              createGithub(projectName(workflow), 
                           path(workflow), 
                           private(workflow))
            }
            message()
            
            message(green(symbol$tick),
                    ' ',
                    glue("Project directory creation complete. See {project_directory}/README.md for details on how to get started."))     
          })

#' @importFrom glue glue

readme <- function(project_name,workflow,path){
  message('Adding project README')
  
  project_directory <- projectDirectory(project_name, path)
  body <- glue("# {project_name}
             
This is a {workflow} metabolomics analysis workflow project.
This project is powered the [targets](https://docs.ropensci.org/targets/) package for workflow management and [renv](https://rstudio.github.io/renv/index.html) package for `R` environment reproducibility.

## Getting started

Add analysis targets to `R/targets.R`, scripts containing functions to the `R/functions` directory, data files to the `data` directory, additional miscellaneous scripts to `misc` and communicate your results in `report/report.Rmd`.
To run the analysis, execute `targets::tar_make()` in an `R` session loaded from within the project directory.
")
  
  writeLines(body, glue('{project_directory}/README.md'))
}

output <- function(project_directory){
  
}

#' @importFrom purrr map
#' @importFrom glue glue_collapse
#' @importFrom styler style_file

writeTargets <- function(workflow_targets,file_path){
  
  workflow_targets <- workflow_targets %>%
    map(~{
      .x %>% 
        map(code)
    })
  
  workflow_targets <- workflow_targets %>%
    map(~{
      wt <- .x
      wt %>%
        names() %>%
        map(~{
          glue('
{.x} = {wt[[.x]]}')
        }) %>%
        glue_collapse(sep = ',
')
    })
  
  workflow_targets <- workflow_targets %>% 
    names() %>% 
    map_chr(~{
      glue('{.x} = list(
    {workflow_targets[[.x]]}
  )')
    }) %>% 
    glue_collapse(sep = ',
')
  
  workflow_targets <- glue('list(
  {workflow_targets}
)')
  
  write(workflow_targets,file_path,append = TRUE)
  
  out <- capture.output(style_file(file_path))
}