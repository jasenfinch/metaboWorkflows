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
                   path(workflow),
                   renv = renv(workflow))
            
            message('Adding targets infrastructure')
            targetsScript(project_directory,type = 'report')
            write('\nmetaboMisc::suitableParallelPlan()\n',
                  file = paste0(project_directory,'/_targets.R'),
                  append = TRUE)
            targetsRun(project_directory)
            writeTargets(targets(workflow),paste0(project_directory,'/_targets.R'))
            
            utils(glue('{project_directory}/R'),
                  cran = c('purrr','targets','tarchetypes','future'),
                  github = githubDependencies(workflow))
            
            inputPrep(workflow)
            
            output(project_directory)
            
            if (isTRUE(renv(workflow))){
              renvInitialise(project_directory,
                             bioc = biocDependencies(workflow),
                             github = c(otherDependencies(workflow),
                                        githubDependencies(workflow)))
            }
            
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

readme <- function(project_name,workflow,path = '.',renv = TRUE){
  message('Adding project README')
  
  project_directory <- projectDirectory(project_name, path)
  
  if (isTRUE(renv)){
    renv_text <- 'The [renv](https://rstudio.github.io/renv/index.html) package is used for managing the project R package dependencies.'
  } else {
    renv_text <- ''
  }
  
  body <- glue("# {project_name}
             
This is an {workflow} metabolomics analysis workflow project.
This project uses the [targets](https://docs.ropensci.org/targets/) package for workflow management and reproducibility.
A [`git`](https://git-scm.com/) repository is initialted within the project that can be used to track changes to the analysis code.
{renv_text}

The R code for the analysis targets can be found in the `_targets.R` file.
Files containing additional functions can be placed in `R/functions` and package loading utilities can be found in `R/utils.R`.

## Running the analysis

To run the analysis, either run the `run.R` script or exucute `targets::tar_make()` in an R session loaded from within the project directory.

")
  
  writeLines(body, glue('{project_directory}/README.md'))
}

output <- function(project_directory){
  
}

#' @importFrom purrr map
#' @importFrom glue glue_collapse
#' @importFrom styler style_file

targetsList <- function(workflow_targets){
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
  
  return(workflow_targets)
}

#' @importFrom utils capture.output

writeTargets <- function(workflow_targets,file_path){
  
  targets_list <- targetsList(workflow_targets)
  
  write(targets_list,file_path,append = TRUE)
  
  out <- capture.output(style_file(file_path))
}

setGeneric('inputPrep',function(x)
  standardGeneric('inputPrep'))

#' @importFrom utils write.csv
#' @importFrom yaml write_yaml

setMethod('inputPrep',signature = 'Workflow',
          function(x){
            input_type <- x %>% 
              input() %>% class()
            
            project_directory <- projectDirectory(
              projectName(x),
              path(x)
            )
            
            if (input_type == 'FilePathInput') {
              x %>% 
                filePaths() %>% 
                writeLines(glue('{project_directory}/data/file_paths.txt'))
              x %>% 
                sampleInformation() %>% 
                write.csv(glue('{project_directory}/data/runinfo.csv'),
                          row.names = FALSE)
              
            }
            
            if (input_type == 'GroverInput') {
              
              grover_client <- list(host = host(x),
                                    port = port(x),
                                    auth = auth(x))
              write_yaml(grover_client,
                         glue('{project_directory}/misc/grover_client.yml'))
            }
          })