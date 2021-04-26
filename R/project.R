#' Create a project directory
#' @description Create a project directory for a workflow.
#' @param project_name project name/title
#' @param workflow workflow to use. See `availableWorkflows()` for a list of available workflows.
#' @param path target file path for project directory 
#' @param rebuild force rebuild of R packages installed into project `renv` cache
#' @param docker TRUE/FALSE. Create project infrastructure for building a docker container to compile the project.
#' @param github TRUE/FALSE. Create a GitHub repository?
#' @param private TRUE/FALSE. Should the GitHub repository be private? Evaluated only if argument `github` is TRUE.
#' @param github_actions TRUE/FALSE. Add Github actions infrastructure? Evaluated only if argument `github` is TRUE.
#' @param force force project creation if project directory already exists
#' @examples 
#' \dontrun{
#' createProject('test',
#'               workflow = 'FIE-HRMS fingerprinting',
#'               path = '.')
#' }
#' @importFrom projecttemplates projectDirectory projectSkeleton targetsScript targetsRun utils renvInitialise docker createGit githubActions createGithub
#' @importFrom cli symbol
#' @importFrom crayon green
#' @export

createProject <- function(project_name,workflow = availableWorkflows(),path = '.',rebuild = FALSE,docker = TRUE,github = FALSE,private = FALSE,github_actions = FALSE,force = FALSE){
  project_directory <- projectDirectory(project_name,path)
  
  workflow <- match.arg(workflow,
                        choices = availableWorkflows())
  
  ## Create project infrastructure
  projectSkeleton(project_directory,force = force)
  readme(project_name,workflow,path)
  
  message('Adding targets infrastructure')
  targetsScript(project_directory,type = 'report')
  targetsRun(project_directory)
  
  utils(project_directory,type = 'report')
  
  output(project_directory)
  
  renvInitialise(project_directory, rebuild = rebuild)
  
  docker(project_name,path)
  
  if (all(github, github_actions)) {
    githubActions(project_name, path)
  }
  
  createGit(project_directory,type = 'report')
  
  if (isTRUE(github)) {
    createGithub(project_name, path, private)
  }
  message()
  
  message(green(symbol$tick),
          ' ',
          glue("Project directory creation complete. See {project_directory}/README.md for details on how to get started."))
}

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