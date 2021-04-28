#' S4 class to store project directory definitions
#' @description An S4 class to store the workflow project directory definitions.
#' @slot project_name the project name
#' @slot path the project directory path
#' @slot rebuild force rebuild of packages installed into project `renv` cache
#' @slot docker creation for project docker infrastructure
#' @slot github creation of a GitHub repository
#' @slot private private GitHub repository
#' @slot github_actions creation of GitHub actions infrastructure
#' @slot force force project creation if the project directory already exists

setClass('Project',
         slots = list(
           project_name = 'character',
           path = 'character',
           rebuild = 'logical',
           docker = 'logical',
           github = 'logical',
           private = 'logical',
           github_actions = 'logical',
           force = 'logical'
         ),
         prototype = list(
           project_name = 'A project name',
           path = '.',
           rebuild = FALSE,
           docker = TRUE,
           github = FALSE,
           private = FALSE,
           github_actions = FALSE,
           force = FALSE
         ))

setMethod('show',signature = 'Project',
          function(object){
            cat('Project name:',projectName(object),'\n')
            cat('Directory path:',path(object),'\n')
            cat('Package rebuild:',rebuild(object),'\n')
            cat('Docker:',docker(object),'\n')
            cat('GitHub repository:',github(object),'\n')
            cat('Private repository:',private(object),'\n')
            cat('GitHub Actions:',githubActions(object),'\n')
            cat('Force creation:',force(object),'\n')
          })

#' `Project` class get and set methods
#' @rdname Project-accessors
#' @description Get and set methods for the `Project` S4 class.
#' @param x S4 object of class `Project`
#' @param value value to set
#' @export

setGeneric('projectName',function(x)
  standardGeneric('projectName'))

#' @rdname Project-accessors

setMethod('projectName',signature = 'Project',
          function(x){
            x@project_name
          })

#' @rdname Project-accessors
#' @export

setGeneric('projectName<-',function(x,value)
  standardGeneric('projectName<-'))

#' @rdname Project-accessors

setMethod('projectName<-',signature = 'Project',
          function(x,value){
           x@project_name <- value
           return(x)
          })

#' @rdname Project-accessors
#' @export

setGeneric('path',function(x)
  standardGeneric('path'))

#' @rdname Project-accessors

setMethod('path',signature = 'Project',
          function(x){
            x@path
          })

#' @rdname Project-accessors
#' @export

setGeneric('path<-',function(x,value)
  standardGeneric('path<-'))

#' @rdname Project-accessors

setMethod('path<-',signature = 'Project',
          function(x,value){
            x@path <- value
            return(x)
          })

#' @rdname Project-accessors
#' @export

setGeneric('rebuild',function(x)
  standardGeneric('rebuild'))

#' @rdname Project-accessors

setMethod('rebuild',signature = 'Project',
          function(x){
            x@rebuild
          })

#' @rdname Project-accessors
#' @export

setGeneric('rebuild<-',function(x,value)
  standardGeneric('rebuild<-'))

#' @rdname Project-accessors

setMethod('rebuild<-',signature = 'Project',
          function(x,value){
            x@rebuild <- value
            return(x)
          })

#' @rdname Project-accessors
#' @export

setGeneric('docker',function(x)
  standardGeneric('docker'))

#' @rdname Project-accessors

setMethod('docker',signature = 'Project',
          function(x){
            x@docker
          })

#' @rdname Project-accessors
#' @export

setGeneric('docker<-',function(x,value)
  standardGeneric('docker<-'))

#' @rdname Project-accessors

setMethod('docker<-',signature = 'Project',
          function(x,value){
            x@docker <- value
            return(x)
          })

#' @rdname Project-accessors
#' @export

setGeneric('github',function(x)
  standardGeneric('github'))

#' @rdname Project-accessors

setMethod('github',signature = 'Project',
          function(x){
            x@github
          })

#' @rdname Project-accessors
#' @export

setGeneric('github<-',function(x,value)
  standardGeneric('github<-'))

#' @rdname Project-accessors

setMethod('github<-',signature = 'Project',
          function(x,value){
            x@github <- value
            return(x)
          })

#' @rdname Project-accessors
#' @export

setGeneric('private',function(x)
  standardGeneric('private'))

#' @rdname Project-accessors

setMethod('private',signature = 'Project',
          function(x){
            x@private
          })

#' @rdname Project-accessors
#' @export

setGeneric('private<-',function(x,value)
  standardGeneric('private<-'))

#' @rdname Project-accessors

setMethod('private<-',signature = 'Project',
          function(x,value){
            x@private <- value
            return(x)
          })

#' @rdname Project-accessors
#' @export

setGeneric('githubActions',function(x)
  standardGeneric('githubActions'))

#' @rdname Project-accessors

setMethod('githubActions',signature = 'Project',
          function(x){
            x@github_actions
          })

#' @rdname Project-accessors
#' @export

setGeneric('githubActions<-',function(x,value)
  standardGeneric('githubActions<-'))

#' @rdname Project-accessors

setMethod('githubActions<-',signature = 'Project',
          function(x,value){
            x@github_actions <- value
            return(x)
          })

#' @rdname Project-accessors
#' @export

setGeneric('force',function(x)
  standardGeneric('force'))

#' @rdname Project-accessors

setMethod('force',signature = 'Project',
          function(x){
            x@force
          })

#' @rdname Project-accessors
#' @export

setGeneric('force<-',function(x,value)
  standardGeneric('force<-'))

#' @rdname Project-accessors

setMethod('force<-',signature = 'Project',
          function(x,value){
            x@force <- value
            return(x)
          })

#' Define a project directory
#' @description Define the project directory for a workflow
#' @param project_name project name/title
#' @param path target file path for project directory 
#' @param rebuild force rebuild of R packages installed into project `renv` cache
#' @param docker TRUE/FALSE. Create project infrastructure for building a docker container to compile the project.
#' @param github TRUE/FALSE. Create a GitHub repository?
#' @param private TRUE/FALSE. Should the GitHub repository be private? Evaluated only if argument `github` is TRUE.
#' @param github_actions TRUE/FALSE. Add Github actions infrastructure? Evaluated only if argument `github` is TRUE.
#' @param force force project creation if project directory already exists
#' @return An S4 object of class `Project`.
#' @examples 
#' workflow_project <- defineProject('A metabolomics project')
#' @export

defineProject <- function(project_name,
                          path = '.',
                          rebuild = FALSE,
                          docker = TRUE,
                          github = FALSE,
                          private = FALSE,
                          github_actions = FALSE,
                          force = FALSE){
  new('Project',
      project_name = project_name,
      path = path,
      rebuild = rebuild,
      docker = docker,
      github = github,
      private = private,
      github_actions = github_actions,
      force = force)
  
}
 
#' Create a project directory
#' @rdname createProject
#' @description Create a project directory for a workflow.
#' @param project S4 object of class `Project`
#' @param workflow workflow to use. See `availableWorkflows()` for a list of available workflows.
#' @examples 
#' \dontrun{
#' workflow_project <- defineProject('A metabolomics project')
#' 
#' createProject(workflow_project,
#'               workflow = 'FIE-HRMS fingerprinting')
#' }
#' @export

setGeneric('createProject',function(project,workflow)
  standardGeneric('createProject'))

#' @rdname createProject
#' @importFrom projecttemplates projectDirectory projectSkeleton targetsScript targetsRun utils renvInitialise docker createGit githubActions createGithub
#' @importFrom cli symbol
#' @importFrom crayon green

setMethod('createProject',signature = 'Project',
          function(project,workflow){
            project_directory <- projectDirectory(projectName(project),
                                                  path(project))
            
            workflow <- match.arg(workflow,
                                  choices = availableWorkflows())
            
            ## Create project infrastructure
            projectSkeleton(project_directory,
                            force = force(project))
            readme(projectName(project),
                   workflow,
                   path(project))
            
            message('Adding targets infrastructure')
            targetsScript(project_directory,type = 'report')
            targetsRun(project_directory)
            
            utils(project_directory,type = 'report')
            
            output(project_directory)
            
            renvInitialise(project_directory, 
                           rebuild = rebuild(project))
            
            projecttemplates::docker(projectName(project),
                                     path(project))
            
            if (all(github(project), githubActions(project))) {
              projecttemplates::githubActions(projectName(project), 
                                              path(project))
            }
            
            createGit(project_directory,type = 'report')
            
            if (isTRUE(github)) {
              createGithub(projectName(project), 
                           path(project), 
                           private(project))
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