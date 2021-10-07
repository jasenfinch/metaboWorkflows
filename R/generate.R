#' Generate a workflow project directory
#' @rdname generateWorkflow
#' @description Generate a workflow project directory from a workflow definition.
#' @param workflow S4 object of class `Workflow`
#' @param start TRUE/FALSE. Automatically activate the project in a new RStudio session after creation
#' @examples 
#' \dontrun{
#' file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
#' sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
#'
#' workflow_input <- inputFilePath(file_paths,sample_information)
#' 
#' workflow_definition <- defineWorkflow(workflow_input,
#'                                       'FIE-HRMS fingerprinting',
#'                                       'Example project')
#'                                       
#' generateWorkflow(workflow_definition)
#' }
#' @export

setGeneric('generateWorkflow',function(workflow,start = TRUE)
  standardGeneric('generateWorkflow'))

#' @rdname generateWorkflow
#' @importFrom projecttemplates projectDirectory projectSkeleton targetsScript targetsRun utils renvInitialise docker createGit githubActions createGithub
#' @importFrom cli symbol
#' @importFrom crayon green
#' @importFrom rstudioapi isAvailable openProject

setMethod('generateWorkflow',signature = 'Workflow',
          function(workflow,start = TRUE){
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
            targetsRun(project_directory,
                       renv = renv(workflow))
            writeTargets(targets(workflow),paste0(project_directory,'/_targets.R'))
            
            utils(glue('{project_directory}/R'),
                  cran = c('purrr','targets','tarchetypes')
                  )
            
            inputPrep(workflow)
            
            if ('report' %in% modules(workflow)) {
              message('Adding R Markdown report')
              output(workflow) 
            }
            
            if (isTRUE(renv(workflow))){
              renvInitialise(project_directory,
                             bioc = biocDependencies(workflow),
                             github = c(otherDependencies(workflow),
                                        githubDependencies(workflow)))
            }
            
            if (isTRUE(docker(workflow))) {
              projecttemplates::docker(projectName(workflow),
                                       path = path(workflow),
                                       renv = renv(workflow)) 
              dockerImage(project_directory)
            }
            
            write(reportFooter(workflow),
                  file = paste0(project_directory,'/README.md'),
                  append = TRUE)
            
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
            
            if (isTRUE(start) & isAvailable()) {
              message('Opening project in a new RStudio session')
              openProject(project_directory,newSession = TRUE)
            }
          })

#' @importFrom purrr map
#' @importFrom glue glue_collapse
#' @importFrom styler style_file

targetsList <- function(workflow_targets){
  
  workflow_targets <- workflow_targets %>%
    map(~{
      wt <- .x
      wt %>%
        names() %>%
        map(~{
          target_code <- code(wt[[.x]])
          
          if (length(target_code) > 1){
            target_code <- glue_collapse(target_code,sep = '
                                         ')
          }
          
          if (length(comment(wt[[.x]])) > 0) {
            glue('
{.x} = 
       {target_code}')
          } else {
            glue('
{.x} = {target_code}')
          }
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

setGeneric('output',function(x)
  standardGeneric('output'))

setMethod('output',signature = 'Workflow',
          function(x){
            project_directory <- projectDirectory(
              projectName(x),
              path(x)
            )
            
            report_directory <- paste0(project_directory,'/report') 
            if (!dir.exists(report_directory)){
              dir.create(report_directory)
            }
            
            rmd(x) %>% 
              writeLines(paste0(report_directory,'/report.Rmd'))
          })