#' @importFrom gh gh
#' @importFrom purrr flatten_chr

dockerImage <- function(project_directory){
  latest_hrm_docker_tag <- gh('GET /users/{username}/packages/{package_type}/{package_name}/versions',
                            username = 'jasenfinch',
                            package_type = 'container',
                            package_name = 'hrm-docker') %>% 
    map(~{.x$metadata$container$tags}) %>% 
    flatten() %>% 
    flatten_chr() %>% 
    .[2]
  
  dockerfile_path <- paste0(project_directory,'/misc/docker/Dockerfile')
  dockerfile <- readLines(dockerfile_path)
  dockerfile[grepl('FROM',dockerfile)] <- paste0('FROM ghcr.io/jasenfinch/hrm-docker:',latest_hrm_docker_tag)
  
  writeLines(dockerfile,dockerfile_path)
}
