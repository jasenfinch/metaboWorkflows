#' @importFrom gh gh
#' @importFrom purrr flatten_chr

dockerImage <- function(project_directory){
  latest_hrm_docker_tag <- try(gh('GET /users/{username}/packages/{package_type}/{package_name}/versions',
                                  username = 'jasenfinch',
                                  package_type = 'container',
                                  package_name = 'hrm-docker'),
                               silent = TRUE) 
  
  if (!inherits(latest_hrm_docker_tag,'try-error')){
    latest_hrm_docker_tag <- latest_hrm_docker_tag %>% 
      map(~{.x$metadata$container$tags}) %>%
      flatten() %>%
      flatten_chr() %>%
      sort(decreasing = TRUE) %>%
      .[2]
  }
  
  if (inherits(latest_hrm_docker_tag,'try-error')){
    warning(paste(
      'Unable to access the package information for the hrm-docker image at <https://github.com/jasenfinch/hrm-docker/pkgs/container/hrm-docker>.',
      'Resorting to the `latest` tag but this will be unreproducible in the long-term',
      'Please consider changing this manually in the generated project.',
      'The returned GitHub API query error was:\n',
      latest_hrm_docker_tag[1]))
    
    latest_hrm_docker_tag <- 'latest'
  }
  
  dockerfile_path <- paste0(project_directory,'/misc/docker/Dockerfile')
  dockerfile <- readLines(dockerfile_path)
  dockerfile[grepl('FROM',dockerfile)] <- paste0('FROM ghcr.io/jasenfinch/hrm-docker:',latest_hrm_docker_tag)
  
  writeLines(dockerfile,dockerfile_path)
}
