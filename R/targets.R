#' Workflow input targets
#' @rdname workflowTargets
#' @description Target definitions for workflow input.
#' @param x S4 object of class `FilePathInput` or `GroverInput`
#' @return A list of `Target` S4 class target definitions.
#' @examples 
#' file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
#' sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
#' 
#' workflow_input <- filePathInput(file_paths,sample_information)
#'
#' inputTargets(workflow_input)
#' @export

setGeneric('workflowTargets',function(x)
  standardGeneric('workflowTargets'))

#' @rdname workflowTargets
#' 
setMethod('workflowTargets',signature = 'Workflow',
          function(x){
            list(
              input = inputTargets(x)
            )
          })

#' @rdname workflowTargets
#' @export

setGeneric('inputTargets',function(x)
  standardGeneric('inputTargets'))

#' @rdname workflowTargets

setMethod('inputTargets',signature = 'FilePathInput',
          function(x){
            list(
              file_paths_list = target('file_paths_list',
                                       c('data/file_paths.txt'),
                                       type = 'tar_file'),
              file_paths = target('file_paths',
                                  readLines(file_path_list)),
              sample_information_file = target(
                'sample_information_file',
                c('data/runinfo.csv'),
                type = 'tar_file'
              ),
              sample_information = target(
                'sample_information',
                readr::read_csv('runinfo.csv')
              ))
          })

#' @rdname workflowTargets

setMethod('inputTargets',signature = 'GroverInput',
          function(x){
            list(
              instrument = target(
                c('instrument'),
                instrument(x)
              ),
              experiment = target(
                c('experiment'),
                directory(x)
              ),
              grover_client_config = target(
                'grover_client_config',
                c('data/grover_client.yml'),
                type = 'tar_file'
              ),
              grover_client = target(
                'grover_client',
                readGrover(grover_client_config)
              ),
              raw_files = target(
                'raw_files',
                listRawFiles(grover_client,
                             instrument,
                             experiment)
              ),
              converted_files = target(
                'converted_files',
                grover::convertFile(grover_client,
                                    instrument,
                                    experiment,
                                    raw_files,
                                    args = conversionArgsPeakPick(),
                                    outDir = 'data/mzML') %>% 
                  .[!str_detect(.,'Ctrl')] %>%
                  .[!str_detect(.,'Play')],
                pattern = map(raw_files)
              ),
              raw_sample_information = target(
                'raw_sample_information',
                suppressMessages(grover::sampleInfo(grover_client,
                                                    instrument,
                                                    experiment,
                                                    raw_files)),
                pattern = map(raw_files)
              ),
              sample_information = target(
                'sample_information',
                raw_sample_information %>%
                  metaboMiscconvertSampleInfo() %>%
                  filter(class != 'Play',class != 'Ctrl')
              )
            )
          })

#' @rdname workflowTargets

setMethod('inputTargets',signature = 'Workflow',
          function(x){
            x %>% 
              input() %>% 
              inputTargets()
          })

#' @rdname workflowTargets
#' @export

setGeneric('spectralProcessingTargets',function(x)
  standardGeneric('spectralProcessingTargets'))

# spectralProcessing
# 
# list(
#   ,
#   tar_target(
#     spectral_binning_parameters,
#     binneR::detectParameters(files)
#   ),
#   tar_target(
#     spectral_binning,
#     binneR::binneRlyse(files,
#                        sample_information,
#                        spectral_binning_parameters)
#   ),
#   tar_target(
#     pre_treatment_parameters,
#     detectPretreatmentParameters(spectral_binning)
#   ),
#   tar_target(
#     pre_treated,
#     metaboMisc::preTreatModes(spectral_binning,
#                               pre_treatment_parameters)
#   ),
#   tar_target(
#     molecular_formula_assignment_parameters,
#     MFassign::assignmentParameters('FIE')
#   ),
#   tar_target(
#     molecular_formula_assignment,
#     MFassign::assignMFs(metabolyseR::dat(pre_treated,
#                                          type = 'pre-treated') %>% 
#                           .[,1:20],
#                         molecular_formula_assignment_parameters)
#   ),
#   tar_target(
#     assigned_data,
#     metabolyseR::dat(pre_treated,'pre-treated') <- MFassign::assignedData(molecular_formula_assignment)
#   ),
#   tar_target(
#     modelling_parameters,
#     detectModellingParameters(assigned_data,cls = 'class')
#   ),
#   tar_target(
#     modelling,
#     reAnalyse(assigned_data,
#               modelling_parameters)
#   ),
#   tar_target(
#     correlations_parameters,
#     analysisParameters('correlations')
#   ),
#   tar_target(
#     correlations,
#     reAnalyse(assigned_data,
#               correlations_parameters)
#   )
# )