
workflowTargets <- function(workflow){
  workflow <- match.arg(workflow,
                        choices = availableWorkflows())
  
  workflow_targets <- list()
  
  return(workflow_targets)
}

# inputTargets <- function(workflow_input = c('file_paths','grover')){
#   workflow_input <- match.arg(workflow_input,
#                               choices = c('file_paths','grover'))
#   
#   inputs <- list(
#     input_file_path= list(
#       file_paths_list = target('file_paths_list',
#                                'data/file_paths.txt',
#                                args = list(format = 'file')),
#       file_paths = target('file_paths',
#                           readLines(file_path_list)),
#       sample_information_file = target(
#         'sample_information_file',
#         'data/runinfo.csv',
#         args = list(
#           format = 'file')
#       ),
#       tar_target(
#         sample_information,
#         readr::read_csv('runinfo.csv')
#       )
#     ),
#     grover = list(
#       tar_target(
#         instrument,
#         'Ex'
#       ),
#       tar_target(
#         experiment,
#         '170425-Brachy-Example-Batch1'
#       ),
#       tar_file(
#         grover_client_config,
#         'data/grover_client.yml'
#       ),
#       tar_target(
#         grover_client,
#         readGrover(grover_client_config)
#       ),
#       tar_target(
#         raw_files,
#         listRawFiles(grover_client,
#                      instrument,
#                      experiment)
#       ),
#       tar_target(
#         converted_files,
#         grover::convertFile(grover_client,
#                             instrument,
#                             experiment,
#                             raw_files,
#                             args = conversionArgsPeakPick(),
#                             outDir = 'data/mzML'),
#         pattern = map(raw_files),
#       ),
#       tar_target(
#         sanitised_converted_files,
#         converted_files %>%
#           .[!str_detect(.,'Ctrl')] %>%
#           .[!str_detect(.,'Play')]
#       ),
#       tar_target(
#         sample_information,
#         suppressMessages(grover::sampleInfo(grover_client,
#                                             instrument,
#                                             experiment,
#                                             raw_files)),
#         pattern = map(raw_files)
#       ),
#       tar_target(
#         sanitised_sample_information,
#         sample_information %>%
#           convertSampleInfo() %>%
#           rename(class = rawFileOrder,
#                  rawFileOrder = class) %>%
#           filter(class != 'Play',class != 'Ctrl')
#       )
#     )
#   )
#   
#   input_targets <- inputs[['workflow_input']]
#   
#   return(input_targets)
# }

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