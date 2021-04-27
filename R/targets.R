# 
# inputTargets <- function(){
#   list(
#     tar_target(
#       file_paths,
#       metaboData::filePaths(technique,
#                             data_set) 
#     ),
#     tar_target(
#       sample_information,
#       metaboData::runinfo(technique,
#                           data_set)
#     )  
#   )
# }
# 
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