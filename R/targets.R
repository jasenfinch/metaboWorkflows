#' Workflow pipeline targets
#' @rdname targetsWorkflow
#' @description Target definitions for workflow input.
#' @param x the workflow type or an S4 object of class `Workflow`, `FilePathInput` or `GroverInput`
#' @return A list of `Target` S4 class target definitions.
#' @examples 
#' ## Full workflow example
#' file_paths <- metaboData::filePaths('FIE-HRMS','BdistachyonEcotypes')
#' sample_information <- metaboData::runinfo('FIE-HRMS','BdistachyonEcotypes')
#'
#' workflow_input <- inputFilePath(file_paths,sample_information)
#'
#' workflow_definition <- defineWorkflow(workflow_input,
#'                                      'FIE-HRMS fingerprinting',
#'                                      'Example project')
#'
#' targetsWorkflow(workflow_definition)
#' 
#' ## Examples for individual modules
#' targetsSpectralProcessing('FIE-HRMS fingerprinting')
#' targetsInput(workflow_input)
#' targetsPretreatment('FIE-HRMS fingerprinting')
#' targetsMFassignment('FIE-HRMS fingerprinting')
#' targetsModelling('FIE-HRMS fingerprinting')
#' targetsCorrelations('FIE-HRMS fingerprinting')
#' @export

setGeneric('targetsWorkflow',function(x)
  standardGeneric('targetsWorkflow'))

fingerprinting <- function(x){
  workflow_type <- type(x)
  
  list(
    input = targetsInput(x),
    spectral_processing = targetsSpectralProcessing(workflow_type),
    pre_treatment = targetsPretreatment(workflow_type),
    molecular_formula_assignment = targetsMFassignment(workflow_type),
    modelling = targetsModelling(workflow_type),
    correlations = targetsCorrelations(workflow_type),
    report = targetsReport(workflow_type)
  )
}

LCprofiling <- function(x){
  fingerprinting(x)
}

GCprofiling <- function(x){
  workflow_type <- type(x)
  
  list(
    input = targetsInput(x),
    spectral_processing = targetsSpectralProcessing(workflow_type),
    pre_treatment = targetsPretreatment(workflow_type),
    results_modelling = targetsModelling(workflow_type),
    correlations = targetsCorrelations(workflow_type),
    report = targetsReport(workflow_type)
  )
}

#' @rdname targetsWorkflow

setMethod('targetsWorkflow',signature = 'Workflow',
          function(x){
            workflow_type <- type(x)
            
            workflow_targets <- switch(workflow_type,
                                       `FIE-HRMS fingerprinting` = fingerprinting(x),
                                       `NSI-HRMS fingerprinting` = fingerprinting(x),
                                       `RP-LC-HRMS profiling` = LCprofiling(x),
                                       `NP-LC-HRMS profiling` = LCprofiling(x),
                                       `GC-MS profiling` = GCprofiling(x)
            )
            
            return(workflow_targets)
          })

input_commands <- list(
  file_paths = list(
    sample_information = 'readr::read_csv(sample_information_file)'
  ),
  grover = list(
    grover_client = 'grover::readGrover(grover_client_config)',
    raw_files = 'grover::listRawFiles(grover_client,
                             instrument,
                             experiment) %>% 
                  .[!grepl("Ctrl",.)] %>%
                  .[!grepl("Play",.)] %>% 
                  sort()',
    mzML = 'grover::convertFile(grover_client,
                                    instrument,
                                    experiment,
                                    raw_files,
                                    args = grover::conversionArgsPeakPick(),
                                    outDir = "data/mzML")',
    raw_sample_information = 'grover::sampleInfo(grover_client,
                                   instrument,
                                   experiment,
                                   raw_files)',
    sample_information = 'metaboMisc::convertSampleInfo(raw_sample_information)'
  )
)

#' @rdname targetsWorkflow
#' @export

setGeneric('targetsInput',function(x)
  standardGeneric('targetsInput'))

#' @rdname targetsWorkflow

setMethod('targetsInput',signature = 'FilePathInput',
          function(x){
            list(
              file_paths_list = target('file_paths_list',
                                       "data/file_paths.txt",
                                       type = 'tar_file',
                                       comment = 'Retrieve data file paths'),
              mzML = target('mzML',
                            readLines(file_paths_list),
                            type = 'tar_files',
                            comment = 'Track individual data files'),
              sample_information_file = target(
                'sample_information_file',
                "data/runinfo.csv",
                type = 'tar_file',
                comment = 'Sample information file path'
              ),
              sample_information = target(
                'sample_information',
                !!parse_expr(input_commands$file_paths$sample_information),
                comment = 'Parse sample information'
              ))
          })
  
#' @rdname targetsWorkflow
#' @importFrom rlang parse_expr

setMethod('targetsInput',signature = 'GroverInput',
          function(x){
            list(
              instrument = target(
                'instrument',
                !!instrument(x),
                comment = 'Instrument name'
              ),
              experiment = target(
                'experiment',
                !!directory(x),
                comment = 'Experiment raw data directory name'
              ),
              grover_client_config = target(
                'grover_client_config',
                "misc/grover_client.yml",
                type = 'tar_file',
                comment = 'File path to grover API host information'
              ),
              grover_client = target(
                'grover_client',
                !!parse_expr(input_commands$grover$grover_client),
                comment = 'Parse grover API host information'
              ),
              raw_files = target(
                'raw_files',
                !!parse_expr(input_commands$grover$raw_files),
                comment = 'Retrieve available raw data files, excluding control and play samples'
              ),
              mzML = target(
                'mzML',
                !!parse_expr(input_commands$grover$mzML),
                args = list(pattern = 'map(raw_files)',
                            format = 'file',
                            memory = 'transient'),
                comment = 'Retrieve converted raw data files in mzML format via grover API'
              ),
              raw_sample_information = target(
                'raw_sample_information',
                !!parse_expr(input_commands$grover$raw_sample_information),
                args = list(pattern = 'map(raw_files)'),
                comment = 'Retrieve sample information from grover API'
              ),
              sample_information = target(
                'sample_information',
                !!parse_expr(input_commands$grover$sample_information),
                comment = 'Convert sample inforrmation table into workflow compatible format'
              )
            )
          })

#' @rdname targetsWorkflow

setMethod('targetsInput',signature = 'Workflow',
          function(x){
            x %>% 
              input() %>% 
              targetsInput()
          })

processing_commands <- list(
  fingerprinting = list(
    parameters_spectral_processing = 'binneR::detectParameters(mzML)',
    spectral_processing = 'binneR::binneRlyse(mzML,
                                              sample_information,
                                              parameters_spectral_processing)',
    plot_fingerprint = 'binneR::plotFingerprint(results_spectral_processing)',
    plot_chromatogram = 'binneR::plotChromatogram(results_spectral_processing)',
    plot_TIC = 'binneR::plotTIC(results_spectral_processing)',
    plot_purity_dist = 'binneR::plotPurity(results_spectral_processing)',
    plot_centrality_dist = 'binneR::plotCentrality(results_spectral_processing)',
    summary_processed_features = 'metaboMisc::featureSummary(results_spectral_processing)',
    export_processed_data = 'metaboMisc::export(results_spectral_processing,
                                                outPath = "exports/spectral_processing")'
  ),
  profiling = list(
    parameters_spectral_processing = 'profilePro::profileParameters("{parameter_specification}")',
    results_spectral_processing = 'profilePro::profileProcess(mzML,
                                                      sample_information,
                                                      parameters_spectral_processing)'
  )
)

fingerprintProcessing <- function(){
  list(
    parameters_spectral_processing = target(
      'parameters_spectral_processing',
      !!parse_expr(processing_commands$fingerprinting$parameters_spectral_processing),
      comment = 'Detect spectral binning parameters'
    ),
    results_spectral_processing = target(
      'results_spectral_processing',
      !!parse_expr(processing_commands$fingerprinting$spectral_processing),
      args = list(
        memory = 'transient'
      ),
      comment = 'Perform spectral binning'
    ),
    plot_fingerprint = target(
      'plot_fingerprint',
      !!parse_expr(processing_commands$fingerprinting$plot_fingerprint),
      comment = 'Plot average spectrum fingerprint'
    ),
    plot_chromatogram = target(
      'plot_chromatogram',
      !!parse_expr(processing_commands$fingerprinting$plot_chromatogram),
      comment = 'Plot average infusion chromatogram'
    ),
    plot_TIC = target(
      'plot_TIC',
      !!parse_expr(processing_commands$fingerprinting$plot_TIC),
      comment = 'Plot sample total ion counts by randomised block'
    ),
    plot_purity_dist = target(
      'plot_purity_dist',
      !!parse_expr(processing_commands$fingerprinting$plot_purity_dist),
      comment = 'Plot bin purity distribution'
    ),
    plot_centrality_dist = target(
      'plot_centrality_dist',
      !!parse_expr(processing_commands$fingerprinting$plot_centrality_dist),
      comment = 'Plot bin centrality distribution'
    ),
    summary_processed_features = target(
      'summary_processed_features',
      !!parse_expr(processing_commands$fingerprinting$summary_processed_features),
      comment = 'Summary of spectrally binned features'
    ),
    export_processed_data = target(
      'export_processed_data',
      !!parse_expr(processing_commands$fingerprinting$export_processed_data),
      type = 'tar_files',
      args = list(
        memory = 'transient'
      ),
      comment = 'Export spectrally binned data'
    )
  )
}

profilingParameters <- function(x){
  parameter_specification <- switch(x,
                                    `RP-LC-HRMS profiling` = 'LCMS-RP',
                                    `NP-LC-HRMS profiling` = 'LCMS-NP',
                                    `GC-MS profiling` = 'GCMS-eRah')
  
  target_command <-glue(processing_commands$profiling$parameters_spectral_processing) 
  
  target(
    'parameters_spectral_processing',
    !!parse_expr(target_command),
    comment = 'Generate spectral processing parameters'
  )
}

profilingProcessing <- function(x){
  list(
    parameters_spectral_processing = profilingParameters(x),
    results_spectral_processing = target(
      'results_spectral_processing',
      !!parse_expr(processing_commands$profiling$results_spectral_processing),
      comment = 'Perform spectral processing'
    )
  )
}

#' @rdname targetsWorkflow
#' @export

targetsSpectralProcessing <- function(x){
  processing_workflow <- switch(x,
                                `FIE-HRMS fingerprinting` = fingerprintProcessing(),
                                `NSI-HRMS fingerprinting` = fingerprintProcessing(),
                                `RP-LC-HRMS profiling` = profilingProcessing(x),
                                `NP-LC-HRMS profiling` = profilingProcessing(x),
                                `GC-MS profiling` = profilingProcessing(x))
  
  return(processing_workflow)
}

pre_treatment_commands <- list(
  parameters_pre_treatment = 'metaboMisc::detectPretreatmentParameters(results_spectral_processing)',
  results_pre_treatment = 'metaboMisc::preTreatModes(results_spectral_processing,
                                           parameters_pre_treatment)',
  export_results_pre_treatment = 'metaboMisc::exportData(results_pre_treatment,
                                               type = "pre-treated",
                                               outPath = "exports/pre-treated")',
  export_results_pre_treatment_sample_info = 'metaboMisc::exportSampleInfo(results_pre_treatment,
                                                                 outPath = "exports/pre-treated")',
  plot_PCA = 'metabolyseR::plotPCA(results_pre_treatment,
                                   type = "pre-treated")',
  plot_LDA = 'metabolyseR::plotLDA(results_pre_treatment,
                                   type = "pre-treated")',
  plot_unsupervised_RF = 'metabolyseR::plotUnsupervisedRF(results_pre_treatment,
                                                          type = "pre-treated",
                                                          title = "Unsupervised random forest")',
  plot_supervised_RF = 'metabolyseR::plotSupervisedRF(results_pre_treatment,
                                                      type = "pre-treated",
                                                      title = "Supervised random forest")'
)

#' @rdname targetsWorkflow
#' @export

targetsPretreatment <- function(x){
  workflow <- checkWorkflow(x)
  
  list(
    parameters_pre_treatment = target(
      'parameters_pre_treatment',
      !!parse_expr(pre_treatment_commands$parameters_pre_treatment),
      comment = 'Detect pre-treatment routine parameters'
    ),
    results_pre_treatment = target(
      'results_pre_treatment',
      !!parse_expr(pre_treatment_commands$results_pre_treatment),
      comment = 'Perform data pre-treatment'
    ),
    export_results_pre_treatment_data = target(
      'export_results_pre_treatment',
      !!parse_expr(pre_treatment_commands$export_results_pre_treatment),
      type = 'tar_file',
      comment = 'Export pre-treated data'
    ),
    export_results_pre_treatment_sample_info = target(
      'export_results_pre_treatment_sample_info',
      !!parse_expr(pre_treatment_commands$export_results_pre_treatment_sample_info),
      type = 'tar_file',
      comment = 'Export sample information of pre-treated data'
    ),
    plot_PCA = target(
      'plot_PCA',
      !!parse_expr(pre_treatment_commands$plot_PCA),
      comment = 'Plot Principle Component Analysis'
    ),
    plot_LDA = target(
      'plot_LDA',
      !!parse_expr(pre_treatment_commands$plot_LDA),
      comment = 'Plot Priniciple Component Analysis - Linear Discriminant Analysis'
    ),
    plot_unsupervised_RF = target(
      'plot_unsupervised_RF',
      !!parse_expr(pre_treatment_commands$plot_unsupervised_RF),
      comment = 'Plot multidimensional scaling plot of unsupervised random forest'
    ),
    plot_supervised_RF = target(
      'plot_supervised_RF',
      !!parse_expr(pre_treatment_commands$plot_supervised_RF),
      comment = 'Plot multidimensional scaling plot of supervised random forest'
    )
  )
}

assignment_commands <- list(
  parameters_molecular_formula_assignment = 'MFassign::assignmentParameters("{technique}")',
  results_molecular_formula_assignment = 'results_pre_treatment %>% 
                                  metabolyseR::dat(type = "pre-treated") %>% 
                                  MFassign::assignMFs(parameters_molecular_formula_assignment)',
  assigned_data = 'metaboMisc::addAssignments(results_pre_treatment,
                                              results_molecular_formula_assignment)',
  summary_assignments = 'MFassign::summariseAssignment(results_molecular_formula_assignment)',
  export_assignments = 'metaboMisc::export(results_molecular_formula_assignment,
                                           outPath = "exports/molecular_formula_assignments")'
)

assignmentParameters <- function(x){
  technique <- switch(x,
                      `FIE-HRMS fingerprinting` = 'FIE',
                      `NSI-HRMS fingerprinting` = 'FIE',
                      `RP-LC-HRMS profiling` = 'RP-LC',
                      `NP-LC-HRMS profiling` = 'NP-LC')
  
  target_command <- glue(assignment_commands$parameters_molecular_formula_assignment)
  target(
    'parameters_molecular_formula_assignment',
    !!parse_expr(target_command),
    comment = 'Generate molecular formula assignment parameters'
  )
}

#' @rdname targetsWorkflow
#' @export

targetsMFassignment <- function(x){
  workflow <- checkWorkflow(x)
  
  list(
    parameters_molecular_formula_assignment = assignmentParameters(workflow),
    results_molecular_formula_assignment = target(
      'results_molecular_formula_assignment',
      !!parse_expr(assignment_commands$results_molecular_formula_assignment), 
      args = list(memory = 'transient'),
      comment = 'Perform molecular formula assignment'
    ),
    assigned_data = target(
      'assigned_data',
      !!parse_expr(assignment_commands$assigned_data),
      comment = 'Retieve pre-treated data with molecular formula assignments added to the feature names'
    ),
    summary_assignments = target(
      'summary_assignments',
      !!parse_expr(assignment_commands$summary_assignments),
      comment = 'Summarise the assigned molecular formulas'
    ),
    export_assignments = target(
      'export_assignments',
      !!parse_expr(assignment_commands$export_assignments),
      type = 'tar_files',
      comment = 'Export molecular formula assignments'
    )
  )
}

modelling_commands <- list(
  parameters_modelling = 'metaboMisc::detectModellingParameters({object_name},
                                                                cls = "class")',
  results_modelling = 'metabolyseR::reAnalyse({object_name},
                                      parameters_modelling)',
  plot_explanatory_heatmap = 'metabolyseR::plotExplanatoryHeatmap(results_modelling)',
  summary_modelling_metrics = 'metabolyseR::metrics(results_modelling)',
  summary_explanatory_features = 'metabolyseR::explanatoryFeatures(results_modelling)',
  export_modelling = 'metaboMisc::exportModelling(results_modelling,
                                                  outPath = "exports/results_modelling")'
)

#' @importFrom rlang expr

modellingTargets <- function(x){
  
  object_name <- switch(x,
                        assigned = expr(assigned_data),
                        unassigned = expr(results_pre_treatment))
  
  list(
    parameters_modelling = target(
      'parameters_modelling',
      !!parse_expr(glue(modelling_commands$parameters_modelling)),
      comment = 'Detect appropriate modelling parameters'
    ),
    results_modelling = target(
      'results_modelling',
      !!parse_expr(glue(modelling_commands$results_modelling)),
      comment = 'Perform modelling'
    ),
    summary_modelling_metrics = target(
      'summary_model_metrics',
      !!parse_expr(modelling_commands$summary_modelling_metrics),
      comment = 'Retrieve modelling metrics'
    ),
    summary_explanatory_features = target(
      'summary_explanatory_features',
      !!parse_expr(modelling_commands$summary_explanatory_features),
      comment = 'Retireve modelling explanatory features'
    ),
    plot_explanatory_heatmap = target(
      'plot_explanatory_heatmap',
      !!parse_expr(modelling_commands$plot_explanatory_heatmap),
      comment = 'Plot a heat map of explanatory features'
    ),
    export_modelling = target(
      'export_modelling',
      !!parse_expr(modelling_commands$export_modelling),
      type = 'tar_files',
      comment = 'Export results_modelling results'
    )
  )
}

#' @rdname targetsWorkflow
#' @export

targetsModelling <- function(x){
  workflow <- checkWorkflow(x)
  
  switch(workflow,
         `FIE-HRMS fingerprinting` = modellingTargets('assigned'),
         `NSI-HRMS fingerprinting` = modellingTargets('assigned'),
         `RP-LC-HRMS profiling` = modellingTargets('assigned'),
         `NP-LC-HRMS profiling` = modellingTargets('assigned'),
         `GC-MS profiling` = modellingTargets('unassigned')
  )
}

correlations_commands <- list(
  parameters_correlations = 'metabolyseR::analysisParameters("correlations")',
  correlations = 'metabolyseR::reAnalyse({object_name},
                                         parameters_correlations)',
  summary_correlations = 'metabolyseR::analysisResults(results_correlations,"correlations")',
  export_correlations = 'metaboMisc::exportCorrelations(results_correlations,
                                                        outPath = "exports/correlations")'
)

correlationsTargets <- function(x){
  
  object_name <- switch(x,
                        assigned = expr(assigned_data),
                        unassigned = expr(results_pre_treatment))
  
  list(
    parameters_correlations = target(
      'parameters_correlations',
      !!parse_expr(correlations_commands$parameters_correlations),
      comment = 'Generate parameters for correlation analysis'
    ),
    results_correlations = target(
      'results_correlations',
      !!parse_expr(glue(correlations_commands$correlations)),
      comment = 'Perform correlation analysis'
    ),
    summary_correlations = target(
      'summary_correlations',
      !!parse_expr(correlations_commands$summary_correlations),
      comment = 'Retrieve correlation analysis results'
    ),
    export_correlations = target(
      'export_correlations',
      !!parse_expr(correlations_commands$export_correlations),
      type = 'tar_files',
      comment = 'Export correlation analysis results'
    )
  )
}

#' @rdname targetsWorkflow
#' @export

targetsCorrelations <- function(x){
  workflow <- checkWorkflow(x) 
  
  switch(workflow,
         `FIE-HRMS fingerprinting` = correlationsTargets('assigned'),
         `NSI-HRMS fingerprinting` = correlationsTargets('assigned'),
         `RP-LC-HRMS profiling` = correlationsTargets('assigned'),
         `NP-LC-HRMS profiling` = correlationsTargets('assigned'),
         `GC-MS profiling` = correlationsTargets('unassigned')
  )
}

#' @rdname targetsWorkflow
#' @export

targetsReport <- function(x){
  list(
    report = target(
      'report',
      "report/report.Rmd",
      type = 'tar_render',
      args = list(output_dir = "exports")
    )
  )
}