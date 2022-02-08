#' Spectral processing using spectral binning
#' @description Targets for spectral processing of FIE-HRMS data using spectral binning.
#' @param name Symbol. The name for the collection of targets. This serves as a prefix for target names.
#' @param mzML A characer vector of mzML data file paths. If `NULL`, target input will be expected from an existing target. See details.
#' @param sample_info A tibble containing the sample information. See details for the specifications. If `NULL`, target input will be expected from an existing target. See details.
#' @param parameters S4 object of class `BinParameters`. If `NULL`, `binneR::detectParameters()` will be used to detect the spectral binning parameters automatically..
#' @param plots Boolean. Include additional plotting targets.
#' @param summary Boolean. Include additional summary targets.
#' @param exports Boolean. Include additional export targets.
#' @param export_path Destination path of export files. Ignored if argument `exports = FALSE`.
#' @details 
#' Specifying arguments `mzML` and `sample_info` as `NULL` enables the use of one of the data file and sample information from on of the  input target factories, `tar_input_file_path()`, `tar_input_grover()` or `tar_input_piggyback()`. See the example using `tar_input_piggyback()` below.
#' @return 
#' A list of target objects for processing `mzML` data files using spectral binning..
#' @examples 
#' \dontrun{
#' ## Perform spectral binning by specifiying the file paths and sample information directly
#' targets::tar_dir({
#'     targets::tar_script({
#'         library(hrmtargets)
#'         file_paths <- metaboData::filePaths('FIE-HRMS','UrineTechnical',
#'                                             ask = FALSE)
#'         sample_info <- metaboData::runinfo('FIE-HRMS','UrineTechnical',
#'                                            ask = FALSE)
#'         
#'         list(
#'             tar_spectral_binning(example,
#'                                  mzML = file_paths,
#'                                  sample_info = sample_info)
#'         )
#'     })
#'     targets::tar_make()
#'     targets::tar_read(example_results_spectral_processing)
#'     targets::tar_read(example_plot_fingerprint)
#' })
#' 
#' ## Perform spectral binning by using tar_input_piggyback() 
#' targets::tar_dir({
#'     targets::tar_script({
#'         library(hrmtargets)
#'         name <- rlang::sym('example')
#'         
#'         list(
#'             tar_input_piggyback(example,
#'                                 'FIE-HRMS_BdistachyonTechnical',
#'                                 repo = 'jasenfinch/metaboData'),
#'             tar_spectral_binning(!!name)
#'         )
#'     })
#'     targets::tar_make()
#'     targets::tar_read(example_results_spectral_processing)
#'     targets::tar_read(example_plot_fingerprint)
#' })
#' }
#' @export

tar_spectral_binning <- function(name,
                                 mzML = NULL,
                                 sample_info = NULL,
                                 parameters = NULL,
                                 verbose = TRUE,
                                 plots = TRUE,
                                 summary = TRUE,
                                 exports = TRUE,
                                 export_path = 'exports/spectral_processing'){
    
    if (!is.null(parameters)) {
        if (class(parameters) != 'BinParameters'){
            stop('If specified, argument `parameters` should be of S4 class `BinParameters`.',
                 call. = FALSE)
        }
    }
    
    envir <- tar_option_get("envir")
    tidy_eval <- tar_option_get("tidy_eval")
    
    name <- tar_deparse_language(enexpr(name))
    
    if (is.null(mzML)){
        mzML_name <- sym(paste0(name,'_mzML'))
    } else {
        mzML_name <- mzML
    }
    
    if (is.null(sample_info)){
        sample_info_name <- sym(paste0(name,'_sample_information'))
    } else {
        sample_info_name <- sample_info
    }
    
    parameters_name <- paste0(name,'_parameters_spectral_processing')
    results_name <- paste0(name,'_results_spectral_processing')
    
    
    if (is.null(parameters)){
        command_parameters <- tar_tidy_eval(
            expr(binneR::detectParameters(!!mzML_name)),
            envir = envir,
            tidy_eval = tidy_eval
        )    
    } else {
        command_parameters <- call2(function(x) x,parameters)
    }
    
    
    command_results <- tar_tidy_eval(
        expr(binneR::binneRlyse(files = !!mzML_name,
                                info = !!sample_info_name,
                                parameters = !!sym(parameters_name),
                                verbose = !!verbose)),
        envir = envir,
        tidy_eval = tidy_eval
    )
    
    target_parameters <- tar_target_raw(
        parameters_name,
        command_parameters
    )
    
    target_results <- tar_target_raw(
        results_name,
        command_results
    )
    
    spectral_binning_list <- list(target_parameters,
                                  target_results)
    
    if (isTRUE(plots)){
        spectral_binning_list <- c(spectral_binning_list,
                                   spectral_binning_plots(name))
    }
    
    if (isTRUE(summary)) {
        summary_name <- paste0(name,'_summary_processed_features')
        command_summary <- expr(metaboMisc::featureSummary(!!sym(results_name)))
        summary_targets <- tar_target_raw(
            summary_name,
            command_summary
        )
        
        spectral_binning_list <- c(spectral_binning_list,
                                   list(summary_targets)
        )
    }
    
    if (isTRUE(exports)) {
        export_name <- paste0(name,'_export_processed_data')
        command_export <- tar_tidy_eval(
            expr(metaboMisc::export(!!sym(results_name),
                                    outPath = !!export_path)),
            envir = envir,
            tidy_eval = tidy_eval
        )
        
        export_targets <- tar_files_raw(
            export_name,
            command_export
        )
        
        spectral_binning_list <- c(spectral_binning_list,
                                   list(export_targets)
        )
    }
    
    return(spectral_binning_list)
}


spectral_binning_plots <- function(name){
    plots <- c('chromatogram',
               'fingerprint',
               'TIC',
               'purity_dist',
               'centrality_dist')
    
    plot_targets <- lapply(plots,function(x,name){
        plot_name <- paste0(name,'_plot_',x)
        results_name <- sym(paste0(name,'_results_spectral_processing'))
        
        plot_command <- switch(x,
                               chromatogram = expr(binneR::plotChromatogram(!!results_name)),
                               fingerprint = expr(binneR::plotFingerprint(!!results_name)),
                               TIC = expr(binneR::plotTIC(!!results_name)),
                               purity_dist = expr(binneR::plotPurity(!!results_name)),
                               centrality_dist = expr(binneR::plotCentrality(!!results_name)))
        
        tar_target_raw(plot_name,
                       plot_command)
    },name = name)
    
    return(plot_targets)
}