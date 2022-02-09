#' Spectral processing for profiling data
#' @description Targets for spectral processing of profiling data.
#' @param name Symbol. The name for the collection of targets. This serves as a prefix for target names.
#' @param technique The profiling technique to use. Should be one of the values returned by `profilePro::availableTechniques()`.
#' @param mzML A character vector of mzML data file paths. If `NULL`, target input will be expected from an existing target. See details.
#' @param sample_info A tibble containing the sample information. See details for the specifications. If `NULL`, target input will be expected from an existing target. See details.
#' @param cls Sample information column to use for plotting.
#' @param parameters S4 object of class `ProfileParameters`. If `NULL`, default parameters for argument `technique` will be used.
#' @param plots A character vector of plot types. Set to `NULL` to skip all plots.
#' @param summary Boolean. Include additional summary targets.
#' @param export_path Destination path of export files. Set to `NULL` to skip exports.
#' @details 
#' Specifying arguments `mzML` and `sample_info` as `NULL` enables the use of one of the data file and sample information from one of the input target factories, `tar_input_file_path()`, `tar_input_grover()` or `tar_input_piggyback()`. See the example using `tar_input_piggyback()` below.
#' @return 
#' A list of target objects for processing `mzML` data files using `profilePro`.
#' @examples 
#' \dontrun{
#' ## Perform profiling processing by specifying the file paths and sample information directly
#' targets::tar_dir({
#'     targets::tar_script({
#'         library(hrmtargets)
#'         file_paths <- metaboData::filePaths('RP-UHPLC-HRMS','BdistachyonEcotypes',
#'                                             ask = FALSE)
#'         sample_info <- metaboData::runinfo('RP-UHPLC-HRMS','BdistachyonEcotypes',
#'                                            ask = FALSE)
#'         
#'         list(
#'             tar_profiling(example,
#'                           technique = 'LCMS-RP',
#'                           mzML = file_paths,
#'                           sample_info = sample_info)
#'         )
#'     })
#'     targets::tar_make()
#'     targets::tar_read(example_results_spectral_processing)
#'     targets::tar_read(example_plot_chromatogram)
#' })
#' 
#' ## Perform spectral binning by using tar_input_piggyback() 
#' targets::tar_dir({
#'     targets::tar_script({
#'         name <- rlang::expr(example)
#'         library(hrmtargets)
#'         file_paths <- metaboData::filePaths('RP-UHPLC-HRMS','BdistachyonEcotypes',
#'                                             ask = FALSE)
#'         sample_info <- metaboData::runinfo('RP-UHPLC-HRMS','BdistachyonEcotypes',
#'                                            ask = FALSE)
#' 
#'         list(
#'             tar_input_file_path(!!name,
#'                                 file_paths,
#'                                 sample_info),
#'             tar_profiling(!!name,
#'                           'LCMS-RP')
#'         )
#'    })
#'    targets::tar_make()
#'    targets::tar_read(example_results_spectral_processing)
#'    targets::tar_read(example_plot_chromatogram)
#' })
#' }
#' @export

tar_profiling <- function(name,
                          technique,
                          mzML = NULL,
                          sample_info = NULL,
                          cls = 'class',
                          parameters = NULL,
                          plots = c('chromatogram',
                                    'TIC'),
                          summary = TRUE,
                          export_path = 'exports/spectral_processing'){
    
    if (!is.null(parameters)) {
        if (class(parameters) != 'ProfileParameters'){
            stop('If specified, argument `parameters` should be of S4 class `ProfileParameters`.',
                 call. = FALSE)
        }
    }
    
    if (length(plots) > 0){
        plots <- match.arg(plots,
                           c('chromatogram',
                             'TIC'),
                           several.ok = TRUE)   
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
            expr(profilePro::profileParameters(!!technique)),
            envir = envir,
            tidy_eval = tidy_eval
        )    
    } else {
        command_parameters <- call2(function(x) x,parameters)
    }
    
    
    command_results <- tar_tidy_eval(
        expr(profilePro::profileProcess(file_paths = !!mzML_name,
                                        sample_info = !!sample_info_name,
                                        parameters = !!sym(parameters_name))),
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
    
    profiling_list <- list(target_parameters,
                           target_results)
    
    if (length(plots) > 0){
        profiling_list <- c(profiling_list,
                            profiling_plots(name,
                                            plots,
                                            cls))
    }
    
    if (isTRUE(summary)) {
        summary_name <- paste0(name,'_summary_processed_features')
        command_summary <- expr(metaboMisc::featureSummary(!!sym(results_name)))
        summary_targets <- tar_target_raw(
            summary_name,
            command_summary
        )
        
        profiling_list <- c(profiling_list,
                            list(summary_targets)
        )
    }
    
    if (!is.null(export_path)) {
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
        
        profiling_list <- c(profiling_list,
                            list(export_targets)
        )
    }
    
    return(profiling_list)
}


profiling_plots <- function(name,plots,cls){
    
    plot_targets <- lapply(plots,function(x,name,cls){
        plot_name <- paste0(name,'_plot_',x)
        results_name <- sym(paste0(name,'_results_spectral_processing'))
        
        plot_command <- switch(x,
                               chromatogram = expr(profilePro::plotChromatogram(!!results_name,
                                                                                cls = !!cls,
                                                                                group = TRUE)),
                               TIC = expr(profilePro::plotTIC(!!results_name)))
        
        tar_target_raw(plot_name,
                       plot_command)
    },name = name,cls = cls)
    
    return(plot_targets)
}
