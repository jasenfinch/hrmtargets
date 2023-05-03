#' Spectral processing
#' @description Targets for spectral processing.
#' @param name Symbol. The name for the collection of targets. This serves as a prefix for target names.
#' @param type The type of analytical technique.
#' @param ... Arguments to pass to the relevant target factory. See details.
#' @details 
#' Technique `FIE-HRMS` will return targets using `tar_spectral_binning()`. 
#' Techniques `RP-LC-HRMS` and `NP-LC-HRMS` will return targets using `tar_profiling()`.
#' @return 
#' A list of target objects for spectral processing for the specified technique.
#' @examples 
#' \dontrun{
#' ## Perform spectral processing using technique `FIE-HRMS` 
#' targets::tar_dir({
#'     targets::tar_script({
#'         library(hrmtargets)
#'         name <- rlang::expr(example)
#'         
#'         list(
#'             tar_input_piggyback(!!name,
#'                                 'FIE-HRMS_BdistachyonTechnical',
#'                                 repo = 'jasenfinch/metaboData'),
#'             tar_spectral_processing(!!name,
#'                                     type = 'FIE-HRMS')
#'         )
#'     })
#'     targets::tar_make()
#'     targets::tar_read(example_results_spectral_processing)
#'     targets::tar_read(example_plot_fingerprint)
#' })
#' }
#' @export

tar_spectral_processing <- function(name,
                                    type = c('FIE-HRMS',
                                                  'RP-LC-HRMS',
                                                  'NP-LC-HRMS'),
                                    ...){
    
    name <- enexpr(name)
    type <- match.arg(type,
                           choices = c('FIE-HRMS',
                                       'RP-LC-HRMS',
                                       'NP-LC-HRMS'))
    
    spectral_processing_targets <- switch(type,
                                          `FIE-HRMS` = tar_spectral_binning(name = !!name,
                                                                            ...),
                                          `RP-LC-HRMS` = tar_profiling(name = !!name,
                                                                       technique = 'LCMS-RP',
                                                                       ...),
                                          `NP-LC-HRMS` = tar_profiling(name = !!name,
                                                                       technique = 'LCMS-NP',
                                                                       ...))
    return(spectral_processing_targets)
}
