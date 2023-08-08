#' Consensus structural classifications
#' @description Targets for consensus structural classification
#' @inheritParams construction::construction
#' @param name  Symbol. The name for the collection of targets. This serves as a prefix for target names.
#' @param x An S4 object of class `Assignment`.  If `NULL`, target input will be expected from an existing target. See details.
#' @param plots A character vector of plot types. Set to `NULL` to skip all plots.
#' @param summary Boolean. Include additional summary targets.
#' @param export_path Destination path of export files. Set to `NULL` to skip exports.
#' @details 
#' Specifying argument `x` as `NULL` enables the use of input from the `tar_mf_assignment()` target factory, See the second example below.
#' @return 
#' A list of target objects for consensus structural classifications.
#' @examples 
#' \dontrun{
#' ## Perform consensus structural classification by specifying assignments directly
#' targets::tar_dir({
#'     targets::tar_script({
#'         library(hrmtargets)
#'         
#'         mf_assignments <- assignments::assignMFs(
#'             assignments::feature_data,
#'             parameters = assignments::assignmentParameters('FIE-HRMS')
#'         )
#'         
#'         list(
#'             tar_construction(
#'                 example,
#'                 mf_assignments,
#'                 library_path = paste0(tempdir(),'/construction_library'),
#'                 classyfireR_cache = paste0(tempdir(),'/classyfireR_cache.db')
#'             )
#'         )
#'     })
#'     targets::tar_make()
#'     targets::tar_read(example_plot_sankey)
#' })
#' 
#' ## Perform consensus structural classification by using inputs from other target factories.
#' targets::tar_dir({
#'     targets::tar_script({
#'         library(hrmtargets)
#'         name <- rlang::expr(example)
#'         
#'         list(
#'             tar_input_piggyback(!!name,
#'                                 'FIE-HRMS_BdistachyonTechnical',
#'                                 repo = 'jasenfinch/metaboData'),
#'             tar_spectral_binning(!!name),
#'             tar_pre_treatment(!!name),
#'             tar_mf_assignment(!!name),
#'             tar_construction(!!name,
#'                              library_path = paste0(tempdir(),'/construction_library'),
#'                              classyfireR_cache = paste0(tempdir(),'/classyfireR_cache.db')
#'             )
#'         )
#'     })
#'     targets::tar_make()
#'     targets::tar_read(example_summary_construction)
#' })
#' }
#' @export

tar_construction <- function(name,
                             x = NULL,
                             library_path = 'data/structural_classifications',
                             db = 'kegg',
                             organism = character(),
                             threshold = 66,
                             classyfireR_cache = 'data/structural_classifications/classyfireR_cache.db',
                             plots = 'sankey',
                             summary = TRUE,
                             export_path = 'exports/structural_classifications'){
    
    envir <- tar_option_get("envir")
    tidy_eval <- tar_option_get("tidy_eval")
    
    name <- tar_deparse_language(enexpr(name))
    
    results_name <- paste0(name,'_results_construction')
    
    if (is.null(x)){
        x_name <- sym(paste0(name,'_results_molecular_formula_assignment'))
        results_expr <- expr(construction::construction(x = !!x_name,
                                                        library_path = !!library_path,
                                                        db = !!db,
                                                        organism = !!organism,
                                                        threshold = !!threshold,
                                                        classyfireR_cache = !!classyfireR_cache))
    } else {
        x_name <- x
        results_expr <- expr(construction::construction(x = !!x_name,
                                                         library_path = !!library_path,
                                                         db = !!db,
                                                         organism = !!organism,
                                                         threshold = !!threshold,
                                                         classyfireR_cache = !!classyfireR_cache))
    }
    
    command_results <- tar_tidy_eval(
        results_expr,
        envir = envir,
        tidy_eval = tidy_eval
    )
    
    target_results <- tar_target_raw(
        results_name,
        command_results
    )
    
    construction_list <- list(target_results)
    
    if (plots == 'sankey') {
        plot_name <- paste0(name,'_plot_sankey')
        command_plot <- expr(construction::plotSankey(!!sym(results_name)))
        plot_targets <- tar_target_raw(
            plot_name,
            command_plot
        )
        
        construction_list <- c(construction_list,
                               list(plot_targets)
        )
    }
    
    if (isTRUE(summary)) {
        summary_name <- paste0(name,'_summary_construction')
        command_summary <- expr(construction::summariseClassifications(!!sym(results_name)))
        summary_targets <- tar_target_raw(
            summary_name,
            command_summary
        )
        
        construction_list <- c(construction_list,
                             list(summary_targets)
        )
    }
    
    if (!is.null(export_path)) {
        export_name <- paste0(name,'_export_construction')
        
        command_export <- tar_tidy_eval(
            expr(metaboMisc::export(!!sym(results_name),
                                    outPath = !!export_path)),
            envir = envir,
            tidy_eval = tidy_eval
        )
        
        export_target <- tar_target_raw(
            export_name,
            command_export,
            format = 'file'
        )
        
        
        construction_list <- c(construction_list,
                             list(export_target)
        )
    }
    
    return(construction_list)
}