#' Workflow input using file paths
#' @description Targets for workflow inputs using file paths
#' @param name Symbol. The name for the collection of targets. This serves as a prefix for target names.
#' @param mzML_files A characer vector of mzML file paths.
#' @param sample_information A tibble containing the sample information. See details for the specifications.
#' @details 
#' The tibble containing sample information should at least contain the following columns:
#' * `fileOrder` - the numeric file order of the input files when order alphabetically as returned by list.files().
#' * `injOrder` - the sample injection order during MS analysis
#' * `fileName` - the sample file name
#' * `batch` - the analytical batch number
#' * `block` - the randomised block number
#' * `name` - the sample name
#' * `class` - the sample class name
#' 
#' The file names contained within the argument `mzML_files` should match in name and order to those within the `fileName` sample information column.
#' @return 
#' A list of target objects specifically for analysis pipeline input using file paths.
#' @examples
#' ## File path input 
#' targets::tar_dir({
#' targets::tar_script({
#'     library(hrmtargets)
#'     file_paths <- metaboData::filePaths('FIE-HRMS','UrineTechnical',ask = FALSE)
#'     sample_info <- metaboData::runinfo('FIE-HRMS','UrineTechnical',ask = FALSE)
#'     
#'     list(
#'         tar_file_path_input(test,
#'                             file_paths,
#'                             sample_info)
#'     )
#' })
#' targets::tar_make()
#' })
#' @importFrom targets tar_deparse_language tar_target_raw tar_option_get
#' @importFrom tarchetypes tar_files_raw
#' @export

tar_file_path_input <- function(name,
                                mzML_files,
                                sample_information){
    
    mzml_pattern <- grepl('.mzML',mzML_files)
    
    if (FALSE %in% mzml_pattern){
        stop('File paths should only contain .mzML files',
             call. = FALSE)
    }
    
    necessary_names <- c('fileOrder','injOrder','fileName','batch','block','name','class')
    
    info_names <- colnames(sample_information)
    
    presence <- necessary_names %in% info_names
    
    if (FALSE %in% presence) {
        stop(paste0('Sample information should contain the following column names: ',
               paste0(necessary_names,collapse = ', '),
               '.'),
             call. = FALSE)
    } 
    
    if (!all(basename(mzML_files) == sample_information$fileName)){
        stop('mzML file names do not match those specified in the `fileName` column of the `sample_information`.',
             call. = FALSE)
    }
    
    envir <- tar_option_get("envir")
    tidy_eval <- tar_option_get("tidy_eval")
    
    name <- tar_deparse_language(substitute(name))
    
    mzML_name <- paste0(name,'_mzML')
    sample_information_name <- paste0(name,'_sample_information')
    
    command_sample_information <- targets::tar_tidy_eval(
        substitute(sample_information),
        envir = envir,
        tidy_eval = tidy_eval
    )
    
    mzML_target <- tar_files_raw(
        mzML_name,
        as.call(list(`c`,mzML_files))
    )
    
    sample_information_target <- tar_target_raw(
        sample_information_name,
        command_sample_information
    )
    
    list(mzML_target,
         sample_information_target)
}

#' Workflow input using a grover API
#' @description Targets for workflow inputs using a grover API
#' @param name Symbol. The name for the collection of targets. This serves as a prefix for target names.
#' @param grover_client
#' @param sample_information A tibble containing the sample information. See details for the specifications.
#' @details 
#' The tibble containing sample information should at least contain the following columns:
#' * `fileOrder` - the numeric file order of the input files when order alphabetically as returned by list.files().
#' * `injOrder` - the sample injection order during MS analysis
#' * `fileName` - the sample file name
#' * `batch` - the analytical batch number
#' * `block` - the randomised block number
#' * `name` - the sample name
#' * `class` - the sample class name
#' 
#' The file names contained within the argument `mzML_files` should match in name and order to those within the `fileName` sample information column.
#' @return 
#' A list of target objects specifically for analysis pipeline input using file paths.
#' @examples
#' ## File path input 
#' targets::tar_dir({
#' targets::tar_script({
#'     library(hrmtargets)
#'     file_paths <- metaboData::filePaths('FIE-HRMS','UrineTechnical',ask = FALSE)
#'     sample_info <- metaboData::runinfo('FIE-HRMS','UrineTechnical',ask = FALSE)
#'     
#'     list(
#'         tar_file_path_input(test,
#'                             file_paths,
#'                             sample_info)
#'     )
#' })
#' targets::tar_make()
#' })
#' @importFrom targets tar_deparse_language tar_target_raw tar_option_get
#' @importFrom tarchetypes tar_files_raw
#' @export

tar_file_path_input <- function(name,
                                mzML_files,
                                sample_information){
    
    mzml_pattern <- grepl('.mzML',mzML_files)
    
    if (FALSE %in% mzml_pattern){
        stop('File paths should only contain .mzML files',
             call. = FALSE)
    }
    
    necessary_names <- c('fileOrder','injOrder','fileName','batch','block','name','class')
    
    info_names <- colnames(sample_information)
    
    presence <- necessary_names %in% info_names
    
    if (FALSE %in% presence) {
        stop(paste0('Sample information should contain the following column names: ',
                    paste0(necessary_names,collapse = ', '),
                    '.'),
             call. = FALSE)
    } 
    
    if (!all(basename(mzML_files) == sample_information$fileName)){
        stop('mzML file names do not match those specified in the `fileName` column of the `sample_information`.',
             call. = FALSE)
    }
    
    envir <- tar_option_get("envir")
    tidy_eval <- tar_option_get("tidy_eval")
    
    name <- tar_deparse_language(substitute(name))
    
    mzML_name <- paste0(name,'_mzML')
    sample_information_name <- paste0(name,'_sample_information')
    
    command_sample_information <- targets::tar_tidy_eval(
        substitute(sample_information),
        envir = envir,
        tidy_eval = tidy_eval
    )
    
    mzML_target <- tar_files_raw(
        mzML_name,
        as.call(list(`c`,mzML_files))
    )
    
    sample_information_target <- tar_target_raw(
        sample_information_name,
        command_sample_information
    )
    
    list(mzML_target,
         sample_information_target)
}