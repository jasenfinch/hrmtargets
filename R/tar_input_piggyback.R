#' Workflow input from a GitHub release
#' @description Targets for workflow input for mzML data files and sample information stored in a GitHub release using the `piggyback` R package. 
#' @inheritParams piggyback::pb_download
#' @param name Symbol. The name for the collection of targets. This serves as a prefix for target names.
#' @param dest The directory destination of the downloaded release data.
#' @param sample_info_file The name of the file included in the release that contains the sample information.
#' @param ext The file extension of the converted (mzML) data files.
#' @return 
#' A list of target objects specifically for analysis pipeline input data retrieval using `piggyback`..
#' @examples
#' \dontrun{
#' ## Example piggyback input
#' targets::tar_dir({
#'     targets::tar_script({
#'         library(hrmtargets)
#'         list(
#'             tar_input_piggyback(example,
#'                                 'FIE-HRMS_BdistachyonTechnical',
#'                                 repo = 'jasenfinch/metaboData')
#'         )
#'     })
#'     targets::tar_make()
#'     targets::tar_read(example_sample_information)
#' })
#' }
#' @export

tar_input_piggyback <- function(name,
                                tag,
                                file = NULL,
                                dest = paste0('data/',tag),
                                sample_info_file = 'runinfo.csv',
                                ext = '.mzML.gz',
                                repo = guess_repo(),
                                overwrite = TRUE,
                                ignore = "manifest.json",
                                use_timestamps = TRUE,
                                show_progress = TRUE,
                                .token = gh::gh_token()){
    

    
    envir <- tar_option_get("envir")
    tidy_eval <- tar_option_get("tidy_eval")
    
    name <- tar_deparse_language(enexpr(name))
    
    release_name <- paste0(name,'_release')
    mzML_name <- paste0(name,'_mzML')
    sample_information_name <- paste0(name,'_sample_information')
    
    command_release <- tar_tidy_eval(
        expr({
            if (!dir.exists(!!dest)) dir.create(!!dest,
                                                recursive = TRUE)
            
            piggyback::pb_download(file = !!file,
                                   dest = !!dest,
                                   tag = !!tag,
                                   repo = !!repo,
                                   overwrite = !!overwrite,
                                   ignore = !!ignore,
                                   use_timestamps = !!use_timestamps,
                                   show_progress = !!show_progress,
                                   .token = !!.token)
            
            list.files(path = !!dest) %>% 
                paste0(!!dest,'/',.)
        }),
        envir = envir,
        tidy_eval = tidy_eval
    )
    
    command_mzML <- tar_tidy_eval(
        expr(subset(!!sym(release_name),grepl(!!ext,!!sym(release_name)))),
        envir = envir,
        tidy_eval = tidy_eval
    )
    
    command_sample_information <- tar_tidy_eval(
        expr(readr::read_csv(subset(!!sym(release_name),
                                    grepl(!!sample_info_file,
                                  !!sym(release_name))))),
        envir = envir,
        tidy_eval = tidy_eval
    )
    
    release_target <- tar_files_raw(
        release_name,
        command_release
    )
    
    mzML_target <- tar_files_raw(
        mzML_name,
        command_mzML
    )
    
    sample_information_target <- tar_target_raw(
        sample_information_name,
        command_sample_information
    )
    
    list(release_target,
         mzML_target,
         sample_information_target)
}

# The following is function `guess_repo()` based upon `piggyback:::guess_repo()` available at https://github.com/ropensci/piggyback/blob/master/R/gh.R as this function is not exported from the package
#' @importFrom gert git_find
#' @importFrom gert git_remote_list

guess_repo <- function(path = '.'){
    repo <- git_find(path)
    remotes <- git_remote_list(repo)
    remotes_names <- remotes$name
    
    if ('upstream' %in% remotes_names){
        remote <- remotes$url[remotes$name == 'upstream']
    } else {
        remote <- remotes$url[1]
    }
    
    remote %>% 
        gsub(".*[:|/]([^/]+/[^/]+)(?:\\.git$)?", "\\1",.) %>% 
        gsub("\\.git$", "",.)
}
