test_that("tar_file_path_input() works", {
    file_paths <- metaboData::filePaths('FIE-HRMS','UrineTechnical',
                                        ask = FALSE)
    sample_info <- metaboData::runinfo('FIE-HRMS','UrineTechnical',
                                       ask = FALSE)
    
    targets <- tar_file_path_input(test,
                                   file_paths,
                                   sample_info)
    
    expect_identical(class(targets),'list')
})

test_that('tar_file_path_input() throws an error if non-mzML data files specified',{
    file_paths <- metaboData::filePaths('FIE-HRMS','UrineTechnical',
                                        ask = FALSE) %>% 
        gsub('.mzML','.csv',x = .)
    
    sample_info <- metaboData::runinfo('FIE-HRMS','UrineTechnical',
                                       ask = FALSE)
    
    expect_error(tar_file_path_input(test,
                                     file_paths,
                                     sample_info))
})

test_that('tar_file_path_input() throws an error if the incorrect column names are found in the sample information',{
    file_paths <- metaboData::filePaths('FIE-HRMS','UrineTechnical',
                                        ask = FALSE)
    
    sample_info <- metaboData::runinfo('FIE-HRMS','UrineTechnical',
                                       ask = FALSE)
    names(sample_info)[1] <- 'incorrect'
    
    expect_error(tar_file_path_input(test,
                                     file_paths,
                                     sample_info))
})

test_that('tar_file_path_input() throws an error if the file names in mzML_files do not match those in the `fileName` column of the sample information',{
    file_paths <- metaboData::filePaths('FIE-HRMS','UrineTechnical',
                                        ask = FALSE)
    sample_info <- metaboData::runinfo('FIE-HRMS','UrineTechnical',
                                       ask = FALSE)
    sample_info$fileName[1] <- 'incorrect.mzML'
    
    expect_error(tar_file_path_input(test,
                                     file_paths,
                                     sample_info))
})