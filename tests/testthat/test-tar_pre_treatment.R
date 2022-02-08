test_that("tar_pre_treatment() works", {
    targets <- tar_pre_treatment(test)
    
    expect_identical(class(targets),'list')
})

test_that("tar_pre_treatment() throws an error if an incorrect parameters object is specified", {
    expect_error(tar_pre_treatment(test,
                                   parameters = 'wrong'))
})

test_that("tar_pre_treatment() works if the `spectral_processed` argument is specified directly", {
    file_paths <- metaboData::filePaths('FIE-HRMS','UrineTechnical',
                                        ask = FALSE)
    sample_info <- metaboData::runinfo('FIE-HRMS','UrineTechnical',
                                       ask = FALSE)
    spectral_processed <- binneR::binneRlyse(file_paths,
                                             sample_info,
                                             binneR::detectParameters(file_paths))
    
    targets <- tar_pre_treatment(example,
                                 spectral_processed = spectral_processed,
                                 parameters = metabolyseR::analysisParameters('pre-treatment'))
    
    expect_identical(class(targets),'list')
})
