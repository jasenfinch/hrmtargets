test_that("tar_spectral_binning() works", {
    targets <- tar_spectral_binning(test)
    
    expect_identical(class(targets),'list')
})

test_that("tar_spectral_binning() throws an error if an incorrect parameters object is specified", {
    expect_error(tar_spectral_binning(test,
                                      parameters = 'wrong'))
})

test_that("tar_spectral_binning() works if `mzML`,`sample_info` and `parameters` arguments are specified directly", {
    file_paths <- metaboData::filePaths('FIE-HRMS','UrineTechnical',
                                        ask = FALSE)
    sample_info <- metaboData::runinfo('FIE-HRMS','UrineTechnical',
                                       ask = FALSE)
    
    targets <- tar_spectral_binning(example,
                                    mzML = file_paths,
                                    sample_info = sample_info,
                                    parameters = binneR::binParameters())
    
    expect_identical(class(targets),'list')
})
