test_that("tar_profiling() works", {
    targets <- tar_profiling(test,
                             'LCMS-RP')
    
    expect_identical(class(targets),'list')
})

test_that("tar_profiling() throws an error if an incorrect parameters object is specified", {
    expect_error(tar_profiling(test,
                               parameters = 'wrong'))
})

test_that("tar_profiling() works if `mzML`,`sample_info` and `parameters` arguments are specified directly", {
    file_paths <- metaboData::filePaths('FIE-HRMS','UrineTechnical',
                                        ask = FALSE)
    sample_info <- metaboData::runinfo('FIE-HRMS','UrineTechnical',
                                       ask = FALSE)
    
    targets <- tar_profiling(example,
                             mzML = file_paths,
                             sample_info = sample_info,
                             parameters = profilePro::profileParameters('LCMS-RP'))
    
    expect_identical(class(targets),'list')
})
