test_that("file path input works", {
    file_paths <- metaboData::filePaths('FIE-HRMS','UrineTechnical')
    sample_info <- metaboData::runinfo('FIE-HRMS','UrineTechnical')
    
    targets <- tar_file_path_input(test,
                                   file_paths,
                                   sample_info)
    
    expect_identical(class(targets),'list')
})
