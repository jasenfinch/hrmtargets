test_that("tar_mf_assignment() works", {
    targets <- tar_mf_assignment(test)
    
    expect_identical(class(targets),'list')
})

test_that("tar_mf_assignment() throws an error if an incorrect parameters object is specified", {
    expect_error(tar_mf_assignment(test,
                                   parameters = 'wrong'))
})

test_that("tar_mf_assignment() works if the `feature_data` argument is specified directly", {
    targets <- tar_mf_assignment(example,
                                 feature_data = assignments::feature_data)
    
    expect_identical(class(targets),'list')
})
