test_that("tar_spectral_processing() works", {
    targets <- tar_spectral_processing(test)
    
    expect_identical(class(targets),'list')
})
