test_that("tar_input_grover() works", {
    targets <- tar_input_piggyback(test,
                                   'a_release',
                                   repo = 'a_repo')
    
    expect_identical(class(targets),'list')
})
