test_that("tar_construction() works", {
    targets <- tar_construction(test)
    
    expect_identical(class(targets),'list')
})

test_that("tar_construction() works if the `x` argument is specified directly", {
    library(assignments)
    targets <- tar_construction(example,
                                 x = new('Assignment'))
    
    expect_identical(class(targets),'list')
})
