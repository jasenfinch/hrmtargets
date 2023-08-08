test_that("tar_export works", {
    targets <- tar_export(test,jfmisc::exportCSV(iris,'iris.csv'))
    
    expect_identical(class(targets),'list')
})
