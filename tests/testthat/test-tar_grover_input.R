test_that("tar_grover_input() works", {
    targets <- tar_grover_input(test,
                                'an_instrument',
                                'an_experiment',
                                grover::grover('a_host',80,'1234'))
    
    expect_identical(class(targets),'list')
})


test_that('tar_grover_input() throws an error if argument grover_client is not of class `GroverClient`',{
    expect_error(tar_grover_input(test,
                                  'an_instrument',
                                  'an_experiment',
                                  'incorrect'))
})