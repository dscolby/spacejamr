library(spacejamr)

test_that("we can compare summary statistics of two networks", {

    # Load data for the test
    data("mexico")
    mex_points <- PointProcess(10, mexico)
    pl <- PowerLawNetwork(mex_points)
    apl <- APLNetwork(mex_points)

    # Ensure the output is the correct class
    expect_identical(class(compare_networks(pl, apl)), "data.frame")

    # Ensure the returned dataframe has the correct number of dimensions
    expect_equal(dim(compare_networks(pl, apl)), c(2, 5))

})
