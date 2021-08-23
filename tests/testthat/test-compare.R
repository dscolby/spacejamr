library(spacejamr)

test_that("we can compare summary statistics of two networks", {

    # Load data for the test
    data("RI")
    ri_points <- PointProcess(3, RI)
    pl <- PowerLawNetwork(ri_points)
    apl <- APLNetwork(ri_points)

    # Ensure the output is the correct class
    expect_identical(class(compare_networks(pl, apl)), "data.frame")

    # Ensure the returned dataframe has the correct number of dimensions
    expect_equal(dim(compare_networks(pl, apl)), c(2, 5))

})
