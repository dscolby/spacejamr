library(spacejamr)

# Temporarily turn off warnings about centrality mesures of social networks
old_warning <- getOption("warn")
options(warn = -1)

# Load data for the test
data("RI")
ri_points <- PointSim(3, RI)
pl <- NetSim(ri_points)
apl <- NetSim(ri_points)

test_that("we can compare summary statistics of two networks", {

    # Ensure the output is the correct class
    expect_identical(class(compare_networks(pl, apl)), "data.frame")

    # Ensure the returned dataframe has the correct number of dimensions
    expect_equal(dim(compare_networks(pl, apl)), c(2, 5))

})

# Turn warnings back on
options(warn = old_warning)
