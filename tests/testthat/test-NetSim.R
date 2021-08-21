library(spacejamr)

# Data for tests
path <- system.file("extdata", "points.rds", package ="spacejamr")
points <- readRDS(path)
pl <- PowerLawNetwork(points)
apl <- APLNetwork(points)

test_that("we can create power law networks from a PointSim object", {

    # Ensures the correct class
    expect_identical(class(pl), c("NetSim", "igraph"))

    # Ensures the correct length
    expect_equal(length(pl), 10)

})

test_that("we can create attenuated power law networks from a PointSim object", {

    # Ensures the correct class
    expect_identical(class(apl), c("NetSim", "igraph"))

    # Ensures the correct length
    expect_equal(length(apl), 10)

})
