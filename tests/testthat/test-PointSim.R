library(spacejamr)

# Data used for testing
data("mexico")

test_that("we can simulate point processes", {

    mex_points <- PointProcess(10, mexico)

    # Ensure correct class
    expect_identical(class(mex_points), c("PointProcess", "PointSim"))
})

test_that("we can generate a Halton Sequence", {

    mex_points <- HaltonSeq(10, mexico)

    # Ensure correct class
    expect_identical(class(mex_points), c("HaltonSeq", "PointSim"))
})
