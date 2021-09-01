library(spacejamr)

test_that("we can use different spatial interaction functions", {

    # Standard power las
    expect_equivalent(class(standard(5, 0.9, 1, -2.8)), "numeric")

    # Attenuated power law
    expect_equivalent(class(attenuated(5, 0.9, 1, -2.8)), "numeric")

    # Arctangent probability law
    expect_equivalent(class(arctan(5, 0.9, 1, -2.8)), "numeric")

    # Exponential decay law
    expect_equivalent(class(decay(5, 0.9, 1, -2.8)), "numeric")

    # Logistic probability law
    expect_equivalent(class(logistic(5, 0.9, 1, -2.8)), "numeric")

})
