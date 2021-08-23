
# spacejamr

<!-- badges: start -->
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/dscolby/spacejamr?branch=master&svg=true)](https://ci.appveyor.com/project/dscolby/spacejamr)
[![R-CMD-check](https://github.com/dscolby/spacejamr/workflows/R-CMD-check/badge.svg)](https://github.com/dscolby/spacejamr/actions)
[![Codecov test coverage](https://codecov.io/gh/dscolby/spacejamr/branch/master/graph/badge.svg)](https://codecov.io/gh/dscolby/spacejamr?branch=master)
<!-- badges: end -->

The goal of spacejamr is to enable social network analysis where conventional
collection of social network data would be impossible. It does this by providing
tools to prepare shapefiles, simulate spatial point processes, generate networks 
from those point processes using a spatial interaction function. It also 
contains plot methods that return ggplot objects that can be further refined.

## Installation

You can install the released version of spacejamr from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("spacejamr")
```

## Example

Simulate a Poisson point process and use it to generate a power law network:

``` r
library(spacejamr)

## load mexico dataset
data(RI)

## Simulate a spatial Poisson point process
ri_points <- PointProcess(points = 5000, window = RI, seed = 88)

## Generate a standard power law network
rinet <- PowerLawNetwork(point_process = ri, base_prob = 0.95, scale = 100, 
                         threshold = 0.5, power = -2.3)

## Visualize the new network
plot(rinet)

```

