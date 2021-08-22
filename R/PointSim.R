# Author: Darren Colby
# Date: 8/22/2021
# Purpose: To simulate spatial point processes

# Constructor methods for the PointProcess class --------------------------

# Validate the input to an instance of a PointProcess class
#
# @description validates the input from the PointProcess constructors
#
# @details This function should not be called directly
#
# @param points the number of points to simulate
# @param a window of class spacejamr to use as the spatial extent
# @param seed an optional seed
#
# @return A validated PointProcess object
#
# @author Darren Colby
#    Email: dscolby17@gmail.com
validate_PointProcess <- function(points, window, seed) {

   stopifnot(methods::is(window, "spacejamr")) # Ensures correct input

   if (!is.null(seed)) {set.seed(seed)}  # Optional seed

   # Homogeneous point process
   point_process <- spatstat.core::rpoint(n = points, win = window$window)

   return(point_process)

}


# Validate and set the class attribute of inputs to the PointProcess constructor
#
# @description Creates a new PointProcess object
#
# @details This function should not be called directly
#
# @param points the number of points to simulate
# @param window a window object of class spacejamr to use as the spatial extent
# @param seed an optional seed
#
# @return An object of classes PointProcess and PointSim
#
# @author Darren Colby
#    Email: dscolby17@gmail.com
new_PointProcess <- function(points, window, seed) {

   # Validates the input
   point_process <- validate_PointProcess(points, window, seed)

   # Sets the class
   validated_process <- structure(point_process,
                                  class = c("PointProcess", "PointSim"))

   return(validated_process)

}


#' Simulate a spatial Poisson point process
#'
#' @description Creates a new Poisson Point Process in a spacejamr object
#'
#' @usage PointProcess(points, window, seed)
#'
#' @param points the number of points to simulate
#' @param window a spacejamr object to use as the spatial extent
#' @param seed an optional seed
#'
#' @return An object of classes PointProcess and PointSim
#'
#' @example \dontrun{
#' # Create spacejamr object
#' mex <- as.spacejamr("Z:shapefiles/mexico_adm0.shp")
#' mex_points <- PointProcess(1000, mex, 42)
#' }
#'
#' @author Darren Colby \cr
#' Email: dscolby17@@gmail.com
#' @export
PointProcess <- function(points, window, seed = NULL) {

   # Create a new object
   point_process <- new_PointProcess(points, window, seed)

   return(point_process)
}


# Constructor for the HaltonSeq class -------------------------------------


# Validate the input to a HaltonSeq instance
#
# @description validates the input from the HaltonSeq constructors
#
# @details This function should not be called directly
#
# @param points the number of points to simulate
# @param a window of class spacejamr to use as the spatial extent
# @param seed an optional seed
#
# @return A validated HaltonSeq object
#
# @author Darren Colby
#    Email: dscolby17@gmail.com
validate_HaltonSeq <- function(points, window, seed) {

   stopifnot(methods::is(window, "spacejamr")) # Ensures correct input

   if (!is.null(seed)) {set.seed(seed)}  # Optional seed

   # Simulates the Halton sequence
   halton_seq <- spatstat.geom::rQuasi(points, window$window, "Halton")

   return(halton_seq)

}


# Validate inputs and set the class attribute for a HaltonSeq instance
#
# @description Creates a new Halton Sequence in a spacejamr objec
#
# @details This function should not be called directly
#
# @param points the number of points to simulate
# @param window a window object of class spacejamr to use as the spatial extent
# @param seed an optional seed
#
# @return An object of classes HaltonSeq and PointSim
#
# @author Darren Colby
#    Email: dscolby17@gmail.com
new_HaltonSeq <- function(points, window, seed) {

   halton <- validate_HaltonSeq(points, window, seed)

   validated_halton <- structure(halton, class = c("HaltonSeq", "PointSim"))

}


#' Simulate a 2D Halton sequence in a spatial window
#'
#' @description Creates a new Halton Sequence in a spacejamr object
#'
#' @usage HaltonSeq(points, window, seed)
#'
#' @param points the maximum number of points to simulate
#' @param window a window object of class spacejamr to use as the spatial extent
#' @param seed an optional seed
#'
#' @details Note that the number of generated points may be less that the points
#' argument. If the spacejamr window is a rectangle, this function will generate
#' the specificed number of point. Otherwise, the specified number of points
#' will be generated within a containing rectangle and only those points within
#' the spacejamr window will be kept.
#'
#' @return An object of classes HaltonSeq and PointSim
#'
#' @example \dontrun{
#' # Create spacejamr object
#' mex <- as.spacejamr("Z:shapefiles/mexico_adm0.shp")
#' mex_seq <- HaltonSeq(1000, mex, 42)
#' }
#'
#' @author Darren Colby \cr
#' Email: dscolby17@@gmail.com
#' @export
HaltonSeq <- function(points, window, seed = NULL) {

   halton_seq <- new_HaltonSeq(points, window, seed)

   return(halton_seq)

}


# Plot, print, and summary methods for PointSim classes -------------------


#' Plot simulated points from a PointSim object
#'
#' @description Plots the results of points simulated in a PointProcess or
#' HaltonSeq class, whcih obht inherit methods from the PointSim class.
#'
#' @details The returned plot can be refined with standard ggplot2 functions
#'
#' @param x an object of class PointSim or one of its child classes
#' @param y ignored.
#' @param ... ignored
#' @param title an optional title. Default is "Simulated Points".
#' @param color an optional color for the simulated points. Default is red.
#'
#' @return A ggplot2 object
#'
#' @examples \dontrun{
#' # Create spacejamr object
#' mex <- as.spacejamr("Z:shapefiles/mexico_adm0.shp")
#' }
#'
#' \dontrun{
#' # With PointProcess
#' mex_points <- PointProcess(1000, mex, 42)
#' plot(mex_points)}
#'
#' \dontrun{
#' # With HaltonSeq
#' mex_seq <- HaltonSeq(1000, mex, 42)
#' plot(mex_seq)
#' }
#'
#' @author Darren Colby \cr
#' Email: dscolby17@@gmail.com
#' @export
plot.PointSim <- function(x, y, ..., title = "Simulated Points", color = "red") {

   window <- sf::st_as_sf(x$window)  # sf object to pass to ggplot
   points <- as.data.frame(cbind(x$x, x$y))  # dataframe of points

   plot <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = window) +
      ggplot2::geom_point(data = points,
                          ggplot2::aes_string(x = "V1",
                                              y = "V2"),
                          color = color) +
      ggplot2::labs(title = title) +
      ggthemes::theme_map()

   return(plot)

}


#' Print information from a PointSim class
#'
#' @description Print method for both the PointProcess and HaltonSeq classes,
#' which inherit methods from the PointSim class.
#'
#' @param x a PointSim object or a child object
#' @param ... ignored.
#'
#' @examples \dontrun{
#' # Create spacejamr object
#' mex <- as.spacejamr("Z:shapefiles/mexico_adm0.shp")
#' }
#'
#' \dontrun{
#' # With PointProcess
#' mex_points <- PointProcess(1000, mex, 42)
#' print(mex_points)
#' }
#'
#' \dontrun{
#' # With HaltonSeq
#' mex_seq <- HaltonSeq(1000, mex, 42)
#' print(mex_seq)
#' }
#'
#' @author Darren Colby \cr
#' Email: dscolby17@@gmail.com
#' @export
print.PointSim <- function(x, ...) {

   cat("PointSim Object\n\n")
   print(x$window)
   cat("\n")
   cat(paste("Points:", x$n, sep = ""))

}


#' Display summary information from a PointSim instance
#'
#' @description Prints a summary of information from either a PointProcess or
#' HaltonSeq object, whcih are both child classes of the PointSim class.
#'
#' @param object a PointSim object
#' @param ... ignored.
#'
#' @examples \dontrun{
#' # Create spacejamr object
#' mex <- as.spacejamr("Z:shapefiles/mexico_adm0.shp")
#' }
#'
#' \dontrun{
#' # With PointProcess
#' mex_points <- PointProcess(1000, mex, 42)
#' summary(mex_points)
#' }
#'
#' \dontrun{
#' # With HaltonSeq
#' mex_seq <- HaltonSeq(1000, mex, 42)
#' summary(mex_seq)
#' }
#'
#' @author Darren Colby \cr
#' Email: dscolby17@@gmail.com
#' @export
summary.PointSim <- function(object, ...) {

   summary(spatstat.geom::ppp(object$x, object$y, object$window))
}
