# Author: Darren Colby
# Date: 9/6/2021
# Purpose: To create a basic class and methods for "spacejamr" objects

# Constructor methods -----------------------------------------------------


#' validate_spacejamr
#'
#' @description Validates the input to the spacejamr constructor.
#'
#' @usage validate_spacejamr(path)
#'
#' @details This function should not be called directly
#'
#' @param path the path to a shapefile as a string.
#'
#' @return A list containing a window object of class "owin" and the crs.
#' @author Darren Colby
#' Email: dscolby17@gmail.com
#' @noRd
validate_spacejamr <- function(path) {

   stopifnot(is.character(path))

   shapefile <- sf::read_sf(path)  # Raw shapefile
   suggested_crs <- crsuggest::suggest_top_crs(shapefile)  # Best projected crs

   # Transform the shapefile to the best projected coordinate reference system
   transformed_shapefile <- sf::st_transform(shapefile, suggested_crs)

   window <- spatstat.geom::as.owin(transformed_shapefile)

   return_list <- list(window = window, crs = suggested_crs)

   return(return_list)

}


#' new_spacejamr
#'
#' @description Create a new spacejamr object
#'
#' @usage new_spacejamr(path)
#'
#' @details This function should not be called by the user
#'
#' @param path the path to a shapefile as a string.
#'
#' @return a spacejamr object containing two items. window: object of class
#' 'owin' that stores geographical boundaries. crs: integer value referring to
#' the coordinate reference system of the geographical boundaries.
#'
#' @author Darren Colby
#' Email: dscolby17@gmail.com
#' @noRd
new_spacejamr <- function(path) {

   # Validate the input
   validated_shapefile <- validate_spacejamr(path)

   # Set the class
   spacejamr_object <- structure(validated_shapefile,
                                 class = c("spacejamr", "owin"))

   return(spacejamr_object)

}


#' Initialize a new spacejamr object
#'
#' @description Creates a new spacejamr object that for further analysis
#'
#' @usage as.spacejamr(path)
#'
#' @details The returned spacejamr object will contain a window object
#' containing a geographical boundary and its coordinate reference system.
#' Since simulating a spatial point process or sequnce requires a projected
#' coordinate reference system, this method will automatically look for the
#' most appropriate projected coordinate reference system and project the
#' geographical boundary to that system. Therefore, this function may take
#' some time to run but is necessary for later steps to simulate a spatial
#' Bernoulli network.
#'
#' @param path the path to a shapefile as a string.
#'
#' @return a spacejamr object containing two items. window: object of class
#' 'owin' that stores geographical boundaries. crs: integer value referring to
#' the coordinate reference system of the geographical boundaries.
#'
#' @examples \donttest{
#' ri <- as.spacejamr(system.file("shape/ri.shp", package = "spacejamr"))
#' }
#'
#' @author Darren Colby \cr
#' Email: dscolby@@gmail.com
#' @export
as.spacejamr <- function(path) {

    # Call the new_spacejamr constructor method
    spacejamr_object <- new_spacejamr(path)

    return(spacejamr_object)

}


# Plot, print, and summary methods ----------------------------------------


#' Plot the spatial extent of a spacejamr object
#'
#' @description Plot method for the spacejamr class
#'
#' @param x an object of class spacejamr.
#' @param y ignored.
#' @param ... ignored.
#' @param title an optional title for the plot. Default is "Spatial Window".
#' @param fill an optional fill for the plot. Default is blue.
#'
#' @details The returned plot can be refined with standard ggplot2 functions
#'
#' @return A plot of classes 'gg' and 'ggplot'
#'
#' @examples
#' # Load spacejamr object
#' data("RI")
#'
#' plot(RI)
#'
#' @author Darren Colby \cr
#' Email: dscolby17@@gmail.com
#' @export
plot.spacejamr <- function(x, y, ..., title = "Spatial Window", fill = "blue") {

   # Generate an sf object from the window
   window <- sf::st_as_sf(x[[1]])

   plot <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = window,
                       fill = fill) +
      ggplot2::labs(title = title) +
      ggthemes::theme_map()

   return(plot)

}


#' Print information from a spacejamr instance
#'
#' @description Print method for the spacejamr class
#'
#' @param x a spacejamr object
#' @param ... ignored.
#'
#' @details Provides a wrapper for the print.owin method in the spatstats.geom
#' package.
#'
#' @return No return value, called for side effects
#'
#' @examples
#' # Load spacejamr object
#' data("RI")
#'
#' print(RI)
#'
#' @author Darren Colby \cr
#' Email: dscolby17@@gmail.com
#'
#' @export
print.spacejamr <- function(x, ...) {

   spatstat.geom::print.owin(x[[1]])  # Only use the window data

}


#' Print summary information of a spacejamr instance
#'
#' @description Summary method for the spacejamr class
#'
#' @param object a spacejamr object
#' @param ... ignored.
#'
#' @details Provides a wrapper for the summary.owin method in the spatstats.geom
#' package.
#'
#' @return No return value, called for side effects
#'
#' @examples
#' # Load spacejamr object
#' data("RI")
#'
#' summary(RI)
#'
#' @author Darren Colby \cr
#' Email: dscolby17@@gmail.com
#'
#' @export
summary.spacejamr <- function(object, ...) {

   spatstat.geom::summary.owin(object[[1]])  # Only the window data

}

