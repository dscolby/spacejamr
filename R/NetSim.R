# Author: Darren Colby
# Date: 8/27/2021
# Purpose: To simulate spatial Bernoulli networks

# Contructor methods to simulate a standard power law network -------------


# Validate input to a PowerLawNetwork constructor
#
# @description validates a spatial Bernoulli network using a standard power law
#
# @details This function should not be called by the user
#
# @param point_sim a PointSim object
# @param base_prob the theoretical probability that two nodes (points) with
# distance 0 share a tie.
# @param scale a coefficient to multiply the distance by.
# @param threshold if two node exceed this probability, they will be coded as
# having a tie.
# @param power the exponent at which tie probability decays.
#
# @return An igraph object
#
# @author Darren Colby
#    Email: dscolby17@gmail.com
validate_PowerLawNetwork <- function(point_sim, base_prob = 0.9, scale = 1,
                                     threshold = 0.5, power = -2.8) {

    # Ensures proper input
    stopifnot(methods::is(point_sim, "PointSim"))


    # Helper function to estimate the probability of a tie with given parameters
    ProbabilityFunction <- function(dist) {

        prob <- (base_prob/((1 + (scale * dist)) ^ abs(power)))

        return(prob)

    }


    # Helper function to create ties if tie probability exceeds the threshold
    ThresholdFunction <- function(prob) {

        ifelse(prob > threshold, 1, 0)

    }


    # Calculate the distance between all pairs of nodes
    distances <- dplyr::as_tibble(spatstat.geom::pairdist(point_sim),
                                  .name_repair = "unique") %>%

    # Apply the power law function
    dplyr::mutate(dplyr::across(.fns = ProbabilityFunction),

    # Code as a tie if the probability exceeds the given threshold
    dplyr::across(.fns = ThresholdFunction)) %>%

    # This is just a good practice
    dplyr::ungroup() %>%
    as.matrix()

    simulated_network <- igraph::graph_from_adjacency_matrix(distances,
                                                             mode = "undirected") %>%
    igraph::simplify() # Removes self loops

    return(simulated_network)

}


# Validate and set class attribute of input to the PowerLawNetwork constructor
#
# @description creates a spatial Bernoulli network using a standard power law
#
# @details This function should not be called by the user
#
# @param point_sim a PointSim object
# @param base_prob the theoretical probability that two nodes (points) with
# distance 0 share a tie.
# @param scale a coefficient to multiply the distance by.
# @param threshold if two node exceed this probability, they will be coded as
# having a tie.
# @param power the exponent at which tie probability decays.
#
# @return An igraph object
#
# @author Darren Colby
#    Email: dscolby17@gmail.com
new_PowerLawNetwork <- function(point_sim, base_prob, scale, threshold, power) {

    validated_network <- validate_PowerLawNetwork(point_sim, base_prob, scale,
                                                  threshold, power)

    validated_network <- structure(validated_network, class = c("NetSim",
                                                                "igraph"))

    return(validated_network)

}


#' Simulate a power law network
#'
#' @description Simulates a spatial Bernoulli network from a NetSim object
#' using a standard power law function as the spatial interaction function.
#'
#' @usage PowerLawNetwork(point_sim, base_prob, scale, threshold, power)
#'
#' @details The algorithm proceeds in three steps. First, it calculates the
#' distance between simulated points from a PointSim object, which includes
#' PointProcess and HaltonSeq objects. Then it calculates the distance between
#' all pairs of points. Finally, it uses a standard power law function to
#' calculate that any two point share a tie. If the threshold is exceeded, a tie
#' is created.
#'
#' @param point_sim a PointSim object
#' @param base_prob the theoretical probability that two nodes (points) with
#' distance 0 share a tie. Default is 0.9.
#' @param scale a coefficient to multiply the distance by. Default is 1.
#' @param threshold if two node exceed this probability, they will be coded as
#' having a tie. Default is 0.5.
#' @param power the exponent at which tie probability decays. Default is -2.8.
#'
#' @return A network object of class 'igraph' that can be manipulated using the
#' 'igraph' package.
#'
#' @example
#' # Load spacejamr object
#' data("RI")
#'
#' # With PointProcess
#' ri_points <- PointProcess(10, RI, 42)
#' power_law <- PowerLawNetwork(ri_points, base_prob = 0.92, scale = 1,
#'                              threshold = 0.5, power = -2.4)
#'
#' # With HaltonSeq
#' ri_seq <- HaltonSeq(10,RI, 42)
#' power_law <- PowerLawNetwork(ri_seq, base_prob = 0.98, scale = 100,
#'                              threshold = 0.5, power = -1.87)
#'
#' @author Darren Colby \cr
#' Email: dscolby17@@gmail.com
#'
#' @references Butts, Carter T. Spatial models of large-scale interpersonal
#' networks. Dissertation. (2002).
#' @export
PowerLawNetwork <- function(point_sim, base_prob = 0.9, scale = 1,
                            threshold = 0.5, power = -2.8) {

    validated_network <- new_PowerLawNetwork(point_sim, base_prob, scale,
                                             threshold, power)

    return(validated_network)

}


# Attenuated power law network generator ----------------------------------


# Validate inputs to an APLNetwork constructor
#
# @description validates a spatial Bernoulli network using an attenuated power
# law
#
# @details This function should not be called by the user
#
# @param point_sim a PointSim object
# @param base_prob the theoretical probability that two nodes (points) with
# distance 0 share a tie.
# @param scale a coefficient to multiply the distance by.
# @param threshold if two node exceed this probability, they will be coded as
# having a tie.
# @param power the exponent at which tie probability decays.
#
# @return An igraph object
#
# @author Darren Colby
#    Email: dscolby17@gmail.com
validate_APLNetwork <- function(point_sim, base_prob, scale, threshold, power) {

    stopifnot(methods::is(point_sim, "PointSim"))


    # Helper function to estimate the probability of a tie with given parameters
    ProbabilityFunction <- function(dist) {

        # Attenuated power law
        prob <- (base_prob / (1 + (scale * dist) ^ abs(power)))


        return(prob)

    }


    # Helper function to create ties if tie probability exceeds the threshold
    ThresholdFunction <- function(prob) {

        ifelse(prob > threshold, 1, 0)

    }


    # Calculate the distance between all pairs of nodes
    distances <- dplyr::as_tibble(spatstat.geom::pairdist(point_sim),
                                  .name_repair = "unique") %>%

        # Apply the power law function
        dplyr::mutate(dplyr::across(.fns = ProbabilityFunction),

        # Code as a tie if the probability exceeds the given threshold
        dplyr::across(.fns = ThresholdFunction)) %>%

        # This is just a good practice
        dplyr::ungroup() %>%
        as.matrix()

    simulated_network <- igraph::graph_from_adjacency_matrix(distances,
                                                             mode = "undirected") %>%
        igraph::simplify() # Removes self loops

    return(simulated_network)

}


# Validate and set class attribute to input passed to APLNetwork
#
# @description creates a spatial Bernoulli network using an attenuated power law
#
# @details This function should not be called by the user
#
# @param point_sim a PointSim object
# @param base_prob the theoretical probability that two nodes (points) with
# distance 0 share a tie.
# @param scale a coefficient to multiply the distance by.
# @param threshold if two node exceed this probability, they will be coded as
# having a tie.
# @param power the exponent at which tie probability decays.
#
# @return An igraph object
#
# @author Darren Colby
#    Email: dscolby17@gmail.com
new_APLNetwork <- function(point_sim, base_prob, scale, threshold, power) {

    validated_network <- validate_APLNetwork(point_sim, base_prob, scale,
                                             threshold, power)

    validated_network <- structure(validated_network, class = c("NetSim",
                                                                "igraph"))

    return(validated_network)

}


#' Simulate an attenuated power law network
#'
#' @description Simulates a spatial Bernoulli network from a NetSim object
#' using an attenuated power law function as the spatial interaction function.
#'
#' @usage APLNetwork(point_sim, base_prob, scale, threshold, power)
#'
#' @details The algorithm proceeds in three steps. First, it calculates the
#' distance between simulated points from a PointSim object, which includes
#' PointProcess and HaltonSeq objects. Then it calculates the distance between
#' all pairs of points. Finally, it uses an attenuated power law function to
#' calculate that any two point share a tie. If the threshold is exceeded, a tie
#' is created.
#'
#' @param point_sim a PointSim object
#' @param base_prob the theoretical probability that two nodes (points) with
#' distance 0 share a tie. Default is 0.9.
#' @param scale a coefficient to multiply the distance by. Default is 1.
#' @param threshold if two node exceed this probability, they will be coded as
#' having a tie. Default is 0.5.
#' @param power the exponent at which tie probability decays. Default is -2.8.
#'
#' @return A network object of class 'igraph' that can be manipulated using the
#' 'igraph' package.
#'
#' @example
#' # Load spacejamr object
#' data("RI")
#'
#' # With PointProcess
#' ri_points <- PointProcess(10, RI, 42)
#' apl_points <- APLNetwork(ri_points, base_prob = 0.92, scale = 1,
#'                          threshold = 0.5, power = -2.4)
#'
#' # With HaltonSeq
#' ri_seq <- HaltonSeq(10,RI, 42)
#' apl_seq <- APLNetwork(ri_seq, base_prob = 0.98, scale = 100,
#'                       threshold = 0.5, power = -1.87)
#'
#' @author Darren Colby \cr
#' Email: dscolby17@@gmail.com
#'
#' @references Butts, Carter T. Spatial models of large-scale interpersonal
#' networks. Dissertation. (2002).
#' @export
APLNetwork <- function(point_sim, base_prob = 0.9, scale = 1,
                       threshold = 0.5, power = -2.8) {

    validated_network <- new_APLNetwork(point_sim, base_prob, scale, threshold,
                                        power)

    return(validated_network)

}


# Plot, print, and summary methods ----------------------------------------


#' Plot a simulated network from a NetSim object
#'
#' @description This can take either a PowerLawNetwork or APLNetwork object as
#' input, both of which are chidren of the NetSim class.
#'
#' @details This method returns a ggraph object, which can be further refined
#' using standard ggraph and ggplot facilities.
#'
#' @param x a NetSim graph
#' @param y ignored.
#' @param ... ignored.
#' @param layout a layout to display the graph. Layout must be a valid string.
#' from the ggraph package. Default is "stress".
#' @param title an optional title.
#' @param node_color a color for the nodes. Default is blue.
#' @param edge_color a color for the edges. Default is red.
#'
#' @return A plot of classes 'gg' and 'ggplot'
#'
#' @examples
#' # Load spacejamr object
#' data("RI")
#'
#' # With PointProcess
#' ri_points <- PointProcess(10, RI, 42)
#' apl_points <- APLNetwork(ri_points, base_prob = 0.92, scale = 1,
#'                          threshold = 0.5, power = -2.4)
#' plot(apl_points)
#'
#' # With HaltonSeq
#' ri_seq <- HaltonSeq(10, RI, 42)
#' apl_seq <- APLNetwork(ri_seq, base_prob = 0.98, scale = 100,
#'                       threshold = 0.5, power = -1.87)
#' plot(apl_seq)
#'
#' @author Darren Colby \cr
#' Email:dscolby17@@gmail.com
#'
#' @seealso [ggraph::ggraph()]
#' @export
plot.NetSim <- function(x, y, ..., layout = "stress",
                        title = "Network Simulation", node_color = "red",
                        edge_color = "blue") {

    stopifnot(methods::is(x, "NetSim"))

    plot <- ggraph::ggraph(x,
                           layout = layout) +
        ggraph::geom_edge_link(color = "blue") +
        ggraph::geom_node_point(color = "red") +
        ggraph::theme_graph()

    return(plot)

}


#' Print information from a NetSim object
#'
#' @description Plots a NetSim object and returns a ggraph object
#'
#' @param x a NetSim object
#' @param ... ignored.
#'
#' @return No return value, called for sied effects
#'
#' @examples
#' # Create spacejamr object
#' data("RI")
#'
#' # With PointProcess
#' ri_points <- PointProcess(10, RI, 42)
#' apl_points <- APLNetwork(ri_points, base_prob = 0.92, scale = 1,
#'                          threshold = 0.5, power = -2.4)
#' print(apl_points)
#'
#' # With HaltonSeq
#' ri_seq <- HaltonSeq(10, RI, 42)
#' apl_seq <- APLNetwork(ri_seq, base_prob = 0.98, scale = 100,
#'                       threshold = 0.5, power = -1.87)
#' print(apl_seq)
#'
#' @author Darren Colby \cr
#' Email: dscolby17@@gmail.com
#' @export
print.NetSim <- function(x, ...) {

    # If it ain't broke, don't fix it
    igraph::print.igraph(x)

}


#' Summary of NetSim graphs
#'
#' @description Prints a summary of a NetSim object
#'
#' @param object a NetSim object
#' @param ... ignored.
#'
#' @return No return value, called for side effects
#'
#' @examples
#' # Load spacejamr object
#' data("RI")
#'
#' # With PointProcess
#' ri_points <- PointProcess(10, RI, 42)
#' apl_points <- APLNetwork(ri_points, base_prob = 0.92, scale = 1,
#'                          threshold = 0.5, power = -2.4)
#' summary(apl_points)
#'
#' # With HaltonSeq
#' ri_seq <- HaltonSeq(1000, RI, 42)
#' apl_seq <- APLNetwork(ri_seq, base_prob = 0.98, scale = 100,
#'                       threshold = 0.5, power = -1.87)
#' summary(apl_seq)
#'
#' @author Darren Colby \cr
#' Email: dscolby17@@gmail.com
#' @export
summary.NetSim <- function(object, ...) {

    print(object)

}
