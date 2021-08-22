# Author: Darren Colby
# Date: 8/22/2021
# Purpose: Provide a function to compare two simulated networks


#' COmpare statistics from two simulated networks
#'
#' @description Generates summary statistics for two simulated networks of class
#' NetSim.
#'
#' @usage compare_networks(net1, net2)
#'
#' @details This function enables a quick comparison of two simulated networks
#' of class NetSim. This allows the user to see how different a network
#' simulated from a Halton sequence is from network simulated from a Poisson
#' point process with the same number of points in the same geographical window.
#' Similarly, it enables comparison of networks generated from different
#' spatial interaction functions. The density, mean degree, mean closeness, mean
#' betweenness, and the size of the largest connected component are computed for
#' each network.
#'
#' @param net1 a NetSim object
#' @param net2 a NetSim object
#'
#' @return A dataframe
#'
#' @example \dontrun{
#' # load data
#' data("mexico)
#'
#' # Simulate point process
#' mex_points <- PointProcess(50, mexico)
#'
#' # Create two networks
#' pl <- PowerLawNetwork(mex_points) # Standard power law
#' apl <- APLNetwork(mex_points) # Attenuated power law
#'
#' compare_networks(pl, apl)
#' }
#'
#' @author Darren Colby \cr
#' Email: dscolby17@@gmail.com
#' @export
compare_networks <- function(net1, net2) {

    stopifnot(methods::is(net1, c("NetSim", "igraph"))&
                  methods::is(net2, c("NetSim", "igraph")))

    # Compute basinc network statistics
    density <- c(igraph::edge_density(net1), igraph::edge_density(net1))
    degree <- c(mean(igraph::degree(net1)), mean(igraph::degree(net2)))
    closeness <- c(mean(igraph::closeness(net1)), mean(igraph::closeness(net2)))
    betweenness <- c(mean(igraph::betweenness(net1)),
                     mean(igraph::betweenness(net2)))
    component <- c(max(igraph::clusters(net1)$csize),
                   max(igraph::clusters(net2)$csize))

    # Coerce into a dataframe
    df <- data.frame(density, degree, closeness, betweenness, component)

    colnames(df) <- c("Density", "Mean Degree", "Mean Closeness",
                      "Mean Betweenness", "Largest Component Size")

    rownames(df) <- c(deparse(substitute(net1)), deparse(substitute(net2)))

    print(df)

    return(df)

}

