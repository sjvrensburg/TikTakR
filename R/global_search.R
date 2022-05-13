#' General Step in The Global Search Stage of TikTak
#'
#' This function implements the ith step the global search stage of the TikTak
#' algorithm.
#'
#' @param i the \eqn{i}th step
#' @param s the matrix of candidate points/parameters from the initial step
#' @param visited_minimum best result from previous local searches
#' @param local_search_fun function perform local search
#' @param theta_min minimum weight
#' @param theta_max maximum weight
#' @param theta_pow determines the rate of increase in weight
#'
#' @details \code{visited_minimum} must be a list with the same structure as the
#' results from \code{nloptr}. Similarly, \code{local_search_fun} is function of
#' a single (vector) parameter that produces a list with the same structure as
#' \code{visited_minimum}.
#'
#' @return The best result from local searches.
#' @export
global_search <- function(i, s, visited_minimum, local_search_fun, theta_min = 0.1,
    theta_max = 0.995, theta_pow = 0.5) {
    # Prep
    n <- nrow(s)
    f <- visited_minimum$objective
    p <- visited_minimum$solution

    # Determine new starting value for local search.
    theta <- schedule(i, n, theta_min, theta_max, theta_pow)
    s_i <- (1 - theta) * s[i, ] + theta * p

    # Perform local search
    visited_i <- local_search_fun(s_i)

    # Determine if new solution is better than previous best.
    if (visited_i$status >= 0) {
        if (visited_i$objective <= f)
            visited_minimum <- visited_i
    }
    visited_minimum
}
