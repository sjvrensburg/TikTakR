#' Implementation of The TikTak Method
#'
#' The 'TikTak' multistart method, as described in \insertCite{arnoud2019;textual}{TikTakR}.
#'
#' @param eval_f function that returns the value of the objective function. It can also return gradient information at the same time in a list with elements 'objective' and 'gradient' (see the documentation for the \code{nloptr} package).
#' @param n desired number of starting values. Ignored if \code{init_res} not \code{NULL}, though the user must still supply a value.
#' @param lb,ub vector with lower bounds (\code{lb}) and upper bounds (\code{ub}) of the parameters (use -Inf or Inf for parameters without lower or upper bounds), by default there are no bounds for any of the parameters.
#' @param N total number of candidate starting values. Must exceed \code{n}. Ignored if \code{init_res} not \code{NULL}.
#' @param eval_grad_f function that returns the value of the gradient of the objective function. Not all of the algorithms require a gradient.
#' @param eval_g_ineq function to evaluate (non-)linear inequality constraints that should hold in the solution. It can also return gradient information at the same time in a list with elements 'constraints' and 'jacobian' (see the documentation for the \code{nloptr} package).
#' @param eval_jac_g_ineq function to evaluate the jacobian of the (non-)linear inequality constraints that should hold in the solution.
#' @param eval_g_eq function to evaluate (non-)linear equality constraints that should hold in the solution. It can also return gradient information at the same time in a list with elements 'constraints' and 'jacobian' (see the documentation for the \code{nloptr} package).
#' @param eval_jac_g_eq function to evaluate the jacobian of the (non-)linear equality constraints that should hold in the solution.
#' @param opts list with options. The option 'algorithm' is required (see the documentation for the \code{nloptr} package).
#' @param theta_min minimum weight See \insertCite{arnoud2019;textual}{TikTakR}.
#' @param theta_max maximum weight. See \insertCite{arnoud2019;textual}{TikTakR}.
#' @param theta_pow parameter that controls rate of decay in weight. See \insertCite{arnoud2019;textual}{TikTakR}.
#' @param init_res an object produced by \code{initialise}. If \code{NULL} (default) then \code{titak} will run \code{initialise}.
#' @param ... additional parameters to pass to the objective and constraint functions.
#'
#' @note Equality constraints pose a challenge for this implementation of the TikTak algorithm. See \code{initialise} for an explanation.
#'
#' @return The best result from performing \code{n} local searches.
#' @export
#' @references
#' \insertRef{arnoud2019}{TikTakR}
#' @importFrom Rdpack reprompt
tiktak <- function(eval_f, n, lb, ub, N = n * 10, eval_grad_f = NULL,
    eval_g_ineq = NULL, eval_jac_g_ineq = NULL, eval_g_eq = NULL, eval_jac_g_eq = NULL,
    opts = list(), theta_min = 0.1, theta_max = 0.995, theta_pow = 0.5, init_res = NULL, ...) {

    if (is.null(init_res)) {
      # Create the test function
      test <- factory_test(eval_g_ineq, eval_g_eq, ...)

      # Initial parameters
      init_res <- initialise(n, N, lb, ub, eval_f, test, ...)
    }

    s <- init_res$Parameters
    n <- length(init_res$Objective)
    visited_minimum <- list(solution = s[1,], objective = init_res$Objective[1])

    # Create the local & global step functions
    local_search <- factory_local(
      eval_f = eval_f, eval_grad_f = eval_grad_f, lb = lb, ub = ub,
      eval_g_ineq = eval_g_ineq, eval_jac_g_ineq = eval_jac_g_ineq,
      eval_g_eq = eval_g_eq, eval_jac_g_eq = eval_jac_g_eq, opts = opts, ...)

    global_step <- factory_global(s, local_search, theta_min, theta_max,
        theta_pow)

    # Fold
    purrr::accumulate(.x = 1:n, .f = global_step, .init = visited_minimum)[[n]]
}
