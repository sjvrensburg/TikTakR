#' Initialise The TikTak Algorithm
#'
#' This function generates the \code{n} candidate points.
#' @param n desired number of candidate points
#' @param N total number of candidate points to generate, must exceed \code{n}
#' @param lb,ub lower and upper bounds on parameters
#' @param eval_f the objective function to minimise
#' @param test optional test function (See details)
#' @param num.cores integer that specifies the number of cores to use
#' @param ... additional arguments to pass to the eval_f
#'
#' @details This function generates a Sobol sequence of length \code{N}. These
#' \code{N} points are scaled using the specified parameter bounds. If
#' \code{test} is not \code{NULL} then the function is applied to the \code{N}
#' points. The function \code{test} should return a boolean vector. Points that
#' result in \code{test} returning \code{FALSE} are discarded before calculating
#' the value of the objective function at each point. After calculating the
#' objective function at each point, the \code{n} best points are identified.
#'
#' @note Equality constraints pose a challenge for this implementation of the TikTak algorithm. It is improbable that the Sobol sequence will produce any values that satisfy the equality constraint. Hence, use with caution.
#'
#' @returns A list with a matrix of \code{n} candidate points and a vector with
#' with the value of the objective function at each point.
#' @export
#'
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
initialise <- function(n, N, lb, ub, eval_f, test = NULL,
                       num.cores = NULL, ...) {
  if (n >= N) {
    stop("N must exceed n.")
  }
  if (length(lb) != length(ub)) {
    msg <- "Lower and upper bounds must be of equal length."
    stop(msg)
  }
  if (is.null(num.cores)) num.cores <- 0
  k <- length(lb)
  s <- qrng::sobol(n = N, d = k, randomize = "none")

  # Register a cluster.
  if (num.cores >= 2) {
    cl <- makeCluster(num.cores)
    registerDoParallel(cl)
  }

  # Scale the values of `s`
  s <- apply(s, 1, function(x) lb + x * (ub - lb))
  s <- t(s)

  # If a test function is provided then apply it to `s`
  if (!is.null(test)) {
    test_ <- function(x) tryCatch(expr = test(x), error = function(e) FALSE)
    if (num.cores < 2) {
      idx <- which(drop(apply(s, 1, test_)))
    } else {
      chunks <- chunk2(1:nrow(s), num.cores)
      idx <- which(foreach(
        i = iterators::iter(chunks), .combine = c, .packages = (.packages())
      ) %dopar% {
        drop(apply(s[i, ], 1, test_))
      })
    }
    s <- s[idx, ]
  }

  if (n >= nrow(s)) {
    msg <- "Too few candidate solutions after applying `test`."
    msg <- paste(msg, "Try increasing the value of `N`.")
    stop(msg)
  }

  # Calculate the objective function for each candidate solution
  # and return the `n` best solutions.
  objective <- factory_objective(eval_f, ...)
  objective_ <- function(x) {
    tryCatch(expr = objective(x), error = function(e) {
      # msg <- "Something went wrong while evaluating the objective function."
      # msg <- paste(msg, "Returning Inf...")
      # warning(msg)
      return(Inf)
    })
  }
  if (num.cores < 2) {
    f <- apply(s, 1, objective_)
  } else {
    chunks <- chunk2(1:nrow(s), num.cores)
    f <- foreach(
      i = iterators::iter(chunks), .combine = c, .packages = (.packages())
    ) %dopar% {
      apply(s[i, ], 1, objective_)
    }
    stopCluster(cl)
  }
  idx <- order(f)[1:n]

  return(list(Parameters = s[idx, ], Objective = f[idx]))
}
