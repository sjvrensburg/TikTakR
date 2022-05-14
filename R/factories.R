# Factory to produce the objective function.
factory_objective <- function(eval_f, ...) {
  arguments <- arg_getter()
  obj_fun <- function(x) {
    new_args <- arguments
    new_args[[1]] <- x
    names(new_args)[1] <- ""
    do.call(eval_f, new_args)
  }
  f <- function(x) {
    objective <- obj_fun(x)
    if (is.list(objective))
      objective <- objective$objective
    return(objective)
  }
  return(f)
}

# Create test function from eval_g_ineq and eval_g_eq
factory_test <- function(eval_g_ineq, eval_g_eq = NULL, ...) {
  if (is.null(eval_g_ineq) & is.null(eval_g_eq))
    return(NULL)
  if (!is.null(eval_g_ineq) & !is.null(eval_g_eq)) {
    f <- function(x, ...) {
      ineq <- eval_g_ineq(x, ...)
      if (is.list(ineq))
        ineq <- ineq$constraints
      eq <- eval_g_eq(x, ...)
      if (is.list(eq))
        eq <- eq$constraints
      res <- all(ineq <= 0) & isTRUE(all.equal(eq, rep(0, length(eq))))
      return(res)
    }
    return(f)
  }
  if (!is.null(eval_g_ineq) & is.null(eval_g_eq)) {
    f <- function(x, ...) {
      ineq <- eval_g_ineq(x, ...)
      if (is.list(ineq))
        ineq <- ineq$constraints
      return(all(ineq <= 0))
    }
    return(f)
  }
  if (is.null(eval_g_ineq) & !is.null(eval_g_eq)) {
    f <- function(x, ...) {
      eq <- eval_g_eq(x, ...)
      if (is.list(eq))
        eq <- eq$constraints
      res <- isTRUE(all.equal(eq, rep(0, length(eq))))
      return(res)
    }
    return(f)
  }
}

# Factory to produce the local search function.
factory_local <- function(...) {
  arguments <- arg_getter()
  f <- function(x) {
    new_args <- c(list(x0 = x), arguments)
    do.call(nloptr::nloptr, new_args)
  }
  return(f)
}

# Factory to produce global step function.
factory_global <- function(s, local_search, theta_min = 0.1, theta_max = 0.995,
                           theta_pow = 0.5, ...) {
  f <- function(visited_minimum, i) {
    global_search(i, s, visited_minimum, local_search, theta_min,
                  theta_max, theta_pow, ...)
  }
  return(f)
}
