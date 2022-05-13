test_that("tiktak function results appropriate answer for NLOPT example problem",
    {
        # Example showing how to solve the problem
        # from the NLopt tutorial.
        # solution: ( 1/3, 8/27 )

        # objective function
        eval_f0 <- function(x, a, b) {
            return(sqrt(x[2]))
        }

        # constraint function
        eval_g0 <- function(x, a, b) {
            return((a * x[1] + b)^3 - x[2])
        }

        # gradient of objective function
        eval_grad_f0 <- function(x, a, b) {
            return(c(0, 0.5/sqrt(x[2])))
        }

        # jacobian of constraint
        eval_jac_g0 <- function(x, a, b) {
            return(rbind(c(3 * a[1] * (a[1] * x[1] + b[1])^2,
                -1), c(3 * a[2] * (a[2] * x[1] + b[2])^2,
                -1)))
        }

        # Define parameters.
        a <- c(2, -1)
        b <- c(0, 1)

        # Define optimal solution.
        solution.opt <- c(1/3, 8/27)

        # Parameters and options for titak
        n <- 10; lb <- c(0, 0); ub <- c(1, 1); N <- 1000
        opts <- list("xtol_rel" = 1e-8, "algorithm" = "NLOPT_LD_SLSQP")

        # Optimise using separate objective and gradient functions
        res_1 <- tiktak(eval_f = eval_f0, n = n, lb = lb, ub = ub, N = N,
                        eval_grad_f = eval_grad_f0, eval_g_ineq = eval_g0,
                        eval_jac_g_ineq = eval_jac_g0, opts = opts, a = a,
                        b = b)

        # Optimise using combined objective and gradient function
        f <- function(x, a, b) list(objective = eval_f0(x, a, b),
                                    gradient = eval_grad_f0(x, a, b))
        res_2 <- tiktak(eval_f = f, n = n, lb = lb, ub = ub, N = N,
                        eval_g_ineq = eval_g0, eval_jac_g_ineq = eval_jac_g0,
                        opts = opts, a = a, b = b)

        # Optimise using combined constraint and Jaccobian function
        g <- function(x, a, b) list(constraints = eval_g0(x, a, b),
                                    jacobian = eval_jac_g0(x, a, b))
        res_3 <- tiktak(eval_f = f, n = n, lb = lb, ub = ub, N = N,
                        eval_g_ineq = g, opts = opts, a = a, b = b)

        # Test
        expect_equal(res_1$objective, eval_f0(solution.opt))
        expect_equal(res_2$objective, eval_f0(solution.opt))
        expect_equal(res_3$objective, eval_f0(solution.opt))
    })

test_that("tiktak works for optimising the Rastrigin function.", {
  # The Rastrigin is a challenging function to optimise since it has
  # many many local minima.
  rastrigin_fun <- function(x, A = 10) {
    n <- length(x)
    A*n + sum(x^2 - A * cos(2 * pi * x)) + 1
  }
  rastrigin_grad <- function(x, A = 10) {
    # I'm being lazy and going to use numerical differentiation.
    numDeriv::grad(rastrigin_fun, x, A = A)
  }
  
  # Setup the optimisation... we want to have many short runs of the local search.
  n <- 10; N <- 100; lb <- rep(-5.12, 2); ub <- rep(5.12, 2)
  max_time <- 0.25  # a quarter of a second.
  xtol_rel <- 1e-6
  opts_sbplx <- list("maxtime" = max_time, "xtol_rel" = xtol_rel, "algorithm" = "NLOPT_LN_SBPLX", "print_level" = 3)
  opts_lbfgs <- list("maxtime" = max_time, "xtol_rel" = xtol_rel, "algorithm" = "NLOPT_LD_LBFGS", "print_level" = 3)
  
  res_sbplx <- tiktak(eval_f = rastrigin_fun, n = n, lb = lb, ub = ub, N = N, opts = opts_sbplx, A = 10)
  res_lbfgs <- tiktak(eval_f = rastrigin_fun, eval_grad_f = rastrigin_grad, n = n, lb = lb, ub = ub, N = N, opts = opts_lbfgs, A = 10)
  
  expect_equal(res_sbplx$solution, c(0, 0), label = "TikTak + Sbplx")
  expect_equal(res_lbfgs$solution, c(0, 0), label = "TikTak + L-BFGS")
})

test_that("tiktak works for optimising the Rastrigin function when the starting values do not include the optimal solution.", {
  # The Rastrigin is a challenging function to optimise since it has
  # many many local minima.
  rastrigin_fun <- function(x, A = 10) {
    n <- length(x)
    A*n + sum(x^2 - A * cos(2 * pi * x)) + 1
  }
  rastrigin_grad <- function(x, A = 10) {
    # I'm being lazy and going to use numerical differentiation.
    numDeriv::grad(rastrigin_fun, x, A = A)
  }
  
  # Setup the optimisation... we want to have many short runs of the local search.
  n <- 50; N <- 1000; lb <- rep(-5.12, 2); ub <- rep(5.12, 2)
  max_time <- 2.4
  xtol_rel <- 1e-8
  opts_sbplx <- list("maxtime" = max_time, "xtol_rel" = xtol_rel, "algorithm" = "NLOPT_LN_SBPLX", "print_level" = 3)
  opts_lbfgs <- list("maxtime" = max_time, "xtol_rel" = xtol_rel, "algorithm" = "NLOPT_LD_LBFGS", "print_level" = 3)
  
  init_res <- initialise(n + 1, N, lb, ub, rastrigin_fun, A = 10)
  # Remove the optimal solution...
  init_res$Parameters <- init_res$Parameters[-1, ]
  init_res$Objective <- init_res$Objective[-1]
  
  res_sbplx <- tiktak(eval_f = rastrigin_fun, n = n, lb = lb, ub = ub, N = N, opts = opts_sbplx, init_res = init_res, A = 10)
  res_lbfgs <- tiktak(eval_f = rastrigin_fun, eval_grad_f = rastrigin_grad, n = n, lb = lb, ub = ub, N = N, opts = opts_lbfgs, A = 10)
  
  expect_equal(res_sbplx$solution, c(0, 0), label = "TikTak + Sbplx (Expected to fail)")
  expect_equal(res_lbfgs$solution, c(0, 0), label = "TikTak + L-BFGS")
})