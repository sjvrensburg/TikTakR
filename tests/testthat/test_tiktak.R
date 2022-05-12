test_that("tiktak function results appropriate answer",
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
        expect_equal(res_1[[n]]$objective, eval_f0(solution.opt))
        expect_equal(res_2[[n]]$objective, eval_f0(solution.opt))
        expect_equal(res_3[[n]]$objective, eval_f0(solution.opt))
    })
