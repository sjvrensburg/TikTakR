################################ TEST THE initialise
################################ FUNCTION #

test_that("initialise works given different types of objective functions", {
  # Objective functions: 1. Only an objective
  # function.
  f_1 <- function(x) {
    (x[1])^2 + (x[2] - 1)^2 + x[1] *
      x[2]
  }
  # 2. Separate objective and gradient
  # functions.
  g <- function(x) {
    c(2 * x[1] + x[2], 2 * (x[2] -
      1))
  }
  # 3. Function that evaluates both the
  # objective and gradient.
  f_2 <- function(x) list(objective = f_1(x), gradient = g(x))

  # Test if they all produce the same results.
  n <- 5
  N <- 1000
  lb <- rep(-2, 2)
  ub <- rep(2, 2)
  res_1 <- initialise(n, N, lb, ub, f_1)
  res_2 <- initialise(n, N, lb, ub, f_2)
  expect_identical(res_1, res_2)
  res_1mc <- initialise(n, N, lb, ub, f_1, num.cores = 4)
  res_2mc <- initialise(n, N, lb, ub, f_2, num.cores = 4)
  expect_identical(res_1mc, res_2mc)
})

test_that("factory_test with initialise work for constraints functions", {
  # Objective function
  f_1 <- function(x) {
    (x[1])^2 + (x[2] - 1)^2 + x[1] *
      x[2]
  }

  # Inequality constraints with and without
  # Jacobian x_1 >= 0 and x_2 >= x_1
  ineq_1 <- function(x) c(-1 * x[1], x[1] - x[2])
  ineq_jac <- function(x) rbind(c(-1, 0), c(1, -1))
  ineq_2 <- function(x) {
    list(
      constraints = ineq_1(x),
      jacobian = ineq_jac(x)
    )
  }

  # Equality constraints with and without
  # Jacobian x_1 == 0
  eq_1 <- function(x) x[1]
  eq_jac <- function(x) 1
  eq_2 <- function(x) {
    list(
      constraints = eq_1(x),
      jacobian = eq_jac(x)
    )
  }

  # Test functions
  test_1 <- factory_test(ineq_1, NULL)
  test_2 <- factory_test(ineq_2, NULL)
  test_3 <- factory_test(NULL, eq_1)
  test_4 <- factory_test(NULL, eq_2)
  test_5 <- factory_test(ineq_1, eq_1)
  test_6 <- factory_test(ineq_1, eq_2)
  test_7 <- factory_test(ineq_2, eq_1)
  test_8 <- factory_test(ineq_2, eq_2)

  # Test if the all produce TRUE when x = c(0,
  # 2)
  expect_true(test_1(c(0, 2)))
  expect_true(test_2(c(0, 2)))
  expect_true(test_3(c(0, 2)))
  expect_true(test_4(c(0, 2)))
  expect_true(test_5(c(0, 2)))
  expect_true(test_6(c(0, 2)))
  expect_true(test_7(c(0, 2)))
  expect_true(test_8(c(0, 2)))

  # Test if FALSE when violating inequality
  # but not equality constraint.
  expect_false(test_1(c(0, -2)))
  expect_false(test_2(c(0, -2)))
  expect_false(test_5(c(0, -2)))
  expect_false(test_6(c(0, -2)))
  expect_false(test_7(c(0, -2)))
  expect_false(test_8(c(0, -2)))

  # Test if FALSE when violating equality but
  # not inequality constraint
  expect_false(test_3(c(1, 2)))
  expect_false(test_4(c(1, 2)))
  expect_false(test_5(c(1, 2)))
  expect_false(test_6(c(1, 2)))
  expect_false(test_7(c(1, 2)))
  expect_false(test_8(c(1, 2)))

  # Test if they all produce the same results.
  n <- 5
  N <- 100
  lb <- rep(-2, 2)
  ub <- rep(2, 2)

  res_1 <- initialise(n, N, lb, ub, f_1, test_1)
  res_2 <- initialise(n, N, lb, ub, f_1, test_2)
  expect_identical(res_1, res_2)

  res_3 <- initialise(
    n, N, c(0, -2), c(0, 2), f_1,
    test_3
  )
  res_4 <- initialise(
    n, N, c(0, -2), c(0, 2), f_1,
    test_4
  )
  expect_identical(res_3, res_4)

  res_5 <- initialise(
    n, N, c(0, -2), c(0, 2), f_1,
    test_5
  )
  res_6 <- initialise(
    n, N, c(0, -2), c(0, 2), f_1,
    test_6
  )
  expect_identical(res_5, res_6)

  res_7 <- initialise(
    n, N, c(0, -2), c(0, 2), f_1,
    test_7
  )
  res_8 <- initialise(
    n, N, c(0, -2), c(0, 2), f_1,
    test_8
  )
  expect_identical(res_7, res_8)
})
