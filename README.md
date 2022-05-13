# TikTakR: An Implementation of The TikTak Method in R

TikTakR provides an R implementation of the TikTak multistart optimisation method/algorithm of [_Arnoud, Guvenen, and Kleineberg (2019)_](https://www.nber.org/system/files/working_papers/w26340/w26340.pdf).

## Installation

Ensure that you are running a recent version of R and have the necessary dependencies installed. Install the package directly from this repository:

```r
# If you don't have the remotes package...
# install.packages("remotes")
remotes::install_github("sjvrensburg/TikTakR")
```

## Usage

Before we begin, this package uses the [`nloptr`](https://astamm.github.io/nloptr/index.html) package for local search/optimisation. Knowledge of the [`nloptr`](https://astamm.github.io/nloptr/index.html) package is useful.

First, define the objective function and (optionally) it's gradient. Here we consider the [Rastrigin function.](https://en.wikipedia.org/wiki/Rastrigin_function)
```r
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
```
Following this, define the options for local optimisation.
```r
# We will use the L-BFGS algorithm. (print_level = 3 is for demonstration purposes)
opts_lbfgs <- list(xtol_rel = 1e-6, algorithm = "NLOPT_LD_LBFGS", print_level = 3)
```
Now, use TikTak...
```r
res_lbfgs <- tiktak(
  # Objective and gradient functions
  eval_f = rastrigin_fun, eval_grad_f = rastrigin_grad,
  # Number of best parameter vectors from Sobol sequence to keep.
  n = 10,
  # Number of parameter vectors to generate via a Sobol sequence.
  N = 1000,
  # Bounds (used to scale results from Sobol sequence and then passed to nloptr)
  lb = rep(-5.12, 2), ub = rep(5.12, 2),
  # Local search options (passed to nloptr)
  opts = opts_lbfgs,
  A = 10)
res_lbfgs
```
In this case, the Sobol sequence actually proposes the optimal solution as a starting value. Nevertheless, this multistart strategy often proves useful with more complex problems. (See the unit tests for an example were the optimal solution is removed from the candidate starting values.)

## _Gotchas_, Tips And Tricks

There are some important things to note concerning parameter restrictions.

### Use with unbounded parameters

As the name implies, a multistart algorithm generates multiple starting values from which to run local searches. In order to generate these starting values, the algorithm needs bounds within which to propose starting values. That is, you need to give the algorithm a sensible range over which to propose starting values. However, imposing these bounds during the local search might not be strictly necessary or even appropriate.

For example, you may believe that values between 3 and 10 will provide good starting values for a certain parameter. Yet, the objective function imposes no actual bounds on that parameter. Imposing such bounds during optimisation may, therefore, lead to suboptimal solutions.

In order to avoid this type of situation, the user may supply `tiktak` with their own candidate starting values via the argument `init_res`. If the user supplies `tiktak` with their own candidate starting values then the arguments `lb`, `ub`, etc. are passed directly to `nloptr`. This means that `lb` and `ub` may contain `-Inf` and  `Inf`, i.e., `lb` and `ub` can be different from the bounds used to generate starting values.

The value passed to `init_res` should be a list with named elements `Parameters` and `Objective`, where
  
  - `Parameters` is a $n \times k$ matrix with a row for each vector of candidate starting values and
  - `Objective` is a vector of length $n$ that contains the value of the objective function evaluated at each of the $n$ vectors of candidate starting values.

For the user's convenience, `TikTakR` exposes the function `initialise`. You may use `initialise` to generate such a list. In fact, `tiktak` uses `initialise` to generate candidate starting values if the user does not supply their own.

### Inequality and equality constraints

The user may supply `tiktak` with functions that calculate in/equality constraints and their Jacobian matrices. If the user does not supply their own candidate starting values then `initialise` will use these functions to remove starting that violate constraints. This may result in situations where `initialise` is unable to generate any candidate starting values. In particular, it is unlikely that `initialise` will be able to generate starting values that are compatible with equality constraints. In such cases, the user should generate their own candidate starting values as described in the previous section.











