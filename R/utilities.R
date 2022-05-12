# Clamp function See
# https://stackoverflow.com/questions/32599695/clamp-variable-within-range
clamp <- function(x, e1, e2 = -e1) {
    e1 <- sort(c(e1, e2))
    pmin(pmax(x, e1[1]), e1[2])
}

# Schedule for increasing the weight.
schedule <- function(i, n, theta_min = 0.1, theta_max = 0.995, theta_pow = 0.5) {
    clamp((i/n)^theta_pow, theta_min, theta_max)
}

# Copied from the curry package. See:
# https://github.com/thomasp85/curry/
arg_getter <- function() {
  fun <- sys.function(sys.parent(1))
  arg_env <- attr(fun, 'arg_env')
  parent <- parent.frame()
  vals <- as.list(parent)
  if(!is.null(formals(fun)[['...']])) {
    vals <- c(vals, eval(quote(list(...)), parent))
  }
  c(arg_env$args, vals, arg_env$args_end)
}
