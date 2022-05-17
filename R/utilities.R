# Clamp function See
# https://stackoverflow.com/questions/32599695/clamp-variable-within-range
clamp <- function(x, e1, e2 = -e1) {
  e1 <- sort(c(e1, e2))
  pmin(pmax(x, e1[1]), e1[2])
}

#' Split a vector into n chunks
#' 
#' @param x a vector
#' @param n number of chunks
#' 
#' @author 
#'   \href{http://stackoverflow.com/users/1563634/mathheadinclouds}{mathheadinclouds}, 
#'   \href{http://stackoverflow.com/users/1737569/dis-shishkov}{Dis Shishkov}
#' @references \url{http://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r}
#' @examples
#'  chunk2(1:30, 6)

chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 

# Schedule for increasing the weight.
schedule <- function(i, n, theta_min = 0.1, theta_max = 0.995, theta_pow = 0.5) {
  clamp((i / n)^theta_pow, theta_min, theta_max)
}

# Copied from the curry package. See:
# https://github.com/thomasp85/curry/
arg_getter <- function() {
  fun <- sys.function(sys.parent(1))
  arg_env <- attr(fun, "arg_env")
  parent <- parent.frame()
  vals <- as.list(parent)
  if (!is.null(formals(fun)[["..."]])) {
    vals <- c(vals, eval(quote(list(...)), parent))
  }
  c(arg_env$args, vals, arg_env$args_end)
}
