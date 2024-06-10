#' @importFrom ggplot2 qplot
#' @export
gcurve <- function(expr) {
  x <- seq(-3, 3, 0.1)

  name <- deparse(substitute(expr))
  y <- eval(expr)
  qplot(x, y, geom = 'line', colour = name) + theme_bw()
}
