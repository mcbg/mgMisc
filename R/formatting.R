#' @export
fm_to_str <- \(fm) Reduce(paste, deparse(fm))

#' @importFrom data.table fifelse
#' @export
format_pvalue <- function(x)  fifelse(x < 0.001, 'P < 0.001', format(round(x, 3), nsmall=3) |> paste0('P = ', .))

#' @export
format_number <- \(x, k) round(x, digits = k) |> format(nsmall = k)

#' @export
format_tb <- \(tb) {
  ii <- seq_along(tb)

  ll <- lapply(ii, \(i) {
    nm <- names(tb)[i]
    col <- as.character(tb[[i]])
    n <- nchar(c(nm, col)) |> max()
    format(c(nm, '', col), width = n, justify = 'centre')
  })

  lns <- do.call('paste', c(ll, sep='   '))
  lns[2] <- rep('-', nchar(lns[1])) |> paste0(collapse = '')
  lns
}
