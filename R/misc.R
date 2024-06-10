
#' @export
gfilter = function(x, expr, perl=TRUE, ignore.case=TRUE, ...) {
  x[grep(expr, x, perl=perl, ignore.case = ignore.case, ...)]
}

#' @export
count_u <- . |> unique() |> NROW()

#' @importFrom data.table data.table
#' @export
gls <- \(regex = '') {
  v <- ls(envir = parent.env(environment()))
  data.table(obj = v, class = sapply(v, \(x) class(get(x))))[
    grepl(regex, class, perl = TRUE)
  ]
}

