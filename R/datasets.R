#' @export
rnd_select <- function(ds, v, k, seed = 123) {
  # checks
  stopifnot('data.table' %in% class(ds))

  # processing
  set.seed(seed)
  selection <- ds[, unique(get(v)) |> sort() |> sample(k)]
  ds[get(v) %in% selection]
}
