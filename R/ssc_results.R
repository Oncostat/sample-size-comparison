# CLASS DEFINITON
ssc_results <-
  new_class(
    "ssc_results",
    # PROPERTIES
    properties = list(
      tbl = class_list, # result table
      design = ssc_design, # design
      method = class_character # rpact, nQuery ...
    ),
    # CONSTUCTOR
    constructor = function(
      tbl,
      design,
      method = c("rpact", "nquery", "east", "rashnu", "gsdesign2", "combined")
    ) {
      method <- arg_match(method)
      new_object(
        S7_object(),
        tbl = tbl,
        design = design,
        method = method
      )
    }
  )

# METHODS
method(get_tbl, ssc_results) <- function(x) x@tbl
method(get_design, ssc_results) <- function(x) x@design
method(get_params, ssc_results) <- function(x) {
  x |> get_design() |> get_params()
}
method(show_params, ssc_results) <- function(x) {
  x |> get_design() |> show_params()
}
method(get_endpoint, ssc_results) <- function(x) {
  x |> get_design() |> get_endpoint()
}


method(print, ssc_results) <- function(x) {
  design <- get_design(x)
  cli_h2("SSC {x@method} results for {design@endpoint} {design@type} design")
  print(x@tbl)
}
method(summary, ssc_results) <- function(x) {
  design <- get_design(x)
  cli_h2("SSC {x@method} results for {design@endpoint} {design@type} design")
  summary(x@tbl)
}
