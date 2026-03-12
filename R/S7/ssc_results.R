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
      method = c(
        "rpact",
        "nquery",
        "east",
        "rashnu",
        "gsdesign2",
        "bbssr",
        "oa2s",
        "sssas",
        "ahern",
        "combined"
      )
    ) {
      method <- arg_match(method)
      new_object(
        S7_object(),
        tbl = tbl,
        design = design,
        method = method
      )
    },
    # VALIDATOR
    validator = function(self) {
      if (nrow(self@tbl) > nrow(self@design@params$table)) {
        "@tbl has to much rows according to the parameters."
      }
      params_names <- names(self@design@params$list)
      cond_duplicate <-
        self@tbl |>
        select(all_of(params_names)) |>
        duplicated() |>
        any()
      if (cond_duplicate) {
        "@tbl should not contains any duplicate of input combinaisons."
      }
      cond_in_params <-
        self@tbl |>
        anti_join(self@design@params$table, by = params_names) |>
        nrow()
      if (cond_in_params > 0) {
        "@tbl should not contain an input combinaison not in @params$table."
      }
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
