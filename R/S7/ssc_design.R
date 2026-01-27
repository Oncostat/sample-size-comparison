# CLASS DEFINITON
ssc_design <-
  new_class(
    "ssc_design",
    # PROPERTIES
    properties = list(
      endpoint = class_character, # survival or binary
      type = class_character, # fixed or gs
      params = class_list, # grid
      computation = new_property(class = class_character, default = NULL)
    ),

    # CONSTUCTOR
    constructor = function(
      endpoint = c("binary", "survival"),
      type = c("fixed", "gs"),
      params,
      computation = character(0)
    ) {
      endpoint <- arg_match(endpoint)
      type <- arg_match(type)
      new_object(
        S7_object(),
        endpoint = endpoint,
        type = type,
        params = params,
        computation = computation
      )
    },

    # VALIDATOR
    validator = function(self) {
      if (self@endpoint == "binary") {
        if (
          length(self@computation) == 0 ||
            !(self@computation %in% c("exact", "pooled", "unpooled"))
        ) {
          "@computation should be one of exact, pooled or unpooled."
        }
      } else if (length(self@computation) != 0) {
        # NOT WORKING, DON'T KNOW WHY
        "@computation should be NULL for survival endpoint"
      }
      if (length(self@params$list) == 0 | length(self@params$table) == 0) {
        "@params should contains at least $list and $table."
      }
    }
  )

# METHODS
method(get_params, ssc_design) <- function(x) x@params
method(get_endpoint, ssc_design) <- function(x) x@endpoint
method(get_type, ssc_design) <- function(x) x@type
method(get_computation, ssc_design) <- function(x) x@computation

method(show_params, ssc_design) <- function(x) {
  params <- x@params
  params_list <- map(
    params$list,
    ~ str_flatten(., collapse = ", ", last = " and ")
  )
  cat("Moving parameters:\n")
  cli_dl(params_list)
  cat("\nAdditional parameters:\n")
  cli_dl(params$additional)
  cat("\nNumber of parameters combinaisons:\n")
  print(nrow(params$table))
}

method(print, ssc_design) <- function(x) {
  cli_h2("SSC {x@endpoint} {x@type} design")
  if (length(x@computation) != 0) {
    cli_alert_info("{.var {x@computation}} computation.")
  }
  cli_par()
  show_params(x)
}
