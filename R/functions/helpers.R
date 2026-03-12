#' Replace values to the closest in a given vector
#'
#' @param a The vector of numericals to approx
#' @param x The vector of values used for approximation
#'
#' @examples
#' closest(c(1.2, 1.7, 2.1), 1:2)
closest <- function(a, x) {
  return(x[unlist(imap(a, ~ which.min(abs(. - x))))])
}


#' Add dataframe names as a suffix for selected columns
#'
#' @param list A named list of dataframe
#' @param columns A vector of column names that will receive a suffix
#'
#' @returns A named list with new names for selected columns
add_name_as_suffix <- function(list, columns) {
  new_list <- map2(
    list,
    names(list),
    \(val, num_val) rename_with(val, ~ paste0(., "_", num_val), any_of(columns))
  )
}
