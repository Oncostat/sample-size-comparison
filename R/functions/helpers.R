# closest <- function(a, x, na.rm = FALSE){
#   FUN <- function(a, x, na.rm = FALSE){
#     if(na.rm){ x <- x[!is.na(x)]}
#     mdist <- min(abs(x-a))
#     if(is.na(mdist)){res <- NA} else {res <- x[(abs(x-a) - mdist) == 0]}
#     return(res)
#     }
#   res <- mapply(FUN=FUN, a = a, MoreArgs = list(x=x, na.rm=na.rm))
#   return(res)
# }
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
    \(val, n_val) rename_with(val, ~ paste0(., "_", n_val), any_of(columns))
  )
}
