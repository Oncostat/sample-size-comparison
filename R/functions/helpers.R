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
closest <- function(a, x){return(x[unlist(imap(a, ~ which.min(abs(. - x))))])}
