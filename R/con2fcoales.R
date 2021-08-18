#' Helper for coalescing vectors of different types
#'
#'@description
#'
#' This helper prevents the error in [fcoalesce][data.table::fcoalesce()]
#' when attempting to coalesce two vectors of different data type (double/integer).
#'
#'@param u,z Vectors of equal length and of different data types (e.g. double and integer).
#'   Missing values are accepted.
#'
#'@return
#'
#' A double data type vector of same length as the arguments.
#'

 con2fcoales = function(u, z) {
         if((is.double(u) && is.integer(z)) || (is.double(z) && is.integer(u))) {
           z = as.double(z); u = as.double(u)
                 } else {u; z}
  fcoalesce(u, z)
 }
