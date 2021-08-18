#' Find duplicated values in data
#'
#' @description
#' The function finds duplicated values in each column of the data
#' file. Although not intended for direct use, it can be applied to a data file
#' once converted into "data.table" class.
#'
#' @param x A data.table.
#'
#' @return A named character vector. Data columns containing distinct sets of duplicated
#' values have the names indexed.
#' @importFrom stats na.omit
#' @importFrom utils type.convert
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'
#'   dir = system.file('extdata', package = 'replacer')
#'   setwd(dir)
#'
#'   x = data.table::fread('data.csv', na.strings = c(NA_character_, ''))
#'
#'   whichDups(x)
#'
#' }
#'

 whichDups = function(x) {
              fcall = function(f, ...) f(...)
                  g = function(i) {`[`(i, duplicated(i))}
               funs = list(unique, na.omit, g)
             dupFun = function(i) {
                try(Reduce(fcall, funs, init = i, right = TRUE)[length(i) > 0])
                     }
               outs = Map(dupFun, x)
               outs = unlist(outs[!is.na(outs)])
 return(outs)
}
