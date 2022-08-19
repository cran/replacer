#' Batch-file value replacement
#'
#' @description
#'
#' User-intended function to process a list of pairs of data files and associated lookup files listed in this order.
#'
#' @template dir-arg
#'
#' @param x List of character vectors each of length 2 containing full names of the data file and the associated
#'   lookup file, as described in [replaceVals][replaceVals()].
#' @param save Logical, default TRUE: save results to directory; FALSE: display only.
#' @param msgs Logical, default FALSE: suppress messages. TRUE: print a named list containing messages specific to each run.
#'
#' @return
#'
#' A named list displaying updated data and multiple replacement count tables. Also, updated csv files which are saved
#' to *dir*.
#'
#' @template Note-replaceVals-template
#'
#' @seealso [replaceVals][replaceVals()]
#'
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#' # A list of data/lookup names:
#' fs = list(c('data.csv', 'lookup.csv')
#'         , c("data_unique.csv", "lookup_unique.csv")
#'         , c('data_id.csv', 'lookupNA.csv')
#'         , c('data_id.csv', 'lookupDUP.csv')
#'         , c('chile.csv', 'chile_nadup.csv')
#'         , c('data_id.csv', 'lookup_id.csv')
#'         , c('data_id.csv', 'lookup_idsimple.csv')
#'         , c('chile.csv', 'chile_id.csv')
#'
#'       )
#'  ##Not run:
#'  dir = system.file("extdata", package = "replacer")
#'  bReplace(dir, fs, save = FALSE, msgs = TRUE)
#'  }
#'

bReplace = function(dir, x, save = TRUE, msgs = FALSE) {
  if (any(lengths(x) != 2L)) {
  stop('\nsome file pairs have improper length\n')
   }
  if (!exists('suppReplace', mode = 'function')) {
    suppReplace = function(dir, ...) {
          if (msgs) {
            replaceVals(dir, ..., save = save)
            } else {
              suppressMessages(replaceVals(dir, ..., save = save))
         }
       }
     }
  cat('\nstarting batch file replacements from', dir, '...\n\n')
  ll = lapply(x, function(i) {
              cat('\nupdating ', i[1L], 'data using', i[2L], 'lookup\n')
              try(suppReplace(dir, i[[1L]], i[[2L]]))
                 }
             )
  if(identical(length(ll), length(x))) cat('\nbatch completed!\n')
  if (save) {
   cat('output saved to: ', dir, '\n')
     }
   return(ll)
}
