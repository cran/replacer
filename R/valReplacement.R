#'User-intended wrapper for single-file replacements
#'
#'@description The function sends the prepared data.tables to [sReplace][sReplace()], receives updated data, displays a
#'  list of updated data and of counts of multiple replacements and saves updated data to disk (see Details).
#'
#'@keywords datasets data manip misc
#'
#'@import data.table
#'
#'@template dir-arg
#'
#'@param ... Not used when file names are "data.csv", "lookup.csv". Otherwise, custom names including file extension,
#'  within quotation marks, such as "<data_name>.csv", "<lookup_name>.csv", **entered in this order!**.
#'@template save-arg
#'
#'@return A named list containing updated data and multiple replacement counts. Also, a csv file saved in the
#'  same directory, under the name updated_<data_name>_using_<lookup_name>.csv.
#'
#'@template details-replaceVals-template
#'
#'@template Note-replaceVals-template
#'
#'@seealso [bReplace][bReplace()], [sReplace][sReplace()]
#'
#'@examples
#'
#' ## Not run: datasets with default names "data.csv", "lookup.csv" located in *dir*
#'
#' if (interactive()) {
#'  dir = system.file("extdata", package = "replacer")
#'  replaceVals(dir, save = FALSE)
#'
#'   ## no messages (not recommended!)
#'  suppressMessages(replaceVals(dir, save = FALSE))
#'  }
#'
#'@export
#'

 replaceVals = function(dir, ..., save = TRUE) {
      id = "id" = NULL # due to NSE in R CMD check
   oldwd = getwd(); on.exit(setwd(oldwd), add = TRUE)
 if (length(dir) == 1L) {
 # to avoid the "cannot change dir" error:
    dir = Reduce(
      file.path
      , strsplit(dir, split = '/|\\\\', fixed = TRUE)[[1L]]
      , right = TRUE
      , accumulate = TRUE
    )[[1L]]
  }
 if (!getwd() %in% dir) {
    setwd(dir)
  }
 message('\nreading data from: ', dir, '...\n')
 default = list('data.csv', 'lookup.csv')
   input = list(...)
      ll = if (length(input)) {
          lapply(input, fread, na.strings = c(NA_character_, ""), encoding = 'UTF-8')
          } else {
           lapply(default, fread, na.strings = c(NA_character_, ""), encoding = 'UTF-8')
    }
 # check and enforce "data.table" class if necessary
 if (!is.null(ll)) {
    noDT = sapply(ll, Negate(is.data.table))
   if (any(noDT)) {
 cat('coercing to "data.table" class ...\n')
 ll[noDT] = lapply(ll[noDT], as.data.table, na.rm = FALSE)
             }
         } else {
 cat('data could not be read from source!\n')
  }
  x0 = ll[[1L]]
  y  = ll[[2L]]
 message('data reading complete ...\n')
 # check lookup for standard column names
 stdyNames = c('vars', 'oldVals', 'newVals', 'id')
 if (length(setdiff(names(y), stdyNames)) >= 0L) {
 message('checking standard columns in lookup ...\n')
  y0 = y[, .SD, .SDcols = names(y) %chin% stdyNames]
 rm(y)
 if (any(!stdyNames[1L:3L] %chin% names(y0))) {
 stop('found less than minimum required standard columns in lookup!\n')
          }
    }
 if (any(is.na(y0$vars))) {
 stop('found missing entries in lookup "vars"! ...\n')
    } else {y0}
       uv = unique(y0$vars)
     n.uv = length(uv)
 colOrder = copy(names(x0))
 stopifnot('found too many involved columns in lookup' = length(x0) >= n.uv)
    x0$id = seq.int(nrow(x0))
 if (length(x0) > n.uv + 1) {
 message('removing uninvolved columns from data ...\n')
        x = x0[, .SD, .SDcols = c('id', uv)]
       xr = x0[, .SD, .SDcols = !names(x0) %in% uv]
       } else {
      x <- x0
     xr <- NULL
 }
 message('starting replacements ...\n')
 results = sReplace(x, y0, uv)
     rez = results[[1L]]
 if (length(xr)) {
 message('rejoining uninvolved columns in data ...\n')
      xx = rez[xr, on = 'id'][, id := NULL]
       } else {
      xx = rez[, id := NULL]
  }
 setcolorder(xx, neworder = colOrder)
 whichFiles = if (length(input)) {input} else {default}
 whichNames = lapply(
                  whichFiles, function(i) gsub('.csv', '', i, fixed = TRUE)
                )
 # write updated data to dir
 if (save && !identical(xx, x) && nrow(xx)) {
   fwrite(xx, paste0('updated_', whichNames[[1L]]
                  , '_using_', whichNames[[2L]], '.csv'))
 message('replacements completed per lookup\n\nupdated data saved to: ', dir, '\n')
            } else if (identical(xx, x) && length(xr)) {
 cat('please check displayed results, updates may not be complete!\n')
            }
 # collect results for console
 outList = list(xx[], results[[2L]], results[[3L]])
 names(outList) = c(paste0(names(results)[1L], whichNames[[1L]]
                           , '_using_', whichNames[[2L]])
                    , names(results)[-1L])
 print(outList, class = TRUE)
}

#' Helper function for value replacement
#'
#' @description
#' The function is not intended for direct use. Once called by [replaceVals][replaceVals()]
#' it firstly checks for index presence in lookup. Upon the result of this check, the
#' function moves along the branches of a decision tree (see Details).
#'
#'@import data.table
#'
#' @param x,y0 Data.tables
#' @param uv Character vector or list of same length as x, containing unique names of
#'   involved columns in data.
#'
#' @return
#' A named list containing updated involved columns in x, count of multiple replacements
#' of duplicated values (if requested), count of multiple replacements of missing values
#' (if requested).
#'
#' @template details-sReplace-template
#'
#' @seealso [dcast][data.table::dcast()], [fcoalesce][data.table::fcoalesce()],
#' [merge][data.table::merge()], [set][data.table::set()]
#'

sReplace = function(x, y0, uv) {
  id = "id" = newVals = oldVals = J = NULL # due to NSE notes in R CMD check
        GEN = '^MIS|^mis|^PRE|^pre|^UN|^un|^YES|^yes|^NO|^no|^999'
      ncolX = copy(names(x))
 if (!exists('id', y0)) {
      dupsX = whichDups(x[, uv, with = FALSE])
       nasX = x[, colSums(is.na(.SD))]
        uNA = names(nasX[nasX == 1]) # unique NA, cross-column split
       vdup = y0[which(y0$oldVals %in% dupsX)]
        nas = y0[newVals %like% GEN]
       unas = intersect(uNA, y0[is.na(oldVals) & !newVals %like% GEN]$vars)
     n.dups = nrow(vdup)
      n.nas = nrow(nas)
   if (exists('nas') && nrow(nas)) {
      vna = dcast(nas, oldVals ~ vars, value.var = 'newVals'
                 )[, oldVals := NULL
                 ][, type.convert(.SD, as.is = TRUE)]
        }
 if (exists('vdup') && n.dups + n.nas < nrow(y0)) {
 message('NOTE: found duplicates and simple replacements but no index in lookup,\nwould recommend User-made index\nproceeding any way ... \n')
 message('subsetting lookup and creating index ...\n')
      y = y0[!nas, on = 'newVals'][!vdup, on = c('oldVals', 'newVals')]
 stopifnot("splits on duplicated values not implemented when missing id!"=!y$oldVals %in% vdup$oldVals)
  nunas = setdiff(y$vars, unas)
 if (length(unas)) {
   for (u in c(unas, nunas)) {
   y[u, id := unlist(sapply(x[, uv, with = FALSE][, u, with = FALSE]
                        , function(i) which(i %in% y[u, on = 'vars']$oldVals))), on = 'vars']
          }
   } else {
   y[, id := unlist(sapply(x[, uv, with = FALSE], function(i) which(i %in% y$oldVals)))]
  }
 rm(y0)
 if (nrow(y)) {
       yc = dcast(y, id ~ vars, value.var = 'newVals'
                 )[, lapply(.SD, function(i) type.convert(as.character(i)
                                                           , as.is = TRUE
                                                           , numerals = 'no.loss'))]
       }
 if (exists('yc') && any(is.na(yc$id))) {
      vna = yc[is.na(id)][, id := NULL]
          }
      }
  } else if (exists('id', y0)) {
 message('found index in lookup, proceeding ...\n')
           if (any(y0$id == 0L)) {
           vdup = y0[J(0L), on = 'id'][, id := NULL]
              }
 y = y0[!J(0L), on = 'id']
 rm(y0)
 if (exists('y') && nrow(y)) {
   yc = dcast(y, id ~ vars, value.var = 'newVals'
             )[, type.convert(.SD, as.is = TRUE, numerals = 'no.loss')]
    if (any(is.na(yc$id))) {
       vna = yc[is.na(id)][, id := NULL]
      nvna = names(vna)
        }
     }
  }
  # separate columns in xx present in yc that may belong to vdup only
  if (exists('yc') && !is.null(yc)) {
     nyc = names(yc)
    n.yc = length(yc)
     uuv = intersect(nyc, uv)
    if (length(uuv)) {
     uvr = setdiff(names(x), nyc)
     xyc = x[, .SD, .SDcols = nyc]
      xm = yc[!is.na(id)][xyc, on = 'id']
     nxm = names(xm)
      xx = xm[, .SD, .SDcols = nyc]
    }
  } else {
     xx <- x
  }
 # process 1:1 replacements
 if (exists('uuv') && !is.null(uuv)) {
 message('found request for 1:1 replacements: proceeding ...\n')
    for (n in seq_along(uuv)) {
      set(xx, NULL, n + 1L, con2fcoales(xx[[uuv[n]]], xm[[nxm[n.yc + n]]]))
      }
 message('completed 1:1 replacements ...\n')
  } else {
 message('found no request for 1:1 replacements ...\n')
  }
  if (exists('xm')) {
 rm(xm)
  }
 # return original columns and restore original column order
 if (exists('uvr') && length(uvr)) {
 message('rejoining columns uninvolved in simple replacements ...\n')
   xrr = x[, .SD, .SDcols = c('id', uvr)]
    xx = xx[xrr, on = 'id']
 setcolorder(xx, neworder = ncolX)
 # check match to original x
 if (any(!names(x) %chin% names(xx)))
 warning('internal: column names in the restricted data.table "xx" do not match original "x"!\n')
 rm(xrr)
  } else if (exists('xx')) {
      xx
    } else {
  xx <- x
  }
 # make a copy of the original NA totals
 naCount = copy(xx[, colSums(is.na(.SD)), .SDcols = !'id'])
 # process 1:many replacements
 message('searching for 1:many replacements ...\n')
 # multiple duplicated value replacement per lookup
 if (exists('vdup') && nrow(vdup)) {
 message('found request for multiple duplicated value replacments: creating index ...\n')
 dupuv = unique(vdup$vars)
 oldup = vdup$oldVals
  # create index for duplicated values
 idup = list()
 for(m in dupuv) {
     for (r in vdup[J(m), on = 'vars']$oldVals) {
              idup[[m]][[r]] = which(xx[[m]] %in% r)
       }
  }
 message('replacing multiple duplicated values ...\n')
 for (m in dupuv) {
    for (n in vdup[m, on = 'vars']$oldVals) {
       set(xx, idup[[m]][[n]]
           , m
           , vdup[J(m,n), type.convert(newVals, as.is = TRUE, numeric = 'no.loss')
                        , on = c('vars', 'oldVals')])
           }
      }
    nn.idup = unlist(sapply(idup, lengths))
 withCounts = vdup[, 'Counts' := nn.idup]
   dupCount = dcast(withCounts, vars + ... ~ vars, value.var = 'Counts')
  } else {
 message('found no request for multiple duplicated values replacements ...\n')
    xx
  }
# multiple NAs replacement as per lookup
 if (exists('vna') && nrow(vna)) {
    nvna = names(vna)
 message('found request for multiple missing value replacements: replacing ...\n')
   for (i in nvna) {
            set(xx, NULL, i, con2fcoales(xx[[i]], vna[[i]]))
       }
  } else {
 message('found no request for multiple missing values replacements ...\n')
    xx
   }
 if (exists('idup')) {
 rm(idup)
   }
 # determine if NAs remain after multiple replacements processed
 stillHasNA = xx[, colSums(is.na(.SD)), .SDcols = !'id']
 if (any(stillHasNA != 0L) && !identical(naCount, stillHasNA)) {
 message('processed 1:many replacements ...\n')
   }
 # collect results related to lookup in a named list
 results = list(xx
                 , if (exists('dupCount')) {
                    dupCount
                   }
                 , if (all(stillHasNA == 0L)) {
                    naCount
                   } else if (any(stillHasNA != 0L) && !identical(naCount, stillHasNA)) {
 cat('NOTE: there are still missing values in some involved columns!\n')
                    stillHasNA
                   } else if (identical(naCount, stillHasNA)) {
                    NULL
                    })
 if (length(results) == 3L) message('\nhelper function has completed!\n')
 names(results) = c(' updated_'
                   , if (exists('dupCount')) {
                    ' multiple_dups_repl_counts'
                     } else {
                    ' no_multiple_dups_repl_requested'
                       }
                   , if (all(stillHasNA == 0L)) {
                    ' multiple_NAs_repl_counts'
                     } else if (any(stillHasNA != 0L) && !identical(naCount, stillHasNA)) {
                    ' NAs_remaining'
                       } else if (identical(naCount, stillHasNA)) {
                    ' no_multiple_NAs_repl_rquested'
                     })
 return(results)
 }
