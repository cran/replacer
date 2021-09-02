## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  root.dir = "C:/Users/Dragos/Desktop/projects",
  collapse = TRUE,
  comment = "#>", 
  out.width = "100%",
  tidy.opts = list(width.cutoff = 120),
  tidy = TRUE,
  if (any(!requireNamespace("data.table", quietly = TRUE))) {
  knitr::opts_chunk$set(eval = FALSE)
}
 )
pks = c('data.table', 'knitr', 'kableExtra', 'replacer')
if(!any(pks %in% search())) {
invisible(lapply(pks, require, character.only = TRUE))
}

## ----echo = FALSE, message = FALSE--------------------------------------------
ll = lapply(c('data_id_vig.csv', 'lookup_id_vig.csv', 'outData.csv'), fread)
    dat = ll[[1]]; orderDat = names(copy(dat))
     lk = ll[[2]]; orderLk = names(copy(lk))
   odat = ll[[3]]; orderOdat = names(copy(odat))
 dat$Rw = seq_len(nrow(dat)); setcolorder(dat, neworder = c('Rw', orderDat))
  lk$Rw = seq_len(nrow(lk)); setcolorder(lk, neworder = c('Rw', orderLk))
odat$Rw = seq_len(nrow(odat)); setcolorder(odat, neworder = c('Rw', orderOdat))

## ----tables, echo = FALSE, message = FALSE------------------------------------
options(knitr.kable.NA = "")
kable_styling(kable(dat, format = 'html', caption = '*In-Data*', escape = TRUE, digits = 1),
               'bordered', full_width = FALSE, position = 'float_left', font_size = 10)
kable_styling(kable(lk, format = 'html', caption = 'Lookup', escape = TRUE, digits = 1), 
               'bordered', full_width = FALSE, position = 'float_left', font_size = 10)
kable_styling(kable(odat, format = 'html', caption = '*Out-Data*', escape = TRUE, digits = 1), 
               'bordered', full_width = FALSE, position = 'float_left', font_size = 10)

## ----dups_in_examples, include=FALSE, warning=FALSE---------------------------
 dir = system.file('extdata', package = 'replacer')
 inData = list.files(dir)
 inData = inData[grep('.csv', inData)]
 inData = inData[-grep('chile_id|chile_nadup|lookup', inData)]
 lData = lapply(inData, function(i) paste0(dir,'/', i))
 data = lapply(lData, fread, na.strings = c(NA_character_, ''))
 n.chile = data$chile[, lapply(.SD, function(i) {sum(duplicated(i))/length(i)})]
 names(data) = unlist(strsplit(inData, split = '.csv', fixed = TRUE))
 ldup = lapply(data, function(i) whichDups(i)[length(i) > 0])
 ldup$chile = sample(ldup$chile, size = 4, prob = n.chile)
 nams = names(ldup) = names(data)
 # Tables
 DUPS = lapply(nams, function(n) {kable(t(ldup[[n]])
                                        , format = 'html'
                                        , align='c'
                                        , caption = 'Duplicated')
        })
 NAS = lapply(nams, function(n) {kable(t(data[[n]][, colSums(is.na(.SD))])
                                        , format = 'html'
                                        , align='c'
                                        , caption = 'Missing')
       })
 names(DUPS) = paste0(nams, 'DUP')
  names(NAS) = paste0(nams, 'NA')

## ----echo = FALSE, results = 'asis'-------------------------------------------
 cat(c('<table><tr valign="top"><td>', DUPS$data_idDUP , '</td><td>', NAS$data_idNA, '</td><tr></table>'), sep = '')

## ----echo = FALSE, results = 'asis'-------------------------------------------
 cat(c('<table><tr valign="top"><td>', DUPS$chileDUP , '</td><td>', NAS$chileNA, '</td><tr></table>'), sep = '')

## ----echo = FALSE, results = 'asis'-------------------------------------------
 cat(c('<table><tr valign="top"><td>', DUPS$dataDUP , '</td><td>', NAS$dataNA, '</td><tr></table>'), sep = '')

## ----echo = FALSE, results = 'asis'-------------------------------------------
cat(c('<table><tr valign="top"><td>', DUPS$data_uniqueDUP , '</td><td>', NAS$data_uniqueNA, '</td><tr></table>'), sep = '')

## ----sessioninfo, echo = FALSE------------------------------------------------
 sessionInfo()

## ----citation, include = FALSE------------------------------------------------
knitr::write_bib(file = 'citation.bib')

