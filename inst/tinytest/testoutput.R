
  require(checkmate)
  using(checkmate)

   dir = system.file("tinytest", package = "replacer")

   outList = replaceVals(dir, save = FALSE)

   out = replaceVals(dir, save = FALSE)[[1]]

  x = data.table::fread('data.csv', na.strings = c(NA_character_, ''))

  checkDataTable(out)
  checkList(outList, len = 3L)
  checkSetEqual(dim(x), dim(out))
