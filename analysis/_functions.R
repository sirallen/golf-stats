sortFreq = function(x) {
  names(sort(table(x), decr=TRUE))
}

week_to_date = function(wk) {
  # convert character string yyyy_ww to a Date
  year = as.Date(paste0(substr(wk, 1, 4), '-01-01'))
  weekNum = as.numeric(substr(wk, 6, 7))
  year + 7*weekNum
}

abbreviateNames = function(nameVec) {
  # "Dustin Johnson" --> "D. Johnson"
  # "K. J. Choi" --> "K. J. Choi"
  sub('^([A-Z])\\w+\\b', '\\1.', nameVec, perl=T)
}

