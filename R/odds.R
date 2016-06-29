#' Convert fractional odds to decimal odds.
#'
#' @param fractional A character vector of fractional odds.
#' @return A numeric vector of decimal odds.
#' @examples
#' to.decimal(c("2/1", "5/3", "1/4"))
#' @export
to.decimal <- function(fractional) {
  sapply(fractional, function(ratio) eval(parse(text = ratio)), USE.NAMES = FALSE) + 1
}

#' Convert decimal odds to fractional odds.
#'
#' @param decimal A numeric vector of decimal odds.
#' @return A character vector of fractional odds.
#' @examples
#' to.fractional(c(3, 2.5, 1.25))
#' @export
to.fractional <- function(decimal, ...) {
  sub("^([[:digit:]]*)$", "\\1/1", as.character(MASS::fractions(decimal - 1, ...)))
}

implied.probability <- function(odds) {
  if(class(odds) == "character") odds = to.decimal(odds)
  1 / odds
}