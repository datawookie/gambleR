#' Convert fractional odds to decimal odds.
#'
#' @details Decimal odds are one more than the numerical value of the fractional odds.
#' 
#' The decimal odds reflect the gross return expected per unit wager. For example, a wager placed on decimal odds of
#' 1.5 (equivalent to fractional odds of 1/2) stands to receive a net win of 50%.
#' @param fractional A character vector of fractional odds.
#' @return A numeric vector of decimal odds.
#' @references \url{http://www.bettingexpert.com/how-to/convert-odds}
#' @examples
#' to.decimal(c("2/1", "5/3", "1/4"))
#' @export
to.decimal <- function(fractional) {
  if (inherits(fractional, "data.frame")) {
    return(data.frame(sapply(fractional, to.decimal), row.names = rownames(fractional)) %>% setNames(names(fractional)) %>% as.matrix)
  }
  sapply(ifelse(fractional == "", NA, fractional), function(ratio) {
    eval(parse(text = ratio))
  }, USE.NAMES = FALSE) + 1
}

#' Convert decimal odds to fractional odds.
#'
#' @details Fractional odds have a numerical value one less than the decimal odds.
#' 
#' The decimal odds reflect the net return expected per unit wager. For example, a wager placed on fractional odds of
#' 2/1 (equivalent to fractional odds of 3.0) stands to receive a net win of 200%.
#' @param decimal A numeric vector of decimal odds.
#' @return A character vector of fractional odds.
#' @examples
#' to.fractional(c(3, 2.5, 1.25))
#' @export
to.fractional <- function(decimal, ...) {
  sub("^([[:digit:]]*)$", "\\1/1", as.character(MASS::fractions(decimal - 1, ...)))
}

#' Calculate implied probability from odds.
#' 
#' @details Implied probability is the reciprocal of the decimal odds.
#' 
#' Decimcal odds of 2.0 are equivalent to an implied probability of 50%.
#' @param odds A vector of either fractional or decimal odds.
#' @examples
#' implied.probability("2/1")
#' @export
implied.probability <- function(odds) {
  if(class(odds) == "character") odds = to.decimal(odds)
  1 / odds
}