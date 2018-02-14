library(rvest)
library(dplyr)
library(xtable)
library(stringr)
library(reshape2)

# ALL HORSE RACES -----------------------------------------------------------------------------------------------------

# html <- read_html("http://www.oddschecker.com/horse-racing")
# 
# # Grab links to today's races.
# #
# race.times <- html %>% html_nodes(css = "a.race-time") %>% html_attr("data-time")
# #
# races.today <- grepl(Sys.Date(), race.times)
# #
# race.links <- html %>% html_nodes(css = "a.race-time") %>% html_attr("href") %>% .[races.today]

# ---------------------------------------------------------------------------------------------------------------------

#' Scrape odds off http://www.oddschecker.com/.
#' 
#' @param event Event specification.
#' @return Table of odds quoted by various bookmakers for each of the contenders.
#' @import stringr rvest xml2
#' @examples
#' oddschecker("politics/us-politics/us-presidential-election-2016/winner")
#' oddschecker("horse-racing/carlisle/18:05/winner")
#' @export
oddschecker <- function(event) {
  URL = paste0("http://www.oddschecker.com/", event)
  
  html <- xml2::read_html(URL)
  
  title <- html %>% rvest::html_nodes("h1") %>% html_text()
  #
  location = str_trim(str_extract(title, "^[^[:digit:]]*"))
  time = str_trim(str_extract(title, "[:digit:][:digit:]:[:digit:][:digit:]"))
  #
  name = html %>% html_nodes(xpath = '//*[@id="betting-odds"]/section[1]/div/div/div/div/p') %>% html_text %>% str_trim
  
  # table <- html %>% html_nodes("table") %>% .[[1]]
  
  #oddsTableContainer > table > thead > tr.eventTableHeader
  
  bookmakers = sapply(html %>% html_nodes("table tr.eventTableHeader td"), function(td) {
    title = html_nodes(td, css = ".bk-logo-click") %>% html_attr("title")
    ifelse(length(title) == 0, "", title)
  })
  #
  # Find contender column.
  #
  contender <- min(which(bookmakers != "")) - 1
  
  odds <- html %>% html_nodes("table.eventTable") %>% .[[1]] %>% html_table()
  
  # Retain only rows with data.
  #
  odds <- odds[-(1:which(odds[, contender] == "")[1]),]
  
  # Remove blank row (seems to appear intermittently as last row).
  #
  odds <- odds[!apply(odds, 1, function(d) all(d == "" | is.na(d))),]
  
  # Name rows.
  #
  rownames(odds)<- odds[, contender]
  
  # Rename columns.
  #
  names(odds) = bookmakers
  odds = odds[, -seq(1, contender)]
  
  # Remove empty/useless columns.
  #
  odds = odds[, sapply(odds, function(column) {any(column != "") && !all(is.na(column))})]
  
  # - Change unit odds: "20" becomes "20/1".
  # - "SP" is starting price. Bookmaker will use price quoted on track as start of race.
  #
  for (n in 1:ncol(odds)) {
    odds[,n] = gsub("^([[:digit:]]+)$", "\\1/1", gsub("^SP$", "", odds[,n]))
  }
  
  odds
}

# print(xtable(odds[, 1:9]), type = "html", html.table.attributes = "")

# ---------------------------------------------------------------------------------------------------------------------

# odds$Location = location
# odds$Time = time
# odds$Name = name
# #
# # Put back horse name for melting.
# #
# odds$Horse = rownames(odds)
# 
# odds.long <- melt(odds, id.vars = c("Name", "Location", "Time", "Horse"), variable.name = "Bookmaker", value.name = "Odds")
