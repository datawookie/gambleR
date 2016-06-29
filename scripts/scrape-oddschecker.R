library(rvest)
library(dplyr)
library(xtable)
library(stringr)
library(reshape2)

# ALL HORSE RACES -----------------------------------------------------------------------------------------------------

html <- read_html("http://www.oddschecker.com/horse-racing")

# Grab links to today's races.
#
race.times <- html %>% html_nodes(css = "a.race-time") %>% html_attr("data-time")
#
races.today <- grepl(Sys.Date(), race.times)
#
race.links <- html %>% html_nodes(css = "a.race-time") %>% html_attr("href") %>% .[races.today]

# ---------------------------------------------------------------------------------------------------------------------

# URL = "http://www.oddschecker.com/horse-racing/stratford/18:20/winner"
URL = "http://www.oddschecker.com/horse-racing/stratford/19:50/winner"
# URL = "http://www.oddschecker.com/horse-racing/hamilton/14:00/winner"
# URL = "http://www.oddschecker.com/horse-racing/hamilton/15:00/winner"

html <- read_html(URL)

title <- html %>% html_nodes("h1") %>% html_text()
#
location = str_trim(str_extract(title, "^[^[:digit:]]*"))
time = str_trim(str_extract(title, "[:digit:][:digit:]:[:digit:][:digit:]"))
#
name = html %>% html_nodes(xpath = '//*[@id="betting-odds"]/section[1]/div/div/div/div/p') %>% html_text %>% str_trim

table <- html %>% html_nodes("table") %>% .[[1]]

bookmakers =  table %>% html_nodes(css = ".bk-logo-click") %>% html_attr("title")
#
NBOOKS = length(bookmakers)

odds <- table %>% html_table(trim = TRUE)

odds = odds[-(1:6), c(2, seq(3, 3 + NBOOKS))]

# Retain only rows with data.
#
odds = subset(odds, !grepl("^(Non-Runners|Each-way|Going)", X2) & X2 != "")

# Remove empty column.
#
odds = odds[, sapply(odds, function(column) {any(column != "")})]

# Name rows.
#
rownames(odds)<- odds$X2
odds$X2 <- NULL

# Rename columns.
#
names(odds) = bookmakers

# Change unit odds: "20" becomes "20/1".
#
for (n in 1:ncol(odds)) {
  odds[,n] = gsub("^([[:digit:]]*)$", "\\1/1", gsub("^(SP)?$", NA, odds[,n]))
}

# Remove columns which have any NA.
#
odds <- odds[, !sapply(odds, function(column) any(is.na(column)))]

print(xtable(odds[, 1:9]), type = "html", html.table.attributes = "")



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
