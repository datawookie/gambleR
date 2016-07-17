tourdefrance <- read.csv("data-raw/tour-de-france.csv", stringsAsFactors = FALSE)

devtools::use_data(tourdefrance)
