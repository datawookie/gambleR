source("R/oddschecker.R")
library(dplyr)

while (TRUE) {
  time = Sys.time()
  time.fmt = strftime(time, "%Y-%m-%d-%H%M%S")
  #
  cat(time.fmt, "\n")
  #
  men = oddschecker("olympics/athletics/mens/mens-100m/winner")
  men$time = time
  #
  women = oddschecker("olympics/athletics/womens/womens-100m/winner")
  women$time = time
  #
  write.csv(select(men, time, everything()), sprintf("M-100-%s.csv", time.fmt), quote = FALSE)
  write.csv(select(women, time, everything()), sprintf("F-100-%s.csv", time.fmt), quote = FALSE)
  Sys.sleep(300)
}