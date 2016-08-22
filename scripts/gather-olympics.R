source("R/oddschecker.R")
library(dplyr)

while (TRUE) {
  time = Sys.time()
  time.fmt = strftime(time, "%Y-%m-%d-%H%M%S")
  #
  cat(time.fmt, "\n")
  #
  men = oddschecker("olympics/athletics/mens/mens-marathon/winner")
  men$time = time
  #
  women = oddschecker("olympics/athletics/womens/womens-800m/winner")
  women$time = time
  #
  write.csv(select(men, time, everything()), sprintf("M-marathon-%s.csv", time.fmt), quote = FALSE)
  write.csv(select(women, time, everything()), sprintf("F-800m-%s.csv", time.fmt), quote = FALSE)
  Sys.sleep(300)
}