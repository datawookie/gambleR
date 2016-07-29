library(dplyr)
library(reshape2)

source("R/odds.R")

odds <- read.csv(file.path("data", "horse-racing.csv")) %>% subset(Location == "Stratford" & Time == "18:20") %>%
  dcast(Horse ~ Bookmaker, value.var = "Odds")
#
rownames(odds) <- odds$Horse
odds$Horse <- NULL

decimal.odds <- sapply(odds, to.decimal)
rownames(decimal.odds) <- rownames(odds)

probability <- implied.probability(decimal.odds)
rownames(probability) <- rownames(odds)

index = order(colSums(probability))
#
odds <- odds[, index]
decimal.odds <- decimal.odds[, index]
probability <- probability[, index]
#
index = order(rowSums(probability), decreasing = TRUE)
#
odds <- odds[index, ]
decimal.odds <- decimal.odds[index, ]
probability <- probability[index, ]

# Over-round per bookmaker.
#
sort(colSums(probability))
#
# Average over-round per runner per bookmaker.
#
sort(colMeans(probability))
