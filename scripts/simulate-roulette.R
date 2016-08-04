set.seed(13)

# Simulate N games of European Roulette, wagering on Odd.
#
simulate <- function(N) sample(c(+1, -1), N, replace = TRUE, prob = c(18, 19))

# Generate a history for multiple players.
#
PLAYERS = 100
GAMES = 1000
#
history <- replicate(PLAYERS, simulate(GAMES))
#
CAPITAL = 100
#
history <- apply(history, 2, cumsum) + CAPITAL

ruin <- function(x) {
  pos <- which(x == 0)[1]
  #
  if (is.na(pos)) return(x)
  #
  x[pos:length(x)] = 0
  x
}
#
history <- apply(history, 2, ruin)

library(reshape2)
library(dplyr)

history <- melt(history, value.name = "capital", varnames = c("game", "player"))
#
expect <- group_by(history, game) %>% summarise(capital = mean(capital))

library(ggplot2)

ggplot(history, aes(x = game, y = capital)) +
  geom_line(aes(group = player), alpha = 0.5) +
  geom_line(data = expect, aes(x = game, y = capital), col = "red") +
  geom_hline(yintercept = CAPITAL, lty = "dashed") +
  labs(x = "Game Number", y = "Gambling Capital") +
  theme_minimal()
