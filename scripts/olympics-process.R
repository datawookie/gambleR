library(readr)
library(dplyr)

PATTERN = "F-800m-.*.csv"
# PATTERN = "M-marathon-.*.csv"
FOLDER = "odds-over-time"

# Original data were logged in UTC.
#
odds <- lapply(list.files(path = FOLDER, pattern = PATTERN, full.names = TRUE), function(path) {
  read_csv(path) %>% rename(athlete = X1, utc = time)
  }) %>% bind_rows
#
names(odds) <- gsub(" ", "_", names(odds))
#
# Convert to decimal.
#
odds = cbind(odds[, 1:2], to.decimal(odds[, c(-1, -2)]))
#
# Convert to local time zone of Rio de Janiero.
#
odds$brt = as.POSIXct(format(odds$utc, tz = "America/Sao_Paulo", usetz = TRUE), tz = "BRT")
#
# Clean athlete names
#
odds$athlete = gsub("  ", " ", odds$athlete)

# Find athletes in final odds.
#
final.odds = subset(odds, utc == max(utc))[, 1:3] %>% na.omit

library(ggplot2)
library(scales)

ggplot(odds, aes(x = brt, y = Bet_365)) +
  geom_line(aes(group = athlete), position = position_jitter(w = 0, h = 0.05), alpha = 0.75) +
  geom_line(data = subset(odds, athlete %in% final.odds$athlete) %>% mutate(
    athlete = factor(athlete, levels = c("Kate Grace", "Maryna Arzamasava", "Lynsey Sharp", "Joanna Jozwik", 
                                         "Melissa Bishop", "Francine Niyonsaba", "Margaret Wambui", "Caster Semenya"))),
    aes(x = brt, y = Bet_365, group = athlete, color = athlete), lwd = 1.5, alpha = 0.85) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2016-08-17 10:55:00", tz = "BRT")), lty = "dashed") +
  geom_vline(xintercept = as.numeric(as.POSIXct("2016-08-18 21:15:00", tz = "BRT")), lty = "dashed") +
  geom_vline(xintercept = as.numeric(as.POSIXct("2016-08-20 21:15:00", tz = "BRT")), lty = "dashed") +
  scale_x_datetime(date_breaks = "1 day", labels = date_format("%d/%m/%Y", tz = "BRT")) +
  scale_y_log10() +
  scale_color_brewer(palette = "Paired") +
  labs(y = "Decimal Odds", x = "") +
  theme_minimal() + theme(legend.title = element_blank(), panel.grid.minor = element_blank())