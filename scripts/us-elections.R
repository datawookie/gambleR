library(gambleR)

elections <- oddschecker("politics/us-politics/us-presidential-election-2016/winner")

decimal.elections <- sapply(elections, to.decimal)
rownames(decimal.elections) <- rownames(elections)

probability <- implied.probability(decimal.elections)
rownames(probability) <- rownames(elections)

index = order(colSums(probability, na.rm = TRUE))
#
elections <- elections[, index]
decimal.elections <- decimal.elections[, index]
probability <- probability[, index]
#
index = order(rowSums(probability, na.rm = TRUE), decreasing = TRUE)
#
odds <- odds[index, ]
decimal.elections <- decimal.elections[index, ]
probability <- probability[index, ]

# Over-round per bookmaker.
#
sort(colSums(probability, na.rm = TRUE))




# Normalise columns
#
normprob <- as.data.frame(apply(probability, 2, function(x) x / sum(x, na.rm = TRUE)))

normprob$colour = "Independent"
normprob$colour = ifelse(rownames(normprob) %in% c("Donald Trump", "John Kasich", "Ted Cruz", "Marco Rubio", "Ben Carson", "Paul Ryan", "Mitt Romney"), "Republican", normprob$colour)
normprob$colour = ifelse(rownames(normprob) %in% c("Hillary Clinton", "Bernie Sanders", "Joe Biden"), "Democratic", normprob$colour)

normprob$name = as.character(rownames(normprob))

normprob <- na.omit(melt(normprob))

normprob$name <- factor(normprob$name, levels = group_by(normprob, name) %>% summarise(value = mean(value)) %>% arrange(value) %>% select(name) %>% .[[1]])
normprob$colour <- factor(normprob$colour, levels = c("Democratic", "Republican", "Independent"))

library(ggplot2)
library(scales)

ggplot(normprob, aes(x = name, y = value)) + geom_boxplot(aes(fill = colour)) +
  labs(y = "", x = "") +
  scale_fill_manual(values = c("blue", "red", "darkgreen")) +
  scale_y_continuous(labels = percent) +
  # scale_y_log10(labels = percent, limits = c(0.001, 1), breaks = c(0.001, 0.01, 0.1, 1)) +
  coord_flip() +
  theme_bw() + theme(legend.position = "none", axis.title.x = element_blank())