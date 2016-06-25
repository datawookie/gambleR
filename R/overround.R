# Set a more reasonable lower/upper in optim().

O1 = 5.23
O2 = 1.18

overround <- function(odds) {
  odds = odds - 1
  #
  probsum <- function(delta) {
    (sum(1 - odds * delta / (odds * delta + 1)) - 1)**2
  }
  
  optim(1, probsum, method = "Brent", lower = 0, upper = 2)$par
}

OR = overround(c(O1, O2))

Q1 = (O1 - 1) * OR
Q2 = (O2 - 1) * OR

P1 = 1 - Q1 / (Q1 + 1)
P2 = 1 - Q2 / (Q2 + 1)