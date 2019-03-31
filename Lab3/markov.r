maxSum <- 100

prob <- function(i, j) {
  A <- function(x) dpois(x, 1/3)
  S <- function(x) pexp(1, 1/3)
  B <- function(x) min(1, x/4)
  p <- function(f, val=0) f(val)
  sum <- function(i, n, fun) base::sum(sapply(i:ifelse(n >= maxSum, maxSum, n), fun))
  
  if (i < 0 || i > 4 || j < 0 || j > 4) {
    0
  } else if (j == 4) {
    1 - prob(i, 0) - prob(i, 1) - prob(i, 2) - prob(i, 3)
  } else if (i == 0) {
    p(A, j)
  } else if (j == 4) {
    1 - P(i, 0) - P(i, 1) - P(i, 2) - P(i, 3)
  } else if (j > i) {
    d <- j - i
    (1 - p(S))*sum(d, Inf, function(k) p(A, k) * p(B, i)^(k - d) * (1 - p(B, i))^d) +
      p(S)*sum(d + 1, Inf, function(k) p(A, k) * p(B, i)^(k - d - 1) * (1 - p(B, i))^(d + 1))
  } else if (j == i) {
    (1 - p(S))*sum(0, Inf, function(k) p(A, k) * p(B, i)^k) +
      p(S)*sum(1, Inf, function(k) p(A, k) * p(B, i)^(k-1) * (1 - p(B, i)))
  } else if (j == i - 1) {
    p(S)*sum(0, Inf, function(k) p(A, k) * p(B, i)^k)
  } else {
    0
  }
}

P <- sapply(0:4, function(j) sapply(0:4, function(i) prob(i, j)))
p0 <- matrix(c(1, 0, 0, 0, 0), ncol=1)

Pn <- P
for (i in 0:1000) {
  Pn <- Pn %*% P
}

pn <- t(Pn) %*% p0

e_t <- matrix(3*(0:4 + 1))

e_time <- t(pn) %*% e_t

