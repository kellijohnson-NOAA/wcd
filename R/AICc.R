
# AIC: -2*LL + 2*K
# AICc AIC + 2*K*(K+1)/(n-K-1)

AICc <- function(x) {
  aic <- x$aic
  K <- (AIC(x) - deviance(x)) / 2
  N <- x$N
  aicc <- aic + 2 * K * (K + 1) / (N - K - 1)
  return(aicc)
}
