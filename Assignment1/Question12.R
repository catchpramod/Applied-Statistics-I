ttest <- function(x){
  tt = t.test(chickwts$weight, mu = x)
  T = unlist(tt$p.value)
  P = as.vector(tt$statistic)
  return(c(T,P))
}

if(ttest(240)[2] > 0.05){
  cat("Null Hypothesis Accepted, sample mean is significantly similar to the mean of the population")
} else {
  cat("Reject Null Hypothesis")
}