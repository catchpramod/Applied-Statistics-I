mycars = Cars93[, c("Min.Price", "Max.Price", "MPG.city", "MPG.highway", "EngineSize", "Length", "Weight")]

Cars.Means = lapply(mycars, mean)
Cars.Means
Cars.Std.Errors = lapply(mycars, function(x) sd(x) / sqrt(length(x)))
Cars.Std.Errors
E = lapply(mycars, function(x)  qt(0.995,df=length(x)-1) * sd(x) / sqrt(length(x)))
Cars.CI.99 = rbind(unlist(Cars.Means) + unlist(E), unlist(Cars.Means) - unlist(E))
Cars.CI.99
