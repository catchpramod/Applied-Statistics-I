
data(state)
data = state.x77
state.data = as.data.frame(data)
state.data$region = as.character(state.region)
state.data$division = as.character(state.division)
state.size <- cut(state.x77[,"Population"], breaks = c(0, 2000, 10000, Inf),  labels = c("Small", "Medium", "Large"))
state.data$size = as.character(state.size)


tapply(state.data$Income, state.data$region, mean)

tapply(state.data$Illiteracy, state.data$division, FUN=max)

tapply(state.data$Illiteracy, state.data$region, length)

tapply(state.data$Illiteracy, list(state.data$region, state.data$size) , length)
