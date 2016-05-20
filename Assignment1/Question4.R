data("chickwts")


data(state)
head(state.x77)
?state.x77


data = state.x77
data = as.data.frame(data)
data$divisons = as.character(state.division)
boxplot(Income ~ divisons, data)
