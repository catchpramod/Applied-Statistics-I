data("chickwts")

data(Cars93, package= "MASS")
chickwts.Cars93 = Cars93[Cars93$MPG.highway < 25 & Cars93$Weight > 3500, c("Model", "Price", "Min.Price", "Max.Price", "Weight", "MPG.highway")]
chickwts.Cars93