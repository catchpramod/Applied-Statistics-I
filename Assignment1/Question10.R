
data("mtcars")
mtcars$carsize <- cut(mtcars[,"wt"], breaks=c(0, 2.5, 3.5, 5.5), labels=c("Compact", "Midsize", "Large"))
pairs(~ mpg+disp+hp+drat+qsec , mtcars, col= mtcars$carsize )
