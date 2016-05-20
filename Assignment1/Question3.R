data("chickwts")

(sum((chickwts$weight ^ 2)) - (sum(chickwts$weight) ^ 2) / length(chickwts$weight) ) / (length(chickwts$weight))
