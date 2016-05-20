data("chickwts")

new.england = data[data$divisons == "New England", ]
row.names(new.england[new.england$Income == max(new.england$Income), ])
