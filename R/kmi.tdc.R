kmi.tdc <- function(y, etype, id, failcode,
                    bootstrap, nboot, index) {
    y <- y[order(id, y[, 2]), ]
    masque <- rbind(1, apply(as.matrix(id), 2, diff))
    masque <- c(masque[-1], 1)
    ysub <- y[masque != 0, ]
    etype.sub <- etype[masque != 0]
    
