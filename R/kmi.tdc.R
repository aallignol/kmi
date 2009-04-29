kmi.tdc <- function(y, etype, id, failcode,
                    bootstrap, nboot, index) {
    y <- y[order(id, y[, 2]), ]
    masque <- rbind(1, apply(as.matrix(id), 2, diff))
    masque <- c(masque[-1], 1)
    ysub <- y[masque != 0, ]
    ysub[, 1] <- 0
    etype.sub <- etype[masque != 0]
    cens.times <- sort(unique(ysub[, 2][ysub[, 3] == 0]))
    ind <- which(y[, 3] == 0 | etype == failcode)
    itimes <- y[-ind, 2]
    otimes <- y[ind, 2]
    if (bootstrap) {
        if (missing(index)) {
            index <- lapply(seq_len(nboot), function(k) {
                sample(seq_len(nrow(ysub)), nrow(ysub), 
                       replace = TRUE)
            })
        }
        g <- matrix(0, nrow = nboot, ncol = length(cens.times))
        for (l in seq_len(nboot)) {
            tmp <- summary(survfit(Surv(ysub[index[[l]], 2, ysub[index[[l]], 3] == 0]) ~ 1))
            ordre <- findInterval(cens.times, tmp$time)
            ordre[ordre == 0] <- NA
            g[l, ] <- tmp$surv[ordre]
        }
        g[is.na(g)] <- 1
        g <- apply(g, 2, mean)
        gg <- c(1, g)
    }
    else {
        g <- summary(survfit(Surv(ysub[, 1], ysub[, 2] == 0) ~ 1))
        gg <- c(1, g$surv)
    }
    a <- FALSE
    if (y[, 2][which.max(y[, 1])] != 0) {
        cens.times <- c(cens.times, max(y[, 1]) + epsilon)
        a <- TRUE
    }
    list(gg = gg, cens.times = cens.times, itimes = itimes,
         otimes = otimes, place = ind, a = a)
}
