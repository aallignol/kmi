kmi.classic <- function(y, etype, failcode, epsilon,
                        bootstrap, nboot, index) {
    if (!is.Surv(y)) stop("y must be a Surv object")
    if (attr(y, "type") != "right") stop("Can only handle right censored data")
    if (is.null(etype)) stop("Argument 'etype' is missing with no default")
    etype[y[, 2] == 0] <- 0
    cens.times <- sort(unique(y[, 1][y[, 2] == 0]))
    indice <- seq_along(etype)
    ##ind <- which(etype %in% c(censcode, failcode))
    ind <- which(y[, 2] == 0 | etype == failcode)
    itimes <- y[-ind, 1]
    otimes <- y[ind, 1]
    if (bootstrap) {
        if (missing(index)) {
            index <- lapply(seq_len(nboot), function(k) {
                sample(seq_len(nrow(y)), nrow(y),
                       replace = TRUE)
            })
        }
        g <- matrix(0, nrow = nboot, ncol = length(cens.times))
        for (l in seq_len(nboot)) {
            tmp <- summary(survfit(Surv(y[index[[l]], 1], y[index[[l]], 2] == 0) ~ 1))
            ordre <- findInterval(cens.times, tmp$time)
            ordre[ordre == 0] <- NA
            g[l, ] <- tmp$surv[ordre]
            g[l, ][is.na(g[l, ])] <- 1
        }
        g <- apply(g, 2, mean)
        gg <- c(1, g)
    }
    else {
        g <- summary(survfit(Surv(y[, 1], y[, 2] == 0) ~ 1))
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
