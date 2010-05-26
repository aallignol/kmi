## This function is used in the presence of time-dependent
## covariates

kmi.tdc <- function(y, etype, id, failcode, epsilon,
                    bootstrap, nboot) {
    ## We need to be careful for getting the censoring times.
    ## The status variable will be 0 before a change of the time-dependent
    ## covariate status, but that doesn't mean the guy is censored
    y <- y[order(id, y[, 2]), ]
    ## etype won't necessary be 0 for censored observations
    ## etype[y[, 3] == 0] <- 0
    ## We want to get the last row of each individual that contains the actual status
    ## at the end of the follow-up
    masque <- rbind(1, apply(as.matrix(id), 2, diff))
    masque <- c(masque[-1], 1)
    ## ysub will contain the data that are of interest to fulfill our purpose
    ysub <- y[masque != 0, ]
    ysub[, 1] <- 0
    etype.sub <- etype[masque != 0]
    cens.times <- sort(unique(ysub[, 2][ysub[, 3] == 0]))
    ind <- which(y[, 3] == 0 | etype == failcode)
    ## times to impute
    itimes <- y[-ind, 2]
    ## the other times
    otimes <- y[ind, 2]
    if (bootstrap) {
        index <- lapply(seq_len(nboot), function(k) {
            sample(seq_len(nrow(ysub)), nrow(ysub), 
                   replace = TRUE)
        })
        g <- matrix(0, nrow = nboot, ncol = length(cens.times))
        for (l in seq_len(nboot)) {
            tmp <- summary(survfit(Surv(ysub[index[[l]], 2], ysub[index[[l]], 3] == 0) ~ 1))
            ordre <- findInterval(cens.times, tmp$time)
            ordre[ordre == 0] <- NA
            g[l, ] <- tmp$surv[ordre]
        }
        g[is.na(g)] <- 1
        g <- apply(g, 2, mean)
        gg <- c(1, g)
    }
    else {
        g <- summary(survfit(Surv(ysub[, 2], ysub[, 3] == 0) ~ 1))
        gg <- c(1, g$surv)
    }
    a <- FALSE
    if (y[, 3][which.max(y[, 2])] != 0) { # TRUE if last time is an event
        cens.times <- c(cens.times, max(y[, 2]) + epsilon)
        a <- TRUE
    }
    list(gg = gg, cens.times = cens.times, itimes = itimes,
         otimes = otimes, place = ind, a = a)
}
