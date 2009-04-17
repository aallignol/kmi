kmi <- function(data, time.col, event.col, id.col = NULL, nimp = 10,
                failcode = 1, censcode = 0, epsilon = 1,
                bootstrap = FALSE, nboot = 10, index) {
    if (!is.null(id.col)) {
        info <- match(c(time.col, event.col, id.col), names(data))
        wdata <- data[order(data[, id.col], data[, time.col]), ]
        masque <- rbind(1, apply(as.matrix(wdata[, id.col]), 2, diff))
        masque <- c(masque[-1], 1)
        wdata <- wdata[masque != 0, ]
        cens.times <- sort(unique(wdata[wdata[, event.col] == censcode, time.col]))
        wtimes <- wdata[, time.col]
        wevent <- wdata[, event.col]
    }
    else {
        info <- match(c(time.col, event.col), names(data))
        wdata <- data
        cens.times <- sort(unique(wdata[wdata[, event.col] == censcode, time.col]))
        wtimes <- wdata[, time.col]
        wevent <- wdata[, event.col]
    }
    ## w... times and events we'll work with
    times <- data[, time.col]
    event <- data[, event.col]
    ind <- which(event %in% c(censcode, failcode))
    itimes <- times[-ind] # times to impute
    otimes <- times[ind] # other times (i.e., event of interest or censoring)
    if (bootstrap) {
        if (missing(index)) {
            index <- lapply(seq_len(nboot), function(k) {
                sample(seq_len(nrow(wdata)), nrow(wdata),
                       replace = TRUE)
            })
        }
        g <- matrix(0, nrow = nboot, ncol = length(cens.times))
        for (l in seq_len(nboot)) {
            tmp <- summary(survfit(Surv(wtimes[index[[l]]], wevent[index[[l]]] == censcode) ~1))
            ordre <- findInterval(cens.times, tmp$time)
            ordre[ordre == 0] <- NA
            g[l, ] <- tmp$surv[ordre]
            g[l, ][is.na(g[l, ])] <- 1
        }
        g <- apply(g, 2, mean)
        gg <- c(1, g)
    }
    else {
        g <- summary(survfit(Surv(wtimes, wevent == censcode) ~ 1))
        gg <- c(1, g$surv)
    }  
    a <- FALSE
    if (wevent[which.max(wtimes)] != censcode) {
        wtimes <- c(wtimes, max(wtimes) + epsilon)
        cens.times <- c(cens.times, max(wtimes) + epsilon)
        a <- TRUE
    }
    lg <- length(gg)
    res <- lapply(seq_len(nimp), function(i) {
        tt <- double(length(itimes))
        for (j in seq_along(itimes)) {
            tmp <- findInterval(itimes[j], c(0, cens.times))
            tmp[tmp == 0] <- 1
            spr <- gg / c(gg[1:tmp], rep(gg[tmp], lg - tmp))
            wp <- -diff(spr)
            wp <- if (a) c(wp, wp[length(wp)]) else wp
            tt[j] <- sample(cens.times, 1, replace = TRUE, prob = wp)
            print(sum(wp))
#            print(tt[j] > itimes[j])
        }
        newtimes <- c(otimes, tt)
        newevent <- c(event[ind], rep(censcode, length(tt)))
        aa <- data.frame(newtimes, newevent)
        aa
    })
    orig.data <- rbind(data[ind, ], data[-ind, ])
    zzz <- list(data = orig.data,
                imp.dat = res,
                info = info,
                nimp = nimp
                )
    class(zzz) <- "kmi"
    zzz
}
