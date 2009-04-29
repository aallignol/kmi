kmi <- function(formula, data, id, etype, nimp = 10, epsilon = 1,
                bootstrap = FALSE, nboot = 10, index) {
    Call <- match.call()
    if ((mode(Call[[2]]) == 'call' &&  Call[[2]][[1]] == as.name('Surv'))
        || inherits(formula, 'Surv'))  {
        stop("'kmi' requires a formula as the first argument")
    }
    if (missing(etype)) stop()
    mfnames <- c('formula', 'data', 'id', 'etype')
    temp <- Call[c(1, match(mfnames, names(Call), nomatch=0))]
    temp[[1]] <- as.name("model.frame")
    m <- eval.parent(temp)
    n <- nrow(m)
    Y <- model.extract(m, 'response')
    if (!is.Surv(Y)) stop("Response must be a survival object")
    id <- model.extract(m, "id")
    etype <- model.extract(m, "etype")
    if (attr(Y, "type") == "counting" && !is.null(id)) {
        toimpute <- kmi.tdp(Y, id = id, etype = etype, failcode = failcode,
                                  bootstrap = bootstrap, nboot = nboot)
    }
    else {
        toimpute <- kmi.classic(Y, etype = etype, failcode = failcode,
                                  bootstrap = bootstrap, nboot = nboot)
    }
    itimes <- toimpute$itimes
    gg <- toimpute$gg
    lg <- length(gg)
    cens.times <- toimpute$cens.times
    tmp <- findInterval(itimes, c(0, cens.times))
    res <- lapply(seq_len(nimp), function(i) {
        for (j in seq_along(itimes)) {
            spr <- gg / c(gg[1:tmp[j]], rep(gg[tmp[j]], lg - tmp[j]))
            wp <- -diff(spr)
            wp <- if (a) c(wp, spr[length(spr)]) else wp
            tt[j] <- sample(cens.times, 1, replace = TRUE, prob = wp)
        }
        newtimes <- c(toimpute$otimes, tt)
        newevent <- c(etype[toimpute$place], rep(0, length(tt)))
        matrix(c(newtimes, newevent), ncol = 2)
    })
    orig.data <- rbind(data[toimpute$place, ], data[-toimpute$place, ])
    zzz <- list(imputed.data = res,
                original.data = orig.data,
                call = Call)
    class(zzz) <- "kmi"
    zzz
}
