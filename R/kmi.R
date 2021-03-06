kmi <- function(formula, data, id = NULL, etype, failcode = 1,
                nimp = 10, epsilon = 1,
                bootstrap = FALSE, nboot = 10) {

    if (missing(data))
        stop("A data frame in which to interpret the formula must be supplied")

    if (missing(etype)) stop("'etype' is missing, with no default")
    Call <- match.call()

    ## I'll need the name of the etype column in cox.kmi()
    arg.etype <- deparse(substitute(etype))
    if ((mode(Call[[2]]) == 'call' &&  Call[[2]][[1]] == as.name('Surv'))
        || inherits(formula, 'Surv'))  {
        stop("'kmi' requires a formula as the first argument")
    }

    ## ugly hack: force the setting of na.action to na.pass to get all
    ## the event times in case of missing values in the covariate.
    Call$na.action <- as.name("na.pass")

    ## Try to order the data at this level. We assume that id is
    ## supplied with counting data. There is a test later on.
    the_surv_part <- Call[[2]][[2]]
    if ("id" %in% names(Call)) {
        col_id <- deparse(substitute(id))
        col_start <- as.character(the_surv_part[[2]])
        col_stop <- as.character(the_surv_part[[3]])
        the_order <- order(data[, col_id], data[, col_start], data[, col_stop])
        data <- data[the_order, ]
    } else {
        the_order <- seq_len(nrow(data))
    }

    mfnames <- c('formula', 'data', 'na.action', 'id', 'etype')
    temp <- Call[c(1, match(mfnames, names(Call), nomatch=0))]
    temp[[1]] <- as.name("model.frame")
    m <- eval(temp, parent.frame())
    n <- nrow(m)
    Y <- model.extract(m, 'response')[the_order]
    if (!is.Surv(Y)) stop("Response must be a survival object")
    id <- model.extract(m, "id")[the_order]
    etype <- model.extract(m, "etype")[the_order]
    mt <- attr(m, "terms")
    X <- model.matrix(mt, m)[the_order, -1, drop = FALSE]

    ## to get the name of the 'time' column
    aa <- Call[[2]][[2]]
    if (attr(Y, "type") %in% c("interval", "interval2", "left")) {
        stop("kmi can only handle right censored data")
    }

    if (attr(Y, "type") == "counting" && !is.null(id)) {
        info <- c(as.character(aa[[3]])[as.character(aa[[3]]) %in% names(data)],
                  arg.etype)
        ## deal with etype when it's a fucking factor
        if (!is.factor(etype)) etype <- factor(etype)
        levels(etype) <- c(levels(etype), 0)
        etype[Y[, 3] == 0] <- 0
        etype <- etype[, drop = TRUE]
        ## for right-censored data with time-dependent covariates, i.e.,
        ## several rows per individual
        toimpute <- kmi.tdc(Y, X, id = id, etype = etype, failcode = failcode,
                            epsilon = epsilon, bootstrap = bootstrap,
                            nboot = nboot)
    } else {
        info <- c(as.character(aa[[2]])[as.character(aa[[2]]) %in% names(data)],
                  arg.etype)
        if (!is.factor(etype)) etype <- factor(etype)
        levels(etype) <- c(levels(etype), 0)
        etype[Y[, 2] == 0] <- 0
        etype <- etype[, drop = TRUE]
        ## for classical right-censored data
        toimpute <- kmi.classic(Y, X, etype = etype, failcode = failcode,
                                epsilon = epsilon,
                                bootstrap = bootstrap, nboot = nboot)
    }

    itimes <- toimpute$itimes
    gg <- toimpute$gg
    lg <- NROW(gg)

    cens.times <- toimpute$cens.times
    tmp <- findInterval(itimes, c(0, cens.times))

    if (length(cens.times) == 1) stop("'kmi' can't make imputation based on one censoring time")

    res <- lapply(seq_len(nimp), function(i) {
        tt <- double(length(itimes))
        for (j in seq_along(itimes)) {
            spr <- gg[, j] / c(gg[1:tmp[j], j], rep(gg[tmp[j], j], lg - tmp[j]))
            wp <- -diff(spr)
            ## if the last time is an event, a == TRUE and we add another probability
            wp <- if (toimpute$a) c(wp, spr[length(spr)]) else wp
            tt[j] <- sample(cens.times, 1, replace = TRUE, prob = wp)
        }
        newtimes <- c(toimpute$otimes, tt)
        newevent <- factor(c(levels(etype)[etype][toimpute$place],
                             rep(0, length(tt))))
        data.frame(newtimes, newevent)
    })

    ## we need to put the original data in the same order as the imputed times
    orig.data <- rbind(data[toimpute$place, ], data[-toimpute$place, ])

    zzz <- list(imputed.data = res,
                original.data = orig.data,
                info = info,
                call = Call)
    class(zzz) <- "kmi"
    zzz
}
