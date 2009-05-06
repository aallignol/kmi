cox.kmi <- function(formula, x, df.complete = Inf, ...) {
    if (!inherits(x, "kmi")) {
        stop("'x' must be of class 'kmi'")
    }
    call <- match.call()
    info <- x$info # that's where we have the column names (time, event)
    result <- lapply(seq_along(x$imputed.data), function(i) {
        daten <- x$original.data
        daten[, info[1]] <- x$imputed.data[[i]][, 1]
        daten[, info[2]] <- x$imputed.data[[i]][, 2]
        tmp <- coxph(formula, data = daten, ...)
        tmp
    })
    res <- MIcombine(result, df.complete = df.complete) ## that's a nice function
    zzz <- list(coefficients = res$coefficients,
                variance = res$variance,
                nimp = res$nimp,
                df = res$df,
                call = call,
                individual.fit = result)
    class(zzz) <- "cox.kmi"
    zzz
}
