cox.kmi <- function(formula, x, ...) {
    if (!inherits(x, "kmi")) {
        stop("'x' must be of class 'kmi'")
    }
    call <- match.call()
    info <- x$info
    result <- lapply(seq_along(x$imputed.data), function(i) {
        daten <- x$original.data
        daten[, info[1]] <- x$imputed.data[[i]][, 1]
        daten[, info[2]] <- x$imputed.data[[i]][, 2]
        tmp <- coxph(formula, data = daten)
        tmp
    })
    res <- MIcombine(result)
    zzz <- list(coefficients = res$coefficients,
                variance = res$variance,
                nimp = res$nimp,
                df = res$df,
                call = call,
                individual.fit = result)
    class(zzz) <- "cox.kmi"
    zzz
}
    
    
