### Wrapper for coxph (survival) to be used
### with kaplan-meier imputation

cox.kmi <- function(formula, kmi.object, ...) {
    if (!inherits(kmi.object, "kmi")) {
        stop("'object' must be of class 'kmi'")
    }
    call <- match.call()
    nimp <- kmi.object$nimp
    result <- lapply(seq_len(nimp), function(i) {
        daten <- kmi.object$data
        daten[, kmi.object$info[1]] <- kmi.object$imp.dat[[i]]$newtimes
        daten[, kmi.object$info[2]] <- kmi.object$imp.dat[[i]]$newevent
        tmp <- coxph(formula, data = daten, ...)
        tmp
    })
    coef <- var <- list()
    for (i in seq_len(nimp)) {
        coef[[i]] <- result[[i]]$coefficients
        var[[i]] <- result[[i]]$var
    }
    coef <- do.call(rbind, coef)
    est.coef <- apply(coef, 2, mean)
    va <- apply(do.call(cbind, var), c(1, 2), mean)
    dim(va) <- c(ncol(coef), ncol(coef), nimp)
    va <- apply(va, c(1,2), mean)
    var.coef <- va + (1 + (1/nimp)) * var(coef)
    zzz <- list(call = call, coef = est.coef, var = var.coef,
                cox.kmi.fit = result)
    class(zzz) <- "cox.kmi"
    zzz
}
