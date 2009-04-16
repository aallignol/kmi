print.cox.kmi <- function(x, print.imp = FALSE, ...) {
    if (!inherits(x, "cox.kmi")) {
        stop("'x' must an object of class 'kmi'")
    }
    if ("coxph.penal" %in% class(x$cox.kmi.fit[[1]])) {
        stop("Some things to think about before using frailties here")
    }
    else {
        cat("Call:\n")
        dput(x$call); cat("\n")
        coef <- x$coef
        se <- sqrt(diag(x$var))
        tmp <- cbind(coef, exp(coef), se, coef/se,
                     signif(1 - pchisq((coef/se)^2, 1)))
        dimnames(tmp) <- list(names(coef), c("coef", "exp(coef)", 
                                             "se(coef)", "z", "p"))
        cat("*****************\n")
        cat("Pooled estimates:\n")
        cat("*****************\n")
        print(tmp); cat("\n")
        if (print.imp) {
            cat("**************************************\n")
            cat("Separate estimate for each imputation:\n")
            cat("**************************************\n\n")
            for (i in seq_along(x$cox.kmi.fit)) {
                cat(paste("*** Imputation", i, "***", sep = " ")); cat("\n")
                print(x$cox.kmi.fit[[i]], ...)
                cat("\n")
            }
        }
    }
    invisible()
}
