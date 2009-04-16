summary.cox.kmi <- function(object, conf.int = 0.95, scale = 1, ...) {
    if (!inherits(object, "cox.kmi")) {
        stop("'object' must be of class 'cox.kmi'")
    }
    if ("cox.penal" %in% class(object$cox.kmi.fit[[1]])) {
        stop("Doesn't work yet")
    }
    rval <- list()
    rval$call <- object$call
    beta <- object$coef
    se.beta <- sqrt(diag(object$var))
    tmp <- cbind(beta, exp(beta), se.beta, beta/se.beta,
                 1 - pchisq((beta/ se.beta)^2, 1))
    dimnames(tmp) <- list(names(beta), c("coef", "exp(coef)",
                                         "se(coef)", "z", "Pr(>|z|)"))
    rval$coefficients <- tmp
    ##
    z <- qnorm((1 + conf.int)/2, 0, 1)
    beta <- beta * scale
    se.beta <- se.beta * scale
    tmp <- cbind(exp(beta), exp(-beta), exp(beta - z * se.beta),
                 exp(beta + z * se.beta))
    dimnames(tmp) <- list(names(beta),
                          c("exp(coef)", "exp(-coef)",
                            paste("lower .", round(100 * conf.int, 2), sep = ""),
                            paste("upper .", round(100 * conf.int, 2), sep = "")))
    rval$conf.int <- tmp
    tmp <- lapply(object$cox.kmi.fit, summary, conf.int = conf.int, scale = scale, ...)
    rval$cox.kmi.fit <- tmp
    class(rval) <- "summary.cox.kmi"
    rval
}
