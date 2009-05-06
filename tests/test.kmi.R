require(kmi)

### test 1 

id <- 1:10
time <- 1:10
ev <- c(0, 0, 0, rep(1, 3), rep(2, 4))
data <- data.frame(id, time, ev)

aa <- kmi(Surv(time, ev != 0) ~ 1, data, etype = ev)

bb <- aa$imputed.data
test1 <- lapply(bb, function(x) {
    x[aa$original.data$id %in% 7:10, ]
})
test1 <- do.call(rbind, test1)

all(test1[, 1] == 11)
all(test1[, 2] == 0)

aa <- kmi(Surv(time, ev != 0) ~ 1, data, etype = ev, failcode = 2)

bb <- aa$imputed.data
test1 <- lapply(bb, function(x) {
    x[aa$original.data$id %in% 4:6, ]
})
test1 <- do.call(rbind, test1)

all(test1[, 1] == 11)
all(test1[, 2] == 0)

aa <- kmi(Surv(time, ev != 0) ~ 1, data, etype = ev, epsilon = 2)

bb <- aa$imputed.data
test1 <- lapply(bb, function(x) {
    x[aa$original.data$id %in% 7:10, ]
})
test1 <- do.call(rbind, test1)

all(test1[, 1] == 12)
all(test1[, 2] == 0)

aa <- kmi(Surv(time, ev != 0) ~ 1, data, etype = ev, nimp = 13)
length(aa$imputed.data) == 13


### test 2

set.seed(198740)
time <- rexp(100)
ev <- sample(c(0, 1, 2), 100, replace = TRUE)
cov <- rbinom(100, 1, 0.5)
dd <- data.frame(time, ev, cov)

set.seed(1440293)
dat.kmi <- kmi(Surv(time, ev != 0) ~ 1, dd, etype = ev, nimp = 5)

fit.kmi <- cox.kmi(Surv(time, ev == 1) ~ cov, dat.kmi)

fit.kmi

summary(fit.kmi)

## avec bootstrap

set.seed(867988)
dat.kmib <- kmi(Surv(time, ev != 0) ~ 1, dd, etype = ev, nimp = 5,
                boot = TRUE, nboot = 5)

fit.kmib <- cox.kmi(Surv(time, ev == 1) ~ cov, dat.kmib)

fit.kmib

summary(fit.kmib)



### test 4

data(icu.pneu)

set.seed(1313)
dat <- kmi(Surv(entry, exit, status) ~ 1, data = icu.pneu,
           etype = event, id= id, failcode = 3, nimp = 5)

icu.pneu$ev <- icu.pneu$event
icu.pneu$ev[icu.pneu$status == 0] <- 0

set.seed(1313)
dat2 <- kmi(Surv(entry, exit, ev != 0) ~ 1, data = icu.pneu,
           etype = ev, id= id, failcode = 3, nimp = 5)

a <- logical(5)
for (i in 1:5) a[i] <- all.equal(dat$imputed.data[[i]][, 1], dat2$imputed.data[[i]][, 1])
a

fit.kmi <- cox.kmi(Surv(entry, exit, event == 3) ~ pneu, dat)

fit.kmi2 <- cox.kmi(Surv(entry, exit, ev == 3) ~ pneu, dat2)

all.equal(fit.kmi$coefficients, fit.kmi2$coefficients)
all.equal(fit.kmi$variance, fit.kmi2$variance)

fit.kmi

fit.kmi2

## avec bootstrap

set.seed(598085)
dat <- kmi(Surv(entry, exit, status) ~ 1, data = icu.pneu,
           etype = event, id= id, failcode = 3, nimp = 5,
           boot = TRUE, nboot = 5)

set.seed(598085)
dat2 <- kmi(Surv(entry, exit, ev != 0) ~ 1, data = icu.pneu,
            etype = ev, id= id, failcode = 3, nimp = 5,
            boot = TRUE, nboot = 5)

a <- logical(5)
for (i in 1:5) a[i] <- all.equal(dat$imputed.data[[i]][, 1], dat2$imputed.data[[i]][, 1])
a

fit.kmi <- cox.kmi(Surv(entry, exit, event == 3) ~ pneu, dat)

fit.kmi2 <- cox.kmi(Surv(entry, exit, ev == 3) ~ pneu, dat2)

all.equal(fit.kmi$coefficients, fit.kmi2$coefficients)
all.equal(fit.kmi$variance, fit.kmi2$variance)

fit.kmi

fit.kmi2
