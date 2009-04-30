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


### test on the bootstrap


