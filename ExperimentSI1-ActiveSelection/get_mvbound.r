function (sj, wsz) 
{
    tmp <- data[[sj]][[3]][, c(10, 11, 13)]
    tmp <- tmp[order(tmp[, 1]), 2:3]
    npts <- dim(tmp)[1] - wsz
    out <- rep(0, times = npts)
    for (i1 in c(1:npts)) {
        tmprange <- c(i1:(i1 + wsz))
        is <- glm(acc ~ Pic, family = "binomial", data = tmp[tmprange, 
            ])$coef
        out[i1] <- -(is[1]/is[2])
    }
    out
}
