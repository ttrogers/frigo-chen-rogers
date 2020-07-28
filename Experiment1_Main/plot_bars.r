function (d, bcol = 1, y.lim = NA, y.lab = "") 
{
    nitems <- dim(d)[1]
    nvars <- dim(d)[2]
    mns <- colMeans(d, na.rm = T)
    serr <- rep(0, times = nvars)
    for (i in c(1:nvars)) {
        serr[i] <- sqrt(var(d[, i], na.rm = T))/sqrt(nitems - 
            sum(is.na(d[, i])))
    }
    if (is.na(y.lim[1])) {
        y.lim <- c(min(mns - serr), max(mns + serr)) * 1.1
    }
    msgn <- rep(1, times = nvars)
    msgn[mns < 0] <- -1
    p <- barplot(mns, beside = T, ylim = y.lim, col = bcol, ylab = y.lab)
    arrows(p, mns, p, mns + (serr * msgn), angle = 90)
    box()
}
