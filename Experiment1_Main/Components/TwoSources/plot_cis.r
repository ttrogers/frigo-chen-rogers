function (d, pcol = 1, x = NA, y.lab = "") 
{
    nitems <- dim(d)[1]
    nvars <- dim(d)[2]
    if (is.na(x[1])) 
        x <- c(1:nvars)
    mns <- colMeans(d, na.rm = T)
    serr <- rep(0, times = nvars)
    for (i in c(1:nvars)) {
        serr[i] <- sqrt(var(d[, i], na.rm = T))/sqrt(nitems - 
            sum(is.na(d[, i])))
    }
    ci <- serr * 1.96
    y.lim <- c(min(mns - ci), max(mns + ci)) * 1.1
    x.lim <- c(min(x) - 0.1 * (max(x) - min(x)), max(x) + 0.1 * 
        (max(x) - min(x)))
    plot(x, mns, pch = 16, cex = 2, col = pcol, ylim = y.lim, 
        xlim = x.lim, xaxt = "n", xlab = "", ylab = y.lab)
    arrows(x, mns - ci, x, mns + ci, angle = 90, length = 0.1, 
        code = 3)
    box()
}
