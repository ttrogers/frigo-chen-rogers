function (pth = "./") 
{
    f <- list.files(path = pth, pattern = ".csv")
    f <- paste(pth, f, sep = "")
    nf <- length(f)
    o <- matrix(9, nf, 2)
    for (i in c(1:nf)) {
        tmp <- get.dat(f[i])[[2]]
        o[i, ] <- as.numeric(tmp[tmp$Trial == 1, c(11, 13)])
        print(f[i])
        flush.console()
    }
    o
}
