dfct<-function (x, y, z, w = NULL, increment, resamp = 1000, latlon = FALSE,
    na.rm = FALSE, quiet = FALSE)
{
    NAO <- FALSE
    if (any(!is.finite(unlist(z)))) {
        if (na.rm) {
            warning("Missing values exist; Pairwise deletion will be used")
            NAO <- TRUE
        }
        else {
            stop("Missing values exist; use na.rm = TRUE for pairwise deletion")
        }
    }
    multivar <- !is.null(dim(z))
    if (multivar == TRUE) {
        warning("Response is multivariate: the correlation matrix will be centered on zero. Use correlog.nc() for the non-centered correlogram")
        n <- length(z[, 1])
        p <- length(z[1, ])
        z <- as.matrix(z) + 0
        if (is.null(w)) {
            moran <- cor2(t(z), circ = FALSE)
            moran <- moran[lower.tri(moran)]
            moran <- moran - mean(moran, na.rm = TRUE)
        }
        else {
            w <- as.matrix(w) + 0
            moran <- cor2(t(z), t(w), circ = FALSE)
            zero <- mean(diag(moran), na.rm = TRUE)
            moran <- moran[row(moran) != col(moran)]
            moran <- moran - mean(moran, na.rm = TRUE)
        }
    }
    else {
        n <- length(z)
        z <- as.vector(z) + 0
        zscal <- (scale(z, center = TRUE, scale = TRUE)[, 1])/(sqrt((n -
            1)/n))
        if (is.null(w)) {
            moran <- t(outer(zscal, zscal))
            moran <- moran[lower.tri(moran)]
        }
        else {
            wscal <- (scale(w, center = TRUE, scale = TRUE)[,
                1])/(sqrt((n - 1)/n))
            zw <- c(zscal, wscal)
            moran <- t(outer(zw, zw))[1:n, (n + 1):(2 * n)]
            zero <- mean(diag(moran), na.rm = TRUE)
            moran <- moran[row(moran) != col(moran)]
        }
    }
    browser()
    if (latlon) {
        dmat <- matrix(0, nrow = n, ncol = n)
        for (i in 1:(n - 1)) {
            for (j in (i + 1):n) {
                dmat[j, i] <- gcdist(x[i], y[i], x[j], y[j])
                dmat[i, j] <- dmat[j, i]
            }
        }
    }
    else {
        dmat <- sqrt(outer(x, x, "-")^2 + outer(y, y, "-")^2)
    }
    if (resamp != 0) {
        dmat2 <- dmat
        moran2 <- moran
    }
    if (is.null(w)) {
        dmat <- dmat[lower.tri(dmat)]
    }
    else {
        dmat <- dmat[row(dmat) != col(dmat)]
    }
    dkl <- ceiling(dmat/increment)
    nlok <- sapply(split(moran, dkl), length)
    dmean <- sapply(split(dmat, dkl), mean, na.rm = TRUE)
    moran <- sapply(split(moran, dkl), mean, na.rm = TRUE)
    ly <- 1:length(dmean)
    x <- c(dmean[ly[moran < 0][1]], dmean[ly[moran < 0][1] -
        1])
    y <- c(moran[ly[moran < 0][1] - 1], moran[ly[moran < 0][1]])
    if (moran[1] < 0) {
        tmp <- 0
    }
    else {
        tmp <- lm(x ~ y)[[1]][1]
    }
    p <- NULL
    if (resamp != 0) {
        perm <- matrix(NA, ncol = length(moran), nrow = resamp)
        for (i in 1:resamp) {
            if (!quiet) {
                cat(i, " of ", resamp, "\n")
            }
            trekk <- sample(1:n)
            dma <- dmat2[trekk, trekk]
            mor <- moran2
            if (is.null(w)) {
                dma <- dma[lower.tri(dma)]
            }
            else {
                dma <- dma[row(dma) != col(dma)]
            }
            dkl <- ceiling(dma/increment)
            perm[i, ] <- sapply(split(mor, dkl), mean, na.rm = TRUE)
        }
        p = (apply(moran <= t(perm), 1, sum))/(resamp + 1)
        p = apply(cbind(p, 1 - p), 1, min) + 1/(resamp + 1)
    }
    res <- list(n = nlok, mean.of.class = dmean, correlation = moran,
        x.intercept = tmp, p = p, call = deparse(match.call()))
    if (!is.null(w)) {
        res$corr0 <- zero
    }
    class(res) <- "correlog"
    res
}
