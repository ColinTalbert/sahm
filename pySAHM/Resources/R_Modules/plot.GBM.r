plot.GBM<-function (x, i.var = 1, n.trees = x$n.trees, continuous.resolution = 100,
    return.grid = FALSE,dat, ...)
{
    #This is plot.gbm from the gbm library (Ridgeway) but changed to use a common grid when
    #used/available data are split on used and predictions from several models must be averaged
    if (all(is.character(i.var))) {
        i <- match(i.var, x$var.names)
        if (any(is.na(i))) {
            stop("Plot variables not used in gbm model fit: ",
                i.var[is.na(i)])
        }
        else {
            i.var <- i
        }
    }
    if ((min(i.var) < 1) || (max(i.var) > length(x$var.names))) {
        warning("i.var must be between 1 and ", length(x$var.names))
    }
    if (n.trees > x$n.trees) {
        warning(paste("n.trees exceeds the number of trees in the model, ",
            x$n.trees, ". Plotting using ", x$n.trees, " trees.",
            sep = ""))
        n.trees <- x$n.trees
    }
    if (length(i.var) > 3) {
        warning("plot.gbm creates up to 3-way interaction plots.\nplot.gbm will only return the plotting data structure.")
        return.grid = TRUE
    }
    browser()
    grid.levels <- vector("list", length(i.var))
    for (i in 1:length(i.var)) {
        if (is.numeric(x$var.levels[[i.var[i]]])) {
            grid.levels[[i]] <- seq(min(x$var.levels[[i.var[i]]]),
                max(x$var.levels[[i.var[i]]]), length = continuous.resolution)
        }
        else {
            grid.levels[[i]] <- as.numeric(factor(x$var.levels[[i.var[i]]],
                levels = x$var.levels[[i.var[i]]])) - 1
        }
    }
    X <- expand.grid(grid.levels)
    names(X) <- paste("X", 1:length(i.var), sep = "")
    X$y <- .Call("gbm_plot", X = as.double(data.matrix(X)), cRows = as.integer(nrow(X)),
        cCols = as.integer(ncol(X)), i.var = as.integer(i.var -
            1), n.trees = as.integer(n.trees), initF = as.double(x$initF),
        trees = x$trees, c.splits = x$c.splits, var.type = as.integer(x$var.type),
        PACKAGE = "gbm")
    f.factor <- rep(FALSE, length(i.var))
    for (i in 1:length(i.var)) {
        if (!is.numeric(x$var.levels[[i.var[i]]])) {
            X[, i] <- factor(x$var.levels[[i.var[i]]][X[, i] +
                1], levels = x$var.levels[[i.var[i]]])
            f.factor[i] <- TRUE
        }
    }
    if (return.grid) {
        names(X)[1:length(i.var)] <- x$var.names[i.var]
        return(X)
    }
    if (length(i.var) == 1) {
        if (!f.factor) {
            j <- order(X$X1)
            plot(X$X1, X$y, type = "l", xlab = x$var.names[i.var],
                ylab = paste("f(", x$var.names[i.var], ")", sep = ""),
                ...)
        }
        else {
            plot(X$X1, X$y, xlab = x$var.names[i.var], ylab = paste("f(",
                x$var.names[i.var], ")", sep = ""), ...)
        }
    }
    else if (length(i.var) == 2) {
        if (!f.factor[1] && !f.factor[2]) {
            levelplot(y ~ X1 * X2, data = X, xlab = x$var.names[i.var[1]],
                ylab = x$var.names[i.var[2]], ...)
        }
        else if (f.factor[1] && !f.factor[2]) {
            xyplot(y ~ X2 | X1, data = X, xlab = x$var.names[i.var[2]],
                ylab = paste("f(", x$var.names[i.var[1]], ",",
                  x$var.names[i.var[2]], ")", sep = ""), type = "l",
                ...)
        }
        else if (!f.factor[1] && f.factor[2]) {
            xyplot(y ~ X1 | X2, data = X, xlab = x$var.names[i.var[1]],
                ylab = paste("f(", x$var.names[i.var[1]], ",",
                  x$var.names[i.var[2]], ")", sep = ""), type = "l",
                ...)
        }
        else {
            stripplot(y ~ X1 | X2, data = X, xlab = x$var.names[i.var[2]],
                ylab = paste("f(", x$var.names[i.var[1]], ",",
                  x$var.names[i.var[2]], ")", sep = ""), ...)
        }
    }
    else if (length(i.var) == 3) {
        i <- order(f.factor)
        X.new <- X[, i]
        X.new$y <- X$y
        names(X.new) <- names(X)
        if (sum(f.factor) == 0) {
            X.new$X3 <- equal.count(X.new$X3)
            levelplot(y ~ X1 * X2 | X3, data = X.new, xlab = x$var.names[i.var[i[1]]],
                ylab = x$var.names[i.var[i[2]]], ...)
        }
        else if (sum(f.factor) == 1) {
            levelplot(y ~ X1 * X2 | X3, data = X.new, xlab = x$var.names[i.var[i[1]]],
                ylab = x$var.names[i.var[i[2]]], ...)
        }
        else if (sum(f.factor) == 2) {
            xyplot(y ~ X1 | X2 * X3, data = X.new, type = "l",
                xlab = x$var.names[i.var[i[1]]], ylab = paste("f(",
                  paste(x$var.names[i.var[1:3]], collapse = ","),
                  ")", sep = ""), ...)
        }
        else if (sum(f.factor) == 3) {
            stripplot(y ~ X1 | X2 * X3, data = X.new, xlab = x$var.names[i.var[i[1]]],
                ylab = paste("f(", paste(x$var.names[i.var[1:3]],
                  collapse = ","), ")", sep = ""), ...)
        }
    }
}
