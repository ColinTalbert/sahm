##These functions are all from the POC paper by Philips and Elith

calibplot <- function(pred, negrug, posrug, ideal, ylim=c(0,1),capuci=TRUE, xlabel = "Predicted probability of presence", filename=NULL, title="Calibration plot", ...) {
  if (!is.null(filename)) png(filename)
  ylow <- pred$y - 2 * pred$se
  ylow[ylow<0] <- 0
  yhigh <- pred$y + 2 * pred$se
  if (capuci) yhigh[yhigh>1] <- 1
  plot(pred$x, ylow, type="l", col="orange", ylim=ylim, xlim=range(pred$x),
    xlab=xlabel, lwd=2, ...)
  lines(pred$x, yhigh, lwd=2, col="orange")
  lines(pred$x, sapply(pred$x, ideal), lty="dashed")
  points(pred$x, pred$y, col="deepskyblue")
  rug(negrug)
  rug(posrug, col = "orange")
  title(title)
  if (!is.null(filename)) dev.off()
}

smoothingdf <- 6
smoothdist <- function(pred, res) {
  require(splines)
  gam1 <- glm(res ~ ns(pred, df=smoothingdf), weights=rep(1, length(pred)), family=binomial)
  x <- seq(min(pred), max(pred), length = 512)
  y <- predict(gam1, newdata = data.frame(pred = x), se.fit = TRUE,
    type = "response")
  data.frame(x=x, y=y$fit, se=y$se.fit)
}

# presence-only smoothed calibration plot
pocplot <- function(pred, back, linearize=TRUE, ...) {
  ispresence <- c(rep(1,length(pred)), rep(0, length(back)))
  predd <- smoothdist(c(pred,back), ispresence)
  c <- mean(back)*length(back)/length(pred)
  if (linearize) {
    fun <- function(x,y) c*y / (1-y)
    predd$y <- mapply(fun, predd$x, predd$y)
    predd$se <- mapply(fun, predd$x, predd$se)
    ideal <- function(x) x
    ylab <- "Relative probability of presence" 
  } 
  else {
    ideal <- function(x) x / (x + c)
    ylab <- "Probability of presence"
  }
  calibplot(predd, negrug=back, posrug=pred, ideal=ideal, ylab=ylab,
    capuci = FALSE, ...)
  predd
}

# presence-absence smoothed calibration plot
pacplot <- function(pred, pa, ...) {
  predd <- smoothdist(pred, pa)
  calibplot(predd, negrug=pred[pa==0], posrug=pred[pa==1], ideal=function(x) x, ylab="Probability of presence", ...)
}

# binned calibration plot with equal width bins
ecalp <- function(preds, acts, bins=10, do.plot=TRUE, do.clear=TRUE, filename=NULL, title="Binned calibration plot", ...){
  g <- floor(preds*bins)
  b <- 0:(bins-1)
  p <- sapply(b, function(x) if (length(acts[g==x])==0) -1 else sum(acts[g==x]) / length(acts[g==x]))
  mx <- sapply(b, function(x,g) mean(preds[g==x]), g)
  if(do.plot) {
    if (!is.null(filename)) png(filename)
    if (do.clear) {
      plot(mx, p, xlim=c(0,1), ylim=c(0,1), ...)
    } else {
      points(mx, p, xlim=c(0,1), ylim=c(0,1), ...)
    }
    rug(preds[acts==0])
    rug(preds[acts==1], col = "orange")
    abline(0,1,lty="dashed")
    title(title)
    if (!is.null(filename)) dev.off()
  }
  return(p)
}


# Code to recalibrate a model, without changing model discrimination
percent <- 95
rescaleinit <- function(plot) {
   mx <<- max(sapply(plot$y, function(y) y/(1-y)))
   len <<- length(plot$x)
   plotx <<- plot$x
   incr <<- isoreg(plot$y)$yf
}
rescale <- function(pred) {
    i <- findInterval(pred, plotx)
    if (i==0) i=1
    f <- incr[i]
    return((percent * (f / (mx * (1-f))) + (100-percent) * pred)/100)
}
