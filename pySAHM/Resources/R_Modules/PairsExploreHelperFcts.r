  ## put histograms on the diagonal
panel.hist <- function(x, ...)
    {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(usr[1:2], 0, 1.5) )
        h <- hist(x, plot = FALSE)
        breaks <- h$breaks; nB <- length(breaks)
        y <- h$counts; y <- y/max(y)
        rect(breaks[-nB], 0, breaks[-1], y, col="steelblue", ...)
    }

## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor <- function(x, y, digits=2, prefix="", cor.range,cor.mult, ...)
      {
      a<-colors()
          usr <- par("usr"); on.exit(par(usr))
          par(usr = c(0, 1, 0, 1))
          r <- abs(cor(x, y,use="pairwise.complete.obs"))
          spear<-abs(cor(x,y,method="spearman",use="pairwise.complete.obs"))
          ken<- abs(cor(x,y,method="kendall",use="pairwise.complete.obs"))
          all.cor<-max(r,spear,ken)
               ramp<-heat.colors(20, alpha = .7)[20:1]
          if(all.cor>=.6){
            rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =
            ramp[which.min(abs(all.cor-seq(from=.65,to=1,length=20)))])}
          r<-max(all.cor)
               cex.cor=3*cor.mult
         txt <- format(c(r, 0.123456789), digits=digits)[1]
          txt <- paste(prefix, txt, sep="")
           #if(missing(cex.cor)) cex.cor <- 1.2/strwidth(txt)

              txt2=""
            if(max(all.cor)>cor.range[2]){
            if(spear==max(all.cor) && spear!=cor(x,y,use="pairwise.complete.obs")) {txt2 <- " s"
              } else if(ken==max(all.cor) && ken!=cor(x,y,use="pairwise.complete.obs")){
              txt2 <-" k"
              }

         }
          text(0.5, 0.5, txt, cex = .7+cex.cor * (r-min(cor.range))/(max(cor.range)-min(cor.range)))
          text(.9,.1,txt2,cex=cor.mult)
         }

### and the big function
MyPairs<-function(x,missing.summary,my.labels,labels, panel = points, ..., lower.panel = panel,
    upper.panel = panel,diag.panel = NULL, text.panel = textPanel,
    label.pos = 0.5 + has.diag/3, cex.labels = NULL, font.labels = 1,
    row1attop = TRUE, gap = 1,cex.mult,famly="binomial",for.dev)
{
    response<-x[,1]
    response[response==-9999]<-0
    x<-x[,2:dim(x)[2]]

    textPanel <- function(x = 0.5, y = 0.5, txt, cex, font) text(x,
        y, txt, cex = cex, font = font)
    localAxis <- function(side, x, y, xpd, bg, col = NULL, main,
        oma, ...) {
        if (side%%2 == 1)
            Axis(x, side = side, xpd = NA, ...)
        else Axis(y, side = side, xpd = NA, ...)
    }
    localPlot <- function(..., main, oma, font.main, cex.main) plot(...)
    localLowerPanel <- function(..., main, oma, font.main, cex.main) lower.panel(...)
    localUpperPanel <- function(..., main, oma, font.main, cex.main) upper.panel(...)
    localDiagPanel <- function(..., main, oma, font.main, cex.main) diag.panel(...)

    dots <- list(...)
    nmdots <- names(dots)
    if (!is.matrix(x)) {
        x <- as.data.frame(x)
        for (i in seq_along(names(x))) {
            if (is.factor(x[[i]]) || is.logical(x[[i]]))
                x[[i]] <- as.numeric(x[[i]])
            if (!is.numeric(unclass(x[[i]])))
                stop("non-numeric argument to 'pairs'")
        }
    } else if (!is.numeric(x))
        stop("non-numeric argument to 'pairs'")
    panel <- match.fun(panel)
    if ((has.lower <- !is.null(lower.panel)) && !missing(lower.panel))
        lower.panel <- match.fun(lower.panel)
    if ((has.upper <- !is.null(upper.panel)) && !missing(upper.panel))
        upper.panel <- match.fun(upper.panel)
    if ((has.diag <- !is.null(diag.panel)) && !missing(diag.panel))
        diag.panel <- match.fun(diag.panel)
    if (row1attop) {
        tmp <- lower.panel
        lower.panel <- upper.panel
        upper.panel <- tmp
        tmp <- has.lower
        has.lower <- has.upper
        has.upper <- tmp
    }
    nc <- ncol(x)
    if (nc < 2)
        stop("only one column in the argument to 'pairs'")
    has.labs <- TRUE
    if (missing(labels)) {
        labels <- colnames(x)
        if (is.null(labels))
            labels <- paste("var", 1L:nc)
    }
    else if (is.null(labels))
        has.labs <- FALSE
    oma <- if ("oma" %in% nmdots)
        dots$oma
    else NULL
    main <- if ("main" %in% nmdots)
        dots$main
    else NULL
    if (is.null(oma)) {
        oma <- c(4, 4, 4, 4)
        if (!is.null(main))
            oma[3L] <- 6
    }
    nCol<-ifelse(length(unique(response))>1,nc+1,nc)
    j.start<-ifelse(length(unique(response))>1,0,1)
    opar <- par(mfrow = c(nc, nCol), mar = rep.int(gap/2, 4))
    on.exit(par(opar))
    par(oma=oma)
    for (i in if (row1attop)
        1L:(nc)
    else nc:1L) for (j in j.start:(nc)) {
       top.gap<-c(rep(gap/2,times=nc))
       bottom.gap<-c(rep(gap/2,times=nc-1),3*gap)
       left.gap<-c(3*gap,3*gap,rep(gap/2,times=nc-1))
       par(mar = c(bottom.gap[i],left.gap[j+1],top.gap[i],gap/2))



         if(j==0){
         localPlot(x[, i],response, xlab = "", ylab = "", axes = FALSE,
                type="n",...)
          if(i==1) mtext("Response",line=.3,cex=.7*cex.mult)

                  box()
                     my.lab<-paste("cor=",round(max(abs(cor(x[,(i)],response,use="pairwise.complete.obs")),abs(cor(x[,(i)],response,method="spearman",use="pairwise.complete.obs")),
                     abs(cor(x[,(i)],response,method="kendall",use="pairwise.complete.obs"))),digits=2),sep="")
                   if(famly=="gaussian") {panel.smooth(as.vector(x[, (i)]), as.vector(response),...)
                      title(ylab=paste("cor=",round(max(abs(cor(x[,(i)],response,use="pairwise.complete.obs")),
                          abs(cor(x[,(i)],response,method="spearman",use="pairwise.complete.obs")),abs(cor(x[,(i)],response,method="kendall",use="pairwise.complete.obs"))),digits=2),
                          sep=""),line=.02,cex.lab=1.5)
                   }  else if(missing(for.dev)) pct.dev<-try(my.panel.smooth(as.vector(x[, (i)]), response,cex.mult=cex.mult,cex.lab=cex.mult,line=1,famly=famly,...),silent=TRUE)
                              else  pct.dev<-try(my.panel.smooth(as.vector(for.dev$dat[, (i)]), as.vector(for.dev$response),cex.mult=cex.mult,cex.lab=cex.mult,line=1,famly=famly,...),silent=TRUE)
                 } else{

             localPlot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE,
           type="n",...)
        if (i == j || (i < j && has.lower) || (i > j && has.upper)) {
            box()
            if(i==1) {
            mtext(paste("Total Cor=",my.labels[j],sep=""),side=3,line=ifelse(missing.summary[j]>.03,3,.3),cex=.65*cex.mult)
            if(missing.summary[j]>.03) mtext(paste(round(missing.summary[j]*100), "% missing",sep=""),side=3,line=.3,cex=cex.mult*.55)
            }
            if (i == nc)
                localAxis(3 - 2 * row1attop, x[, j], x[, i],cex.axis=cex.mult*.5,
                  ...)
            if (j == 1 && (i!=1 || !has.upper || !has.lower))
                localAxis(2, x[, j], x[, i],cex.axis=cex.mult*.5, ...)

            mfg <- par("mfg")
            if (i == j) {
                if (has.diag)
                  localDiagPanel(as.vector(x[, i]),...)
                if (has.labs) {
                  par(usr = c(0, 1, 0, 1))
                  if(i==1){
                     for(k in 1:length(labels)){
                         if((lng<-nchar(labels[k]))>=14) labels[k]<-paste(substr(labels[k],1,12),"\n",substr(labels[k],13,lng),sep="")
                     }
                       if (is.null(cex.labels)) {
                          l.wid <- strwidth(labels, "user")
                          cex.labels <- max(0.8, min(2, 0.9/max(l.wid)))
                      }
                  }
                  text.panel(0.5, label.pos, labels[i], cex = .45*cex.labels*cex.mult,
                    font = font.labels)
               }
            }
            else if (i < j)
                  if(length(unique(x[,i])>2)){
                  localLowerPanel(as.vector(x[, j]), as.vector(x[,
                    i]),cex=cex.mult*3,cor.mult=cex.mult,...) } else {
                       if(missing(for.dev)) pct.dev<-try(my.panel.smooth(as.vector(x[,j]), as.vector(x[,i]),cex.mult=cex.mult,cex.lab=cex.mult,line=1,famly=famly,...),silent=TRUE)
                              else  pct.dev<-try(my.panel.smooth(as.vector(for.dev$dat[, (i)]), as.vector(for.dev$response),cex.mult=cex.mult,cex.lab=cex.mult,line=1,famly=famly,...),silent=TRUE)
                    }
            else {
            localUpperPanel(as.vector(x[, j]), as.vector(x[,
                i]),cex=cex.mult,...)
                }
            if (any(par("mfg") != mfg))
                stop("the 'panel' function made a new plot")
        }
        else par(new = FALSE)
    }}
    if (!is.null(main)) {
        font.main <- if ("font.main" %in% nmdots)
            dots$font.main
        else par("font.main")
        cex.main <- if ("cex.main" %in% nmdots)
            dots$cex.main
        else par("cex.main")
        mtext(main, 3, 3, TRUE, 0.5, cex = cex.main, font = font.main)
    }
    invisible(NULL)
}
