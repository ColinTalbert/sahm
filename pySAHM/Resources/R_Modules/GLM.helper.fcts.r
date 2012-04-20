###############################################################################
##
## Copyright (C) 2010-2012, USGS Fort Collins Science Center. 
## All rights reserved.
## Contact: talbertc@usgs.gov
##
## This file is part of the Software for Assisted Habitat Modeling package
## for VisTrails.
##
## "Redistribution and use in source and binary forms, with or without 
## modification, are permitted provided that the following conditions are met:
##
##  - Redistributions of source code must retain the above copyright notice, 
##    this list of conditions and the following disclaimer.
##  - Redistributions in binary form must reproduce the above copyright 
##    notice, this list of conditions and the following disclaimer in the 
##    documentation and/or other materials provided with the distribution.
##  - Neither the name of the University of Utah nor the names of its 
##    contributors may be used to endorse or promote products derived from 
##    this software without specific prior written permission.
##
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
## AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, 
## THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
## PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR 
## CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, 
## EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
## PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; 
## OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
## WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR 
## OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
## ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
##
## Although this program has been used by the U.S. Geological Survey (USGS), 
## no warranty, expressed or implied, is made by the USGS or the 
## U.S. Government as to the accuracy and functioning of the program and 
## related program material nor shall the fact of distribution constitute 
## any such warranty, and no responsibility is assumed by the USGS 
## in connection therewith.
##
## Any use of trade, firm, or product names is for descriptive purposes only 
## and does not imply endorsement by the U.S. Government.
###############################################################################

my.termplot <- function (model, data = NULL, envir = environment(formula(model)),
    partial.resid = FALSE, rug = FALSE, terms = NULL, se = FALSE,
    xlabs = NULL, ylabs = NULL, main = NULL, col.term = 2, lwd.term = 1.5,
    col.se = "orange", lty.se = 2, lwd.se = 1, col.res = "gray",
    cex = 1, pch = 1, col.smth = "darkred", lty.smth = 2,
    span.smth = 2/3, ask = FALSE,
    use.factor.levels = TRUE, smooth = NULL, ylim = "common",plot.it=F,
    ...)
{   # this function is borrowed from the stats library #

    which.terms <- terms
    terms <- if (is.null(terms))
        predict(model, type = "terms", se.fit = se)
    else predict(model, type = "terms", se.fit = se, terms = terms)
    n.tms <- ncol(tms <- as.matrix(if (se)
        terms$fit
    else terms))
    mf <- model.frame(model)
    if (is.null(data))
        data <- eval(model$call$data, envir)
    if (is.null(data))
        data <- mf
    if (NROW(tms) < NROW(data)) {
        use.rows <- match(rownames(tms), rownames(data))
    }
    else use.rows <- NULL
    nmt <- colnames(tms)
    cn <- parse(text = nmt)
    if (!is.null(smooth))
        smooth <- match.fun(smooth)
    if (is.null(ylabs))
        ylabs <- paste("Partial for", nmt)
    if (is.null(main))
        main <- ""
    else if (is.logical(main))
        main <- if (main)
            deparse(model$call, 500)
        else ""
    else if (!is.character(main))
        stop("'main' must be TRUE, FALSE, NULL or character (vector).")
    main <- rep(main, length.out = n.tms)
    pf <- envir
    carrier <- function(term) {
        if (length(term) > 1)
            carrier(term[[2]])
        else eval(term, data, enclos = pf)
    }
    carrier.name <- function(term) {
        if (length(term) > 1)
            carrier.name(term[[2]])
        else as.character(term)
    }
    if (is.null(xlabs))
        xlabs <- unlist(lapply(cn, carrier.name))
    if (partial.resid || !is.null(smooth)) {
        pres <- residuals(model, "partial")
        if (!is.null(which.terms))
            pres <- pres[, which.terms, drop = FALSE]
    }
    is.fac <- sapply(nmt, function(i) is.factor(mf[, i]))
    se.lines <- function(x, iy, i, ff = 2) {
        tt <- ff * terms$se.fit[iy, i]
        lines(x, tms[iy, i] + tt, lty = lty.se, lwd = lwd.se,
            col = col.se)
        lines(x, tms[iy, i] - tt, lty = lty.se, lwd = lwd.se,
            col = col.se)
    }
    if(plot.it) nb.fig <- prod(par("mfcol"))
    if (ask & plot.it) {
        oask <- devAskNewPage(TRUE)
        on.exit(devAskNewPage(oask))
    }
    ylims <- ylim
    if (identical(ylims, "common")) {
        ylims <- if (!se)
            range(tms, na.rm = TRUE)
        else range(tms + 1.05 * 2 * terms$se.fit, tms - 1.05 *
            2 * terms$se.fit, na.rm = TRUE)
        if (partial.resid)
            ylims <- range(ylims, pres, na.rm = TRUE)
        if (rug)
            ylims[1] <- ylims[1] - 0.07 * diff(ylims)
    }
    preds <- list()
    response <- list()
    p.names <- xlabs
    for (i in 1:n.tms) {
        if (identical(ylim, "free")) {
            ylims <- range(tms[, i], na.rm = TRUE)
            if (se)
                ylims <- range(ylims, tms[, i] + 1.05 * 2 * terms$se.fit[,
                  i], tms[, i] - 1.05 * 2 * terms$se.fit[, i],
                  na.rm = TRUE)
            if (partial.resid)
                ylims <- range(ylims, pres[, i], na.rm = TRUE)
            if (rug)
                ylims[1] <- ylims[1] - 0.07 * diff(ylims)
        }
        if (is.fac[i]) {
            ff <- mf[, nmt[i]]
            if (!is.null(model$na.action))
                ff <- naresid(model$na.action, ff)
            preds[[i]] <- ll <- levels(ff)
            xlims <- range(seq_along(ll)) + c(-0.5, 0.5)
            xx <- as.numeric(ff)
            response[[i]]<-rep(NA,length(ll))
            if (rug) {
                xlims[1] <- xlims[1] - 0.07 * diff(xlims)
                xlims[2] <- xlims[2] + 0.03 * diff(xlims)
            }

            if(plot.it){
                plot(1, 0, type = "n", xlab = xlabs[i], ylab = ylabs[i],
                    xlim = xlims, ylim = ylims, main = main[i], xaxt = "n",
                    ...)
                if (use.factor.levels)
                    axis(1, at = seq_along(ll), labels = ll, ...)
                else axis(1)
                for (j in seq_along(ll)) {
                    ww <- which(ff == ll[j])[c(1, 1)]
                    jf <- j + c(-0.4, 0.4)
                    lines(jf, response[[i]][j]<-tms[ww, i], col = col.term, lwd = lwd.term,...)
                    if (se) se.lines(jf, iy = ww, i = i)
                    }
            } else {
            for (j in seq_along(ll)) {
                    ww <- which(ff == ll[j])[c(1, 1)]
                    response[[i]][j]<-tms[ww, i]
                    }
                }
        }
        else {
            xx <- carrier(cn[[i]])
            if (!is.null(use.rows))
                xx <- xx[use.rows]
            xlims <- range(xx, na.rm = TRUE)
            if (rug)
                xlims[1] <- xlims[1] - 0.07 * diff(xlims)

            response[[i]]<-  logit(seq(min(tms[,i]),max(tms[,i]),length=100))  #aks
            preds[[i]]<-seq(min(xx),max(xx),length=100) #aks
            if(plot.it){
                 oo <- order(xx)
                 plot(xx[oo], logit(tms[oo, i]), type = "l", xlab = xlabs[i],
                    ylab = ylabs[i], xlim = xlims, ylim = logit(ylims),
                    main = main[i], col = col.term, lwd = lwd.term,
                    ...)
                if (se)
                    se.lines(xx[oo], iy = oo, i = i)
                }
        }
        if(plot.it){
            if (partial.resid) {
                if (!is.fac[i] && !is.null(smooth)) {
                    smooth(xx, pres[, i], lty = lty.smth, cex = cex,
                      pch = pch, col = col.res, col.smooth = col.smth,
                      span = span.smth)
                }
                else points(xx, pres[, i], cex = cex, pch = pch,
                    col = col.res)
            }
            if (rug) {
                n <- length(xx)
                lines(rep.int(jitter(xx), rep.int(3, n)), rep.int(ylims[1] +
                    c(0, 0.05, NA) * diff(ylims), n))
                if (partial.resid)
                    lines(rep.int(xlims[1] + c(0, 0.05, NA) * diff(xlims),
                      n), rep.int(pres[, i], rep.int(3, n)))
            }
        }
    }
    invisible(list(names=p.names,preds=preds,resp=response))
}
