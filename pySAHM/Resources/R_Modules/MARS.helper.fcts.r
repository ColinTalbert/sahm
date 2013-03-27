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

###########################################################################################
#  The following functions are from Elith et al.
###########################################################################################
"mars.predict" <-
function (mars.glm.object,new.data)
{
#
# j leathwick, j elith - August 2006
#
# version 3.1 - developed in R 2.3.1 using mda 0.3-1
#
# calculates a mars/glm object in which basis functions are calculated
# using an initial mars model with single or multiple responses
# data for individual species are then fitted as glms using the
# common set of mars basis functions with results returned as a list
#
# takes as input a dataset and args selecting x and y variables, and degree of interaction
# along with site and species weights, the CV penalty, and the glm family arguments
# the latter would normally be one of "binomial" or "poisson" - "gaussian" could be used
# but in this case the model shouldn't differ from that fitted using mars on its own
#
# naming problem for dataframes fixed - je - 15/12/06
#
# requires mda and leathwick/elith's mars.export
#
  require(mda)

# first recreate both the original mars model and the glm model
    
# setup input data and create original temporary data
  dataframe.name <- mars.glm.object$mars.call$dataframe  # get the dataframe name
  mars.x <- mars.glm.object$mars.call$mars.x
  mars.y <- mars.glm.object$mars.call$mars.y
  n.spp <- length(mars.y)
  family <- mars.glm.object$mars.call$family
  mars.degree <- mars.glm.object$mars.call$degree
  penalty <- mars.glm.object$mars.call$penalty
  site.weights <- mars.glm.object$weights[[1]]
  spp.weights <- mars.glm.object$weights[[2]]
 
   base.data<-mars.glm.object$fit.dat
  
  x.temp <- eval(base.data[, mars.x])                 #form the temporary datasets
  base.names <- names(x.temp)

  xdat <- mars.new.dataframe(x.temp)[[1]]

  ydat <- as.data.frame(base.data[, mars.y])
  names(ydat) <- names(base.data)[mars.y]

  assign("xdat", xdat, pos = 1)               #and assign them for later use
  assign("ydat", ydat, pos = 1)

# now create the temporary dataframe for the new data
 
  new.names <- names(new.data)
  #selector <- na.rm(match(names(x.temp),names(new.data)))

  pred.dat <- mars.new.dataframe(new.data)[[1]]

  mars.fit <- mars.glm.object$mars.object  #AKS
  old.bf.data <- as.data.frame(eval(mars.fit$x))
  n.bfs <- ncol(old.bf.data)
  bf.names <- paste("bf", 1:n.bfs, sep = "")
  old.bf.data <- as.data.frame(old.bf.data[,-1])
  names(old.bf.data) <- bf.names[-1]
  new.bf.data <- as.data.frame(my.model.matrix.mars(mars.fit,new.data))
  new.bf.data <- as.data.frame(new.bf.data[,-1])
  names(new.bf.data) <- bf.names[-1]

# now cycle through the species fitting glm models

 
  prediction <- as.data.frame(matrix(0, ncol = n.spp, nrow = nrow(pred.dat)))
  names(prediction) <- names(ydat)
  standard.errors <- as.data.frame(matrix(0, ncol = n.spp, nrow = nrow(pred.dat)))
  names(standard.errors) <- names(ydat)

  for (i in 1:n.spp) {
    model.glm <- glm(ydat[, i] ~ ., data = old.bf.data, weights = site.weights,
      family = family, maxit = 100)
    temp <- predict.glm(model.glm,new.bf.data,type="response")
    prediction[,i] <- temp
    standard.errors[,i] <- temp[[2]]

    }

  return(list("prediction"=prediction))
}


"mars.contribs" <-
function (mars.glm.object,sp.no = 1, verbose = TRUE)
{

# j leathwick/j elith August 2006
#
# version 3.1 - developed in R 2.3.1 using mda 0.3-1
#
# takes a mars/glm model and uses the updated mars export table
# stored as the second list item from mars.binomial
# assessing the contribution of the fitted functions,
# amalgamating terms for variables as required
#
# amended 29/9/04 to pass original mars model details
# and to return f-statistics
#
# amended 7th January to accommodate any glm model family
#
# modified 050609 by aks to output a numeric deviance table #

  mars.detail <- mars.glm.object$mars.call
  pred.names <- mars.detail$predictor.base.names #get the names from the original data
  n.preds <- length(pred.names)

  spp.names <- mars.detail$y.names
  family <- mars.detail$family

  m.table <- mars.glm.object$mars.table[-1,]
  m.table$names1 <- as.character(m.table$names1)   #convert from a factor

  x.data <- as.data.frame(eval(mars.glm.object$basis.functions))
  y.data <- as.data.frame(eval(mars.glm.object$y.values))

  assign("x.data", x.data, pos = 1)
  assign("y.data", y.data, pos = 1)
  assign("sp.no", sp.no, pos = 1)

  glm.model <- glm(y.data[,sp.no] ~ .,data = x.data,family = family)

  n.bfs <- length(m.table[,1])

  delta.deviance <- rep(0,n.preds)
  df <- rep(0,n.preds)
  signif <- rep(0,n.preds)

  for (i in 1:n.preds) {   #start at two because first line is the constant

    # look for variable names in the table matching those in the var list

    var.nos <- grep(as.character(pred.names[i]),m.table$names1)

    #drop.list stores numbers of basis functions to be dropped
    if (length(var.nos) > 0) {
      drop.list <- 0 - var.nos
      x.data.new <- x.data[,drop.list]
      assign("x.data.new",x.data.new,pos=1)

     
      x.data.new<-as.data.frame(x.data.new)
      if(dim(x.data.new)[2]==0){
           new.model <- glm(y.data[,sp.no] ~ 1, family = family)
           }else new.model <- glm(y.data[,sp.no] ~ ., data=x.data.new, family = family)

      comparison <- anova(glm.model,new.model,test="Chisq")
     
      df[i] <- comparison[2,3]
      delta.deviance[i] <- zapsmall(comparison[2,4],4)
      signif[i] <- zapsmall(comparison[2,5],6)
    }

  }

  rm(x.data,y.data,sp.no,pos=1)  # tidy up temporary files

  #deviance.table <- as.data.frame(cbind(pred.names,delta.deviance,df,signif))
  #names(deviance.table) <- c("variable","delta_dev","deg. free.","p-value")
  deviance.table <- data.frame(variable=pred.names,delta_dev=delta.deviance,df=df,p_value=signif)#aks
  return(list(mars.call=mars.detail,deviance.table=deviance.table))
}


"mars.export" <-
function (object,lineage)
{
#
# j leathwick/j elith August 2006
#
# takes a mars model fitted using library mda
# and extracts the basis functions and their
# coefficients, returning them as a table
# caters for models with degree up to 2
#
# version 3.1 - developed in R 2.3.1 using mda 0.3-1

  which <- object$selected.terms
  nterms <- length(which)
  nspp <- ncol(eval(object$call$y))
  dir <- object$factor
  cut <- object$cuts
  var.names <- dimnames(object$factor)[[2]]
  p <- length(var.names)
  coefs <- as.data.frame(object$coefficients)
  names(coefs) <- names(eval(object$call$y))

# setup storage for results

  names1 <- rep("null", length = nterms)
  types1 <- rep("null", length = nterms)
  levels1 <- rep("null", length = nterms)
  signs1 <- rep(0, length = nterms)
  cuts1 <- rep(0, length = nterms)

  names2 <- rep("null", length = nterms)
  types2 <- rep("null", length = nterms)
  levels2 <- rep("null", length = nterms)
  signs2 <- rep(0, length = nterms)
  cuts2 <- rep(0, length = nterms)
  names1[1] <- "constant"
  signs1[1] <- 1

# now cycle through the terms
if(nterms>1){
  for (i in seq(2, nterms)) {
    j <- which[i]
      term.count = 1
      for (k in 1:p) {
        if (dir[j, k] != 0) {
          if (term.count == 1) {
            n <- match(var.names[k],lineage$full.name)
            names1[i] <- lineage$base.name[n] #var.names[k]
            types1[i] <- lineage$type[n]
            levels1[i] <- lineage$level[n]
            signs1[i] <- dir[j, k]
            cuts1[i] <- cut[j, k]
            term.count <- term.count + 1
          }
          else {
            names2[i] <- var.names[k]
            n <- match(var.names[k],lineage$full.name)
            names2[i] <- lineage$base.name[n] #var.names[k]
            types2[i] <- lineage$type[n]
            levels2[i] <- lineage$level[n]
            signs2[i] <- dir[j, k]
            cuts2[i] <- cut[j, k]
          }
        }
      }
    }
    }
  mars.export.table <- data.frame(names1, types1, levels1, signs1, cuts1,
       names2, types2, levels2, signs2, cuts2, coefs)

  return(mars.export.table)
}

"mars.glm" <-
function (data,                         # the input data frame
  mars.x,                               # column numbers of the predictors
  mars.y,                               # column number(s) of the response variable(s)
  mars.degree = 1,                      # level of interactions - 1 = zero, 2 = 1st order, etc
  site.weights = rep(1, nrow(data)),    # one weight per site
  spp.weights = rep(1,length(mars.y)),  # one wieght per species
  penalty = 2,                          # the default penaly for a mars model
  family =family)                  # the family for the glm model
{
#
# j leathwick, j elith - August 2006
#
# version 3.1 - developed in R 2.3.1 using mda 0.3-1
#
# calculates a mars/glm object in which basis functions are calculated
# using an initial mars model with single or multiple responses
# data for individual species are then fitted as glms using the
# common set of mars basis functions with results returned as a list
#
# takes as input a dataset and args selecting x and y variables, and degree of interaction
# along with site and species weights, the CV penalty, and the glm family argument
# the latter would normally be one of "binomial" or "poisson" - "gaussian" could be used
# but in this case the model shouldn't differ from that fitted using mars on its own
#
# requires mda and leathwick/elith's mars.export
#
# modified 3/11/04 to store information on glm phase convergence
# and with number of iterations raised to 100 to encourage convergence for low prevalence species
# modified 4/11/04 to accommodate observation weights in both mars and glm steps
# modified 12/04 to accommodate non-binomial families
# modified 11/05 to accommodate factor variables
# these are done as 0/1 dummy variables in a new dataframe
# created using mars.new.dataframe

  require(mda)

  n.spp <- length(mars.y)

# setup input data and assign to position one

  dataframe.name <- deparse(substitute(data))  # get the dataframe name

  xdat <- as.data.frame(eval(data[, mars.x]))                 #form the temporary datasets
  predictor.base.names <- names(xdat)

# create the new dataframe with dummy vars for factor predictors
  xdat <- mars.new.dataframe(xdat)
  lineage <- xdat[[2]]   # tracks which variables have had dummy's created
  xdat <- xdat[[1]]
  predictor.dummy.names <- names(xdat)

  ydat <- as.data.frame(eval(data[, mars.y]))
  names(ydat) <- names(data)[mars.y]

  assign("xdat", xdat, pos = 1)               #and assign them for later use
  assign("ydat", ydat, pos = 1)

# create storage space for glm model results

  n.cases <- nrow(xdat)

  fitted.values <- matrix(0,ncol = n.spp, nrow = n.cases)
  model.residuals <- matrix(0,ncol = n.spp, nrow = n.cases)
  null.deviances <- rep(0,n.spp)
  residual.deviances <- rep(0,n.spp)
  null.dfs <- rep(0,n.spp)
  residual.dfs <- rep(0,n.spp)
  converged <- rep(TRUE,n.spp)

# fit the mars model and extract the basis functions

   mars.object <- mars(x = xdat, y = ydat, degree = mars.degree, w = site.weights,
       wp = spp.weights, penalty = penalty)
  if(length(mars.object$coefficients)==1) stop("MARS has fit the null model (intercept only) \n new predictors are required")
  bf.data <- as.data.frame(eval(mars.object$x))
  n.bfs <- ncol(bf.data)
  bf.names <- paste("bf", 1:n.bfs, sep = "")
  names(bf.data) <- bf.names
  bf.data <- as.data.frame(bf.data[,-1])

  m.table <- as.data.frame(mars.export(mars.object,lineage))
  names(m.table)[(10 + 1):(10 + n.spp)] <- names(ydat)

  p.values <- matrix(0, ncol = n.spp, nrow = n.bfs)
  rownames(p.values) <- paste("bf", 1:n.bfs, sep = "")
  colnames(p.values) <- names(ydat)

# now cycle through the species fitting glm models

  for (i in 1:n.spp) {
    model.glm <- glm(ydat[, i] ~ ., data = bf.data, weights = site.weights,
  	  family = family, maxit = 100)

# update the coefficients and other results

    # then match names and insert as appropriate
    m.table[ , i + 10] <- 0   					# set all values to zero
    m.table[ , i + 10] <- model.glm$coefficients  	      # update all the constant
    sum.table <- summary(model.glm)$coefficients
    p.values[,i] <- sum.table[,4]
    fitted.values[,i] <- model.glm$fitted
    model.residuals[,i] <- resid(model.glm)
    null.deviances[i] <- model.glm$null.deviance
    residual.deviances[i] <- model.glm$deviance
    null.dfs[i] <- model.glm$df.null
    residual.dfs[i] <- model.glm$df.residual
    converged[i] <- model.glm$converged
  }

# now assemble data to be returned

  fitted.values <- as.data.frame(fitted.values)
  names(fitted.values) <- names(ydat)

  deviances <- data.frame(names(ydat),null.deviances,null.dfs,residual.deviances,residual.dfs,converged)
  names(deviances) <- c("species","null.dev","null.df","resid.dev","resid.df","converged")

  weights = list(site.weights = site.weights, spp.weights = spp.weights)

  mars.detail <- list(dataframe = dataframe.name, mars.x = mars.x,
    predictor.base.names = predictor.base.names, predictor.dummy.names = predictor.dummy.names,
    mars.y = mars.y, y.names = names(ydat), degree=mars.degree, penalty = penalty,
    family = family)

  rm(xdat,ydat,pos=1)           #finally, clean up the temporary dataframes

  return(list(mars.table = m.table, basis.functions = bf.data, y.values = ydat,
    fitted.values = fitted.values, residuals = model.residuals, weights = weights, deviances = deviances,
    p.values = p.values, mars.call = mars.detail,mars.object=mars.object,model.glm=model.glm))
}

"mars.new.dataframe" <-
function (input.data)
{
#
# j leathwick, j elith - August 2006
#
# version 3.1 - developed in R 2.3.1 using mda 0.3-1
#
# takes an input data frame and checks for factor variables
# converting these to dummy variables, one each for each factor level
# returning it for use with mars.glm so that factor vars can be included
# in a mars analysis
#

 

  n <- 1
  for (i in 1:ncol(input.data)) {  #first transfer the vector variables
    if (is.vector(input.data[,i])) {
      if (n == 1) {
        output.data <- as.data.frame(input.data[,i])
        names.list <- names(input.data)[i]
        var.type <- "vector"
        factor.level <- "na"
      }
      else {
        output.data[,n] <- input.data[,i]
        names.list <- c(names.list,names(input.data)[i])
        var.type <- c(var.type,"vector")
        factor.level <- c(factor.level,"na")
      }
      names(output.data)[n] <- names(input.data)[i]
      n <- n + 1
    }
  }

  for (i in 1:ncol(input.data)) {  # and then the factor variables
    if (is.factor(input.data[,i])) {
      temp.table <- summary(input.data[,i])
      for (j in 1:length(temp.table)) {
        names.list <- c(names.list,names(input.data)[i])
        var.type <- c(var.type,"factor")
        factor.level <- c(factor.level,names(temp.table)[j])
        output.data[,n] <- ifelse(input.data[,i] == names(temp.table)[j],1,0)
        names(output.data)[n] <- paste(names(input.data)[i],".",names(temp.table)[j],sep="")
        n <- n + 1
      }
    }
  }

  lineage <- data.frame(names(output.data),names.list,var.type,factor.level)
  for (i in 1:4) lineage[,i] <- as.character(lineage[,i])
  names(lineage) <- c("full.name","base.name","type","level")

  return(list(dataframe = output.data, lineage = lineage))
}

"mars.plot" <-
function (mars.glm.object,  #the input mars object
   sp.no = 0,               # the species number for multi-response models
   plot.rug=T,              # plot a rug of deciles
   plot.layout = c(3,4),    # the plot layout to use
   file.name = NA,          # giving a file name will send results to a pdf
   plot.it=T)               # option for making curves but no plots (aks)
{

# j leathwick/j elith August 2006
#
# version 3.1 - developed in R 2.3.1 using mda 0.3-1
#
# requires mars.export of leathwick/elith
#
# takes a mars or mars/glm model and either
# creates a mars export table (vanilla mars object)
# and works from this or uses the updated mars export table
# stored as the first list item from mars.binomial
# plotting out the fitted functions, amalgamating terms
# for variables and naming the pages as required
#
# caters for multispecies mars models by successively plotting
# all species unless a value other than zero is given for sp.no
#
# modified by aks 050609 to only open one plot window and use the record=T option.

  max.plots <- plot.layout[1] * plot.layout[2]
  if(plot.it){ #aks
      if (is.na(file.name)) {
        use.windows = TRUE
        windows(width = 11, height = 8, record=T) #AKS
        par(mfrow = plot.layout) #AKS
               }
      else {
        use.windows = FALSE
        pdf(file=file.name,width = 11, height=8)
        par(mfrow = plot.layout) #AKS
      }
  } else use.windows = FALSE

  if (class(mars.glm.object) == "mars") {  #then we have a mars object
    mars.binomial = FALSE
    model <- mars.glm.object
   
    xdat <- eval(model$call$x)
    Y <- as.data.frame(eval(model$call$y))
    n.env <- ncol(xdat)
    m.table <- mars.export(mars.glm.object)
  }
  else {
    mars.binomial = TRUE

    dat <- mars.glm.object$mars.call$dataframe
     xdat <- try(as.data.frame(eval(parse(text = dat))),silent=TRUE)
   if(class(xdat)=="try-error") xdat<-mars.glm.object$fit.dat
   
    mars.x <- mars.glm.object$mars.call$mars.x
    xdat <- xdat[,mars.x]

    m.table <- mars.glm.object[[1]]

  }

  n.bfs <- length(m.table[,1])
  n.spp <- length(m.table[1,]) - 10
  r.curves <- list(names=NULL,preds=list(),resp=list()) #aks
  spp.names <- names(m.table)[(10+1):(10+n.spp)]

  if (sp.no == 0) {
    wanted.species <- seq(1:n.spp)
  }
  else {
    wanted.species <- sp.no
  }

  xrange <- matrix(0,nrow = 2,ncol = ncol(xdat))
  factor.filter <- rep(FALSE,ncol(xdat))
  for (i in 1:ncol(xdat)) factor.filter[i] <- is.vector(xdat[,i])

  if(sum(factor.filter==FALSE)>0) {
  xrange[,factor.filter] <- sapply(xdat[,factor.filter], range)
  } else  xrange<-apply(xdat,2,range)

  for (i in wanted.species) {
    n.pages <- 1
    plotit <- rep(TRUE, n.bfs)
    print(paste("plotting responses for ",spp.names[i]),quote=F)
    nplots <- 0
    cntr <- 1 #aks
    for (j in 2:n.bfs) {
      if (m.table$names2[j] == "null") {
        if (plotit[j]) {
          varno <- pmatch(as.character(m.table$names1[j]),
          names(xdat))
          if (factor.filter[varno]) {
            Xi <- seq(xrange[1, varno], xrange[2, varno],
               length = 100)
            bf <- pmax(0, m.table$signs1[j] * (Xi - m.table$cuts1[j]))
            bf <- bf * m.table[j, i + 10]
            bf <- bf - mean(bf)
          }
          else {
            factor.table <- as.data.frame(table(xdat[,varno]))
            names(factor.table) <- c("levels","coefficients")
            factor.table$coefficients <- 0
            level.no <- match(m.table$levels1[j],factor.table$levels)
            factor.table$coefficients[level.no] <- m.table[j, i + 10]
          }
          if (j < n.bfs) {
            for (k in ((j + 1):n.bfs)) {
              if (m.table$names1[j] == m.table$names1[k] & m.table$names2[k] == "null") {
                    if (factor.filter[varno]) {
                      bf.add <- pmax(0, m.table$signs1[k] *
                          (Xi - m.table$cuts1[k]))
                      bf.add <- bf.add * m.table[k, i + 10]
                      bf <- bf + bf.add
                    }
                    else {
                      level.no <- match(m.table$levels1[k],factor.table$levels)
                      factor.table$coefficients[level.no] <- m.table[k, i + 10]
                    }
                    plotit[k] <- FALSE
              }
            }
          }


            if (factor.filter[varno]) {
              if(plot.it) plot(Xi, bf, type = "l", xlab = names(xdat)[varno], ylab = "response") #aks
              if (plot.rug & plot.it) rug(quantile(xdat[,varno], probs = seq(0, 1, 0.1), na.rm = FALSE))
              r.curves$preds[[cntr]] <- Xi #aks
              r.curves$resp[[cntr]] <- bf #aks
            }
            else {
              if(plot.it) plot(factor.table$levels, factor.table$coefficients, xlab = names(xdat)[varno]) #aks
              r.curves$preds[[cntr]] <- factor.table$levels #aks
              r.curves$resp[[cntr]] <- factor.table$coefficients #aks

            }
            r.curves$names <- c(r.curves$names,names(xdat)[varno])  #aks
            cntr <- cntr + 1  #aks
            nplots = nplots + 1
            plotit[j] <- FALSE
          }
        }
        else {  # case where there is an interaction #
          if (plotit[j]) {
            varno1 <- pmatch(as.character(m.table$names1[j]), names(xdat))
            X1 <- seq(xrange[1, varno1], xrange[2, varno1], length = 20)
            bf1 <- pmax(0, m.table$signs1[j] * (X1 - m.table$cuts1[j]))
            varno2 <- pmatch(as.character(m.table$names2[j]), names(xdat))
            X2 <- seq(xrange[1, varno2], xrange[2, varno2], length = 20)
            bf2 <- pmax(0, m.table$signs2[j] * (X2 - m.table$cuts2[j]))
            if(factor.filter[varno1] & factor.filter[varno2]){ #aks
                zmat <- bf1 %o% bf2
                zmat <- zmat * m.table[j, i + 10]
                if (j < n.bfs) {
                  for (k in ((j + 1):n.bfs)) {
                    if (m.table$names1[j] == m.table$names1[k] & m.table$names2[j] == m.table$names2[k]) {
                      bf1 <- pmax(0, m.table$signs1[k] * (X1 - m.table$cuts1[k]))
                      bf2 <- pmax(0, m.table$signs2[j] * (X2 - m.table$cuts2[j]))
                      zmat2 <- bf1 %o% bf2
                      zmat2 <- zmat2 * m.table[j, i + 10]
                      zmat = zmat + zmat2
                      plotit[k] <- FALSE
                    }
                  }
                }
              #if (nplots == 0) {  #AKS
    #            if (use.windows) windows(width = 11, height = 8)
    #            par(mfrow = plot.layout)
    #          }
                if(plot.it){
                    persp(x = X1, y = X2, z = zmat, xlab = names(xdat)[varno1],
                              ylab = names(xdat)[varno2], theta = 45, phi = 25) }
                r.curves$preds[[cntr]] <- X1 #aks
                r.curves$resp[[cntr]] <- apply(zmat,1,mean,na.rm=T) #aks
                nplots = nplots + 1
                } else {
                    r.curves$preds[[cntr]] <- NA #aks
                    r.curves$resp[[cntr]] <- NA #aks

                }
        r.curves$names <- c(r.curves$names,names(xdat)[varno]) #aks
        cntr <- cntr + 1 #aks
        }
      }
      if (nplots == 1 & plot.it) {
        title(paste(spp.names[i], " - page ", n.pages, sep = ""))
      }
      if (nplots == max.plots) {
        nplots = 0
        n.pages <- n.pages + 1
      }
    }
  }
  if (!use.windows & plot.it) dev.off()
  invisible(r.curves) #aks
}

"mars.plot.fits" <-
function(mars.glm.object,    # the input mars object
   sp.no = 0,                # allows selection of individual spp for multiresponse models
   mask.presence = FALSE,    # plots out just presence records
   use.factor = FALSE,       # draws plots as factors for faster printing
   plot.layout = c(4,2),     # the default plot layout
   file.name = NA)           # allows plotting to a pdf file
{
#
# j leathwick, j elith - August 2006
#
# version 3.1 - developed in R 2.3.1 using mda 0.3-1
#
# to plot distribution of fitted values in relation to ydat from mars or other p/a models
# allows masking out of absences to enable focus on sites with high predicted values
# fitted values = those from model; raw.values = original y values
# label = text species name; ydat = predictor dataset
# mask.presence forces function to only plot fitted values for presences
# use.factor forces to use quicker printing box and whisker plot
# file.name routes to a pdf file of this name
#

  max.plots <- plot.layout[1] * plot.layout[2]

  if (is.na(file.name)) {    #setup for windows or file output
    use.windows = TRUE
  }
  else {
    pdf(file.name, width=8, height = 11)
    par(mfrow = plot.layout)
    par(cex = 0.5)
    use.windows = FALSE
  }

  dat <- mars.glm.object$mars.call$dataframe    #get the dataframe name
  dat <- as.data.frame(eval(parse(text=dat)))   #and now the data

  n.cases <- nrow(dat)

  mars.call <- mars.glm.object$mars.call	#and the mars call details
  mars.x <- mars.call$mars.x
  mars.y <- mars.call$mars.y
  family <- mars.call$family

  xdat <- as.data.frame(dat[,mars.x])
  ydat <- as.data.frame(dat[,mars.y])

  n.spp <- ncol(ydat)
  n.preds <- ncol(xdat)

  fitted.values <- mars.glm.object$fitted.values

  pred.names <- names(dat)[mars.x]
  spp.names <- names(dat)[mars.y]

  if (sp.no == 0) {
    wanted.species <- seq(1:n.spp)
    }
  else {
    wanted.species <- sp.no
    }

  for (i in wanted.species) {

    if (mask.presence) {
	mask <- ydat[,i] == 1 }
    else {
      mask <- rep(TRUE, length = n.cases)
    }

    robust.max.fit <- approx(ppoints(fitted.values[mask,i]), sort(fitted.values[mask,i]), 0.99) #find 99%ile value
    nplots <- 0

    for (j in 1:n.preds) {
      if (use.windows & nplots == 0) {
        windows(width = 8, height = 11)
        par(mfrow = plot.layout)
        par(cex = 0.5)
      }
	nplots <- nplots + 1
      if (is.vector(xdat[,j])) wt.mean <- mean((xdat[mask, j] * fitted.values[mask, i]^5)/mean(fitted.values[mask, i]^5))
        else wt.mean <- 0
	if (use.factor) {
	temp <- factor(cut(xdat[mask, j], breaks = 12))
	if (family == "binomial") {
	  plot(temp, fitted.values[mask,i], xlab = pred.names[j], ylab = "fitted values", ylim = c(0, 1))
      }
	else {
	  plot(temp, fitted.values[mask,i], xlab = pred.names[j], ylab = "fitted values")}
	}
	else {
	  if (family == "binomial") {
	    plot(xdat[mask, j], fitted.values[mask,i], xlab = pred.names[j], ylab = "fitted values",
					ylim = c(0, 1))
        }
	  else {
          plot(xdat[mask, j], fitted.values[mask,i], xlab = pred.names[j], ylab = "fitted values")
        }
	}
	abline(h = (0.333 * robust.max.fit$y), lty = 2.)
	if (nplots == 1) {
  	  title(paste(spp.names[i], ", wtm = ", zapsmall(wt.mean, 4.)))}
	else {
	  title(paste("wtm = ", zapsmall(wt.mean, 4.)))}
	  nplots <- ifelse(nplots == max.plots, 0, nplots)
	}
    }
  if (!use.windows) dev.off()
}




pred.mars <- function(model,x) {
    # retrieve key items from the global environment #
    # make predictionss.
    y <- rep(NA,nrow(x))
    y[complete.cases(x)] <- as.vector(mars.predict(model,x[complete.cases(x),])$prediction[,1])

#which(is.na(y)
    # encode missing values as -1.
    y[is.na(y)]<- NaN

    # return predictions.
    return(y)
    }


my.model.matrix.mars<-function (object, x, which = object$selected.terms, full = FALSE, 
    ...) 
{
#I'm rewriting this model.matrix.mars which is used by the mars.predict function because
#it requires all preditors even those not selected by the model and this is inefficient for producing tiffs
    if (missing(x)) 
        return(object$x)
         nterms <- length(which)
         which <- which[-1] 
        x<-(mars.new.dataframe(x))[[1]]
    used.predictors<-which(apply(abs(object$factor[object$selected.terms,]),2,sum)!=0)
    x <- as.matrix(x[,match(names(used.predictors),colnames(x))])
    
    dir <- object$factor[which,match(names(used.predictors),colnames(object$factor))]
    cut <- object$cuts[which,match(names(used.predictors),colnames(object$factor))]
    n<-nrow(x)
    p<-ncol(x)
   bx <- matrix(1, nrow = n, ncol = nterms)
        for (i in seq(along = which)) {
        j <- which[i]
        if (all(dir[i, ] == 0)) {
            stop("error in factor or which")
        }
        temp1 <- 1
        for (k in 1:p) {
            if (dir[i, k] != 0) {
                temp2 <- dir[i, k] * (x[, k] - cut[i, k])
                temp1 <- temp1 * temp2 * (temp2 > 0)
            }
        }
       bx[, i + 1] <- temp1
    }
    bx
}
