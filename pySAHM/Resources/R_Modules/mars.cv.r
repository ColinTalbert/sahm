"mars.cv" <-
function (mars.glm.object, out, sp.no = 1, prev.stratify = F)
{
#
# j. leathwick/j. elith - August 2006
#
# version 3.1 - developed in R 2.3.1 using mda 0.3-1
#
# function to perform k-fold cross validation
# with full model perturbation for each subset
#
# requires mda library from Cran
# requires functions mw and calibration
#
# takes a mars/glm object produced by mars.glm
# and first assesses the full model, and then
# randomly subsets the dataset into nk folds and drops
# each subset in turn, fitting on remaining data
# and predicting for withheld data
#
# caters for both single species and community models via the argument sp.no
# for the first, sp.no can be left on its default of 1
# for community models, sp.no can be varied from 1 to n.spp
#
# modified 29/9/04 to
#   1. return mars analysis details for audit trail
#   2. calculate roc and calibration on subsets as well as full data
#      returning the mean and se of the ROC scores
#      and the mean calibration statistics
#
# modified 8/10/04 to add prevalence stratification
# modified 7th January to test for binomial family and return if not
#
# updated 15th March to cater for both binomial and poisson families
#
# updated 16th June 2005 to calculate residual deviance
#
  data <- mars.glm.object$mars.call$dataframe    #get the dataframe name
  dataframe.name <- deparse(substitute(data))

  data <- as.data.frame(eval(parse(text=data)))   #and now the data
  n.cases <- nrow(data)

  mars.call <- mars.glm.object$mars.call          #and the mars call details
  mars.x <- mars.call$mars.x
  mars.y <- mars.call$mars.y
  mars.degree <- mars.call$degree
  mars.penalty <- mars.call$penalty
  family <- mars.call$family
  site.weights <- eval(mars.glm.object$weights$site.weights)

  n.spp <- length(mars.y)

  if (sp.no > n.spp) {
    print(paste("the value specified for sp.no of",sp.no),quote=F)
    print(paste("exceeds the total number of species, which is ",n.spp),quote=F)
    return()
  }

  xdat <- as.data.frame(data[,mars.x])
  xdat <- mars.new.dataframe(xdat)[[1]]
  ydat <- mars.glm.object$y.values[,sp.no]
  target.sp <- names(data)[mars.y[sp.no]]

#  if (prev.stratify) {
#    presence.mask <- ydat == 1
#    absence.mask <- ydat == 0
#    n.pres <- sum(presence.mask)
#    n.abs <- sum(absence.mask)
#  }

  print(paste("Calculating ROC and calibration from full model for",target.sp),quote=F)

  u_i <- mars.glm.object$fitted.values[,sp.no]
  y_i <- ydat

  if (family == "binomial") {
    full.resid.deviance <- calc.deviance(y_i,u_i, weights = site.weights, family="binomial")
    full.test <- roc(y_i, u_i)
    full.calib <- calibration(y_i, u_i)
  }

  if (family=="poisson") {
    full.resid.deviance <- calc.deviance(y_i,u_i, weights = site.weights, family="poisson")
    full.test <- cor(y_i, u_i)
    full.calib <- calibration(y_i, u_i, family = "poisson")
  }

# set up for results storage
   nk<-length(out$dat$ma)-1
  subset.test <- rep(0,nk)
  subset.calib <- as.data.frame(matrix(0,ncol=5,nrow=nk))
  names(subset.calib) <- c("intercept","slope","test1","test2","test3")
  subset.resid.deviance <- rep(0,nk)

# now setup for withholding random subsets

  pred.values <- rep(0, n.cases)
  fitted.values <- rep(0, n.cases)

  print("", quote = FALSE)
  print("Creating predictions for subsets...", quote = F)

 selector<-out$dat$selector
  for (i in 1:nk) {
    cat(i," ")
    model.mask <- selector != i  #used to fit model on majority of data
    pred.mask <- selector == i   #used to identify the with-held subset
    assign("species.subset", ydat[model.mask], pos = 1)
    assign("predictor.subset", xdat[model.mask, ], pos = 1)

    # fit new mars model

    mars.object <- mars(y = species.subset, x = predictor.subset,
      degree = mars.degree, penalty = mars.penalty)

    # and extract basis functions

    n.bfs <- length(mars.object$selected.terms)
    bf.data <- as.data.frame(mars.object$x)
    names(bf.data) <- paste("bf",1:n.bfs,sep="")
    assign("bf.data", bf.data, pos=1)

    # then fit a binomial model to them

    mars.binomial <- glm(species.subset ~ .,data=bf.data[,-1], family= family, maxit = 100)

    pred.basis.functions <- as.data.frame(mda:::model.matrix.mars(mars.object,
      xdat[pred.mask, ]))

    #now name the bfs to match the approach used in mars.binomial

    names(pred.basis.functions) <- paste("bf",1:n.bfs,sep="")

    # and form predictions for them and evaluate performance

    fitted.values[pred.mask] <- out$dat$ma[[i]]$pred<-predict(mars.binomial,
      pred.basis.functions, type = "response")

    # calculate the threshold seperately based on each train set
    out$dat$ma[[i]]$thresh <- as.numeric(optimal.thresholds(data.frame(ID=1:length(species.subset),pres.abs=species.subset,
                pred=mars.binomial$fitted.values),opt.methods=out$input$opt.methods))[2]

    y_i <- ydat[pred.mask]
    u_i <- fitted.values[pred.mask]
    weights.subset <- site.weights[pred.mask]

    if (family == "binomial") {
      subset.resid.deviance[i] <- calc.deviance(y_i,u_i,weights = weights.subset, family="binomial")
      subset.test[i] <- roc(y_i,u_i)
      subset.calib[i,] <- calibration(y_i, u_i)
    }

    if (family=="poisson"){
      subset.resid.deviance[i] <- calc.deviance(y_i,u_i,weights = weights.subset, family="poisson")
      subset.test[i] <- cor(y_i, u_i)
      subset.calib[i,] <- calibration(y_i, u_i, family = family)
    }
  }

  cat("","\n")

# tidy up temporary files

  rm(species.subset,predictor.subset,bf.data,pos=1)

# and assemble results for return

#  mars.detail <- list(dataframe = dataframe.name,
#    x = mars.x, x.names = names(xdat),
#    y = mars.y, y.names = names(data)[mars.y],
#    target.sp = target.sp, degree=mars.degree, penalty = mars.penalty, family = family)

  y_i <- ydat
  u_i <- fitted.values

  if (family=="binomial") {
    cv.resid.deviance <- calc.deviance(y_i,u_i,weights = site.weights, family="binomial")
    cv.test <- roc(y_i, u_i)
    cv.calib <- calibration(y_i, u_i)
  }

  if (family=="poisson"){
    cv.resid.deviance <- calc.deviance(y_i,u_i,weights = site.weights, family="poisson")
    cv.test <- cor(y_i, u_i)
    cv.calib <- calibration(y_i, u_i, family = "poisson")
  }

  subset.test.mean <- mean(subset.test)
  subset.test.se <- sqrt(var(subset.test))/sqrt(nk)

  subset.test <- list(test.scores = subset.test, subset.test.mean = subset.test.mean,
    subset.test.se = subset.test.se)

  subset.calib.mean <- apply(subset.calib[,c(1:2)],2,mean)
  names(subset.calib.mean) <- names(subset.calib)[c(1:2)] #mean only of parameters

  subset.calib <- list(subset.calib = subset.calib,
    subset.calib.mean = subset.calib.mean)

  subset.deviance.mean <- mean(subset.resid.deviance)
  subset.deviance.se <- sqrt(var(subset.resid.deviance))/sqrt(nk)

   subset.deviance <- list(subset.deviances = subset.resid.deviance, subset.deviance.mean = subset.deviance.mean,
    subset.deviance.se = subset.deviance.se)
    
  cv.list<-list(mars.call = mars.call, full.resid.deviance = full.resid.deviance,
    full.test = full.test, full.calib = full.calib, pooled.deviance = cv.resid.deviance, pooled.test = cv.test,
    pooled.calib = cv.calib,subset.deviance = subset.deviance, subset.test = subset.test, subset.calib = subset.calib)
   out$cv<-cv.list
  return(out)
}
