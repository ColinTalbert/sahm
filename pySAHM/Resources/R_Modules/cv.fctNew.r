"cv.fct" <-
function (fit.object, out, sp.no = 1, prev.stratify = F,Model=Model)
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
attach(out$input)
on.exit(detach(out$input))
if(Model=="mars"){
      Model.Call <- fit.object$mars.call          #and the mars call details
      mars.degree <- Model.Call$degree
      mars.penalty <- Model.Call$penalty
        mars.x <- Model.Call$mars.x
        mars.y <- Model.Call$mars.y

      n.spp <- length(mars.y)

      if (sp.no > n.spp) stop(paste("the value specified for sp.no of",sp.no,"exceeds the total number of species, which is ",n.spp,sep=" "))


      target.sp <- names(data)[mars.y[sp.no]]
      u_i <- fit.object$fitted.values[,sp.no]
  }

data<-out$dat$ma$train
n.cases<-nrow(out$dat$ma$train$dat)
xdat<-out$dat$ma$train$dat[,2:ncol(out$dat$ma$train$dat)]
ydat<-out$dat$ma$train$dat[,1]
u_i<-out$dat$ma$train$pred
family<-out$input$model.family
site.weights<-out$dat$ma$train$weight

if(Model=="mars") n.spp <- length(Model.Call$mars.y)
else{ n.spp=1
target.sp<-names(ydat)
}
  ############################################################
  print(paste("Calculating ROC and calibration from full model for",target.sp),quote=F)


  y_i <- ydat

  if (family == "binomial" | family=="bernoulli") {
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

if(Model=="mars"){    mars.object <- mars(y = species.subset, x = predictor.subset,
                      degree = mars.degree, penalty = mars.penalty)
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
                    fitted.values[pred.mask] <- out$dat$ma[[i]]$pred<-predict(mars.binomial,
               pred.basis.functions, type = "response")
       out$dat$ma[[i]]$thresh <- as.numeric(optimal.thresholds(data.frame(ID=1:length(species.subset),pres.abs=species.subset,
                pred=mars.binomial$fitted.values),opt.methods=out$input$opt.methods))[2]
      }
if(Model=="glm"){
    penalty <- if(out$input$simp.method=="AIC") 2 else log(nrow(out$dat$ma$ma))
          scope.glm <- list(lower=as.formula(paste("species.subset","~1")),
          upper=as.formula(paste("response","~",paste(out$dat$used.covs,collapse='+'))))

          mymodel.glm.step <- step(glm(as.formula(paste("species.subset","~1")),family=out$input$model.family,data=cbind(species.subset,predictor.subset),
                           weights=site.weights[model.mask],na.action="na.exclude"),
                           direction='both',scope=scope.glm,trace=0,k=penalty)
           fitted.values[pred.mask]<-out$dat$ma[[i]]$pred<-predict(mymodel.glm.step, xdat[pred.mask, ],type="response")
           out$dat$ma[[i]]$thresh <- as.numeric(optimal.thresholds(data.frame(ID=1:length(species.subset),pres.abs=species.subset,
                pred=mymodel.glm.step$fitted.values),opt.methods=out$input$opt.methods))[2]
          }
if(Model=="brt"){
            if(debug.mode) assign("out",out,envir=.GlobalEnv)

            cat("\nfinished with learning rate estimation, lr=",out$mods$lr.mod$lr0,", t=",round(out$mods$lr.mod$t.elapsed,2),"sec\n")
            cat("\nfor final fit, lr=",out$mods$lr.mod$lr,"and tc=",out$mods$parms$tc.full,"\n");flush.console()

                # remove variables with <1% relative influence and re-fit model

                t1 <- unclass(Sys.time())
                    if(length(out$mods$lr.mod$good.cols)<=1) stop("BRT must have at least two independent variables")
                    out$input$max.trees<-NULL
                m0 <- gbm.step.fast(dat=cbind(species.subset,predictor.subset),gbm.x=out$mods$lr.mod$good.cols,gbm.y=1,family=out$input$model.family,
                      n.trees = c(300,600,800,1000,1200,1500,1800),step.size=out$input$step.size,max.trees=out$input$max.trees,
                      tolerance.method=out$input$tolerance.method,tolerance=out$input$tolerance, n.folds=out$input$n.folds,tree.complexity=out$mods$parms$tc.sub,
                      learning.rate=out$mods$lr.mod$lr0,bag.fraction=out$input$bag.fraction,site.weights=site.weights[model.mask],autostop=T,debug.mode=F,silent=!debug.mode,
                      plot.main=F,superfast=F)
                      if(debug.mode) assign("m0",m0,envir=.GlobalEnv)

                      t1b <- unclass(Sys.time())

                simp.mod<- gbm.simplify(m0,n.folds=out$input$n.folds,plot=F,verbose=F,alpha=out$input$alpha) # this step is very slow #
                      if(debug.mode) assign("out",out,envir=.GlobalEnv)

                 cv.final.mod <- gbm.step.fast(dat=cbind(species.subset,predictor.subset),gbm.x=simp.mod$pred.list[[length(simp.mod$pred.list)]],gbm.y = 1,family=out$input$model.family,
                  n.trees = c(300,600,700,800,900,1000,1200,1500,1800,2200,2600,3000,3500,4000,4500,5000),n.folds=out$input$n.folds,
                  tree.complexity=out$mods$parms$tc.full,learning.rate=out$mods$lr.mod$lr,bag.fraction=out$input$bag.fraction,site.weights=site.weights[model.mask],
                  autostop=T,debug.mode=F,silent=!debug.mode,plot.main=F,superfast=F)
                     {cat("\n");cat(paste("50",".",i,"%\n",sep=""))}


          #predict the fitted values
          pred=predict.gbm(cv.final.mod,cbind(species.subset,predictor.subset),
                        cv.final.mod$target.trees,type="response")
          fitted.values[pred.mask]<-out$dat$ma[[i]]$pred<-predict.gbm(cv.final.mod, cbind(ydat[pred.mask],xdat[pred.mask, ]),
                    cv.final.mod$target.trees,type="response")
          
           out$dat$ma[[i]]$thresh <-as.numeric(optimal.thresholds(data.frame(ID=1:length(species.subset),pres.abs=species.subset,
                  pred=pred),opt.methods=out$input$opt.methods))[2]
            }

if(Model=="rf"){

                if(is.null(out$input$mtry)){
               mtry <- tuneRF(x=predictor.subset,y=factor(species.subset),mtryStart=3,importance=TRUE,ntreeTry=100,
                  replace=FALSE, doBest=F, plot=F)
                    mtry <- mtry[mtry[,2]==min(mtry[,2]),1][1]
                  }
                   y<-factor(species.subset)
                   x<-predictor.subset
               rf.full <- randomForest(x=x,y=y,xtest=xtest,ytest=ytest,importance=importance, ntree=n.trees,
                  mtry=mtry,replace=samp.replace,sampsize=ifelse(is.null(sampsize),(ifelse(samp.replace,nrow(x),ceiling(.632*nrow(x)))),sampsize),
                  nodesize=ifelse(is.null(nodesize),(if (!is.null(y) && !is.factor(y)) 5 else 1),nodesize),maxnodes=maxnodes,
                  localImp=localImp, nPerm=nPerm, keep.forest=ifelse(is.null(keep.forest),!is.null(y) && is.null(xtest),keep.forest),
                  corr.bias=corr.bias, keep.inbag=keep.inbag)
         #out of bag predictions
         pred<-tweak.p(as.vector(predict(rf.full,type="prob")[,2]))
          #predict the fitted values
          fitted.values[pred.mask]<-out$dat$ma[[i]]$pred<-predict(rf.full, xdat[pred.mask, ],type="prob")[,2]
           out$dat$ma[[i]]$thresh <-as.numeric(optimal.thresholds(data.frame(ID=1:length(species.subset),pres.abs=species.subset,
                pred=pred),opt.methods=out$input$opt.methods))[2]

       }
    y_i <- ydat[pred.mask]
    u_i <- fitted.values[pred.mask]
    weights.subset <- site.weights[pred.mask]

    if (family == "binomial" | family=="bernoulli") {
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

  rm(species.subset,predictor.subset,pos=1)

# and assemble results for return

#  mars.detail <- list(dataframe = dataframe.name,
#    x = mars.x, x.names = names(xdat),
#    y = mars.y, y.names = names(data)[mars.y],
#    target.sp = target.sp, degree=mars.degree, penalty = mars.penalty, family = family)

  y_i <- ydat
  u_i <- fitted.values

  if (family=="binomial" | family=="bernoulli") {
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
    
  cv.list<-list(full.resid.deviance = full.resid.deviance,
    full.test = full.test, full.calib = full.calib, pooled.deviance = cv.resid.deviance, pooled.test = cv.test,
    pooled.calib = cv.calib,subset.deviance = subset.deviance, subset.test = subset.test, subset.calib = subset.calib)
   out$cv<-cv.list
  return(out)
}
