generic.model.fit<-function(out,Model,t0){
attach(out$input)
on.exit(detach(out$input))
  if(Model=="mars"){
  out$mods$final.mod<-mars.glm(data=out$dat$ma$train$dat, mars.x=c(2:ncol(out$dat$ma$train$dat)), mars.y=1, mars.degree=out$input$mars.degree, family=out$input$model.family,
          site.weights=out$dat$ma$train$weight, penalty=out$input$mars.penalty)
          fit_contribs <- mars.contribs(out$mods$final.mod)


          x<-fit_contribs$deviance.table
          x <- x[x[,2]!=0,]
          x <- x[order(x[,4]),]
          row.names(x) <- x[,1]
          x$df <- -1*x$df
          x <- x[,-1]
          cat("Summary of Model:","\n")
          print(out$mods$summary <- x)

           out$mods$final.mod$contributions$var<-names(out$dat$ma$train$dat)[-1]
             out$mods$n.vars.final<-nrow(out$mods$summary)
              out$mods$vnames<-rownames(out$mods$summary)

              txt0 <- paste("\nMARS Model Results\n","\n","Data:\n",ma.name,"\n","\n\t n(pres)=",
                      out$dat$nPresAbs$train[2],"\n\t n(abs)=",out$dat$nPresAbs$train[1],"\n\t n covariates considered=",length(out$dat$used.covs),
                      "\n",
                      "\n   total time for model fitting=",round((unclass(Sys.time())-t0)/60,2),"min\n",sep="")

                  capture.output(cat(txt0),file=paste(out$dat$bname,"_output.txt",sep=""))

                  cat("\n","Finished with MARS","\n")
                  cat("Summary of Model:","\n")
                  print(out$mods$summary <- x)
                  if(!is.null(out$dat$bad.factor.cols)){
                      cat("\nWarning: the following categorical response variables were removed from consideration\n",
                          "because they had only one level:",paste(out$dat$bad.factor.cols,collapse=","),"\n\n")
                      }
                  cat("\n","Storing output...","\n","\n")
                  #flush.console()
                  capture.output(cat("\n\nSummary of Model:\n"),file=paste(out$dat$bname,"_output.txt",sep=""),append=TRUE)
                  capture.output(print(out$mods$summary),file=paste(out$dat$bname,"_output.txt",sep=""),append=TRUE)

          }

  if(Model=="glm") {
  penalty <- if(out$input$simp.method=="AIC") 2 else log(nrow(out$dat$ma$ma))
 
          if(!out$input$squared.terms){   
              scope.glm <- list(lower=as.formula(paste("response","~1")),
              upper=as.formula(paste("response","~",paste(out$dat$used.covs,collapse='+'))))
              }else{
              factor.mask<-na.omit(match(names(out$dat$factor.levels),out$dat$used.covs))
              cont.mask<-seq(1:length(out$dat$used.covs))
              if(length(factor.mask!=0)) cont.mask<-cont.mask[-c(factor.mask)]

               scope.glm <- list(lower=as.formula(paste("response","~1")),
                 upper=as.formula(paste("response","~",paste(c(if(length(factor.mask)>0) paste(out$dat$used.covs[factor.mask],collapse=" + "),
                 paste("(",paste(out$dat$used.covs[cont.mask],collapse=" + "),")^2",sep=""),
                 paste("I(",out$dat$used.covs[cont.mask],"^2)",sep="")),collapse=" + "),sep="")))
           }  
         
          mymodel.glm.step <- step(glm(as.formula(paste("response","~1")),family=out$input$model.family,data=out$dat$ma$train$dat,weights=out$dat$ma$train$weight,na.action="na.exclude"),
          direction='both',scope=scope.glm,k=penalty,trace=1)
          
          out$mods$final.mod<-mymodel.glm.step
         
            txt0 <- paste("Generalized Linear Results\n",out$input$run.time,"\n\n","Data:\n\t ",ma.name,"\n\t ","n(pres)=",
        out$dat$nPresAbs$train[2],"\n\t n(abs)=",out$dat$nPresAbs$train[1],"\n\t number of covariates considered=",length(out$dat$used.covs),
        "\n\n","Settings:\n","\n\t model family=",out$input$model.family,
        "\n\n","Results:\n\t ","number covariates in final model=",length(attr(terms(formula(out$mods$final.mod)),"term.labels")),
        "\n\t total time for model fitting=",round((unclass(Sys.time())-t0)/60,2),"min\n",sep="")

          capture.output(cat(txt0),file=paste(out$dat$bname,"_output.txt",sep=""))

                  print(out$mods$final.mod$summary <- summary(mymodel.glm.step))
              capture.output(out$mods$final.mod$summary,file=paste(out$dat$bname,"_output.txt",sep=""),append=TRUE)
                cat("\n","Finished with stepwise GLM","\n")
                cat("Summary of Model:","\n")

                if(length(coef(out$mods$final.mod))==1) stop("Null model was selected.  \nEvaluation metrics and plots will not be produced")

              #storing number of variables in final model
              out$mods$n.vars.final<-length(attr(terms(formula(out$mods$final.mod)),"term.labels"))
              out$mods$vnames<-attr(terms(formula(out$mods$final.mod)),"term.labels")
              #have to remove all the junk with powsers and interactions for mess map production to work
              out$mods$vnames<-unique(unlist(strsplit(gsub("I\\(","",gsub("\\^2)","",out$mods$vnames)),":")))
               }
         
 if(Model=="brt"){

          if(out$input$model.family=="binomial")  out$input$model.family="bernoulli"
            if(!is.null(out$input$tc)) out$mods$parms$tc.full<-out$mods$parms$tc.sub<-tc
            
            out <-est.lr(out)
            if(debug.mode) assign("out",out,envir=.GlobalEnv)

            cat("\nfinished with learning rate estimation, lr=",out$mods$lr.mod$lr0,", t=",round(out$mods$lr.mod$t.elapsed,2),"sec\n")
            cat("\nfor final fit, lr=",out$mods$lr.mod$lr,"and tc=",out$mods$parms$tc.full,"\n");flush.console()

            if(out$input$simp.method=="cross-validation"){
                # remove variables with <1% relative influence and re-fit model

                t1 <- unclass(Sys.time())
                    if(length(out$mods$lr.mod$good.cols)<=1) stop("BRT must have at least two independent variables")
                    out$input$max.trees<-NULL
                m0 <- gbm.step.fast(dat=out$dat$Subset$dat,gbm.x=out$mods$lr.mod$good.cols,gbm.y=1,family=out$input$model.family,
                      n.trees = c(300,600,800,1000,1200,1500,1800),step.size=out$input$step.size,max.trees=out$input$max.trees,
                      tolerance.method=out$input$tolerance.method,tolerance=out$input$tolerance, n.folds=out$input$n.folds,prev.stratify=out$input$prev.stratify,
                      tree.complexity=out$mods$parms$tc.sub,learning.rate=out$mods$lr.mod$lr0,bag.fraction=out$input$bag.fraction,site.weights=out$dat$Subset$weight,
                      autostop=T,debug.mode=F,silent=!debug.mode,
                      plot.main=F,superfast=F)
                      if(debug.mode) assign("m0",m0,envir=.GlobalEnv)

                      t1b <- unclass(Sys.time())

                out$mods$simp.mod <- gbm.simplify(m0,n.folds=out$input$n.folds,plot=F,verbose=F,alpha=out$input$alpha) # this step is very slow #
                      if(debug.mode) assign("out",out,envir=.GlobalEnv)

                      out$mods$simp.mod$good.cols <- out$mods$simp.mod$pred.list[[length(out$mods$simp.mod$pred.list)]]
                      out$mods$simp.mod$good.vars <- names(out$dat$ma$ma)[out$mods$simp.mod$good.cols]
                      cat("\nfinished with model simplification, t=",round((unclass(Sys.time())-t1b)/60,2),"min\n");flush.console()
                     {cat("\n");cat("50%\n")}
                      }

                  # fit final model #
                  t2 <- unclass(Sys.time())

           if(out$mods$lr.mod$lr==0) out$mods$lr.mod$lr<-out$mods$lr.mod$lr0
          out$mods$final.mod <- gbm.step.fast(dat=out$dat$ma$train$dat,gbm.x=out$mods$simp.mod$good.cols,gbm.y = 1,family=out$input$model.family,
                          n.trees = c(300,600,700,800,900,1000,1200,1500,1800,2200,2600,3000,3500,4000,4500,5000),n.folds=out$input$n.folds,out$input$max.trees,
                          tree.complexity=out$mods$parms$tc.full,learning.rate=out$mods$lr.mod$lr,bag.fraction=out$input$bag.fraction,site.weights=out$dat$ma$train$weight,
                          autostop=T,debug.mode=F,silent=!debug.mode,plot.main=F,superfast=F)

                          y <- gbm.interactions(out$mods$final.mod)
       if(debug.mode) assign("out",out,envir=.GlobalEnv)

        int <- y$rank.list;
        int<-int[int$p<.05,]
        int <- int[order(int$p),]
        int$p <- round(int$p,4)
        names(int) <- c("v1","name1","v2","name2","int.size","p-value")
        row.names(int)<-NULL
        if(nrow(int)>0) out$mods$interactions <- int else out$mods$interactions <- NULL

          out$mods$summary <- summary(out$mods$final.mod,plotit=F)
          out$mods$n.vars.final<-length(out$mods$final.mod$contributions$var)
           txt0 <- paste("\nBoosted Regression Tree Modeling Results\n",out$input$run.time,"\n\n",
                        "Data:\n",ma.name,"\n",
                        "\n\tn(pres)=",out$dat$nPresAbs$train[2],
                        "\n\tn(abs)=",out$dat$nPresAbs$train[1],
                        "\n\tn covariates considered=",length(out$dat$used.covs),
              "\n\n","Settings:\n",
                      "\n\trandom seed used =",out$input$seed,
                      "\n\ttree complexity=",out$mods$parms$tc.full,
                      "\n\tlearning rate=",round(out$mods$lr.mod$lr,4),
                      "\n\tn(trees)=",out$mods$final.mod$target.trees,
                      "\n\tmodel simplification=",out$input$simp.method,
                      "\n\tn folds=",out$input$n.folds,
                      "\n\tn covariates in final model=",nrow(out$mods$final.mod$contributions),
             sep="")
          txt1 <- "\nRelative influence of predictors in final model:\n\n"
          txt2 <- "\nImportant interactions in final model:\n\n"

          capture.output(cat(txt0),cat(txt1),print(out$mods$final.mod$contributions),cat(txt2),print(out$mods$interactions,row.names=F),file=paste(out$dat$bname,"_output.txt",sep=""))
          cat(txt0);cat(txt1);print(out$mods$final.mod$contributions);cat(txt2);print(out$mods$interactions,row.names=F)

          #storing number of variables in final model
              out$mods$n.vars.final<-nrow(out$mods$final.mod$contributions)
              out$mods$vnames<-as.character(out$mods$final.mod$contributions$var)

 }
 
if(Model=="rf"){

              # tune the mtry parameter - this controls the number of covariates randomly subset for each split #
              cat("\ntuning mtry parameter\n")

            x=out$dat$ma$train$dat[,-1]
            y=factor(out$dat$ma$train$dat[,1])
                if(is.null(out$input$mtry)){
               mtry <- tuneRF(x=out$dat$ma$train$dat[,-1],y=factor(out$dat$ma$train$dat[,1]),mtryStart=3,importance=TRUE,ntreeTry=100,
                  replace=FALSE, doBest=F, plot=F)
                    mtry <- mtry[mtry[,2]==min(mtry[,2]),1][1]
                    t2 <- unclass(Sys.time())
                    {cat("\n");cat("30%\n")}
                  }

              cat("\nnow fitting full random forest model using mtry=",mtry,"\n")
              if(debug.mode) flush.console()

               rf.full <- randomForest(x=out$dat$ma$train$dat[,-1],y=factor(out$dat$ma$train$dat[,1]),xtest=xtest,ytest=ytest,importance=TRUE, ntree=n.trees,
                  mtry=mtry,replace=samp.replace,sampsize=ifelse(is.null(sampsize),(ifelse(samp.replace,nrow(x),ceiling(.632*nrow(x)))),sampsize),
                  nodesize=ifelse(is.null(nodesize),(if (!is.null(y) && !is.factor(y)) 5 else 1),nodesize),maxnodes=maxnodes,
                  localImp=localImp, nPerm=nPerm, keep.forest=ifelse(is.null(keep.forest),!is.null(y) && is.null(xtest),keep.forest),
                  corr.bias=corr.bias, keep.inbag=keep.inbag)

                        out$mods$parms$mtry<-mtry
                        out$mods$final.mod <- rf.full

                  model.summary <- importance(out$mods$final.mod)
              model.summary<-model.summary[order(model.summary[,3],decreasing=T),]
              out$mods$summary <- model.summary
              txt0 <- paste("Random Forest Modeling Results\n",out$input$run.time,"\n\n",
                "Data:\n\t",ma.name,
                "\n\tn(pres)=",out$dat$nPresAbs$train[2],
                "\n\tn(abs)=",out$dat$nPresAbs$train[1],
                "\n\tn covariates considered=",length(out$dat$used.covs),
              "\n\n","Settings:",
              "\n\trandom seed used =",out$input$seed,
              "\n\tn covariates considered at each split =",mtry,
              "\n\tn trees=",out$input$n.tree,
              "\n\ttotal time for model fitting=",round((unclass(Sys.time())-t0)/60,2),"min\n",sep="")
          txt1 <- "\nRelative performance of predictors in final model:\n\n"
          txt2 <- "\nDefault summary of random forest fit:\n"
          capture.output(cat(txt0),cat(txt1),print(round(model.summary,4)),cat(txt2),print(out$mods$final.mod),file=paste(out$dat$bname,"_output.txt",sep=""))
          cat(txt0);cat(txt1);print(round(model.summary,4));cat(txt2);print(out$mods$final.mod);cat("\n\n")

         #storing number of variables in final model
              out$mods$n.vars.final<-length(out$dat$used.covs) #random forest doesn't drop variables
              out$mods$vnames<-out$dat$used.covs
      }
  return(out)
 }