pred.fct<-function(model,x,Model){

  if(Model=="glm"){

          y <- as.vector(predict(model,x,type="response"))

          # encode missing values as -1.
          y[is.na(y)]<- NaN

          # return predictions.
          return(y)
     }

  if(Model=="mars"){
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
  if(Model=="brt"){
         # retrieve key items from the global environment #
          # make predictions from complete data only #
          #y <- rep(NA,nrow(x))
          #y[complete.cases(x)] <- predict.gbm(model, x[complete.cases(x),],model$target.trees,type="response")

          # make predictions from full data #
          y <- predict.gbm(model,x,model$target.trees,type="response")
          # encode missing values as -1.
           a<-complete.cases(x)
          y[!(a)]<- NaN

          # return predictions.
          return(y)
      }

  if(Model=="rf"){
       # retrieve key items from the global environment #
          # make predictions from complete data only #
          y <- rep(NA,nrow(x))
          y[complete.cases(x)] <- as.vector(predict(model,newdata=x[complete.cases(x),],type="prob")[,2])

          # make predictions from full data #

          # encode missing values as -1.
          y[is.na(y)]<- NaN

          # return predictions.
          return(y)
   }
}