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
  
}