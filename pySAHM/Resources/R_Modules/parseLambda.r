

maxent.predict<-function(x,lambdas){
    normalizers<-lambdas[(nrow(lambdas)-3):nrow(lambdas),]
    entropy<-normalizers[4,2]
    lambdas<-lambdas[1:(nrow(lambdas)-4),]
    variableNames <-names(x)
    fctType <- rep("raw",times=nrow(lambdas)-4)
    fctType[grep("`",as.character(lambdas[,1]))] <- "reverse.hinge"
    fctType[grep("'",as.character(lambdas[,1]))] <- "forward.hinge"
    fctType[grep("\\^",as.character(lambdas[,1]))]<-"quadratic"
    fctType[grep("[*]",as.character(lambdas[,1]))]<-"product"
    fctType[grep("[(]",as.character(lambdas[,1]))]<-"threshold"
    prod.names<-matrix(unlist(strsplit(as.character(lambdas[fctType=="product",1]),"[*]")),ncol=2)
    thresh <- matrix(unlist(strsplit(sub("[)]","",sub("[(]","",as.character(lambdas[fctType=="threshold",1]))),split="[<]")),ncol=2,byrow=TRUE)
    qx <-
    exp(
        #raw
        sum(lambdas[fctType=="raw",2]*(x[match(as.character(lambdas[fctType=="raw",1]),names(x))]-
        lambdas[fctType=="raw",3])/(lambdas[fctType=="raw",4]-lambdas[fctType=="raw",3])) +
        #quadratic
        sum(lambdas[fctType=="quadratic",2]*((x[match(sub("\\^2","",as.character(lambdas[fctType=="quadratic",1])),names(x))])^2-
        lambdas[fctType=="quadratic",3])/(lambdas[fctType=="quadratic",4]-lambdas[fctType=="quadratic",3])) +
        #product
        sum(lambdas[fctType=="product",2]*((x[match(prod.names[,1],names(x))]*x[match(prod.names[,2],names(x))])-
        lambdas[fctType=="product",3])/(lambdas[fctType=="product",4]-lambdas[fctType=="product",3])) +
        #forward hinge
        sum(lambdas[fctType=="forward.hinge",2]*((x[match(sub("'","",as.character(lambdas[fctType=="forward.hinge",1])),names(x))])-
        lambdas[fctType=="forward.hinge",3])/(lambdas[fctType=="forward.hinge",4]-lambdas[fctType=="forward.hinge",3])*(((x[match(sub("'","",as.character(lambdas[fctType=="forward.hinge",1])),names(x))])-
        lambdas[fctType=="forward.hinge",3])>0)) +
        #reverse hinge
        sum(lambdas[fctType=="reverse.hinge",2]*(lambdas[fctType=="reverse.hinge",4]-x[match(sub("`","",as.character(lambdas[fctType=="reverse.hinge",1])),names(x))])/(
        lambdas[fctType=="reverse.hinge",4]-lambdas[fctType=="reverse.hinge",3])*(((x[match(sub("`","",as.character(lambdas[fctType=="reverse.hinge",1])),names(x))])-
        lambdas[fctType=="reverse.hinge",4])<0)) +
        #threshold
        sum(lambdas[fctType=="threshold",2]*((x[match(thresh[,2],names(x))]-
        as.numeric(thresh[,1]))>0))-normalizers[1,2]
    )/normalizers[2,2]

    response<-qx*exp(entropy)/(1+qx*exp(entropy))
    return(response)
}