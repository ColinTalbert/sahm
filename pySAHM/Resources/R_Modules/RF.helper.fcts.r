tweak.p <- function(p){
	p[p==1]<-max(p[p<1])
	p[p==0]<-min(p[p>0])
	return(p)
	}