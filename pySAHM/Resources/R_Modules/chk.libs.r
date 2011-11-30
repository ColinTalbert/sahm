chk.libs <- function(Model){

     if(Model=="mars") libs<-list("PresenceAbsence","rgdal","sp","survival","mda","raster","tcltk2","foreign","ade4")
     if(Model=="glm") libs<-list("PresenceAbsence","rgdal","sp","survival","tools","raster","tcltk2","foreign","ade4")
     if(Model=="rf") libs<-list("randomForest","PresenceAbsence","rgdal","sp","raster","tcltk2","foreign","ade4")
     if(Model=="brt") libs<-list("PresenceAbsence","rgdal","sp","survival","lattice","raster","tcltk2","foreign","ade4")
     
      lib.mssg <- unlist(suppressMessages(suppressWarnings(lapply(libs,require,quietly = T, warn.conflicts=F,character.only=T))))
      if(any(!lib.mssg)){
            install.packages(unlist(libs[!lib.mssg]), repos = "http://cran.r-project.org")
            lib.mssg <- unlist(suppressMessages(suppressWarnings(lapply(unlist(libs[!lib.mssg]),require,quietly = T, warn.conflicts=F,character.only=T))))
            }
        if(any(!lib.mssg)) stop("the following package(s) could not be loaded:",out$dat$missing.libs)

      }


