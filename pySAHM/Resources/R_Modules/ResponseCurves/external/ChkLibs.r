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

ChkLibs <- function(libs){
#Checks libraries and installs any that are missing
#documentation on all libraries required by SAHM 
#Written by Marian Talbert 2/2012
      lib.mssg <- unlist(suppressMessages(suppressWarnings(lapply(libs,require,quietly = T, warn.conflicts=F,character.only=T))))
      if(any(!lib.mssg)){
            install.packages(unlist(libs[!lib.mssg]), repos = "http://cran.r-project.org")
            lib.mssg <- unlist(suppressMessages(suppressWarnings(lapply(libs,require,quietly = T, warn.conflicts=F,character.only=T))))
            }
        if(any(!lib.mssg)) stop(paste(paste("\n\nthe following package(s) could not be loaded: ",paste(unlist(libs[!lib.mssg]),sep="")),sep=""))

}


