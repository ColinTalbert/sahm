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

Pred.Surface<-function(object, model, filename="", na.rm=TRUE,NAval) {
    predrast <- raster(object)
		filename <- trim(filename)
			firstrow <- 1
			firstcol <- 1
		ncols <- ncol(predrast)
		lyrnames <- names(object)
		xylyrnames <- c('x', 'y', lyrnames)
		v <- matrix(NA, ncol=nrow(predrast), nrow=ncol(predrast))
      na.rm <- FALSE

    tr <- blockSize(predrast, n=nlayers(object)+5)
		ablock <- 1:(ncol(object) * tr$nrows[1])
		napred <- rep(NA, ncol(predrast)*tr$nrows[1])
  	predrast <- writeStart(predrast, filename=filename,overwrite=TRUE)
  ############################################################
  	for (i in 1:tr$n) {
			if (i==tr$n) { 
				ablock <- 1:(ncol(object) * tr$nrows[i])
				napred <- rep(NA, ncol(predrast) * tr$nrows[i])
			}
			rr <- firstrow + tr$row[i] - 1
				p <- xyFromCell(predrast, ablock + (tr$row[i]-1) * ncol(predrast)) 
				p <- na.omit(p)
				blockvals <- data.frame(x=p[,1], y=p[,2])
        if (na.rm) {
					blockvals <- na.omit(blockvals)		
				}
    if (nrow(blockvals) == 0 ) {
					predv <- napred
				} else {

				predv <- predict(model, blockvals)
				predv[is.na(predv)]<-NA
   	}
				if (na.rm) {  
					naind <- as.vector(attr(blockvals, "na.action"))
					if (!is.null(naind)) {
						p <- napred
						p[-naind] <- predv
						predv <- p
						rm(p)
					}
				}

				# to change factor to numeric; should keep track of this to return a factor type RasterLayer
				predv = as.numeric(predv)
				predrast <- writeValues(predrast, predv, tr$row[i])
				#NAvalue(predrast)<-NAval
				print(i)
			}

	predrast <- writeStop(predrast)

		return(predrast)
	}
