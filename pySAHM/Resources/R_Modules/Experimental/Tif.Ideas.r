 cellStats(raster(fullnames[k]),stat='countNA')
a<-raster("C:/VisTrails/mtalbert_20110504T132851/PARC_1/aspect.tif")
cellStats(a,stat='countNA')
a@file@nodatavalue
b<-a
cellStats(b,stat='countNA')
NAvalue(b)
b[2:5000]<-NA
b[5001:10000]<-NAvalue(b)
writeRaster(b,"C:/VisTrails/mtalbert_20110504T132851/PARC_1/NATest.tif",)
a<-raster("C:/VisTrails/mtalbert_20110504T132851/PARC_1/NATest.tif")
cellStats(a,stat='countNA')
a[1:10]
a[5001:5002]
a<-raster("C:/temp/SAHMDebugJunk/BRTOut1/glm_1_prob_map.tif")
cellStats(a,stat=min)
a@data@min=cellStats(a,stat=min)
a@data@max=cellStats(a,stat=max)
writeRaster(a,"C:/temp/SAHMDebugJunk/BRTOut1/glm_1_prob2_map.tif")
a<-raster("C:/temp/SAHMDebugJunk/BRTOut1/glm_6_prob_map.tif")

#something like this SHOULD work unfortunately it does not appear to be working now
a<-raster("C:/temp/SAHMDebugJunk/BRTOut1/glm_6_prob_map.tif")
writeRaster(a,"C:/temp/SAHMDebugJunk/BRTOut1/glm_6_probGDALStats.tif",options="setStatistics=TRUE",overwrite=TRUE)
a<-readGDAL("C:/temp/SAHMDebugJunk/BRTOut1/glm_6_prob_map.tif")
writeGDAL(a,"C:/temp/SAHMDebugJunk/BRTOut1/glm_6_probGDALStats2.tif",setStatistics=TRUE)
str(a) #I don't see where to enter statistics here