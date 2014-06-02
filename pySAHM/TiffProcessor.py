
import sys, os
import csv

from osgeo import gdalconst
from osgeo import gdal
from osgeo import osr

import numpy as np
import scipy.stats.stats as stats

import utilities
import SpatialUtilities

def main(args):
    """
    Process commandline Arguments, 
    Create an instance of PARC with the Variables,
    Kick off the parkFiles function of our PARC instance
    """
    ourLittleWorker = eval(args[0]+"()")
    for arg in args[1:]:
        k, v = arg.split("=")
        exec "ourLittleWorker." + k + " = " + 'r"' + v + '"'
    ourLittleWorker.run()
    
class rasterProcessor(object):
    '''The base class that all other Raster processing
    objects will inherit from
    '''
    def __init__(self):
        #instance level variables
        self.verbose = True
        self.outDir = ""
        self.outName = ""
        self.inputFname = ""
        self.templateFName = ""
        self.logger = None

    def run(self):
        setupGDAL()
        self.validateArgs()
        
        
    def validateArgs(self):
        if not os.path.exists(self.outDir):
            os.makedirs(self.outDir)
            
        if not os.path.exists(self.outDir):    
            raise utilities.TrappedError("Specified Output directory " + self.outDir + " not found on file system")
        
        if not os.path.isdir(self.outDir):
            raise utilities.TrappedError("Specified Output directory " + self.outDir + " is not a directory")
     
        if self.logger is None:
            self.logger = utilities.logger(self.outDir, self.verbose)
        self.writetolog = self.logger.writetolog

        # Validate template image.
        if not SpatialUtilities.isRaster(self.inputFname):
            raise utilities.TrappedError("input raster does not appear to be a valid raster")
        
        if self.templateFName != "" and not SpatialUtilities.isRaster(self.templateFName):
            raise utilities.TrappedError("template raster does not appear to be a valid raster")
        elif self.templateFName != "":
            self.templateRaster = SpatialUtilities.SAHMRaster(self.templateFName)
        
        self.inputRaster = SpatialUtilities.SAHMRaster(self.inputFname)
    
class rasterReclassifier(rasterProcessor):
    '''produces an output raster that has been reclassified according to 
    a reclass text file provided.
    The format of this file conforms to the ESRI reclass by asci form
    Information available at: http://resources.arcgis.com/en/help/main/10.1/index.html#//00q90000003w000000
    
    Values not specified in the reclass file will remain unchanged.
    No Data values are specified in both the input and output line with a NoData string
    
    '''
    def __init__(self):
        rasterProcessor.__init__(self)
        #template is optional and is only used to specify extent
        self.reclassFName = ""
    
    def run(self):
        #delete our output if it exists
        outputFName = os.path.join(self.outDir, self.outName)
        gdal.Unlink(outputFName)
        rasterProcessor.run(self)
        
        outputRaster = SpatialUtilities.SAHMRaster(outputFName)
        outputRaster.pullParamsFromRaster(self.inputFname)
        outputRaster.pixelType = gdal.GetDataTypeByName("int32")
        outputRaster.NoData = SpatialUtilities.defaultNoData(outputRaster.pixelType)
        outputRaster.createNewRaster()
        
        if self.templateFName != "":
            pass
            #TODO: what we're going to have to do here is first PARC the input to match the template
            #with nearest neighbor, majority
            #Then use this use this tmpPARC output as our input
        
        
        for blockData in self.inputRaster.iterBlocks():
#              print blockData
#              print SpatialUtilities.GDALToNPDataType(outputRaster.pixelType)
            outBlock = np.ma.asarray(blockData.copy(), dtype=SpatialUtilities.GDALToNPDataType(outputRaster.pixelType))
            for k,v in self.reclassDict.iteritems():
                if v == "NoData":
                    v = self.inputRaster.NoData
                if k[0] == "NoData":
                    outBlock = np.where(np.ma.getmask(blockData), v, outBlock)
                elif len(k) == 1:
                    outBlock[(blockData == k[0]).filled(False)] = v
#                    outBlock = np.where(blockData == k[0], v, outBlock)
                elif len(k) == 2:
                    outBlock[((blockData >= k[0]) & (blockData < k[1])).filled(False)] = v
##                    outBlock = np.where((blockData >= k[0]) & (blockData < k[1]), v, outBlock)
#            print outBlock
            outputRaster.putBlock(outBlock, self.inputRaster.curCol, self.inputRaster.curRow)
            
        outputRaster.calcStats()
        outputRaster.band.FlushCache()
        outputRaster = None
        
    def validateArgs(self):
        rasterProcessor.validateArgs(self)
        
        self.reclassDict = self.loadReclassDict(self.reclassFName)
           
    def loadReclassDict(self, reclassFName):
        '''loads our reclass File into a dictionary for latter processing
        the reclass file is the same format as that used by ESRI:
        see http://help.arcgis.com/en/arcgisdesktop/10.0/help/index.html#/How_Reclass_By_ASCII_File_works/009z000000t3000000/
        the dictionary is in the format
        {(oldVal,):newVal,
        (minOldVal, maxOldVal):newVal}
        '''
        outDict = {}
        reclassFile = open(reclassFName, "r")
        for line in reclassFile.readlines():
            split_line = line.split()
            #ignore comments
            if not split_line[0].startswith("#"):
                if str(split_line[-1]).lower() in ["nodata", "no data"]:
                    val = "NoData"
                else:
                    val = float(split_line[-1])

                if split_line[1] == ":": #single value reclass
                    outDict[(float(split_line[0]),)] = val
                elif split_line[2] == ":": #range of values reclass
                    outDict[(float(split_line[0]), float(split_line[1]))] = val
                    
        return outDict 

class categoricalToContinuousRasters(rasterProcessor):
    def __init__(self):
        rasterProcessor.__init__(self)
        #template is optional and is only used to specify extent
        self.templateFName = ""
    
    def run(self):
        rasterProcessor.run(self)

        cellRatio = SpatialUtilities.getTemplateSRSCellSize(self.inputRaster, 
                            self.templateRaster)/self.templateRaster.xScale
        
        targetCellSize, numSourcePerTarget = SpatialUtilities.getAggregateTargetCellSize(self.inputRaster, self.templateRaster)
        
        if cellRatio > 0.5:
            utilities.TrappedError("The input raster did not have cells at least half the dimensions of the template raster")
            
        shortName = SpatialUtilities.getRasterShortName(self.inputFname)
        tmpIntermediaryFname = os.path.join(self.outDir, shortName + "_tmpRepo.tif")
        gdal.Unlink(tmpIntermediaryFname)
        SpatialUtilities.intermediaryReprojection(self.inputRaster, self.templateRaster, tmpIntermediaryFname, gdalconst.GRA_NearestNeighbour)
        
        self.tmpRaster = SpatialUtilities.SAHMRaster(tmpIntermediaryFname)
                #dictionaries to hold our outputRasters 
        outputs = {}
        
        bSize = 2048 #source pixels
        #convert this to the nearest whole number of target pixels
        bSize = int(round(bSize / numSourcePerTarget) * numSourcePerTarget)
        if bSize == 0:
            bSize = int(numSourcePerTarget)
        
        self.tmpRaster.blockSize = bSize
        
        pixelsPerBlock = numSourcePerTarget*numSourcePerTarget
        
        for block in self.tmpRaster.iterBlocks():
            uniques = np.unique(block)
            
            X, Y = block.shape
            x = X // numSourcePerTarget
            y = Y // numSourcePerTarget
            ndMask = block.reshape( (x, numSourcePerTarget, y, numSourcePerTarget) )
            ndMask = ndMask.transpose( [0, 2, 1, 3] )
            ndMask = ndMask.reshape( (x*y, pixelsPerBlock) )
            ans =  np.array(stats.mode(ndMask, 1)[0]).reshape(x, y)
            
            for uniqueVal in uniques:
                if not type(uniqueVal) is np.ma.core.MaskedConstant:
                    ans = ((ndMask == uniqueVal).sum(1)/(pixelsPerBlock)).reshape(x, y) * 100
                else:
                    ans = ((ndMask.mask).sum(1)/(pixelsPerBlock)).reshape(x, y) * 100
                    
                if not outputs.has_key(uniqueVal):
                    outputs[uniqueVal] = self.initializeOneOutput(uniqueVal)
                    
                outputs[uniqueVal].putBlock(ans, int(self.tmpRaster.curCol / numSourcePerTarget), int(self.tmpRaster.curRow  / numSourcePerTarget))
        
        self.tmpRaster.close()
        gdal.Unlink(tmpIntermediaryFname)
        
        #write our outputs to a PredictorListFile
        output_fname = os.path.join(self.outDir, "ouputFileList.csv")
        csv_writer = csv.writer(open(output_fname, 'wb'))
        csv_writer.writerow(["file", "Resampling", "Aggregation"])
        for k,v in outputs.iteritems():
            csv_writer.writerow([os.path.normpath(v.source), "0", "NearestNeighbor", "Mean"])
        del csv_writer
        
        self.outputPredictorsList = output_fname
        
    def initializeOneOutput(self, val):
        inputJustName = SpatialUtilities.getRasterShortName(self.inputFname)
        if type(val) is np.ma.core.MaskedConstant:
            val = "NoData"
        outputFName = os.path.join(self.outDir, inputJustName + "_" + str(val) + ".tif")
        gdal.Unlink(outputFName)
        outputRaster = SpatialUtilities.SAHMRaster(outputFName)
        outputRaster.getParams(self.templateRaster.ds)
        outputRaster.NoData = self.inputRaster.NoData
        outputRaster.signedByte = self.inputRaster.signedByte
        outputRaster.createNewRaster()
        
        return outputRaster
        
    def validateArgs(self):
        rasterProcessor.validateArgs(self)
        validDTypes = ["Unknown", "Byte", "Int16",
                "UInt32", "Int32", "CInt16", "CInt32"]
        if not gdal.GetDataTypeName(self.inputRaster.pixelType) in validDTypes:
            utilities.TrappedError("The input raster is not of a discrete pixel type")
        
 
def setupGDAL():
    parentDir = os.path.split(os.path.dirname(__file__))[0]
    gdal_data = os.path.join(parentDir, "GDAL_Resources", "gdal-data")
    os.environ['GDAL_DATA'] = gdal_data
    projlib = os.path.join(parentDir, "GDAL_Resources", "projlib")
    os.environ['PROJ_LIB'] = projlib
       
if __name__ == "__main__":
    
    try:
        main(sys.argv[1:])
    except Exception as e:
        print e
        import traceback
        print traceback.format_exc()
        sys.stderr.write(traceback.format_exc())
        print "Job failed!", sys.exc_info()[0]
