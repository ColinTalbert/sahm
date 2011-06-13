#!/usr/bin/python

import glob
import math
import os
import shutil
import struct
import sys
import csv

import subprocess

from optparse import OptionParser

from osgeo import gdalconst
from osgeo import gdal
from osgeo import osr


from numpy import *
import numpy as np

import utilities

def main(args_in):
    """
    Process commandline Arguments, 
    Create an instance of PARC with the Variables,
    Kick off the parkFiles function of our PARC instance
    """
    # Process command-line args.  
    usageStmt = "usage:  %prog [options] <template image> <input dir or list of input files>"
    desc = "This application projects, aggregates, resamples, and clips imagery."
    
    parser = OptionParser(usage=usageStmt, description=desc)
    parser.add_option("-l", dest="listMethodFlag", default=False, action="store_true", help="print the names of all known aggregation methods")
    parser.add_option("-o", dest="outDir", default="./", help="directory in which to put processed images, defaults to current directory")
    parser.add_option("-v", dest="verbose", default=False, action="store_true", help="the verbose flag causes diagnostic output to print")
    parser.add_option("-t", dest="templateRaster", help="The template raster used for projection, origin, cell size and extent")
    parser.add_option("-i", dest="inputsCSV", help="The CSV containing the list of files to process.  Format is 'FilePath, Categorical, Resampling, Aggreagtion")
    parser.add_option("-m", dest="multicore", default=True, help="'True', 'False' indicating whether to use multiple cores or not") 
    (options, args) = parser.parse_args(args_in)
    
    ourPARC = PARC()
    ourPARC.verbose = options.verbose
    ourPARC.template = options.templateRaster
    ourPARC.outDir = options.outDir
    ourPARC.inputsCSV = options.inputsCSV
    ourPARC.multicores = options.multicore
    
    ourPARC.parcFiles()

class PARC:
    '''
     PARC:  Project, Aggregate, Resample, Clip
      The workflow on this beast is as follows:
            For each dataset
            Step 1: RePrject the source raster into a tmp raster using
                the projection info from the template and the method if 
                supplied or the default of nearest if not.
                At this stage the tmp output will have a cell size about
                the same as the input.  We just use the default for this 
                setting.
            Step 2: Aggregate the tmpRaster to have the same origin, 
                cell size and extent as our template.
    '''    
    
    def __init__(self):
        #instance level variables
        self.verbose = False
        self.template = ""
        self.templateParams = {}
        self.outDir = ""
        self.inputsCSV = ''
        self.inputs = []
        self.aggMethods = ['Min', 'Mean', 'Max', 'Majority']
        self.resampleMethods = ['NearestNeighbor', 'Bilinear', 'Cubic', 'CubicSpline', 'Lanczos']
        self.logger = None
        self.multicores = 'False'

    def parcFiles(self):
        '''
            1: Parse the inputsCSV into our inputs list
            2: Make sure all of our instance variables are good and proper
            3: Loop through the list of sourceImages and PARC each one.
            4: The outputs will be stored in the output directory
            5: Additionally an output CSV will be produced that lists the 
            inputs, parameters used, and outputs
        '''
        
        self.validateArgs()
        self.logger.writetolog("Starting PARC", True, True)
        if self.multicores.lower() in ['true', 'yes', 't', 'y', '1']:
            self.processFilesMC()
        else:
            self.processFiles()
        
        self.logger.writetolog("Finished PARC", True, True)
        
    def processFiles(self):
        # Clip and reproject each source image.
        for image in self.inputs:
            # Ensure source is different from template.
            #if not os.path.samefile(template, image):
            inPath, inFileName = os.path.split(image[0])
            outFile, ext = os.path.splitext(inFileName) 
            outFile = os.path.join(self.outDir, outFile + ".tif")
            
            # os.path.samefile(image, outFile):
            if os.path.exists(outFile) and \
               os.path.abspath(image[0]) == os.path.abspath(outFile):

                baseName, extension = os.path.splitext(outFile)
                outFile = baseName + "-PARC.tif"
                
            if os.path.abspath(self.template) != os.path.abspath(image[0]):
                self.parcFile(image, outFile)
            elif os.path.abspath(self.template) == os.path.abspath(image[0]): 
                shutil.copyfile(self.template, outFile)
                
		
        
    def processFilesMC(self):
        '''This function has the same functionality as parcFiles
        with the addition of utilizing multiple cores to do the processing.
        '''
        processes = []
        for image in self.inputs:
            # Ensure source is different from template.
            #if not os.path.samefile(template, image):
            inPath, inFileName = os.path.split(image[0])
            outFile, ext = os.path.splitext(inFileName) 
            outFile = os.path.join(self.outDir, outFile + ".tif")
            
            # os.path.samefile(image, outFile):
            if os.path.exists(outFile) and \
               os.path.abspath(image[0]) == os.path.abspath(outFile):

                baseName, extension = os.path.splitext(outFile)
                outFile = baseName + "-PARC.tif"
            
            if os.path.abspath(self.template) != os.path.abspath(image[0]):
                args = '-s ' + image[0]
                args += ' -d ' + outFile
                args += ' -t ' + self.template
                args += ' -r ' + image[2]
                args += ' -a ' + image[3]
                
                execDir = os.path.split(sys.argv[0])[0]
                executable = os.path.join(execDir, 'singlePARC.py')
                
                pyEx = sys.executable
                command = ' '.join([pyEx, executable, args])
                processes.append(subprocess.Popen(command))
            
                
        
        
        self.logger.writetolog("Finished PARC", True, True)

    def parcFile(self, source, dest):
        """
        Processes a single file
        """
        shortName = os.path.split(os.path.splitext(source[0])[0])[1]
        self.logger.writetolog("    Starting processing of " + source[0])
        sourceParams = self.getRasterParams(source[0])
                
        gdalType = None
        if source[2].lower() == "nearestneighbor":
            gdalType = gdalconst.GRA_NearestNeighbour
        if source[2].lower() == "bilinear":
            gdalType = gdalconst.GRA_Bilinear
        if source[2].lower() == "cubic":
            gdalType = gdalconst.GRA_Cubic
        if source[2].lower() == "cubicspline":
            gdalType = gdalconst.GRA_CubicSpline
        if source[2].lower() == "lanczos":
            gdalType = gdalconst.GRA_Lanczos
        if gdalType == None:
            self.logger.writetolog("   Specified resampling method (" + source[2] + ") not one of 'NearestNeighbor', 'Bilinear', 'Cubic', 'CubicSpline', or 'Lanczos'.  Defaulting to 'NearestNeighbor'")
            gdalType = gdalconst.GRA_NearestNeighbour
        
        #Open dgal dataset of the source to pull some values from
        srcDs = gdal.Open(source[0])
        
        cellRatio = self.getTemplateSRSCellSize(sourceParams)/self.templateParams["xScale"]
        msg = "  ratio of source cell size to template cell size = " + str(cellRatio)
        msg += "    template cell size = " + str(self.templateParams["xScale"])
        msg += "    " + shortName + " cell size = " + str(self.getTemplateSRSCellSize(sourceParams))
        self.writetolog(msg)
            
        if cellRatio > .5:
            #The source cell size is close enough to our template cell size,
            #or smaller so
            #that all we need to do is reproject and resample.
            self.logger.writetolog("  cell ratio > .5: reprojecting and resampling to template parameters only")
            self.reprojectRaster(srcDs, sourceParams, self.templateParams, dest, 
                                gdalType, shortName, self.templateParams["xScale"])
        else:
            #Our Target cell size is much bigger than our source we need to do 
            #some aggregation to make things work.
            msg = '  cell ratio <= .5: reprojecting and resampling to template parameters'
            msg += '    then aggregating the reprojected raster to match template parameters'
            self.writetolog(msg)   
			    
            targetCellSize, numSourcePerTarget = self.getAggregateTargetCellSize(sourceParams)
            tmpOutput = os.path.join(os.path.dirname(dest), "tmp_" + os.path.basename(dest))
            
            self.reprojectRaster(srcDs, sourceParams, self.templateParams,
                                tmpOutput, gdalType,  shortName, targetCellSize)
            self.writetolog("   Stating on Aggregating: " + shortName)
                
            tmpOutput2 = os.path.splitext(tmpOutput)[0] + ".tif"
            self.Aggregate(tmpOutput2, dest, 
                        sourceParams, self.templateParams,
                        source[3], numSourcePerTarget)
            
            os.remove(tmpOutput2)
            
    
    def getTemplateSRSCellSize(self, sourceParams):
        """
        Calculate what size our source image pixels would be in the template SRS
        """
        #first convert our template origin into the source srs
        tOriginX, tOriginY = self.transformPoint(self.templateParams["west"], self.templateParams["north"], 
                                        self.templateParams["srs"], sourceParams["srs"])
        #next add the source xScale to the converted origin x and convert that back to template srs
        tOriginX1 = self.transformPoint (tOriginX + sourceParams["xScale"], tOriginY, 
                                                sourceParams["srs"], self.templateParams["srs"])[0]                        
        
        
#        templateCellXCorner1 = (self.templateParams["west"], self.templateParams["north"], 
#                                        self.templateParams["srs"], sourceParams["srs"])[0]
#        
#        targetCellXCorner1 = (sourceParams["west"], sourceParams["north"], 
#                                                sourceParams["srs"], self.templateParams["srs"])[0]
#        targetCellXCorner2 = self.transformPoint(sourceParams["west"] + sourceParams["xScale"], 
#                                                sourceParams["north"], sourceParams["srs"], self.templateParams["srs"])[0]
        templateSRSCellSize = abs(abs(tOriginX1) - abs(self.templateParams["west"]))
        return templateSRSCellSize

    def getAggregateTargetCellSize(self, sourceParams):
        """
        This function determines the appropriate cell size to
        reproject/resample our source raster into before 
        aggregating.
        This size is the cell size that results in a template 
        cell containing a whole number of cells which are as 
        close as possible to the cell dimension that would 
        result if you reprojected the source cells into the 
        target srs without changing cell size.
        """
        #first determine what cell size we are going to use for the initial reproject/resample 
        #step 1:  Determine the native cell size in the template coordinate system.
        templateSRSCellSize = self.getTemplateSRSCellSize(sourceParams)
        #step 2:  round this up or down to an even fraction of the template cell size
        # for example source = 30, target = 250 resampledSource = 250/round(250/30)
        sourcePixelsPerTarget = round(self.templateParams["xScale"]/templateSRSCellSize)
        nearestWholeCellSize = (self.templateParams["xScale"] / 
                            sourcePixelsPerTarget)
        return nearestWholeCellSize, sourcePixelsPerTarget
        
        
    def Aggregate(self, inFile, outFile, sourceParams, templateParams, method, numSourcePerTarget):
        sourceDs = gdal.Open(inFile, gdalconst.GA_ReadOnly)
        sourceBand  = sourceDs.GetRasterBand(1)
        
        tmpOutput = os.path.splitext(outFile)[0] + ".tif"
        tmpOutDataset = self.generateOutputDS(sourceParams, templateParams, tmpOutput)
        outBand = tmpOutDataset.GetRasterBand(1)
        
        rows = int(sourceParams["height"])
        cols = int(sourceParams["width"])

        row = 0
        col = 0
        
        
        pcntDone = 0
        while row < templateParams["width"]:
            while col < templateParams["height"]:
                sourceRow = row * numSourcePerTarget
                sourceCol = col * numSourcePerTarget

                #kernel = self.getKernel(sourceRow, sourceCol, numSourcePerTarget, sourceDs)
                kernel = sourceDs.GetRasterBand(1).ReadAsArray(int(sourceRow), 
                                                    int(sourceCol), 
                                                    int(numSourcePerTarget),
                                                    int(numSourcePerTarget))
                #convert kenel values of our nodata to nan
                ndMask = ma.masked_array(kernel, mask=(kernel==sourceParams["NoData"]))
                #print kernel
                if self.aggMethod == "Min":
                    ans = ndMask.min()
                elif self.aggMethod == "Max":
                    ans = ndMask.max()
                elif self.aggMethod == "Majority":
                    uniques = np.unique(ndMask)
                    histogram = np.histogram(ndMask, uniques)
                    ans = histogram[1][histogram[0].argmax()]
                else:
                    ans = ndMask.mean()
                
#                print ndMask
#                print ans
                #special case real ugly
                if ans < 0 and sourceParams["signedByte"]:
                    ans = ans + 255
                
                ansArray = empty([1, 1])
                if type(ans) == ma.core.MaskedArray:
                    ansArray[0, 0] = sourceParams["NoData"]
                else:
                    ansArray[0, 0] = ans

                outBand.WriteArray(ansArray, row, col)
                
                col += 1
                
            row += 1
            col  = 0
            if float(row)/templateParams["width"] > float(pcntDone)/100:
                pcntDone += 10
                if self.verbose:
                    print str(pcntDone) + "...",
        if self.verbose:
            print "Done"
#        if self.verbose:
#            print "Done\nSaving to ASCII format"
#                            
#        driver = gdal.GetDriverByName("AAIGrid")
#        driver.Register()
#        
#        dst_ds = driver.CreateCopy(outFile, tmpOutDataset, 0)
#        if self.verbose:
#            print "    Finished Saving ", self.shortName
        
        dst_ds = None
        tmpOutDataset=None
        
    def getRasterParams(self, rasterFile):
        """
        Extracts a series of bits of information from a passed raster
        All values are stored in a dictionary which is returned.
        If errors are encountered along the way the error messages will
        be returned as a list in the Error element.
        """
        try:
            #initialize our params dictionary to have None for all parma
            params = {}
            allRasterParams = ["Error", "xScale", "yScale", "width", "height",
                            "east", "north", "west", "south",  
                            "tEast", "tNorth", "tWest", "tSouth",
                            "gEast", "gNorth", "gWest", "gSouth",  
                            "Wkt", "srs", "gt", "prj", "NoData", "PixelType"]
            
            for param in allRasterParams:
                params[param] = None
            params["Error"] = []
            
            # Get the PARC parameters from the rasterFile.
            dataset = gdal.Open(rasterFile, gdalconst.GA_ReadOnly)
            if dataset is None:
                params["Error"].append("Unable to open file")
                return params
                
                #print "Unable to open " + rasterFile
                #raise Exception, "Unable to open specifed file " + rasterFile
                
            
            xform  = dataset.GetGeoTransform()
            params["xScale"] = xform[1]
            params["yScale"] = xform[5]
    
            params["width"]  = dataset.RasterXSize
            params["height"] = dataset.RasterYSize
    
            params["west"] = xform[0]
            params["north"] = xform[3]
            params["east"] = params["west"] + params["width"]  * params["xScale"]
            params["south"] = params["north"] + params["height"] * params["yScale"]
    
            try:
                wkt = dataset.GetProjection()
                params["gt"] = dataset.GetGeoTransform()
                params["prj"] = dataset.GetProjectionRef()
                params["srs"] = osr.SpatialReference(wkt)
                if wkt == '':
                    params["Error"].append("Undefined projection")
                else:
                    
                    if rasterFile == self.template:
                        params["tWest"], params["tNorth"] = params["west"], params["north"]
                        params["tEast"], params["tSouth"] = params["east"], params["south"]
                    elif params["srs"].ExportToWkt() == self.templateParams["srs"].ExportToWkt():
                        params["tWest"], params["tNorth"] = params["west"], params["north"]
                        params["tEast"], params["tSouth"] = params["east"], params["south"]
                    else:
                        try:
                            params["tWest"], params["tNorth"] = self.transformPoint(params["west"], params["north"], params["srs"], self.templateParams["srs"])
                            params["tEast"], params["tSouth"] = self.transformPoint(params["east"], params["south"], params["srs"], self.templateParams["srs"])
                        except:
                            params["Error"].append("Could not transform extent coordinates to template spatial reference")
                            #params["Error"] = "We ran into problems converting projected coordinates to template for " +  rasterFile
                    try:
                        geographic = osr.SpatialReference()
                        geographic.ImportFromEPSG(4326)
                        params["gWest"], params["gNorth"] = self.transformPoint(params["west"], params["north"], params["srs"], geographic)
                        params["gEast"], params["gSouth"] = self.transformPoint(params["east"], params["south"], params["srs"], geographic)
                    except:
                        pass
                    
            except:
                #print "We ran into problems getting the projection information for " +  rasterFile
                params["Error"].append("Undefined problems extracting the projection information")
                
            try:
                params["signedByte"] = dataset.GetRasterBand(1).GetMetadata('IMAGE_STRUCTURE')['PIXELTYPE'] == 'SIGNEDBYTE'
            except KeyError:
                params["signedByte"] = False
            
            params["NoData"] = dataset.GetRasterBand(1).GetNoDataValue()
            if params["NoData"] == None:
                driver = dataset.GetDriver()
                if dataset.GetRasterBand(1).DataType == 1:
                    print "Warning:  Could not extract NoData value.  Using assumed nodata value of 255"
                    params["NoData"] = 255
                elif dataset.GetRasterBand(1).DataType == 2:
                    print "Warning:  Could not extract NoData value.  Using assumed nodata value of 65536"
                    params["NoData"] = 65536
                elif dataset.GetRasterBand(1).DataType == 3:
                    print "Warning:  Could not extract NoData value.  Using assumed nodata value of 32767"
                    params["NoData"] = 32767
                elif dataset.GetRasterBand(1).DataType == 4:
                    print "Warning:  Could not extract NoData value.  Using assumed nodata value of 2147483647"
                    params["NoData"] = 2147483647
                elif dataset.GetRasterBand(1).DataType == 5:
                    print "Warning:  Could not extract NoData value.  Using assumed nodata value of 2147483647"
                    params["NoData"] = 2147483647
                elif dataset.GetRasterBand(1).DataType == 6:
                    print "Warning:  Could not extract NoData value.  Using assumed nodata value of -3.40282346639e+038"
                    params["NoData"] = -3.40282346639e+038
                else:
                    params["Error"].append("Could not identify nodata value")
            params["PixelType"] = dataset.GetRasterBand(1).DataType
            if params["PixelType"] == None:
                params["Error"].append("Could not identify pixel type (bit depth)")
            
        except:
            #print "We ran into problems extracting raster parameters from " + rasterFile
            params["Error"].append("Some untrapped error was encountered")
        finally:
            del dataset
            return params

    def transformPoint(self, x, y, from_srs, to_srs):
        """
        Transforms a point from one srs to another
        """
        coordXform = osr.CoordinateTransformation(from_srs, to_srs)
        yRound = round(y, 4)
        xRound = round(x, 4)

        result = coordXform.TransformPoint(xRound, yRound)
        
        gx = result[0]
        gy = result[1]

        return gx, gy
        
    def TemplateCoversImage(self, sourceParams):
        """
        Checks to see if the templatate images 
        falls completely inside the source raster
        """
        inside = False
        if (sourceParams["tWest"] <= self.templateParams["tWest"] and 
            sourceParams["tNorth"] >= self.templateParams["tNorth"] and                                     
               sourceParams["tEast"] >=  self.templateParams["tEast"] and 
               sourceParams["tSouth"] <=  self.templateParams["tSouth"]):
            inside = True

        if (sourceParams["gWest"] <= self.templateParams["gWest"] and 
            sourceParams["gNorth"] >= self.templateParams["gNorth"] and                                     
               sourceParams["gEast"] >=  self.templateParams["gEast"] and 
               sourceParams["gSouth"] <=  self.templateParams["gSouth"]):
            inside = True


        return inside

    def validateArgs(self):
        """
        Make sure the user sent us some stuff we can work with
        """

        if not os.path.exists(self.outDir):
            raise Exception, "Specified Output directory " + self.outDir + " not found on file system"
        
        if not os.path.isdir(self.outDir):
            raise Exception, "Specified Output directory " + self.outDir + " is not a directory"
     
        if self.logger is None:
            self.logger = utilities.logger(self.outDir, self.verbose)
        self.writetolog = self.logger.writetolog

        # Validate template image.
        if self.template is None:
            raise Exception, "template raster not provided (-t command line argument missing)"
        
        if not os.path.exists(self.template):
            raise Exception, "Template file, " + self.template + ", does not exist on file system"

        self.templateParams = self.getRasterParams(self.template)
        if len(self.templateParams["Error"]) <> 0:
            raise Exception, ("There was a problem with the provided template: \n    " + 
                                    "    " + "\n    ".join(self.templateParams["Error"]))
        
        # Ensure the template has square pixels.
        if abs(abs(self.templateParams["xScale"]) - abs(self.templateParams["yScale"])) > 1e-6:
            raise Exception, ("template image must have square pixels." + 
                            "/n    x pixel scale = " + str(xScale) +
                            "/n    y pixel scale = " + str(yScale))

        
        #Validate input rasters
        if not os.path.exists(self.inputsCSV):
            raise Exception, "Inputs CSV, " + self.inputsCSV + ", does not exist on file system."
        
        inputsCSV = csv.reader(open(self.inputsCSV, 'r'))
        header = inputsCSV.next()
        strInputFileErrors = ""

        outputCSV = os.path.join(self.outDir, "PARC_Files.csv")
        output = csv.writer(open(outputCSV, "wb"))
        output.writerow(["PARCOutputFile", "Categorical", "Resampling", "Aggregation", "OriginalFile"])
        
        for row in inputsCSV:
            inputFile = row[0]
            sourceParams = self.getRasterParams(inputFile)
            if len(sourceParams["Error"]) > 0:
                strInputFileErrors += ("  " + os.path.split(inputFile)[1] + " had the following errors:\n" + 
                                    "    " + "\n    ".join(sourceParams["Error"])) + "\n"
            else:
                if not self.TemplateCoversImage(sourceParams):
                    strInputFileErrors += ("\n  Some part of the template image falls outside of " + os.path.split(inputFile)[1])
                    strInputFileErrors += "\n        template upper left  = (" + str(self.templateParams["west"]) + ", " + str(self.templateParams["north"]) + ")"
                    strInputFileErrors += "\n        template lower right = (" + str(self.templateParams["east"]) + ", " + str(self.templateParams["south"]) + ")"
                    strInputFileErrors += "\n        image    upper left  = (" + str(sourceParams["west"]) + ", " + str(sourceParams["north"]) + ")"
                    strInputFileErrors += "\n        image    lower right = (" + str(sourceParams["east"]) + ", " + str(sourceParams["south"]) + ")"
                    strInputFileErrors += "\n        points are given in projected coordinates."
                    strInputFileErrors += "\n        template upper left  = (" + str(self.templateParams["tWest"]) + ", " + str(self.templateParams["tNorth"]) + ")"
                    strInputFileErrors += "\n        template lower right = (" + str(self.templateParams["tEast"]) + ", " + str(self.templateParams["tSouth"]) + ")"
                    strInputFileErrors += "\n        image    upper left  = (" + str(sourceParams["tWest"]) + ", " + str(sourceParams["tNorth"]) + ")"
                    strInputFileErrors += "\n        image    lower right = (" + str(sourceParams["tEast"]) + ", " + str(sourceParams["tSouth"]) + ")"
                    strInputFileErrors += "\n        Note: points are given in the template coordinates." + "\n"
            
            if len(row) < 2 or not row[1] in ['0', '1']:
                self.writetolog("  " + os.path.split(inputFile)[1] + " categorical either missing or not 0 or 1:\n   Defaulting to 0 (continuous)")
                if len(row) < 2:
                    row.append('0')
                else:
                    row[1] = '0'
                 
            if len(row) < 3 or not row[2].lower() in [item.lower() for item in self.resampleMethods]:
                 self.writetolog("  " + os.path.split(inputFile)[1] + " resample method either missing or not one of " + 
                                        ", ".join(self.resampleMethods) + "\n  Defaulting to 'Bilinear'")                  
                 
                 if row[1] == '0':
                     default = 'Bilinear'
                 else:
                     default = 'NearestNeighbor'
                 if len(row) < 3:
                     row.append(default)
                 else:
                     row[2] = default

            if len(row) < 4 or not row[3].lower() in [item.lower() for item in self.aggMethods]:
                 self.writetolog("  " + os.path.split(inputFile)[1] + " aggregation method either missing or not one of " + 
                                        ", ".join(self.aggMethods) + "\n  Defaulting to 'Mean'")
                 if row[1] == '0':
                     default = 'Mean'
                 else:
                     default = 'Majority'
                 if len(row) < 4:
                     row.append(default)
                 else:
                     row[3] = default
                 
            self.inputs.append(row)
            #also write the output row, reconfigured to our output file
            fileName = self.getShortName(row[0])
            fileName = os.path.join(self.outDir, fileName + ".tif")
            outputrow = [fileName] + row[1:4] + [row[0]]
            output.writerow(outputrow)
        del output
        
        if strInputFileErrors <> "":
            raise Exception, "There was one or more problems with your input rasters: \n" + strInputFileErrors
        

     

#    def getInputFiles(self):
#        """ Parses the arguments to generate a list
#        of covariate files that we will be processing 
#        against the template
#        """
        
#        sourceImages = None
#
#        # Validate input directory or files.
#        if (os.path.isdir(self.inputs[0]) and
#            not os.path.exists(os.path.join(self.inputs[0], "hdr.adf"))):
#            if self.verbose == True:  print "Input directory found..."
#
#            if not os.path.exists(self.inputs[0]):
#                raise Exception, "Input directory, " + self.inputs[0] + ", does not exist."
#
#            sourceImages = glob.glob(self.inputs[0] + "/*.tif")
#            sourceImages += glob.glob(self.inputs[0] + "/*.img")
#            sourceImages += glob.glob(self.inputs[0] + "/*.asc")
#            hdrs = glob.glob(self.inputs[0] + "/*/hdr.adf")
#            for hdr in hdrs:
#                folder,hdr = os.path.split(hdr)
#                sourceImages.append(folder)
#
#            if len(sourceImages) == 0:
#                raise Exception, "Input directory (" + self.inputs[0] + ") did not contain any rasters in one of the recognized formats (tif, img, asc, grid)"
#
#        else:
#            sourceImages = self.inputs
#            #if self.verbose == True:  print "source images: " + str(sourceImages)
#
#        return sourceImages

    def reprojectRaster(self, srcDs, sourceParams, templateParams, 
                    destFile, resamplingType, shortName='', outputCellSize = None):
        """
        Reprojects a raster to match the templateParams
        if outputCellSize is not provided defaults to the template cellSize
        """
#        driver = gdal.GetDriverByName("AAIGrid")
#        driver.Register()
        
        tmpOutput = os.path.splitext(destFile)[0] + ".tif"
        
        tmpOutDataset = self.generateOutputDS(sourceParams, templateParams, tmpOutput, outputCellSize)

        err = gdal.ReprojectImage(srcDs, tmpOutDataset, sourceParams["srs"].ExportToWkt(), 
                                templateParams["srs"].ExportToWkt(), resamplingType)
        self.writetolog("    Saving " + shortName)
#        dst_ds = driver.CreateCopy(destFile, tmpOutDataset, 0)
        self.writetolog("    Finished Saving " + shortName)
        dst_ds = None
        tmpOutDataset = None
        
    def generateOutputDS(self, sourceParams, templateParams, 
                        tmpOutput, outputCellSize = None):
        """
        Creates an output dataset (tiff format) that
          has the nodata value of the sourceParams but
          all other attributes from the templateParams
        This output is saved to tmpOutput.
        
        The optional cell size will override the cell size 
            specified in the templateParams
        """
        tifDriver = gdal.GetDriverByName("GTiff")
        
        if outputCellSize == None:
            width = templateParams["width"]
            height = templateParams["height"]
        else:
            width = templateParams["width"] * int(templateParams["xScale"]/outputCellSize)
            height = templateParams["height"] * int(templateParams["xScale"]/outputCellSize)
        
        if sourceParams["signedByte"]: 
            tmpOutDataset = tifDriver.Create(tmpOutput, 
                                            width,
                                            height,
                                            1, sourceParams["PixelType"], ["PIXELTYPE=SIGNEDBYTE"])
        else:
            tmpOutDataset = tifDriver.Create(tmpOutput, 
                                            width,
                                            height,
                                            1, sourceParams["PixelType"])
        
            
        if outputCellSize == None:
            outputCellSize = templateParams["xScale"]
        gtList = list(templateParams["gt"])
        if templateParams["xScale"] < 0:
            gtList[1] = -1 * outputCellSize
        else:
            gtList[1] = outputCellSize
        if templateParams["yScale"] < 0:
            gtList[5] = -1 * outputCellSize
        else:
            gtList[5] = outputCellSize
        gt = tuple(gtList)
        
        tmpOutDataset.SetGeoTransform(gt)
        tmpOutDataset.SetProjection(templateParams["prj"])
        tmpOutDataset.GetRasterBand(1).SetNoDataValue(sourceParams["NoData"])
        if sourceParams["signedByte"]:
            #tmpOutDataset.GetRasterBand(1).SetMetadataItem('PIXELTYPE', "SIGNEDBYTE")
            tmpOutDataset.GetRasterBand(1).PixelType = "SIGNEDBYTE"
            tmpOutDataset.GetRasterBand(1).SetMetadata({'PIXELTYPE': 'SIGNEDBYTE'}, 'IMAGE_STRUCTURE')
            
        if self.verbose:
            print tmpOutput
            print "noDataValue = ", tmpOutDataset.GetRasterBand(1).GetNoDataValue()
            print "Pixel type = ", gdal.GetDataTypeName(tmpOutDataset.GetRasterBand(1).DataType)
        return tmpOutDataset

    def getShortName(self, fullPathName):
        if fullPathName.endswith('hdr.adf'):
            shortname = os.path.split(fullPathName)[0]
            shortname = os.path.split(shortname)[1]
        else:
            shortname = os.path.split(fullPathName)[1]
            shortname = os.path.splitext(shortname)[0]
        return shortname
    
if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
#    try:
##        PARC().testing()
#        sys.exit(PARC().main(sys.argv[1:]))
#    except Exception as e:
#        print e
#        sys.exit(1)
