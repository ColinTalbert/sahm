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

import sys
import csv
import os
import time
import random
import shutil
import numpy as np
import scipy.stats.stats as stats 

from osgeo import gdalconst
from osgeo import gdal

from optparse import OptionParser
import utilities
#from Utilities import self.writetolog

class MDSBuilder(object):
    '''Takes a csv with animal location x,y and attributes each with the values 
    extracted from each of the grids (covariates) indicated in the supplied 
    csv list of files
    '''
    def __init__(self):
        #instance level variables
        self.verbose = False
        self.inputsCSV = ''
        self.inputs = []
        self.fieldData = ''
        self.outputMDS = ''
        self.pointType = 'Background'
        self.probSurface = ''
        self.pointCount = 0
        self.ndFlag = 'NA'
        self.seed = None
        self.deleteTemp = False
        self.logger = None
    
    def validateArgs(self):
        """run a series of checks on our inputs
        
        Checks for:
        1) field data csv exists
        2) inputs csv exists
        3) each of the files listed in the inputs csv exists.
        4) if a propSurface file was supplied it exists
        5) if point count supplied its an integer greater than 0
        6) output directory exists
        """
        
        #1) field data csv exists
        if self.fieldData != '' and not os.path.exists(self.fieldData):
            raise RuntimeError, "Could not find supplied CSV file of fieldData provided.  Please check input file: " + str(self.fieldData)
         
        #2) inputs csv exists
        if self.inputsCSV != '' and not os.path.exists(self.inputsCSV):
            raise RuntimeError, "Could not find CSV file of inputs provided.  Please check input file: " + str(self.inputsCSV) 
        
        #3) each of the files listed in the inputs csv exists.
        if os.path.exists(self.inputsCSV):
            reader = csv.reader(open(self.inputsCSV, 'r'))
            header = reader.next()
            missingFiles = []
            for row in reader:
                if not self.isRaster(row[0]):
                    missingFiles.append(row[0])
            if not len(missingFiles) ==0:
                msg = "One or more of the files in the input covariate list CSV could not be identified as rasters by GDAL."
                msg += "\n    ".join(missingFiles)
                raise RuntimeError, msg
            
        #4) if a propSurface file was supplied it exists
        if self.probSurface <> '':
            if not self.isRaster(self.probSurface):
                raise RuntimeError, "The supplied probability surface, " + self.probSurface + ", does not appear to be a valid raster."
        
        #5) if point count supplied its an integer greater than 0
        try:
            self.pointCount = int(self.pointCount)
        except:
            raise RuntimeError, "The supplied point count parameter, " + self.pointCount +", does not appear to be an integer "
        if not self.pointCount > 0:
            raise RuntimeError, "The supplied point count parameter, " + self.pointCount +", must be greater than 0" 
        
        #6) output directory exists
        outDir = os.path.split(self.outputMDS)[0]
        if not os.path.exists(outDir):
            raise RuntimeError, "The directory of the supplied MDS output file path, " + self.outputMDS +", does not appear to exist on the filesystem"
        
        if self.logger is None:
            self.logger = utilities.logger(outDir, self.verbose)  
        self.writetolog = self.logger.writetolog
                  
    def run(self):
        '''
        The main execution method which calls 
        each of the processing step functions
        '''
        
        self.validateArgs()
        self.writetolog('\nRunning MDSBuilder', True, True)
        # start timing
        startTime = time.time()
        gdal.UseExceptions()
        gdal.AllRegister()
        
        self.constructEmptyMDS()
        
        if self.pointCount <> 0:
            self.addBackgroundPoints()
        
        if self.fieldData != '':
            self.pullFieldDataValues()
        
        
        # figure out how long the script took to run
        endTime = time.time()
        
        if self.verbose:
            self.writetolog('Finished running MDSBuilder', True, True)
            self.writetolog('    The process took ' + str(endTime - startTime) + ' seconds')
        
    def constructEmptyMDS(self):
        '''Creates the triple header line format of our output file.
        Also parses the inputs file to append the '_categorical' flag 
        to the covariate names of all categorical inputs.
        '''        
        
        if os.path.exists(self.fieldData):
            fieldDataCSV = csv.reader(open(self.fieldData, "r"))
            origHeader = fieldDataCSV.next()
            fullHeader = ["X", "Y"]
            if origHeader[2].lower not in ["responsebinary", "responsecount"]:
                #inputs not conforming to our expected format will be assumed
                #to be binary data
                fullHeader.append("responseBinary")
            else:
                fullHeader.append(origHeader[2])
        else:
            # no field data provided construct output MDS which is header only.
            fullHeader = ["X", "Y", "responseBinary"]
            origHeader = []

        inputs_CSV = csv.reader(open(self.inputsCSV, "r"))
        inputs_header = inputs_CSV.next()
        self.inputs = []

        #each row contains a covariate raster layer
        #item 0 is the full path to the file
        #item 1 is 0/1 indicating categorical
        #Construct our output header by extracting each individual 
        #file (raster) name and appending '_categorical' if the flag is 1
        rasters = {}
        for row in inputs_CSV:
            temp_raster = row[0]
            raster_shortname = os.path.split(temp_raster)[1]
            raster_shortname = os.path.splitext(raster_shortname)[0]
            if len(row) > 1 and row[1] == '1':
                raster_shortname += "_categorical"
            rasters[raster_shortname] = temp_raster
            
        keys = rasters.keys()
        keys.sort(key=lambda s: s.lower())
        keys.sort(key=lambda s: s.lower())
        for key in keys:
            self.inputs.append(rasters[key])
            fullHeader.append(key)

        #Open up and write to an output file
        oFile = open(self.outputMDS, 'wb')
        fOut = csv.writer(oFile, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
        
        #Format and write out our three header lines
        #    the original template, fieldData, and parc folder are 
        #    stored in spots 1,2,3 in the second header line
        if len(origHeader) > 3 and \
            (origHeader[3] == 'Weights' or
             origHeader[3] == 'frequency'):
            #The input is an output from Field data query
            original_field_data = origHeader[-1]
            field_data_template = origHeader[-2]
        else:
            #The input is a raw field data file
            original_field_data = self.fieldData
            field_data_template = "NA"
        
        if len(inputs_header) > 5:
            self.parc_template = inputs_header[5]
            parc_workspace = inputs_header[6]
        else:
            self.parc_template = "Unknown"
            parc_workspace = "Unknown"
        

        secondRow = [original_field_data, field_data_template, ""] + ["1" for elem in self.inputs]
        thirdRow = [self.parc_template, parc_workspace, ""] + self.inputs
        
        if "Weights" in origHeader:
            fullHeader.append("Weights")
            secondRow.append("0")
            thirdRow.append("0")
            self.hasWeight = True
            self.weightCol = origHeader.index("Weights")
        else:
            self.hasWeight = False
        
        fOut.writerow(fullHeader)
        fOut.writerow(secondRow)
        fOut.writerow(thirdRow)
        oFile.close()
        del fOut
    
    def readInPoints(self):
        '''Loop through each row in our field data and add the X, Y, response
        to our in memory list of rows to write to our output MDS file
        '''
        fieldDataCSV = csv.reader(open(self.fieldData, "r"))
        origHeader = fieldDataCSV.next()
        points = []
        for row in fieldDataCSV:
            tmpRow = row[:3]
            if self.hasWeight:
                try:
                    tmpRow.append(row[self.weightCol])
                except IndexError:
                    tmpRow.append("1")
            points.append(tmpRow)

        del fieldDataCSV
        return points
    
    def addBackgroundPoints(self):
        '''Add self.pointCount number of points to the supplied field data
        If a probability surface was provided the distribution will 
        follow this otherwise it will be uniform within the extent of the 
        first of our inputs.
        Each pixel is sampled only once.
        '''
        #determine what value we'll be using (background/pseudoabsense)
        if self.pointType == 'Background':
            pointval = '-9999'
        else:
            pointval = '-9998'
        
        #initialize the random seed in case one was passed
        if not self.seed:
            self.seed = random.randint(0, sys.maxint)
        random.seed(self.seed)
        msg = "    seed used for background point generation = " + str(self.seed)
        self.writetolog(msg)
        
        #step one check how many points we'll have to sample before we can 
        #expect to get as many as requested        
        proportionToCheck = self.probSurfaceSanityCheck()
        
        #First we'll create a temp copy of the Field Data to work with.
        shutil.copy(self.fieldData, self.fieldData + ".tmp.csv")
        self.fieldData = self.fieldData + ".tmp.csv"
        self.deleteTemp = True
        
        #Open up and write to an output file
        oFile = open(self.fieldData, 'ab')
        fOut = csv.writer(oFile, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
        
        if proportionToCheck < 0.33:
            #we need relatively few points it will be most efficient 
            #to simply loop through random points and throw out the 
            #duplicates
            self.pullBackgroundSingly(fOut, pointval)
        else:
            #The simple algorithm doesn't scale.
            #instead we're going to tile through our probability surface
            #randomly assign each pixel to it's include/remove status
            #and store a list of the pixels to include.  
            #A random sample of this list will be used as our background points.
            if self.probSurface == '':
                self.pullBackgroundTiledNoProbSurface(fOut, pointval)
            else: 
                self.pullBackgroundTiledWithProbSurface(fOut, pointval)
            
        print "Done!\n"
        oFile.close()
        del fOut
            
    def pullBackgroundSingly(self, fOut, pointVal):
        '''We need a small proportion of the total pixels.
        We'll do our sampling without replacement by randomly sampling pixels
        and keeping track of the ones we've tried so we don't retry them.
        
        Each found pixel will be written to a new row in our temp output file 
        as it's found.
        '''
        if self.probSurface != '':
            probRaster = SAHMRaster(self.probSurface)
            useProbSurf = True
        else:
            probRaster = SAHMRaster(self.inputs[0])
            useProbSurf = False
        
        if self.verbose:
            self.writetolog('    Starting generation of ' + str(self.pointCount) + ' random background points, ')
            if self.probSurface <> '':
                self.writetolog('      using ' + self.probSurface + ' as the probability surface.')
            print "    Percent Done:    ",
        
        triedPixels = set()  #all the pixels we've tried.
       
        pcntDone = 0
        foundPoints = 0
        pixelProb = 100
        
        while foundPoints < self.pointCount: #loop until we find enough:
            col, row = probRaster.getRandomPixel()
            
            if (col, row) in triedPixels:
                pass
            else:
                triedPixels.add((col, row))
                # if they supplied a probability surface ignore the random pixel
                # if a random number between 1 and 100 is > the probability surface value
                if useProbSurf:
                    pixelProb = probRaster.getPixelValueFromIndex(col, row)
                keep = pixelProb >= random.randint(1,100)
                if keep:
                    #convert our outValues for row, col to coordinates
                    x, y = probRaster.convertColRowToCoords(col, row)
                    tmpPixel = [x, y, pointVal]
                    
                    fOut.writerow(tmpPixel)
                    foundPoints += 1
                    if self.verbose:
                        if float(foundPoints)/self.pointCount > float(pcntDone)/100:
                            pcntDone += 10
                            print str(pcntDone) + "...",


    def pullBackgroundTiledNoProbSurface(self, fOut, pointVal):
        """
        The user didn't supply a probability surface but we still need to 
        sample a large proportion of the total number of cells.  
        Either they have a small template or a large number of points.
        """
        probsurf = SAHMRaster(self.inputs[0])
        rows = int(probsurf.params["height"])
        cols = int(probsurf.params["width"])
        
        cellIndices = np.arange(cols * rows)
        
        backgrounds = random.sample(cellIndices, self.pointCount) 
        
        for cell in backgrounds:
            col, row = divmod(cell, rows)
            x, y = probsurf.convertColRowToCoords(col, row)
            tmpPixel = [x, y, pointVal]
            fOut.writerow(tmpPixel)
        
    def pullBackgroundTiledWithProbSurface(self, fOut, pointVal):
        probRaster = SAHMRaster(self.probSurface)
          
        rows = int(probRaster.params["height"])
        cols = int(probRaster.params["width"])
          
#        outDir = os.path.split(self.outputMDS)[0] 
#        tmpOutput = os.path.join(outDir,  + "tmp_classified_prob.tif")
#        
#        driver = probRaster.DS.GetDriver()
#        outDataset = driver.Create(tmpOutput, cols, rows, 1, gdalconst.GDT_Byte)
    
        bSize = 1024 #pixels per block
        fullPixelCount = bSize * bSize  #number of pixels in one block
        
        goodIndices = []
        
        for i in range(0, rows, bSize):
            if i + bSize  < rows:
                numRows = bSize
            else:
                numRows = rows - i
                
            #loop through each block(tile) in the raster and add
            #it's average to our array of averages. 
            for j in range(0, cols, bSize):
                if j + bSize < cols:
                    numCols = bSize
                else:
                    numCols = cols - j
                
                data = probRaster.band.ReadAsArray(j, i, numCols, numRows)
                data[data==probRaster.params["NoData"]] = 0
                randomVals = np.random.rand(numRows, numCols)*100
                
                successes = np.where(data>randomVals)
                
                indexes = list((successes[1] + j) * rows + (successes[0] + i))
                goodIndices.extend(indexes)

        backgrounds = random.sample(goodIndices, self.pointCount) 
        
        for cell in backgrounds:
            col, row = divmod(cell, rows)
            x, y = probRaster.convertColRowToCoords(col, row)
            tmpPixel = [x, y, pointVal]
            fOut.writerow(tmpPixel)
        
    def pullFieldDataValues(self):
        outputRows = self.readInPoints()
        
        #loop though each of the supplied rasters and add the 
        #extracted values to
        badpoints = []
        for input in self.inputs:
            inputND = self.getND(input)
            rasterDS = gdal.Open(input, gdalconst.GA_ReadOnly)
            # get image size
            rows = rasterDS.RasterYSize
            cols = rasterDS.RasterXSize
            band = rasterDS.GetRasterBand(1)
            # get georeference info
            transform = rasterDS.GetGeoTransform()
            xOrigin = transform[0]
            yOrigin = transform[3]
            pixelWidth = transform[1]
            pixelHeight = transform[5]
            
            if self.verbose:
                self.writetolog("    Extracting raster values for " + input)
                print "    ",
            
            pcntDone = 0
            i = 1
            for row in outputRows:
                'loop through each of our points'
                x = float(row[0])
                y = float(row[1])
#                if 
                # compute pixel offset
                xOffset = int((x - xOrigin) / pixelWidth)
                yOffset = int((y - yOrigin) / pixelHeight)
#                try:
                if xOffset < 0 or yOffset < 0:
                    if row[:3] not in badpoints:
                        badpoints.append(row[:3])
                    row.append(str(self.ndFlag))
                else:
                    try:
                        data = band.ReadAsArray(xOffset, yOffset, 1, 1)
                        value = data[0,0]
                        if value <> inputND and not np.isnan(value):
                            row.append(value)
                        else:
                            row.append(str(self.ndFlag))
                    except:
                        badpoints.append(row[:3])
                        row.append(str(self.ndFlag))
                
                if self.verbose:
                    if i/float(len(outputRows)) > float(pcntDone)/100:
                        pcntDone += 10
                        print str(pcntDone) + "...",
                i += 1
                
            
            if self.verbose:
                self.writetolog("    Done")
        if len(badpoints) > 0:
                msg =  "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                msg += "\n!!!!!!!!!!!!!!!!!!!!!WARNING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
                msg += str(len(badpoints)) + " point fell outside the Covariate coverage."
                msg += "\nThese points were assigned the NoData value of 'NA' for all covariates and will"
                msg += "not be included in subsequent models.\n     These points are:"
                for badpoint in badpoints:
                    msg += "     x:" + str(row[0]) + " Y: " + str(row[1]) 
                    msg += " response: " + str(row[2]) 
                self.writetolog(msg)
            
            
        outputMDS = csv.writer(open(self.outputMDS, 'ab'))
        thrownOut = 0
        kept = 0
        for row in outputRows:
            #remove this if when Marian handles the ND
            if self.hasWeight:
                #move the weight value to the end of our list
                weight = row[3]
                del row[3]
                row.append(weight)

            if not str(self.ndFlag) in row[3:]:
                outputMDS.writerow(row)
                kept += 1
            else:
                outputMDS.writerow(row)
                thrownOut += 1
        if thrownOut > 0:
            msg =  "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
            msg += "\n!!!!!!!!!!!!!!!!!!!!!WARNING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
            msg += str(thrownOut) + " points had 'nodata' in at least one of the covariates."
            msg += "\nThese points will not be considered in subsequent Models."
            msg +=  "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
            msg +=  "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
            self.writetolog(msg)
        del outputMDS
        
        #convert the mds csv to a shapefile
        output_shp = self.outputMDS.replace(".csv", "_shapefiles")
        utilities.mds_to_shape(self.outputMDS, output_shp)
        

        
        if self.deleteTemp:
            #if this flag is trud the field data we're working with is 
            # a temporary copy which we created so that we could add
            # background points.  Delete it to clean up our working file.
            os.remove(self.fieldData)
    
    def floatEquality(self, a, b):
        #equality comparison between two floating point values
        epsilon = 0.000000000001 
        return (abs(a - b)) <= epsilon
    
    def getRasters(self, directory):
        #the list of rasters in the given directory
        rasters = []
        dirList = os.listdir(directory)
        for file in [elem for elem in dirList if elem[-4:].lower() == ".tif"]:
            if isRaster(os.path.join(directory, file)):
                rasters.append(os.path.join(directory, file))
        for file in [elem for elem in dirList if elem[-4:].lower() == ".asc"]:
            if isRaster(os.path.join(directory,file)):
                rasters.append(os.path.join(directory, file))
        for folder in [name for name in dirList 
                       if os.path.isdir(os.path.join(directory, name)) ]:
            if isRaster(os.path.join(directory, folder)):
                rasters.append(os.path.join(directory, folder))
    
        return rasters
    
    def isRaster(self, filePath):
        '''Verifies that a passed file and path is recognized by
        GDAL as a raster file.
        '''
        try:
            dataset = gdal.Open(filePath, gdalconst.GA_ReadOnly)
            if dataset is None:
                return False
            else:
                return True
                del dataset
        except:
            return False
        
    def getND(self, raster):
        dataset = gdal.Open(raster, gdalconst.GA_ReadOnly)
        ND = dataset.GetRasterBand(1).GetNoDataValue()
        if ND is None:
            if dataset.GetRasterBand(1).DataType == 1:
                print "Warning:  Could not extract NoData value.  Using assumed nodata value of 255"
                return 255
            elif dataset.GetRasterBand(1).DataType == 2:
                print "Warning:  Could not extract NoData value.  Using assumed nodata value of 65536"
                return 65536
            elif dataset.GetRasterBand(1).DataType == 3:
                print "Warning:  Could not extract NoData value.  Using assumed nodata value of 32767"
                return 32767
            elif dataset.GetRasterBand(1).DataType == 4:
                print "Warning:  Could not extract NoData value.  Using assumed nodata value of 2147483647"
                return 2147483647
            elif dataset.GetRasterBand(1).DataType == 5:
                print "Warning:  Could not extract NoData value.  Using assumed nodata value of 2147483647"
                return 2147483647
            elif dataset.GetRasterBand(1).DataType == 6:
                print "Warning:  Could not extract NoData value.  Using assumed nodata value of -3.40282346639e+038"
                return -3.40282346639e+038
            else:
                return None
        else:
            return ND

    def estimatePointsToCheck(self):
        """calculate the expected number of points we will have 
        to sample to get the desired number of background sample points.
        
        return integer 
        """
        if self.probSurface == '':
            #everypoint is included
            return self.pointCount
        else:
            probsurfaceDS = gdal.Open(self.probSurface, gdalconst.GA_ReadOnly)
            ave_prob = self.getMeanProb(probsurfaceDS)
            #  The 1.1 is a 10% fudge factor.
            return int(1.0/ave_prob * self.pointCount * 1.1) 

    def probSurfaceSanityCheck(self):
        '''ascertain how hard it will be to find the required
        number of points given the entered probability surface.
        
        return proportion we will have to sample.  
        '''

        #step2 calculate the expected maximum number of points available
        if self.probSurface == '':
            probparams = utilities.getRasterParams(self, self.inputs[0])
            aveProb = 100
        else:
            probparams = utilities.getRasterParams(self, self.probSurface)
            aveProb = self.getMeanProb(self.probSurface)
            
        if aveProb < 1.0:
            msg = "WARNING:  This function expects rasters scaled from 0 to 100.\n"
            msg += "The average probability in the specified probability surface raster "
            msg += " is less than one. This might indicate an input that is scaled from 0.0 to 1.0."
            msg += "\nThis might also indicate numerous cells with a probability of 0 which is acceptable."
            self.writetolog(msg, True, True)
        if aveProb > 100:    
            msg = "WARNING:  This function expects rasters scaled from 0 to 100.\n"
            msg += "The average probability in the specified probability surface raster "
            msg += " is greater than 100. This might indicate an input that is not scaled appropriately."
            self.writetolog(msg, True, True)
             
        max_points = aveProb/100 * probparams["width"] * probparams["height"]
        
        #Check if our expected number of points is over or approaching
        #the number of available cells
        if  self.pointCount > max_points:
            msg = "The number of random background points, " + str(self.pointCount)
            msg += " exceeds the expected number of available points given the specified probability surface. "
            msg += "\n" + self.probSurface + " has an average pixel probability of " + str(aveProb) + ".\n"
            msg += "Which when multiplied by the cell dimensions of " + str(probparams["width"]) + " x " + str(probparams["height"])
            msg += " results in an expected maximum number of available random points of " + str(max_points)
            msg += "\n\nTry either reducing the number of background points or using a less restrictive probability surface\n"
            raise RuntimeError, msg
        elif self.pointCount > max_points * 0.5:
            msg = "The number of random background points, " + str(self.pointCount)
            msg += " is greater than 50% the expected number of available points given the specified probability surface. "
            msg += "\n" + self.probSurface + " has an average pixel probability of " + str(aveProb) + ".\n"
            msg += "Which when multiplied by the cell dimensions of " 
            msg += str(probparams["width"]) + " x " + str(probparams["height"])
            msg += " results in an expected maximum number of available random points of " + str(max_points)
            msg += "\n\n processing time and memory use could be excessive and problematic.\n" 
            self.writetolog(msg, True, True)

        return self.pointCount / float(max_points)
            
    def getMeanProb(self, inputRaster):
        """Calculate the mean probability (value) for a raster.
        
        return float.
        
        arguments:
        probSurfaceDS    --  GDAL raster dataset
        """
        DS = gdal.Open(inputRaster, gdalconst.GA_ReadOnly)
        band = DS.GetRasterBand(1)
        probParams = utilities.getRasterParams(self, inputRaster)
        rows = int(probParams["height"])
        cols = int(probParams["width"])
          
        bSize = 1024 #pixels per block
        fullPixelCount = bSize * bSize  #number of pixels in one block
        
        avs = []
        wts = []
        
        for i in range(0, rows, bSize):
            if i + bSize  < rows:
                numRows = bSize
            else:
                numRows = rows - i
                
            #loop through each block(tile) in the raster and add
            #it's average to our array of averages. 
            for j in range(0, cols, bSize):
                if j + bSize < cols:
                    numCols = bSize
                else:
                    numCols = cols - j
                
                data = band.ReadAsArray(j, i, numCols, numRows)
                data[data==probParams["NoData"]] = 0
                
                avs.append(np.average(data))
                wts.append(data.size / float(fullPixelCount))
        
        #return a weighted average of each tile where the weights are the tile sizes.
        return np.average(avs,weights=wts)

#    def drawSample(self, sampleSize, rows, cols):
#        if sampleSize < (rows * cols * 0.3):
#            # we need a relatively small sample relative to the total number of possible pixels
#            #it's most efficient to randomly select points and throw out duplicates.
#
#            triedCells = set()
#            keptCells = set()
#            
#            pcntDone
#            while sampleSize < keptCells:
#                x = rdm.randint(0, xCount)
#                y = rdm.randint(0, yCount)
#                if (x, y) in triedCells:
#                    pass
#                else:
#                    triedCells.add((x, y))
#                    keep = rdm.random() < keep_probability
#                    if keep:
#                        keptCells.add((x,y))
#        else:
#            #We need many samples relative to the total population of pixels.
#            #
#            #every possible cell
#            all_cells = np.arange(xCount * yCount)

class SAHMRaster():

    def __init__(self, rasterFile):
        self.source = rasterFile
        self.params = utilities.getRasterParams(self, rasterFile)
        self.DS = gdal.Open(rasterFile, gdalconst.GA_ReadOnly)
        self.band = self.DS.GetRasterBand(1)
        
    def getRandomPixel(self):
        col = random.randint(0, self.params["width"] - 1) 
        row = random.randint(0, self.params["height"] - 1)
        return col, row
    
    def convertColRowToCoords(self, col, row, pixelCenter=True):
        x = self.params["west"] + col * self.params["xScale"]
        
        y = self.params["north"] + row * self.params["yScale"]
        
        if pixelCenter:
            x += self.params["xScale"] / 2.0
            y += self.params["yScale"] / 2.0
        return x, y
        
    def convertCoordsToColRow(self, x, y):
        row = x/self.params["xScale"] - self.params["west"]
        col = y/self.params["yScale"] - self.params["north"]
        return col, row
    
    def getPixelValueFromIndex(self, col, row):
        return self.band.ReadAsArray(col, row, 1, 1)[0,0]
    
    def getPixelValueFromCoords(self, x, y):
        col, row = self.convertCoordsToColRow(x, y)
        return self.getPixelValueFromIndex(col, row)

         
def main(argv):
    
    usageStmt = "usage:  options: -f --fieldData -i --inCSV -o --output -pc --pointcount -ps --probSurface"
    desc = "Creates a merged dataset file (mds) from a csv of field points and rasters located in a csv listing one raster per line.  Optionally this module adds a number of background points to the output."
    parser = OptionParser(usage=usageStmt, description=desc)
    
    parser.add_option("-v", 
                      dest="verbose", 
                      default=False, 
                      action="store_true", 
                      help="the verbose flag causes diagnostic output to print")
    parser.add_option("-f", "--fieldData", 
                      dest="fieldData", 
                      help="The input CSV of field data points")
    parser.add_option("-i", "--inCSV", 
                      dest="inputsCSV", 
                      help="The input CSV containing a list of our inputs, one per line.")              
    parser.add_option("-o", "--output", 
                      dest="outputMDS", 
                      help="Output MDS file to save to.")
    parser.add_option("-p", "--probSurface", 
                      dest="probSurface",
                      default='', 
                      help="Probability surface to use for generation of background points (optional)")
    parser.add_option("-c", "--pointCount", 
                      dest="pointCount",
                      default=0, 
                      help="Number of random background points to add(optional)")
    
    (options, args) = parser.parse_args(argv)
    
    ourMDS = MDSBuilder()
    ourMDS.verbose = options.verbose
    ourMDS.fieldData = options.fieldData
    ourMDS.inputsCSV = options.inputsCSV
    ourMDS.probSurface = options.probSurface
    ourMDS.pointCount = options.pointCount
    ourMDS.outputMDS = options.outputMDS
    ourMDS.run()
   

if __name__ == '__main__':
    sys.exit(main(sys.argv))