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
        self.outputMDS  = ''
        self.pointtype = 'Background'
        self.probsurf = ''
        self.pointcount = 0
        self.NDFlag = 'NA'
        self.seed = None
        self.deleteTmp = False
        self.logger = None
    
    def validateArgs(self):
        #check our CSV file for expectations
        if self.fieldData != '' and not os.path.exists(self.fieldData):
            raise RuntimeError, "Could not find supplied CSV file of fieldData provided.  Please check input file: " + str(self.fieldData)
         
        if not os.path.exists(self.inputsCSV):
            raise RuntimeError, "Could not find CSV file of inputs provided.  Please check input file: " + str(self.inputsCSV) 
        
        #check the input CSV file of inputs
        reader = csv.reader(open(self.inputsCSV, 'r'))
        header = reader.next()
        missingfiles = []
        for row in reader:
            if not self.isRaster(row[0]):
                missingfiles.append(row[0])
        if not len(missingfiles) ==0:
            msg = "One or more of the files in the input covariate list CSV could not be identified as rasters by GDAL."
            msg += "\n    ".join(missingfiles)
            raise RuntimeError, msg
            
        if self.probsurf <> '':
            if not self.isRaster(self.probsurf):
                raise RuntimeError, "The supplied probability surface, " + self.probsurf + ", does not appear to be a valid raster."
        
        try:
            self.pointcount = int(self.pointcount)
        except:
            raise RuntimeError, "The supplied point count parameter, " + self.pointcount +", does not appear to be an integer"
        
        #make sure the directory the mds file is going into exists:
        outDir = os.path.split(self.outputMDS)[0]
        if not os.path.exists(outDir):
            raise RuntimeError, "The directory of the supplied MDS output file path, " + self.outputMDS +", does not appear to exist on the filesystem"
        
        if self.logger is None:
            self.logger = utilities.logger(outDir, self.verbose)  
        self.writetolog = self.logger.writetolog
            
    def run(self):
        '''
        This routine loops through a set of rasters and creates an MDS file
        '''
        
        self.validateArgs()
        self.writetolog('\nRunning MDSBuilder', True, True)
        # start timing
        startTime = time.time()
        gdal.UseExceptions()
        gdal.AllRegister()
        
        self.constructEmptyMDS()
        
        if self.pointcount <> 0:
            self.addBackgroundPoints()
        
        if self.fieldData != '':
            self.pull_fielddata_vals()
        
        
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
            field_data_CSV = csv.reader(open(self.fieldData, "r"))
            orig_header = field_data_CSV.next()
            full_header = ["X", "Y"]
            if orig_header[2].lower not in ["responsebinary", "responsecount"]:
                #inputs not conforming to our expected format will be assumed
                #to be binary data
                full_header.append("responseBinary")
            else:
                full_header.append(orig_header[2])
        else:
            full_header = ["X", "Y", "responseBinary"]
            orig_header = []

        
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
#            self.inputs.append(temp_raster)
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
            full_header.append(key)

        #Open up and write to an output file
        oFile = open(self.outputMDS, 'wb')
        fOut = csv.writer(oFile, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
        
        #Format and write out our three header lines
        #    the original template, fieldData, and parc folder are 
        #    stored in spots 1,2,3 in the second header line
        if len(orig_header) > 3 and \
            (orig_header[3] == 'Weights' or
             orig_header[3] == 'frequency'):
            #The input is an output from Field data query
            original_field_data = orig_header[-1]
            field_data_template = orig_header[-2]
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
        
        if "Weights" in orig_header:
            full_header.append("Weights")
            secondRow.append("0")
            thirdRow.append("0")
            self.hasWeight = True
            self.weightCol = orig_header.index("Weights")
        else:
            self.hasWeight = False
        
        fOut.writerow(full_header)
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
#        tmpRow = 
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
        '''Add pointcount number of points to the supplied field data
        If a probability surface was provided the distribution will 
        follow this otherwise it will be uniform within the extent of the first of our inputs.
        No more than one per pixel is used.
        '''
        
        if self.pointtype == 'Background':
            pointval = '-9999'
        else:
            pointval = '-9998'
        
        #initialize the random seed in case one was passed
        if not self.seed:
            self.seed = random.randint(0, sys.maxint)
        random.seed(self.seed)
        self.writetolog("    seed used for background point generation = " + str(self.seed))
        
        #First we'll create a temp copy of the Field Data to work with.
        shutil.copy(self.fieldData, self.fieldData + ".tmp.csv")
        self.fieldData = self.fieldData + ".tmp.csv"
        self.deleteTmp = True
        
        #step one check how many points we'll have to sample before we can 
        #expect to get as many as requested
        tocheck_pointcount = self.estimate_backgroundpoints()
        
        proportion_to_check = self.probsurface_sanitycheck()
        if proportion_to_check < 0.33:
            #we need relatively few points it will be most efficient 
            #to simply loop through random points and throw out the 
            #duplicates
            self.pull_background_singly()
        else:
            #The simple algorithm doesn't scale.
            #instead we're going to tile through our probability surface
            #randomly assign each pixel to it's include/remove status
            #and store a list of the pixels to include.  
            #A random sample of this list will be used as our background points.
            if self.probsurf == '':
                self.pull_background_tiled_noprob()
            else: 
                self.pull_background_tiled_probsurface()
            
            
    def pull_background_singly(self):
        
        if self.probsurf == '':
            probparams = utilities.getRasterParams(self, self.inputs[0])
            probDS = gdal.Open(self.probsurf, gdalconst.GA_ReadOnly)
            band = probDS.GetRasterBand(1)
            useProbSurf = True
        else:
            probparams = utilities.getRasterParams(self, self.probsurf)
            useProbSurf = False
        
        #Open up and write to an output file
        oFile = open(self.fieldData, 'ab')
        fOut = csv.writer(oFile, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
        
        if self.verbose:
            self.writetolog('    Starting generation of ' + str(self.pointcount) + ' random background points, ')
            if self.probsurf <> '':
                self.writetolog('      using ' + self.probsurf + ' as the probability surface.')
            print "    Percent Done:    ",
        
        tried_cells = set()#all the pixels we've tried.
       
        pcntDone = 0
        foundPoints = 0
        pixelProb = 100
        while foundPoints < self.pointcount: #loop until we find enough:
            x = random.randint(0, probparams["width"] - 1) 
            y = random.randint(0, probparams["height"] - 1)
            
            if (x, y) in tried_cells:
                pass
            else:
                tried_cells.add((x, y))
                # if they supplied a probability surface ignore the random pixel
                # if a random number between 1 and 100 is > the probability surface value
                if useProbSurf:
                    pixelProb = int(band.ReadAsArray(x, y, 1, 1)[0,0])
                keep = random.randint(1,100) > pixelProb
                if keep:
                    #convert our outValues for row, col to coordinates
                    tmpPixel = [x, y, pointval]
                    tmpPixel[0] = probparams["west"] + tmpPixel[0] * probparams["xScale"]
                    tmpPixel[1] = probparams["north"] + tmpPixel[1] * probparams["yScale"]
                    
                    fOut.writerow(tmpPixel)
                    foundPoints += 1
                    if self.verbose:
                        if float(foundPoints)/self.pointcount > float(pcntDone)/100:
                            pcntDone += 10
                            print str(pcntDone) + "...",
                    if float(len(kept_cells))/sample_count > float(pcntDone)/100:
                        pcntDone += 10
                        print str(pcntDone) + "...",
        
        print "Done!\n"
        oFile.close()
        del fOut

    def pull_background_tiled_noprob(self):
        if self.probsurf == '':
            probparams = utilities.getRasterParams(self, self.inputs[0])
            probDS = gdal.Open(self.probsurf, gdalconst.GA_ReadOnly)
            band = probDS.GetRasterBand(1)
            useProbSurf = True
        else:
            probparams = utilities.getRasterParams(self, self.probsurf)
            useProbSurf = False
          
        outDir = os.path.split(self.outputMDS)[0] 
        tmpOutput = os.path.join(outDir,  + "tmp_classified_prob.tif")
        
        os.path.unlink(tmpOutput)

    def pull_background_tiled_probsurface(self):
        if self.probsurf == '':
            probparams = utilities.getRasterParams(self, self.inputs[0])
            probDS = gdal.Open(self.probsurf, gdalconst.GA_ReadOnly)
            band = probDS.GetRasterBand(1)
            useProbSurf = True
        else:
            probparams = utilities.getRasterParams(self, self.probsurf)
            useProbSurf = False
          
        outDir = os.path.split(self.outputMDS)[0] 
        tmpOutput = os.path.join(outDir,  + "tmp_classified_prob.tif")
        
        os.path.unlink(tmpOutput)
    
    def pull_fielddata_vals(self):
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
                    row.append(str(self.NDFlag))
                else:
                    try:
                        data = band.ReadAsArray(xOffset, yOffset, 1, 1)
                        value = data[0,0]
                        if value <> inputND and not np.isnan(value):
                            row.append(value)
                        else:
                            row.append(str(self.NDFlag))
                    except:
                        badpoints.append(row[:3])
                        row.append(str(self.NDFlag))
                
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
                    msg += "     x:" + str(row[0]) + " Y: " + str(row[1]) + " response: " + str(row[2]) 
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

            if not str(self.NDFlag) in row[3:]:
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
        

        
        if self.deleteTmp:
            #if this flag is trud the field data we're working with is 
            # a temporary copy which we created so that we could add
            # background points.  Delete it to clean up our working file.
            os.remove(self.fieldData)
    
    def equal_float(self, a, b):
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
        for folder in [name for name in dirList if os.path.isdir(os.path.join(directory, name)) ]:
            if isRaster(os.path.join(directory, folder)):
                rasters.append(os.path.join(directory, folder))
    
        return rasters
    
    def isRaster(self, filePath):
        '''Verifies that a pased file and path is recognized by
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

    def estimate_pointstocheck(self):
        if self.probsurf == '':
            #everypoint is included
            return self.pointcount
        else:
            probsurfaceDS = gdal.Open(self.probsurf, gdalconst.GA_ReadOnly)
            ave_prob = self.get_mean_prob(probsurfaceDS)
            # return the expected number of points we will have to sample to get the 
            # desired number (self.pointcount).  The 1.1 is a 10% fudge factor.
            return 1.0/ave_prob * self.pointcount * 1.1 

    def probsurface_sanitycheck(self, ave_prob):
        '''function to asertain how hard it will be to find the required
        number of points given the entered probability surface. 
        '''

        #step2 calculate the expected maximum number of points available
        if self.probsurf == '':
            probparams = utilities.getRasterParams(self, self.inputs[0])
        else:
            probparams = utilities.getRasterParams(self, self.probsurf)
            
        max_points = ave_prob * probparams["width"] * probparams["height"]
        
        #Check if our number of points is approaching the number of available points
        if  self.pointcount > max_points:
            msg = "The number of random background points, " + str(self.pointcount)
            msg += " exceeds the expected number of available points given the specified probability surface. "
            msg += "\n" + self.probsurf + " has an average pixel probability of " + str(ave_prob) + ".\n"
            msg += "Which when multiplied by the cell dimensions of " + str(probparams["width"]) + " x " + str(probparams["height"])
            msg += " results in an expected maximum number of available random points of " + str(max_points)
            msg += "\n\nTry either reducing the number of background points or using a less restrictive probability surface\n"
            raise RuntimeError, msg
        elif self.pointcount > max_points * 0.5:
            msg = "The number of random background points, " + str(self.pointcount)
            msg += " is greater than 50% the expected number of available points given the specified probability surface. "
            msg += "\n" + self.probsurf + " has an average pixel probability of " + str(ave_prob) + ".\n"
            msg += "Which when multiplied by the cell dimensions of " + str(probparams["width"]) + " x " + str(probparams["height"])
            msg += " results in an expected maximum number of available random points of " + str(max_points)
            msg += "\n\n processing time and memory use could be excessive and problematic.\n" 
            self.writetolog()

        # return the expected number of points we will have to sample to get the 
        # desired number (self.pointcount).  The 1.1 is a 10% fudge factor.
        return 1.0/ave_prob 
    

                
#                ndMask = ma.masked_array(data, mask=(data==probparams["NoData"]))
#                if method == None:
#                    method = "Mean"
#                if method in ["Mean", "Max", "Min"]:
#                    ans = self.rebin(ndMask, (numRows/numSourcePerTarget, numCols/numSourcePerTarget), method)
#                else:
#                    X, Y = ndMask.shape
#                    x = X // numSourcePerTarget
#                    y = Y // numSourcePerTarget
#                    ndMask = ndMask.reshape( (x, numSourcePerTarget, y, numSourcePerTarget) )
#                    ndMask = ndMask.transpose( [0, 2, 1, 3] )
#                    ndMask = ndMask.reshape( (x*y, numSourcePerTarget*numSourcePerTarget) )
#                    ans =  np.array(stats.mode(ndMask, 1)[0]).reshape(x, y)
#            
#            
#                outBand.WriteArray(ans, int(j / numSourcePerTarget), int(i / numSourcePerTarget))
            


    def get_mean_prob(self, probsurfaceDS):
        probparams = utilities.getRasterParams(self, self.probsurf)
        rows = int(probparams["height"])
        cols = int(probparams["width"])
        
        #loop of 'blocks' of data maybe.  
        bSize = 1024 #source pixels
        fullPixelCount = float(bSize * bSize)
        #convert this to the nearest whole number of target pixels
        
        avs = []
        wts = []
        
        for i in range(0, rows, bSize):
            if i + bSize  < rows:
                numRows = bSize
            else:
                numRows = rows - i
                
            for j in range(0, cols, bSize):
                if j + bSize < cols:
                    numCols = bSize
                else:
                    numCols = cols - j
                
                
                    
                data = probsurfaceDS.GetRasterBand(1).ReadAsArray(j, i, numCols, numRows)
                data[data==probparams["NoData"]] = 0
                
                avs.append(np.average(data))
                wts.append(data.size / fullPixelCount)
                
            
        return np.average(avs,weights=wts)

    def draw_sample(self, sample_size, rows, cols):
        if sample_size < (rows * cols * 0.3):
            # we need a relatively small sample relative to the total number of possible pixels
            #it's most efficient to randomly select points and throw out duplicates.

            tried_cells = set()
            kept_cells = set()
            
            pcntDone
            while sample_size < kept_cells:
                x = rdm.randint(0, x_count)
                y = rdm.randint(0, y_count)
                if (x, y) in tried_cells:
                    pass
                else:
                    tried_cells.add((x, y))
                    keep = rdm.random() < keep_probability
                    if keep:
                        kept_cells.add((x,y))
        else:
            #We need many samples relative to the total population of pixels.
            #
            #every possible cell
            all_cells = np.arange(x_count * y_count)
            
def main(argv):
    
    usageStmt = "usage:  options: -f --fieldData -i --inCSV -o --output -pc --pointcount -ps --probsurf"
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
    parser.add_option("-p", "--probsurf", 
                      dest="probsurf",
                      default='', 
                      help="Probability surface to use for generation of background points (optional)")
    parser.add_option("-c", "--pointcount", 
                      dest="pointcount",
                      default=0, 
                      help="Number of random background points to add(optional)")
    
    (options, args) = parser.parse_args(argv)
    
    ourMDS = MDSBuilder()
    ourMDS.verbose = options.verbose
    ourMDS.fieldData = options.fieldData
    ourMDS.inputsCSV = options.inputsCSV
    ourMDS.probsurf = options.probsurf
    ourMDS.pointcount = options.pointcount
    ourMDS.outputMDS = options.outputMDS
    ourMDS.run()
   

if __name__ == '__main__':
    sys.exit(main(sys.argv))