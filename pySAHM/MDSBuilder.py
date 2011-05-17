'''
Created on Jan 31, 2011

This module creates a Merged Data Set (MDS)
from an input CSV and directory 
or list of rasters. The MDS consists of three
header lines 1st is a x, y, ResponseBinary, 
{other info fields}, then a list of raster covariates.
The next header line contains ones and zeros for each of the 
raster covariates and indicates if the covariate is to be 
used in subsequent analyses.  The last header line contains
the full network path the raster dataset for that covariate.
After the header lines comes the data which is the 
covariate values pulled from the raster cell 
with those coordinates. 

@author: talbertc

'''

import sys
import csv
import os
import time
import random
import shutil

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
        self.probsurf = ''
        self.pointcount = 0
        self.deleteTmp = False
        self.logger = None
    
    def validateArgs(self):
        #check our CSV file for expectations
        if not os.path.exists(self.fieldData):
            raise RuntimeError, "Could not find CSV file of fieldData provided.  Please check input file: " + str(self.fieldData)
         
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
        
        outputRows = self.readInPoints()
        
        #loop though each of the supplied rasters and add the 
        #extracted values to
        for input in self.inputs:
            
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
                x = float(row[0])
                y = float(row[1])
                # compute pixel offset
                xOffset = int((x - xOrigin) / pixelWidth)
                yOffset = int((y - yOrigin) / pixelHeight)
                data = band.ReadAsArray(xOffset, yOffset, 1, 1)
                # xOffset, yOffset
                value = data[0,0]
                row.append(value)
                #print value
                if self.verbose:
                    if i/float(len(outputRows)) > float(pcntDone)/100:
                        pcntDone += 10
                        print str(pcntDone) + "...",
                i += 1
            if self.verbose:
                self.writetolog("    Done")
            
        outputMDS = csv.writer(open(self.outputMDS, 'ab'))
        for row in outputRows:   
            outputMDS.writerow(row)
        del outputMDS
        
        # figure out how long the script took to run
        endTime = time.time()
        
        if self.deleteTmp:
            #if this flag is trud the field data we're working with is 
            # a temporary copy which we created so that we could add
            # background points.  Delete it to clean up our working file.
            os.remove(self.fieldData)
        
        if self.verbose:
            self.writetolog('Finished running MDSBuilder', True, True)
            self.writetolog('    The process took ' + str(endTime - startTime) + ' seconds')
        
    def constructEmptyMDS(self):
        '''Creates the triple header line format of our output file.
        Also parses the inputs file to append the '_categorical' flag 
        to the covariate names of all categorical inputs.
        '''        
        
        fieldDataCSV = csv.reader(open(self.fieldData, "r"))
        origHeader = fieldDataCSV.next()
        fullHeader = origHeader[:3]
        
        inputsCSV = csv.reader(open(self.inputsCSV, "r"))
        inputsCSV.next()
        self.inputs = []

        #each row contains a covariate raster layer
        #item 0 is the full path to the file
        #item 1 is 0/1 indicating categorical
        #Construct our output header by extracting each individual 
        #file (raster) name and appending '_categorical' if the flag is 1
        for row in inputsCSV:
            tmpRaster = row[0]
            self.inputs.append(tmpRaster)
            rasterShortName = os.path.split(tmpRaster)[1]
            rasterShortName = os.path.splitext(rasterShortName)[0]
            if row[1] == '1':
                rasterShortName += "_categorical"
            fullHeader.append(rasterShortName)
                
    
        #Open up and write to an output file
        oFile = open(self.outputMDS, 'wb')
        fOut = csv.writer(oFile, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
        
        #Format and write out our three header lines
        secondRow = ["0" for elem in ["x", "y", "Response"]] + ["1" for elem in self.inputs]
        thirdRow = ["" for elem in ["x", "y", "Response"]] + self.inputs
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
            points.append(row[:3])
            
        del fieldDataCSV
        return points
    
    def addBackgroundPoints(self):
        '''Add pointcount number of points to the supplied field data
        If a probability surface was provided the distribution will 
        follow this otherwise it will be uniform within the extent of the first of our inputs.
        No more than one per pixel is used.
        '''
        #First we'll create a temp copy of the Field Data to work with.
        shutil.copy(self.fieldData, self.fieldData + ".tmp.csv")
        self.fieldData = self.fieldData + ".tmp.csv"
        self.deleteTmp = True
        
        if self.probsurf <> '':
            rasterDS = gdal.Open(self.probsurf, gdalconst.GA_ReadOnly)
            useProbSurf = True
        else:
            rasterDS = gdal.Open(self.inputs[0], gdalconst.GA_ReadOnly)
            useProbSurf = False
            
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
        xRange = [xOrigin, xOrigin * pixelWidth * cols]
        yRange = [yOrigin, yOrigin * pixelHeight * rows]
        
        #Open up and write to an output file
        oFile = open(self.fieldData, 'ab')
        fOut = csv.writer(oFile, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
        
        
        if self.verbose:
            self.writetolog('    Starting generation of ' + str(self.pointcount) + 'random background points, ')
            if self.probsurf <> '':
                self.writetolog('      using ' + self.probsurf + ' as the probablity surface.')
            print "    Percent Done:    ",
        
        foundPoints = 0 # The running count of how many we've found
        pcntDone = 0 #for status bar
        while foundPoints < self.pointcount: #loop until we find enough
            x = random.randint(0, cols - 1) 
            y = random.randint(0, rows - 1)
            #print x, y
            tmpPixel = [x, y, -9999] # a random pixel in the entire image
            if useProbSurf:
                # if they supplied a probability surface ignore the random pixel
                # if a random number between 1 and 100 is > the probability surface value
                pixelProb = int(band.ReadAsArray(tmpPixel[0], tmpPixel[1], 1, 1)[0,0])
                #pixelProb is the extracted probability from the probability surface
                rand = random.randint(1,100)
                #rand is a uniform random integer between 1 and 100 inclusive
                if rand > pixelProb:
                    continue
                    #don't record this pixel in our output file 
                    #because our rand number was lower (or equal) than that pixel's probability
                    
            #convert our outValues for row, col to coordinates
            tmpPixel[0] = xOrigin + tmpPixel[0] * pixelWidth
            tmpPixel[1] = yOrigin + tmpPixel[1] * pixelHeight
            
            fOut.writerow(tmpPixel)
            foundPoints += 1
            if self.verbose:
                if float(foundPoints)/self.pointcount > float(pcntDone)/100:
                    pcntDone += 10
                    print str(pcntDone) + "...",
        print "Done!\n"
        oFile.close()
        del fOut
    
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