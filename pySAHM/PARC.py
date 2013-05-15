#!/usr/bin/python
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

import glob
import math
import os
import shutil
import struct
import sys
import csv
import Queue
import thread
import subprocess
import time

from optparse import OptionParser

from osgeo import gdalconst
from osgeo import gdal
from osgeo import osr


from numpy import *
import numpy as np
import scipy.stats.stats as stats

import utilities
import SpatialUtilities

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
    parser.add_option("-o", dest="out_dir", default="./", help="directory in which to put processed images, defaults to current directory")
    parser.add_option("-v", dest="verbose", default=False, action="store_true", help="the verbose flag causes diagnostic output to print")
    parser.add_option("-t", dest="templateRaster", help="The template raster used for projection, origin, cell size and extent")
    parser.add_option("-i", dest="inputs_CSV", help="The CSV containing the list of files to process.  Format is 'FilePath, Categorical, Resampling, Aggreagtion")
    parser.add_option("-m", dest="multicore", default=False, action="store_true", help="Flag indicating to use multiple cores")
    parser.add_option("-n", dest="ignoreNonOverlap", default=False, action="store_true", help="Flag indicating to use ignore non-overlapping area")
    
    (options, args) = parser.parse_args(args_in)
    
    ourPARC = PARC()
    ourPARC.verbose = options.verbose
    ourPARC.template = options.templateRaster
    ourPARC.out_dir = options.out_dir
    ourPARC.inputs_CSV = options.inputs_CSV
    ourPARC.multicores = options.multicore
    ourPARC.ignoreNonOverlap = options.ignoreNonOverlap
    ourPARC.logger = utilities.logger(os.path.join(ourPARC.out_dir, "logfile.txt"), True)
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
        self.out_dir = ""
        self.inputs_CSV = ''
        self.inputs = []
        self.agg_methods = ['Min', 'Mean', 'Max', 'Majority', 'STD']
        self.resample_methods = ['NearestNeighbor', 'Bilinear', 'Cubic', 'CubicSpline', 'Lanczos']
        self.logger = None
        self.multicores = True
        self.ignoreNonOverlap = False
        self.module = None

    def parcFiles(self):
        '''
            1: Parse the inputs_CSV into our inputs list
            2: Make sure all of our instance variables are good and proper
            3: Loop through the list of sourceImages and PARC each one.
            4: The outputs will be stored in the output directory
            5: Additionally an output CSV will be produced that lists the 
            inputs, parameters used, and outputs
        '''
        
        self.logger.writetolog("Starting PARC", True, True)
        self.validateArgs()
        self.logger.writetolog("    Arguments validated successfully", True, True)
        self.processFiles()
        
        self.logger.writetolog("Finished PARC", True, True)
        
    def processFiles(self):
        processQueue = []
        coreCount = utilities.getProcessCount(self.processingMode)
            
        # Clip and reproject each source image.
        for image in self.inputs:
            #if we're running to many jobs wait for one to finish
            utilities.waitForProcessesToFinish(processQueue, coreCount)
                        
            inPath, inFileName = os.path.split(image[0])          
            outFile, ext = os.path.splitext(inFileName)
            outFile = os.path.join(self.out_dir, outFile + ".tif")
            
            if os.path.exists(outFile):
                try: 
                    gdal.Open(outFile)
                    shortname = SpatialUtilities.getRasterShortName(outFile)
                    msg = "The output " + shortname + " already exists. \tSkipping this file."
                    self.logger.writetolog(msg, True, True)
                except:
                    #we bombed trying to open the outFile with gdal. Lets rerun it.
                    processQueue.append(self.gen_singlePARC_thread(image, outFile))
                    pass
                
            else:
                processQueue.append(self.gen_singlePARC_thread(image, outFile))

        #wait for the last set of processes to finish up 
        if self.processingMode == "FORT Condor":
            self.waitForCondorProcessesToFinish(processQueue)
        else:
            utilities.waitForProcessesToFinish(processQueue)
            
        print "done"
            
    def gen_singlePARC_thread(self, image, outFile):
            image_short_name = os.path.split(image[0])[1]

            args = ['-s', os.path.abspath(image[0]),
                    '-c', image[1],
                    '-d', os.path.abspath(outFile),
                    '-t', os.path.abspath(self.template),
                    '-r', image[2],
                    '-a', image[3]]

            if self.ignoreNonOverlap:
                args.extend(['-i'])
    
            execDir = os.path.split(__file__)[0]
            executable = os.path.abspath(os.path.join(execDir, 'runSinglePARC.py'))
            pyEx = sys.executable
            command_arr = [pyEx, executable] + args
            command = ' '.join(command_arr)
            #self.logger.writetolog(command, False, False)
            
            if self.processingMode == "FORT Condor":
                workspace, fname = os.path.split(os.path.abspath(outFile))
                prefix = os.path.splitext(fname)[0]
                utilities.runCondorPythonJob(command_arr, workspace, prefix)
                return os.path.abspath(outFile)
            else:
                proc = subprocess.Popen( command_arr )
                return  proc
#                thread.start_new_thread(utilities.process_waiter,
#                        (proc, image_short_name, results))

    def log_result(self, result):
        print result

    def waitForCondorProcessesToFinish(self, outputs):
        errors = []
        originalCount = len(outputs)
        successes = 0
        failures = 0
        while outputs:
            for process in outputs:
                result = self.jobFinished(process)
                if result == "finished":
                    outputs.remove(process)
                    
                    #cleanup some condor files
                    success = False
                    while not success:
                        try: 
                            for f in ["log", "stdOut", "stdErr", "CondorSubmit"]:
                                fname = process.replace(".tif", "_" + f + ".txt")
                                if os.path.exists(fname):
                                    os.remove(fname)
                            success = True
                        except:
                            pass
                        
                    print str(originalCount - len(outputs)) + " PARC layers finished"
                elif result == "error":
                    if os.path.exists(process):
                        os.remove(process)
                    errors.append(process)
                    outputs.remove(process)
                    print str(originalCount - len(outputs)) + " PARC layers finished"

        if len(errors) > 0:
            msg = "There were problems with one or more runs."
            for process in errors:
                msg += "\n" + process + " did not run correctly"
            raise utilities.TrappedError(msg)
            
    def jobFinished(self, output):
        stdOut = output.replace(".tif", "_stdOut.txt")
        try:
            lastLine = open(stdOut, "r").readlines()[-1]
            if lastLine.startswith("Finished successfully!"):
                return "finished"
            elif lastLine.startswith("Job failed!"):
                return "error"
            else:
                return "running"
        except (IndexError, IOError):
            pass
    
    def parcFile(self, source, dest):
        """
        Processes a single file
        """
        gdal.UseExceptions()
        
        shortName = os.path.split(os.path.splitext(source[0])[0])[1]
        self.logger.writetolog("    Starting processing of " + source[0])
        sourceRaster = SpatialUtilities.SAHMRaster(source[0])
                
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
        
        
        templateSRCCellSize = SpatialUtilities.getTemplateSRSCellSize(sourceRaster, 
                             self.templateRaster)
        cellRatio = templateSRCCellSize / self.templateRaster.xScale
        msg = "  ratio of source cell size to template cell size = " + str(cellRatio)
        msg += "    template cell size = " + str(self.templateRaster.xScale)
        msg += "    " + shortName + " cell size = " + str(templateSRCCellSize)
        self.writetolog(msg)
            
        if cellRatio > 0.5:
            #The source cell size is close enough to our template cell size,
            #or smaller so that all we need to do is reproject and resample.
            self.logger.writetolog("  cell ratio > .5: reprojecting and resampling to template parameters only")
            SpatialUtilities.intermediaryReprojection(sourceRaster, 
                            self.templateRaster, dest, gdalType, True)
        else:
            #Our Target cell size is much bigger than our source we need to do 
            #some aggregation to make things work.
            msg = '  cell ratio <= .5: reprojecting and resampling to template parameters'
            msg += '    then aggregating the reprojected raster to match template parameters'
            self.writetolog(msg)   

            targetCellSize, numSourcePerTarget = \
                SpatialUtilities.getAggregateTargetCellSize(sourceRaster, self.templateRaster)
            tmpOutput = os.path.join(os.path.dirname(dest), "tmp_" + os.path.basename(dest))
            
            SpatialUtilities.intermediaryReprojection(sourceRaster, 
                            self.templateRaster, tmpOutput, gdalType, False)
            self.writetolog("   Starting on Aggregating: " + shortName)
                
            tmpOutputRaster = SpatialUtilities.SAHMRaster(tmpOutput)
            self.Aggregate(tmpOutputRaster, dest, source[3], numSourcePerTarget)
            
            self.writetolog("   Finished Aggregating: " + shortName)
            try:
                os.remove(tmpOutput)
            except WindowsError:
                pass

    def Aggregate(self, sourceRaster, outFName,
                  method=None, numSourcePerTarget=10):
              
        tmpOutput = os.path.splitext(outFName)[0] + ".tif"
        tmpOutDataset = SpatialUtilities.SAHMRaster(tmpOutput)
        tmpOutDataset.pullParamsFromRaster(self.templateRaster.source)
        tmpOutDataset.createNewRaster()
        
        rows = int(sourceRaster.height)
        cols = int(sourceRaster.width)

        #loop of 'blocks' of data maybe.  
        bSize = 2048 #source pixels
        #convert this to the nearest whole number of target pixels
        bSize = int(round(bSize / numSourcePerTarget) * numSourcePerTarget)
        if bSize == 0:
            bSize = int(numSourcePerTarget)
            

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
                    
                data = sourceRaster.getBlock(j, i, numCols, numRows)
                
                if method == None:
                    method = "Mean"
                if method in ["Mean", "Max", "Min", "STD"]:
                    ans = self.rebin(data, (numRows/numSourcePerTarget, numCols/numSourcePerTarget), method)
                else:
                    X, Y = data.shape
                    x = X // numSourcePerTarget
                    y = Y // numSourcePerTarget
                    ndMask = data.reshape( (x, numSourcePerTarget, y, numSourcePerTarget) )
                    ndMask = ndMask.transpose( [0, 2, 1, 3] )
                    ndMask = ndMask.reshape( (x*y, numSourcePerTarget*numSourcePerTarget) )
                    ans =  np.array(stats.mode(ndMask, 1)[0]).reshape(x, y)
            
                tmpOutDataset.putBlock(ans, int(j / numSourcePerTarget), int(i / numSourcePerTarget))
                
        tmpOutDataset.calcStats()
        tmpOutDataset.close()
       
        
    def rebin(self, a, shape, method): 
        sh = shape[0],a.shape[0]//shape[0],shape[1],a.shape[1]//shape[1] 
        if method =="Mean":
            return a.reshape(sh).mean(-1).mean(1)
        elif method == "Min":
            return a.reshape(sh).min(-1).min(1)
        elif method == "Max":
            return a.reshape(sh).max(-1).max(1)
        elif method == "STD":
            sh2 = sh[0], sh[2], sh[1] * sh[3] 
            return np.rollaxis(a.reshape(sh), 1, -1).reshape(sh2).std(-1)  
                                                                            
    def ImageCoversTemplate(self, sourceRaster):
        """
        Checks to see if the template images 
        falls completely inside the source raster
        
        it does this by generating a list of 25 coordinate
        pairs equally distributed across the template,
        including the four absolute corners.  
        These points are in the CRS of the image.
        If all of these points have a valid data or nodata
        value in the image, then the image covers the template.
        (in nearly every case anyway)
        """
        gdal.UseExceptions()
        n = 5
        xOffset = (self.templateRaster.east - self.templateRaster.west) / (n) - \
                    ((self.templateRaster.east - self.templateRaster.west) / self.templateRaster.width / 1000)
        yOffset = (self.templateRaster.north - self.templateRaster.south) / (n) - \
                    ((self.templateRaster.north - self.templateRaster.south) / self.templateRaster.height / 1000)
        curX = self.templateRaster.west
        curY = self.templateRaster.north
        testPoints =[]
        
        for x in range(n + 1):
            for y in range(n + 1):
                testPoints.append(SpatialUtilities.transformPoint(curX, curY, self.templateRaster.srs, 
                                                    sourceRaster.srs))
                curY -= yOffset
                
            curX += xOffset
            curY = self.templateRaster.north  
                
        badPoint = False        
        for point in testPoints:
            try:
                xOffset = int((point[0] - sourceRaster.west) / sourceRaster.xScale)
                yOffset = int((point[1] - sourceRaster.north) / sourceRaster.yScale)
                data = sourceRaster.getBlock(xOffset, yOffset, 1, 1)
                value = data[0,0]
            except:
                badPoint = True
        
        #if valid values were returned from each of our points then
        #the template falls entirely within the Source image.
        if badPoint:
            return False
        else:
            return True
        
    def shrink_template_extent(self, sourceRaster):
        '''The template extent will be reduced by the extent of 
        an individual source layer if the layer has a smaller extent
        This results in the intersection of the grids being used.
        '''
        if self.ImageCoversTemplate(sourceRaster):
            #the template is already smaller than the image in question
            #Do nothing
            return False
        else:
            gt = list(self.templateRaster.gt)
            
            #because the translation of a rectangle between crs's results 
            #in a paralellogram (or worse) I'm taking the four corner points in 
            #source projection and transforming these to template crs and then 
            #using the maximum/minimum for each extent.          
            largest_north = self.maxNorth(sourceRaster)
            smallest_south = self.minSouth(sourceRaster)
            largest_east = self.maxEast(sourceRaster)
            smallest_west = self.minWest(sourceRaster)
            
            #Now for each direction step through the pixels until we have one smaller
            #or larger than our min/max source extent.
            orig_tNorth = self.templateRaster.tNorth
            shrinkN = 0
            while self.templateRaster.tNorth > largest_north:
                #yScale is negative
                self.templateRaster.tNorth += self.templateRaster.yScale
                self.templateRaster.height -= 1
                shrinkN += 1
            if orig_tNorth <> self.templateRaster.tNorth:
                msg = "Northern edge of template reduced " + str(shrinkN) + " pixels due to, "
                msg += "the extent of " + sourceRaster.source
                self.writetolog(msg) 
            
            orig_tSouth = self.templateRaster.tSouth
            shrinkN = 0    
            while self.templateRaster.tSouth < smallest_south:
                self.templateRaster.tSouth -= self.templateRaster.yScale
                self.templateRaster.height -= 1
                shrinkN += 1
            if orig_tSouth <> self.templateRaster.tSouth:
                msg = "NSouthern edge of template reduced " + str(shrinkN) + " pixels due to, "
                msg += "the extent of " + sourceRaster.source
                self.writetolog(msg)
            
            gt[3] = self.templateRaster.tNorth
            
            
            orig_tWest = self.templateRaster.tWest
            shrinkN = 0
            while self.templateRaster.tWest < smallest_west:
                #yScale is negative
                self.templateRaster.tWest += self.templateRaster.xScale
                self.templateRaster.width -= 1
                shrinkN += 1
            if orig_tWest <> self.templateRaster.tWest:
                msg = "Western edge of template reduced " + str(shrinkN) + " pixels due to, "
                msg += "the extent of " + sourceRaster.source   
                self.writetolog(msg)
            
            orig_tEast = self.templateRaster.tEast
            shrinkN = 0
            while self.templateRaster.tEast > largest_east:
                self.templateRaster.tEast -= self.templateRaster.xScale
                self.templateRaster.width -= 1
                shrinkN += 1
            gt[0] = self.templateRaster.tWest
            if orig_tEast <> self.templateRaster.tEast:
                msg = "Eastern edge of template reduced " + str(shrinkN) + " pixels due to, "
                msg += "the extent of " + sourceRaster.source
                self.writetolog(msg)
            
            #set the template geotransform to be our modified one.
            self.templateRaster.gt = tuple(gt)

    def maxNorth(self, sourceRaster):
        northWidth = sourceRaster.east - sourceRaster.west
        curW = sourceRaster.west
        steps = 10
        maxNorth = -999999
        for step in range(steps + 1):
            curWest = sourceRaster.west + step*(northWidth/steps)
            transPoint = self.transformPoint(curWest, sourceRaster.north, 
                        sourceRaster.srs, self.templateRaster.srs)
#            print curWest, sourceParams['north'], " = ", transPoint
            if transPoint[1] > maxNorth:
                maxNorth = transPoint[1]
        return maxNorth
    
    def minSouth(self, sourceRaster):
        southWidth = sourceRaster.east - sourceRaster.west
        curW = sourceRaster.west
        steps = 10
        minSouth = 999999
        for step in range(steps + 1):
            curWest = sourceRaster.west + step*(southWidth/steps)
            transPoint = self.transformPoint(curWest, sourceRaster.south, 
                        sourceRaster.srs, self.templateRaster.srs)
#            print curWest, sourceParams['south'], " = ", transPoint
            if transPoint[1] < minSouth:
                minSouth = transPoint[1]
        return minSouth
    
    def maxEast(self, sourceRaster):
        eastHeight = sourceRaster.north - sourceRaster.south
        curN = sourceRaster.north
        steps = 10
        maxEast = -999999
        for step in range(steps + 1):
            curNorth = sourceRaster.south + step*(eastHeight/steps)
            transPoint = self.transformPoint(sourceRaster.east, curNorth,
                        sourceRaster.srs, self.templateRaster.srs)
#            print curWest, sourceParams['south'], " = ", transPoint
            if transPoint[0] > maxEast:
                maxEast = transPoint[0]
        return maxEast
    
    def minWest(self, sourceRaster):
        westHeight = sourceRaster.north - sourceRaster.south
        curN = sourceRaster.north
        steps = 10
        minWest = 999999
        for step in range(steps + 1):
            curNorth = sourceRaster.south + step*(westHeight/steps)
            transPoint = self.transformPoint(sourceRaster.west, curNorth,
                        sourceRaster.srs, self.templateRaster.srs)
#            print curWest, sourceParams['south'], " = ", transPoint
            if transPoint[0] < minWest:
                minWest = transPoint[0]
        return minWest

    def validateArgs(self):
        """
        Make sure the user sent us some stuff we can work with
        """

        if not os.path.exists(self.out_dir):
            raise utilities.TrappedError("Specified Output directory " + self.out_dir + " not found on file system")
        
        if not os.path.isdir(self.out_dir):
            raise utilities.TrappedError("Specified Output directory " + self.out_dir + " is not a directory")
     
        if self.logger is None:
            self.logger = utilities.logger(self.out_dir, self.verbose)
        self.writetolog = self.logger.writetolog

        # Validate template image.
        if self.template is None:
            raise utilities.TrappedError("template raster not provided.")
        
        if not os.path.exists(self.template):
            raise utilities.TrappedError("Template file, " + self.template + ", does not exist on file system")

        self.templateRaster = SpatialUtilities.SAHMRaster(self.template)
        if len(self.templateRaster.Error) <> 0:
            raise utilities.TrappedError("There was a problem with the provided template: \n    " + 
                                    "    " + "\n    ".join(self.templateRaster.Error))
        
        # Ensure the template has square pixels.
        if abs(abs(self.templateRaster.xScale) - abs(self.templateRaster.yScale)) > 1e-6:
            raise utilities.TrappedError("template image must have square pixels." + 
                            "/n    x pixel scale = " + str(abs(self.templateRaster.xScale)) +
                            "/n    y pixel scale = " + str(abs(self.templateRaster.yScale)))

        
        #Validate input rasters
        if not os.path.exists(self.inputs_CSV):
            raise utilities.TrappedError("Inputs CSV, " + self.inputs_CSV + ", does not exist on file system.")
        
        inputsCSV = csv.reader(open(self.inputs_CSV, 'r'))
        header = inputsCSV.next()
        strInputFileErrors = ""

        outputCSV = os.path.join(self.out_dir, "PARC_Files.csv")
        output = csv.writer(open(outputCSV, "wb"))
        output.writerow(["PARCOutputFile", "Categorical", "Resampling", 
                         "Aggregation", "OriginalFile", 
                         os.path.abspath(self.template), 
                         os.path.abspath(self.out_dir)])
        
        inputs = []
        for row in inputsCSV:
            inputFile = row[0]
            input_just_file = os.path.splitext(os.path.split(inputFile)[1])[0]
            
            if input_just_file == "hdr":
                inputFile = os.path.split(inputFile)[0]
                row[0] = inputFile
                input_just_file = os.path.split(inputFile)[1]
                
            if input_just_file in inputs:
                strInputFileErrors += "\n  PARC not currently set up to handle identically named inputs."
                strInputFileErrors += "\n\t" + input_just_file + " used multiple times"
            else:
                inputs.append(input_just_file)
                
                
            sourceRaster = SpatialUtilities.SAHMRaster(inputFile)                
                
            if len(sourceRaster.Error) > 0:
                strInputFileErrors += ("  " + os.path.split(inputFile)[1] + " had the following errors:\n" + 
                                    "    " + "\n    ".join(sourceRaster.Error)) + "\n"
            else:
                pass
                if not self.ignoreNonOverlap and not self.ImageCoversTemplate(sourceRaster):
                    strInputFileErrors += "\n  Some part of the template image falls outside of " + input_just_file
                    strInputFileErrors += "\n        template upper left  = (" + str(self.templateRaster.west) + ", " + str(self.templateRaster.north) + ")"
                    strInputFileErrors += "\n        template lower right = (" + str(self.templateRaster.east) + ", " + str(self.templateRaster.south) + ")"
                    upperLeftX, upperLeftY = SpatialUtilities.transformPoint(sourceRaster.west, 
                                                        sourceRaster.north,sourceRaster.srs, 
                                                        self.templateRaster.srs)
                    strInputFileErrors += "\n        image    upper left  = (" + \
                        str(sourceRaster.west) + ", " + str(sourceRaster.north) + ")"
                    strInputFileErrors += "\n        image    lower right = (" + str(sourceRaster.east) + ", " + str(sourceRaster.south) + ")"

                if self.ignoreNonOverlap:
                    #if this input is smaller in any of the dimensions
                    pass

            if len(row) < 2 or not row[1] in ['0', '1']:
                self.writetolog("  " + os.path.split(inputFile)[1] + " categorical either missing or not 0 or 1:\n   Defaulting to 0 (continuous)")
                if len(row) < 2:
                    row.append('0')
                else:
                    row[1] = '0'
                 
            if len(row) < 3 or not row[2].lower() in [item.lower() for item in self.resample_methods]:
                self.writetolog("  " + os.path.split(inputFile)[1] + " resample method either missing or not one of " + 
                                ", ".join(self.resample_methods) + "\n  Defaulting to 'Bilinear'")                  
                 
                if row[1] == '0':
                    default = 'Bilinear'
                else:
                    default = 'NearestNeighbor'
                if len(row) < 3:
                    row.append(default)
                else:
                    row[2] = default

            if len(row) < 4 or not row[3].lower() in [item.lower() for item in self.agg_methods]:
                self.writetolog("  " + os.path.split(inputFile)[1] + " aggregation method either missing or not one of " + 
                                ", ".join(self.agg_methods) + "\n  Defaulting to 'Mean'")
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
            shortName = SpatialUtilities.getRasterShortName(row[0])
            fileName = os.path.abspath(os.path.join(self.out_dir, shortName + ".tif"))
            outputrow = [fileName] + row[1:4] + [os.path.abspath(row[0]), os.path.abspath(self.out_dir)]
            output.writerow(outputrow)
        del output
        
        if strInputFileErrors <> "":
            self.writetolog(strInputFileErrors, False, False)
            raise utilities.TrappedError("There was one or more problems with your input rasters: \n" + strInputFileErrors)

    
if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))

