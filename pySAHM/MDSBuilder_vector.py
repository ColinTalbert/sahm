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
import numpy

from osgeo import gdalconst
from osgeo import gdal
from osgeo import ogr, osr

from optparse import OptionParser
import utilities
#from Utilities import self.writetolog

class MDSBuilder_vector(object):
    '''Takes a shapefile or other vector file and iterates through
    the unique geometries specified in a certain field.  
    Extracting out min, max, mean or majority for each of the input covariates.
    Outputs a csv with this info
    '''
    def __init__(self):
        #instance level variables
        self.verbose = False
        self.inputsCSV = ''
        self.inputs = []
        self.VectorFieldData = ''
        self.KeyField = ''
        self.Statistic = 'mean'
        self.ResponseType = "Binary"
        self.outputMDS  = ''
        self.NDFlag = 'NA'
        self.deleteTmp = False
        self.logger = None
    
    def validateArgs(self):
        #check our CSV file for expectations
        if not os.path.exists(self.VectorFieldData):
            raise RuntimeError, "Could not find spatial file of fieldData provided.  Please check input file: " + str(self.fieldData)
         
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
        self.writetolog('\nRunning MDSBuilder_vector', True, True)
        # start timing
        startTime = time.time()
        gdal.UseExceptions()
        gdal.AllRegister()
        
        self.constructEmptyMDS()
        
        self.reproject_input_vector()
        
#        if self.pointcount <> 0:
#            self.addBackgroundPoints()
        
        self.outputRows = self.readInPoints()
        
        #loop though each of the supplied rasters and add the 
        #extracted values to
        badpoints = []
        for input in self.inputs:
            self.process_one(input)
#            inputND = self.getND(input)
#            rasterDS = gdal.Open(input, gdalconst.GA_ReadOnly)
#            # get image size
#            rows = rasterDS.RasterYSize
#            cols = rasterDS.RasterXSize
#            band = rasterDS.GetRasterBand(1)
#            # get georeference info
#            transform = rasterDS.GetGeoTransform()
#            xOrigin = transform[0]
#            yOrigin = transform[3]
#            pixelWidth = transform[1]
#            pixelHeight = transform[5]
#            
#            if self.verbose:
#                self.writetolog("    Extracting raster values for " + input)
#                print "    ",
#            
#            pcntDone = 0
#            i = 1
#            for row in outputRows:
#                'loop through each of our points'
#                x = float(row[0])
#                y = float(row[1])
##                if 
#                # compute pixel offset
#                xOffset = int((x - xOrigin) / pixelWidth)
#                yOffset = int((y - yOrigin) / pixelHeight)
##                try:
#                if xOffset < 0 or yOffset < 0:
#                    if row[:3] not in badpoints:
#                        badpoints.append(row[:3])
#                    row.append(str(self.NDFlag))
#                else:
#                    try:
#                        data = band.ReadAsArray(xOffset, yOffset, 1, 1)
#                        value = data[0,0]
#                        if value <> inputND and not numpy.isnan(value):
#                            row.append(value)
#                        else:
#                            row.append(str(self.NDFlag))
#                    except:
#                        badpoints.append(row[:3])
#                        row.append(str(self.NDFlag))
                
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
        
    def reproject_input_vector(self):
        '''reprojects our input vector shapefile into the projection 
        used by PARC.  Uses the first file from the parc input csv.
        '''
        output_dir = os.path.split(self.outputMDS)[0]
        out_fname = os.path.split(self.VectorFieldData)[1]
        out_fname, ext = os.path.splitext(out_fname)
        self.repro_vector = os.path.join(output_dir, out_fname + "_repro" + ext)
        if os.path.exists(self.repro_vector):
            os.unlink(self.repro_vector)
            
        self.reproject_vector(self.VectorFieldData, self.repro_vector, self.inputs[0])
            
    def reproject_vector(self, VectorFieldData, output_vector, template):
        '''originally taken from http://www.gis.usu.edu/~chrisg/python/2009/lectures/ospy_hw2b.py
        '''
        # get the shapefile driver
        driver = ogr.GetDriverByName('ESRI Shapefile')
        
        # open the input data source and get the layer
        inDS = driver.Open(VectorFieldData, 0)
        if inDS is None:
          print 'Could not open file'
        inLayer = inDS.GetLayer()
        
        # create the input SpatialReference
#        inSpatialRef = osr.SpatialReference()
        inSpatialRef = inLayer.GetSpatialRef() 
        
        
        dataset = gdal.Open(template, gdalconst.GA_ReadOnly)
        # create the output SpatialReference
        outSpatialRef = osr.SpatialReference()
        outSpatialRef.ImportFromWkt(dataset.GetProjection())
        del dataset
        
        # create the CoordinateTransformation
        coordTrans = osr.CoordinateTransformation(inSpatialRef, outSpatialRef)
        
        
        # create a new data source and layer
        fn = output_vector
        if os.path.exists(fn):
          driver.DeleteDataSource(fn)
        outDS = driver.CreateDataSource(fn)
        if outDS is None:
          print 'Could not create file'
        just_fname = os.path.splitext(os.path.split(output_vector)[1])[0]
#        just_fname = just_fname.replace(" ", "")
        outLayer = outDS.CreateLayer(just_fname, geom_type=ogr.wkbPolygon)
        
        
        #copy the old fields to the new file
        inlayerDef = inLayer.GetLayerDefn()
        outlayerDef = outLayer.GetLayerDefn()
        for i in range(inlayerDef.GetFieldCount() - 1):
            field = inlayerDef.GetFieldDefn(i)
#            outlayerDef.AddFieldDefn(field)
            outLayer.CreateField(field)
            
#        # get the FieldDefn for the county name field
#        feature = inLayer.GetFeature(0)
#        fieldDefn = feature.GetFieldDefnRef('name')
#        
#        # add the field to the output shapefile
#        outLayer.CreateField(fieldDefn)
#        
#        # get the FeatureDefn for the output shapefile
#        featureDefn = outLayer.GetLayerDefn()
        
        # loop through the input features
        inFeature = inLayer.GetNextFeature()
        while inFeature:
        
            # get the input geometry
            geom = inFeature.GetGeometryRef()
            
            # reproject the geometry
            geom.Transform(coordTrans)
            
            # create a new feature
            outFeature = ogr.Feature(outlayerDef)
            
            # set the geometry and attribute
            outFeature.SetGeometry(geom)
          
            for i in range(inlayerDef.GetFieldCount() - 1):
                field = inlayerDef.GetFieldDefn(i)
                outFeature.SetField(field.name, inFeature.GetField(field.name))
            
            # add the feature to the shapefile
            outLayer.CreateFeature(outFeature)
        
            # destroy the features and get the next input feature
            outFeature.Destroy
            inFeature.Destroy
            inFeature = inLayer.GetNextFeature()
        
        # close the shapefiles
        inDS.Destroy()
        outDS.Destroy()
        
        # create the *.prj file
        outSpatialRef.MorphToESRI()
        prjfile = os.path.splitext(output_vector)[0] + ".prj"
        file = open(prjfile, 'w')
        file.write(outSpatialRef.ExportToWkt())
        file.close()
        
    def constructEmptyMDS(self):
        '''Creates the triple header line format of our output file.
        Also parses the inputs file to append the '_categorical' flag 
        to the covariate names of all categorical inputs.
        '''        
        
#        field_data_CSV = csv.reader(open(self.fieldData, "r"))
#        orig_header = field_data_CSV.next()
#        full_header = ["X", "Y"]
#        if orig_header[2].lower not in ["responsebinary", "responsecount"]:
#            #inputs not conforming to our expected format will be assumed
#            #to be binary data
#            full_header.append("responseBinary")
#        else:
#            full_header.append(orig_header[2])
        
        full_header = ["ID", "response" + self.ResponseType]
        
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
        
#        #Format and write out our three header lines
#        #    the original template, fieldData, and parc folder are 
#        #    stored in spots 1,2,3 in the second header line
#        if len(orig_header) > 3 and \
#            (orig_header[3] == 'Weights' or
#             orig_header[3] == 'frequency'):
#            #The input is an output from Field data query
#            original_field_data = orig_header[-1]
#            field_data_template = orig_header[-2]
#        else:
#            #The input is a raw field data file
#            original_field_data = self.fieldData
#            field_data_template = "NA"
#        
#        if len(inputs_header) > 5:
#            parc_template = inputs_header[5]
#            parc_workspace = inputs_header[6]
#        else:
#            parc_template = "Unknown"
#            parc_workspace = "Unknown"
        

        secondRow = [self.VectorFieldData, self.KeyField] + ["1" for elem in self.inputs]
        thirdRow = ["", ""] + self.inputs
        
#        if "Weights" in orig_header:
#            full_header.append("Weights")
#            secondRow.append("0")
#            thirdRow.append("0")
#            self.hasWeight = True
#            self.weightCol = orig_header.index("Weights")
#        else:
#            self.hasWeight = False
        
        fOut.writerow(full_header)
        fOut.writerow(secondRow)
        fOut.writerow(thirdRow)
        oFile.close()
        del fOut
    
    def readInPoints(self):
        '''Loop through each row in our field data and add the X, Y, response
        to our in memory list of rows to write to our output MDS file
        '''
        
        # get the driver
        driver = ogr.GetDriverByName('ESRI Shapefile')
        # open the data source
        datasource = driver.Open(self.VectorFieldData, 0)
        if datasource is None:
            print 'Could not open file'

        # get the data layer
        layer = datasource.GetLayer()
        # loop through the features and count them
        uniques = set()
        feature = layer.GetNextFeature()
        while feature:
            uniques.add(feature.GetFieldAsString(self.KeyField))
            feature.Destroy()
            feature = layer.GetNextFeature()
        print 'There are ' + str(len(uniques)) + ' unique features'
        # close the data source
        datasource.Destroy()
        
        return uniques
    
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
        
        if self.probsurf <> '':
            rasterDS = gdal.Open(self.probsurf, gdalconst.GA_ReadOnly)
            probND = self.getND(self.probsurf)
            useProbSurf = True
        else:
            print self.inputs[0]
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
            self.writetolog('    Starting generation of ' + str(self.pointcount) + ' random background points, ')
            if self.probsurf <> '':
                self.writetolog('      using ' + self.probsurf + ' as the probability surface.')
            print "    Percent Done:    ",
        
        foundPoints = 0 # The running count of how many we've found
        pcntDone = 0 #for status bar
        while foundPoints < self.pointcount: #loop until we find enough
            x = random.randint(0, cols - 1) 
            y = random.randint(0, rows - 1)
            #print x, y
            tmpPixel = [x, y, pointval] # a random pixel in the entire image
            if useProbSurf:
                # if they supplied a probability surface ignore the random pixel
                # if a random number between 1 and 100 is > the probability surface value
                pixelProb = int(band.ReadAsArray(tmpPixel[0], tmpPixel[1], 1, 1)[0,0])
                if self.equal_float(pixelProb, probND) or numpy.isnan(pixelProb):
                    continue
                    #don't record this pixel it was ND in the Prob Surface
                
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


    def process_one(self, raster):
        # get the driver
        driver = ogr.GetDriverByName('ESRI Shapefile')
        # open the data source
        datasource = driver.Open(self.repro_vector, 0)
        if datasource is None:
            print 'Could not open file'

        # get the data layer
        layer = datasource.GetLayer()
        
        for feature in self.outputRows:
            layer.SetAttributeFilter(self.KeyField + " = '" + feature + "'")
            extent = self.get_extent(layer)
            print layer.GetExtent()
        
        
    def get_extent(self, layer):
        feature = layer.GetNextFeature()
#        geom = feature.GetGeometryRef()
#        fullgeom = ogr.Geometry(ogr.wkbPolygon)
        maxenv = feature.GetGeometryRef().GetEnvelope()
        while feature:
#            print feature.GetGeometryRef().GetEnvelope()
#            geom = feature.GetGeometryRef()
#            fullgeom = fullgeom.Union(geom)
            env = feature.GetGeometryRef().GetEnvelope()
            maxenv = (min(env[0], maxenv[0]), 
                      max(env[1], maxenv[1]),
                      min(env[2], maxenv[2]),
                      max(env[3], maxenv[3]))
            feature = layer.GetNextFeature()
#        env = fullgeom.GetEnvelope()
        return maxenv

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