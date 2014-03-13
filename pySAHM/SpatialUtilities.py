import os
import csv
import random
import string

from math import floor
import numpy as np
import itertools

from osgeo import gdalconst
from osgeo import gdal
from osgeo import osr
from osgeo import ogr

import utilities

gdal.UseExceptions()
gdal.AllRegister()

class SAHMRaster():
    '''An extension to a GDAL raster that contains convenience methods for
    some of the operations we use use.
    '''
    def __init__(self, rasterFile):
        self.source = rasterFile

        self.resetBlocks()
        #  default values
        self.blockSize = 2048
        self.driverName = "GTiff"
        self.pixelType = gdalconst.GDT_Int32
        self.NoData = -2147483647
        self.signedByte = False
        #  Must be supplied for new rasters or read from input
        self.xScale = None
        self.yScale = None
        self.width = None
        self.height = None
        self.east = None
        self.north = None
        self.prj = None
        #  convenience values pulled or calculated
        self.west = None
        self.south = None
        self.gt = None

        if isRaster(rasterFile):
            self.loadRaster()
            self.getParams()
        else:
            self.Error = ['Input file is not a raster']

    def loadRaster(self):
        if not os.path.exists(self.source):
            self.Error.append("The input file (" + self.source + ") does not exist on the file system.")
            return

        self.ds = gdal.Open(self.source, gdalconst.GA_ReadOnly)
        self.band = self.ds.GetRasterBand(1)

    def createNewRaster(self):
        self.Error = []
        #  delete the output if it exists
        gdal.Unlink(self.source)

        #  register the gdal driver
        driver = gdal.GetDriverByName(self.driverName)
        if self.signedByte:
            self.ds = driver.Create(self.source,
                                            self.width, self.height,
                                            1, self.pixelType, ["PIXELTYPE=SIGNEDBYTE"])
        else:
            self.ds = driver.Create(self.source,
                                            self.width, self.height,
                                            1, self.pixelType)

        self.gt = (self.west, self.xScale, 0, self.north, 0, self.yScale)
        self.ds.SetGeoTransform(self.gt)
        self.band = self.ds.GetRasterBand(1)

        if self.prj is not None:
            self.ds.SetProjection(self.prj)
        self.band.SetNoDataValue(self.NoData)
        if self.signedByte:
            self.band.pixelType = "SIGNEDBYTE"
            self.band.SetMetadata({'PIXELTYPE': 'SIGNEDBYTE'}, 'IMAGE_STRUCTURE')

    def pullParamsFromRaster(self, otherRasterFile):
        if not os.path.exists(otherRasterFile):
            raise utilities.TrappedError("Raster does not appear to be a valid raster.\nThe input file (" + otherRasterFile + ") does not exist on the file system.")

        #  Get the PARC parameters from the rasterFile.
        otherDS = gdal.Open(otherRasterFile, gdalconst.GA_ReadOnly)
        self.getParams(otherDS)

    def getParams(self, ds=None):
        """
        Extracts properties from a passed raster
        All values are stored as instance variables
        If errors are encountered along the way the error messages will
        be returned as a list in the 'Error' element.
        """
        try:
            #  initialize our params dictionary to have None for all parma
            allRasterParams = ["Error", "xScale", "yScale", "width", "height",
                            "east", "north", "west", "south",
                            "gEast", "gNorth", "gWest", "gSouth",
                            "Wkt", "srs", "gt", "prj", "NoData", "pixelType", "file_name"]

            for param in allRasterParams:
                setattr(self, param, None)

            self.Error = []

            if ds is None:
                ds = self.ds
                band = self.band
            else:
                band = ds.GetRasterBand(1)

            xform = ds.GetGeoTransform()
            self.xScale = xform[1]
            self.yScale = xform[5]

            self.width = ds.RasterXSize
            self.height = ds.RasterYSize

            self.west = xform[0]
            self.north = xform[3]
            self.east = self.west + self.width * self.xScale
            self.south = self.north + self.height * self.yScale

            try:
                self.wkt = ds.GetProjection()
                self.gt = ds.GetGeoTransform()
                self.prj = ds.GetProjectionRef()
                self.srs = osr.SpatialReference(self.wkt)
                if self.wkt == '':
                    self.Error.append("Undefined projection")
                else:
                    try:
                        geographic = osr.SpatialReference()
                        geographic.ImportFromEPSG(4326)
                        self.gWest, self.gNorth = transformPoint(self.west, self.north, self.srs, geographic)
                        self.gEast, self.gSouth = transformPoint(self.east, self.south, self.srs, geographic)
                    except:
                        pass
            except:
                #  print "We ran into problems getting the projection information for " +  rasterFile
                self.Error.append("Undefined problems extracting the projection information")

            try:
                self.signedByte = band.GetMetadata('IMAGE_STRUCTURE')['PIXELTYPE'] == 'SIGNEDBYTE'
            except KeyError:
                self.signedByte = False

            self.NoData = band.GetNoDataValue()
            if self.NoData == None:
                self.NoData = defaultNoData(band.DataType)
            self.pixelType = band.DataType
            if self.pixelType == None:
                self.Error.append("Could not identify pixel type (bit depth)")

        except:
            #  print "We ran into problems extracting raster parameters from " + rasterFile
            self.Error.append("Some untrapped error was encountered")

    def getRandomPixel(self):
        col = random.randint(0, self.width - 1)
        row = random.randint(0, self.height - 1)
        return col, row

    def convertColRowToCoords(self, col, row, pixelCenter=True):
        x = self.west + col * self.xScale

        y = self.north + row * self.yScale

        if pixelCenter:
            x += self.xScale / 2.0
            y += self.yScale / 2.0
        return x, y

    def convertCoordsToColRow(self, x, y):
        col = int(floor(round(((x - self.west) / self.xScale), 5)))
        row = int(floor(round(((y - self.north) / self.yScale), 5)))
        return col, row

    def getPixelValueFromIndex(self, col, row):
        return self.band.ReadAsArray(int(col), int(row), 1, 1)[0, 0]

    def getPixelValueFromCoords(self, x, y):
        col, row = self.convertCoordsToColRow(x, y)
        return self.getPixelValueFromIndex(col, row)

    def getBlock(self, row, col, numCols, numRows):
        data = self.band.ReadAsArray(row, col, numCols, numRows)
        ndMask = np.ma.masked_array(data, mask=(data == self.NoData))
        return ndMask

    def putBlock(self, data, col, row):
        '''this only works on rasters we've opened for writing
        '''
        data = np.where(data.mask, self.NoData, data)
        self.band.WriteArray(data, col, row)

    def iterBlocks(self):
        rows = int(self.height)
        cols = int(self.width)
        for i in range(0, rows, self.blockSize):
            self.curRow = i
            if i + self.blockSize < rows:
                numRows = self.blockSize
            else:
                numRows = rows - i

            for j in range(0, cols, self.blockSize):
                self.curCol = j
                if j + self.blockSize < cols:
                    numCols = self.blockSize
                else:
                    numCols = cols - j
                yield self.getBlock(j, i, numCols, numRows)

    def resetBlocks(self):
        self.curRow = 0
        self.curCol = 0

    def pointInExtent(self, x, y):
        if (float(x) >= self.west and
            float(x) <= self.east and
            float(y) >= self.south and
            float(y) <= self.north):
            return True
        else:
            return False

    def calcStats(self):

#        histogram = self.band.GetDefaultHistogram()
#        self.band.SetDefaultHistogram(histogram[0], histogram[1], histogram[3])
        self.ds.BuildOverviews(overviewlist=[2, 4, 8, 16, 32, 64, 128])
        self.band.FlushCache()
        self.band.GetStatistics(0, 1)
        histogram = self.band.GetDefaultHistogram()
        self.band.SetDefaultHistogram(histogram[0], histogram[1], histogram[3])


    def close(self):
        self.ds = None

def mds_to_shape(MDSFile, outputfolder):

    #  bit of a hack but currently saving the shape file as
    #  three shape files presence, absence, and background
    #  as I'm having trouble getting QGIS to display points
    #  with categorical symbology

    h, t = os.path.split(MDSFile)
    t_no_ext = os.path.splitext(t)[0]
    outputfiles = {"pres":os.path.join(outputfolder, t_no_ext + "_pres.shp"),
                   "abs":os.path.join(outputfolder, t_no_ext + "_abs.shp"),
                   "backs":os.path.join(outputfolder, t_no_ext + "_backs.shp")}

    driver = ogr.GetDriverByName('ESRI Shapefile')

    MDSreader = csv.reader(open(MDSFile, 'r'))
    header1 = MDSreader.next()
    header2 = MDSreader.next()
    header3 = MDSreader.next()

    h, t = os.path.split(MDSFile)
    t_no_ext = os.path.splitext(t)[0]
#

    for outfile in outputfiles.values():
        h, t = os.path.split(outfile)
        if os.path.exists(outfile):
            os.chdir(h)
            driver.DeleteDataSource(t)

    ds = driver.CreateDataSource(h)

    preslayer = ds.CreateLayer(t_no_ext + "_pres",
                           geom_type=ogr.wkbPoint)
    abslayer = ds.CreateLayer(t_no_ext + "_abs",
                           geom_type=ogr.wkbPoint)
    backslayer = ds.CreateLayer(t_no_ext + "_backs",
                           geom_type=ogr.wkbPoint)

    #  cycle through the items in the header and add
    #  these to each output shapefile attribute table
    fields = {}
    for field in header1:
        field_name = Normalized_field_name(field, fields)
        print field_name
        fields[field_name] = field
        if field == "Split":
            #  this is the test training split field that we add
            fieldDefn = ogr.FieldDefn(field_name, ogr.OFTString)
        else:
            fieldDefn = ogr.FieldDefn(field_name, ogr.OFTReal)

        preslayer.CreateField(fieldDefn)
        abslayer.CreateField(fieldDefn)
        backslayer.CreateField(fieldDefn)

    featureDefn = preslayer.GetLayerDefn()

    #  cycle through the rows and add each geometry to the
    #  appropriate shapefile
    for row in MDSreader:
        feature = ogr.Feature(featureDefn)
        point = ogr.Geometry(ogr.wkbPoint)

        point.SetPoint_2D(0, float(row[0]), float(row[1]))
        feature.SetGeometry(point)

        #  for this feature add in each of the attributes from the row
        header_num = 0
        for field in header1:
            short_field = utilities.find_key(fields, field)
            if field == "Split":
                feature.SetField(short_field, row[header_num])
            else:
                if row[header_num] == 'NA':
                    feature.SetField(short_field, float(-9999))
                else:
                    feature.SetField(short_field, float(row[header_num]))
            header_num += 1

        response = float(row[2])

        if abs(response - 0) < 1e-9:
            abslayer.CreateFeature(feature)
        elif response > 0:
            preslayer.CreateFeature(feature)
        elif abs(response - -9999.0) < 1e-9 or \
            abs(response - -9998.0) < 1e-9:
            backslayer.CreateFeature(feature)

    #  close the data sources
    del MDSreader
    ds.Destroy()

def Normalized_field_name(field_name, previous_fields):
    short_name = field_name[:10]

    #  remove Non alpha numeric characters
    short_name = ''.join(ch for ch in short_name if ch in (string.ascii_letters + string.digits + '_'))

    if previous_fields.has_key(short_name):
        i = 1
        shorter_name = short_name[:8]
        short_name = shorter_name + "_" + str(i)
        while previous_fields.has_key(short_name):
            i += 1
            shorter_name = short_name[:9 - len(str(i))]
            short_name = shorter_name + "_" + str(i)
    return short_name

def getRasterName(fullPathName):
    if fullPathName.endswith('hdr.adf'):
        rastername = os.path.split(fullPathName)[0]
    else:
        rastername = fullPathName
    return rastername

def getRasterShortName(fullPathName):
    rasterName = getRasterName(fullPathName)
    rasterJustName = os.path.split(rasterName)[1]
    return os.path.splitext(rasterJustName)[0]

def transformPoint(x, y, from_srs, to_srs):
    """
    Transforms a point from one srs to another
    """
    coordXform = osr.CoordinateTransformation(from_srs, to_srs)
    yRound = round(y, 8)
    xRound = round(x, 8)

    result = coordXform.TransformPoint(xRound, yRound)

    gx = result[0]
    gy = result[1]

    return gx, gy

def isRaster(filePath):
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

def extentMatch(raster1, raster2):
    answer = True
    if not utilities.approx_equal(raster1.xScale, raster2.xScale): answer = False
    if not utilities.approx_equal(raster1.yScale, raster2.yScale): answer = False
    if not utilities.approx_equal(raster1.width, raster2.width): answer = False
    if not utilities.approx_equal(raster1.height, raster2.height): answer = False
    if not utilities.approx_equal(raster1.east, raster2.east): answer = False
    if not utilities.approx_equal(raster1.north, raster2.north): answer = False
    if raster2.srs.ExportToProj4() != raster1.srs.ExportToProj4(): answer = False

    return answer

def defaultNoData(GDALdatatype, signedByte=False):
    '''returns a reasonable default NoData value for a given
    GDAL data type.
    '''
    if signedByte:
        return -128

    crossWalk = {"Unknown":0,
                 "Byte":255,
                "Int16":-32768,
                "UInt32":4294967295,
                "Int32":-2147483648,
                "Float32":-3.4028235e+038,
                "Float64":2.2250738585072014e-308,
                "CInt16":-32768,
                "CInt32":-2147483648,
                "CFloat32":-3.4028235e+038,
                "CFloat64":2.2250738585072014e-308,
                }
    return crossWalk[gdal.GetDataTypeName(GDALdatatype)]

def GDALToNPDataType(GDALdatatype, signedByte=False):
    '''returns the coresponding numpy data time for a gdal data type
    '''
    if signedByte:
        return np.int8

    crossWalk = {"Unknown":np.int32,
                 "Byte":np.uint8,
                "Int16":np.int16,
                "UInt32":np.uint32,
                "Int32":np.int32,
                "Float32":np.float32,
                "Float64":np.float64,
                "CInt16":np.int16,
                "CInt32":np.int32,
                "CFloat32":np.float32,
                "CFloat64":np.float64,
                }
    return crossWalk[gdal.GetDataTypeName(GDALdatatype)]

def getAggregateTargetCellSize(sourceRaster, templateRaster):
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
    #  first determine what cell size we are going to use for the initial reproject/resample
    #  step 1:  Determine the native cell size in the template coordinate system.
    templateSRSCellSize = getTemplateSRSCellSize(sourceRaster, templateRaster)
    #  step 2:  round this up or down to an even fraction of the template cell size
    #  for example source = 30, target = 250 resampledSource = 250/round(250/30)
    sourcePixelsPerTarget = round(templateRaster.xScale / templateSRSCellSize)
    nearestWholeCellSize = (templateRaster.xScale / sourcePixelsPerTarget)
    return nearestWholeCellSize, sourcePixelsPerTarget

def getTemplateSRSCellSize(sourceRaster, templateRaster):
    """
    Calculate what size our source image pixels would be in the template SRS
    """
    #  first convert our template origin into the source srs
    tOriginX, tOriginY = transformPoint(templateRaster.west, templateRaster.north,
                                    templateRaster.srs, sourceRaster.srs)
    #  next add the source xScale to the converted origin x and convert that back to template srs
    tOriginX1 = transformPoint (tOriginX + sourceRaster.xScale, tOriginY,
                                            sourceRaster.srs, templateRaster.srs)[0]


#        templateCellXCorner1 = (self.template_params["west"], self.template_params["north"],
#                                        self.template_params["srs"], sourceParams["srs"])[0]
#
#        targetCellXCorner1 = (sourceParams["west"], sourceParams["north"],
#                                                sourceParams["srs"], self.template_params["srs"])[0]
#        targetCellXCorner2 = self.transformPoint(sourceParams["west"] + sourceParams["xScale"],
#                                                sourceParams["north"], sourceParams["srs"], self.template_params["srs"])[0]
    templateSRSCellSize = abs(abs(tOriginX1) - abs(templateRaster.west))
    return templateSRSCellSize

def intermediaryReprojection(sourceRaster, templateRaster, outRasterFName,
                             resamplingType, matchTemplateCellSize=False):
    '''Reprojects the sourceRaster into the templateRaster projection, datum
    and extent.  The output cell size is determined to be the closest dimension
    to the sourceRaster cell size that will evenly go into the template raster
    cell size
    '''
    outputFile = SAHMRaster(outRasterFName)
    outputFile.getParams(templateRaster.ds)
    outputFile.gt = templateRaster.gt
    outputFile.width = templateRaster.width
    outputFile.height = templateRaster.height
    outputFile.west = templateRaster.west
    outputFile.north = templateRaster.north
    outputFile.east = templateRaster.east
    outputFile.south = templateRaster.south

    outputFile.NoData = sourceRaster.NoData
    outputFile.pixelType = sourceRaster.pixelType
    outputFile.signedByte = sourceRaster.signedByte

    if matchTemplateCellSize:
        targetCellSize, numSourcePerTarget = (templateRaster.xScale, 1)
    else:
        targetCellSize, numSourcePerTarget = getAggregateTargetCellSize(sourceRaster, templateRaster)
    outputFile.xScale = targetCellSize
    outputFile.yScale = -1 * targetCellSize
    outputFile.height = templateRaster.height * int(templateRaster.xScale / targetCellSize)
    outputFile.width = templateRaster.width * int(templateRaster.xScale / targetCellSize)
    outputFile.createNewRaster()

    err = gdal.ReprojectImage(sourceRaster.ds, outputFile.ds,
                              sourceRaster.srs.ExportToWkt(),
                              templateRaster.srs.ExportToWkt(), resamplingType)


def average_nparrays(arrays):
    '''return the average of a list of np arrays
    These arrays must be 2d and have the same shape
    '''
    dstack = np.ma.dstack(arrays)
    return np.ma.mean(dstack, axis=2)

def average_geotifs(raster_fnames, outfname):
    '''takes a list of raster fnames and saves
    the average of their pixel values to a new raster
    with the outfname
    '''
    #  load our rasters
    #  we end up with a list of data iterators on each
    rasters = []
    for fname in raster_fnames:
        rasters.append(SAHMRaster(fname).iterBlocks())

    #  create our output raster
    out_raster = SAHMRaster(outfname)
    out_raster.pullParamsFromRaster(raster_fnames[0])
    out_raster.createNewRaster()

    #  loop though the blocks in our output raster
    #  calculate the cooresponding block from the inputs
    #  and put this value in the output blockd
#     average = itertools.imap(average_nparrays, zip(rasters))
    rasters.insert(0, out_raster.iterBlocks())
    for block in zip(*rasters):
        d = average_nparrays(block[1:])
        out_raster.putBlock(d, out_raster.curCol, out_raster.curRow)

    out_raster.close()
