import os
import csv
import random
import string

from math import floor

from osgeo import gdalconst
from osgeo import gdal
from osgeo import osr
from osgeo import ogr

import utilities

gdal.UseExceptions()
gdal.AllRegister()

class SAHMRaster():
    '''An extension to a GDAL raster that contains methods for 
    the various methods we call on rasters.
    '''
    def __init__(self, rasterFile):
        self.source = rasterFile
        self.getParams()
        
    def getParams(self):
        """
        Extracts properties from a passed raster
        All values are stored in a dictionary which is returned.
        If errors are encountered along the way the error messages will
        be returned as a list in the 'Error' element.
        """
        try:
            #initialize our params dictionary to have None for all parma
            allRasterParams = ["Error", "xScale", "yScale", "width", "height",
                            "east", "north", "west", "south",  
                            "gEast", "gNorth", "gWest", "gSouth",  
                            "Wkt", "srs", "gt", "prj", "NoData", "PixelType", "file_name"]
            
            for param in allRasterParams:
                setattr(self, param, None)
                
            self.Error = []
            
            if not os.path.exists(self.source):
                self.Error.append("The input file (" + self.source + ") does not exist on the file system.")
                return
            
            # Get the PARC parameters from the rasterFile.
            self.ds = gdal.Open(self.source, gdalconst.GA_ReadOnly)
            self.band = self.ds.GetRasterBand(1)
            if self.ds is None:
                self.Error.append("Unable to open file with GDAL")
                return params
                
            xform  = self.ds.GetGeoTransform()
            self.xScale = xform[1]
            self.yScale = xform[5]
    
            self.width  = self.ds.RasterXSize
            self.height = self.ds.RasterYSize
    
            self.west = xform[0]
            self.north = xform[3]
            self.east = self.west + self.width  * self.xScale
            self.south = self.north + self.height * self.yScale
    
            try:
                wkt = self.ds.GetProjection()
                self.gt = self.ds.GetGeoTransform()
                self.prj = self.ds.GetProjectionRef()
                self.srs = osr.SpatialReference(wkt)
                if wkt == '':
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
                #print "We ran into problems getting the projection information for " +  rasterFile
                self.Error.append("Undefined problems extracting the projection information")
    
            try:
                self.signedByte = self.band.GetMetadata('IMAGE_STRUCTURE')['PIXELTYPE'] == 'SIGNEDBYTE'
            except KeyError:
                self.signedByte = False
            
            self.NoData = self.band.GetNoDataValue()
            if self.NoData == None:
                if self.band.DataType == 1:
                    print "Warning:  Could not extract NoData value.  Using assumed nodata value of 255"
                    self.NoData = 255
                elif self.band.DataType == 2:
                    print "Warning:  Could not extract NoData value.  Using assumed nodata value of 65536"
                    self.NoData = 65536
                elif self.band.DataType == 3:
                    print "Warning:  Could not extract NoData value.  Using assumed nodata value of 32767"
                    self.NoData = 32767
                elif self.band.DataType == 4:
                    print "Warning:  Could not extract NoData value.  Using assumed nodata value of 2147483647"
                    self.NoData = 2147483647
                elif self.band.DataType == 5:
                    print "Warning:  Could not extract NoData value.  Using assumed nodata value of 2147483647"
                    self.NoData = 2147483647
                elif self.band.DataType == 6:
                    print "Warning:  Could not extract NoData value.  Using assumed nodata value of -3.40282346639e+038"
                    self.NoData = -3.40282346639e+038
                else:
                    self.Error.append("Could not identify nodata value")
            self.PixelType = self.band.DataType
            if self.PixelType == None:
                self.Error.append("Could not identify pixel type (bit depth)")
            
        except:
            #print "We ran into problems extracting raster parameters from " + rasterFile
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
        col = int(floor((x - self.west) / self.xScale))
        row = int(floor((y - self.north) / self.yScale))
        return col, row
    
    def getPixelValueFromIndex(self, col, row):
        return self.band.ReadAsArray(int(col), int(row), 1, 1)[0,0]
    
    def getPixelValueFromCoords(self, x, y):
        col, row = self.convertCoordsToColRow(x, y)
        return self.getPixelValueFromIndex(col, row)
    
    def pointInExtent(self, x, y):
        if (float(x) >= self.west and
            float(x) <= self.east and
            float(y) >= self.south and
            float(y) <= self.north):
            return True
        else:
            return False
    
def mds_to_shape(MDSFile, outputfolder):
    
    #bit of a hack but currently saving the shape file as 
    #three shape files presence, absence, and background
    #as I'm having trouble getting QGIS to display points
    #with categorical symbology
    
    h, t = os.path.split(MDSFile)
    t_no_ext = os.path.splitext(t)[0]
    outputfiles = {"pres":os.path.join(outputfolder, t_no_ext + "_pres.shp"),
                   "abs":os.path.join(outputfolder, t_no_ext +  "_abs.shp"),
                   "backs":os.path.join(outputfolder, t_no_ext +  "_backs.shp")}

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
    
    #cycle through the items in the header and add
    #these to each output shapefile attribute table
    fields = {}
    for field in header1:
        field_name = Normalized_field_name(field, fields)
        fields[field_name] = field
        if field == "Split":
            #this is the test training split field that we add
            fieldDefn = ogr.FieldDefn(field_name, ogr.OFTString)
        else:
            fieldDefn = ogr.FieldDefn(field_name, ogr.OFTReal)
        
        preslayer.CreateField(fieldDefn)
        abslayer.CreateField(fieldDefn)
        backslayer.CreateField(fieldDefn)
        
    featureDefn = preslayer.GetLayerDefn()
    
    #cycle through the rows and add each geometry to the 
    #appropriate shapefile
    for row in MDSreader:
        feature = ogr.Feature(featureDefn)
        point = ogr.Geometry(ogr.wkbPoint)
        
        point.SetPoint_2D(0, float(row[0]), float(row[1]))
        feature.SetGeometry(point)
        
        # for this feature add in each of the attributes from the row
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

    # close the data sources
    del MDSreader
    ds.Destroy()

def Normalized_field_name(field_name, previous_fields):
    short_name = field_name[:10]
    
    #remove Non alpha numeric characters
    short_name = ''.join(ch for ch in short_name if ch in (string.ascii_letters + string.digits + '_'))
    
    if previous_fields.has_key(short_name):
        i = 1
        shorter_name = short_name[:8]
        short_name = shorter_name + "_" + str(i)
        while previous_fields.has_key(short_name):
            i += 1
            short_name = shorter_name + "_" + str(i)
    return short_name

def getRasterName(fullPathName):
    if fullPathName.endswith('hdr.adf'):
        rastername = os.path.split(fullPathName)[0]
    else:
        rastername = fullPathName
    return rastername

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