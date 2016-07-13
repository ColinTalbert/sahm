#  -*- coding: latin-1 -*-
###############################################################################
# This file is part of the Software for Assisted Habitat Modeling (SAHM) package
# developed by the U.S. Geological Survey Fort Collins Science Center.
# It is intended to be used in the VisTrails Scientific
# VisTrails was developed by New York University (2014-2016), NYU-Poly (2011-2014),
# University of Utah (2006-2011).  VisTrails Contact: contact@vistrails.org
#
# SAHM Contact: talbertc@usgs.gov
#
# --------------------------------------------------------------------------------
# U.S. Geological Survey Disclaimers
# Any use of trade, product or firm names is for descriptive purposes only and does
# not imply endorsement by the U.S. Geological Survey.
#
# Although this information product, for the most part, is in the public domain,
# it also contains copyrighted material as noted in the text. Permission to reproduce
# copyrighted items for other than personal use must be secured from the copyright owner.
#
# Although these data have been processed successfully on a computer system at the
# U.S. Geological Survey, no warranty, expressed or implied is made regarding the
# display or utility of the data on any other system, or for general or scientific
# purposes, nor shall the act of distribution constitute any such warranty. The
# U.S. Geological Survey shall not be held liable for improper or incorrect use
# of the data described and/or contained herein.
#
# Although this program has been used by the U.S. Geological Survey (USGS), no
# warranty, expressed or implied, is made by the USGS or the U.S. Government as
# to the accuracy and functioning of the program and related program material nor
# shall the fact of distribution constitute any such warranty, and no responsibility
# is assumed by the USGS in connection therewith.
# --------------------------------------------------------------------------------
#
# This code is in the public domain and is licensed under Creative Commons CC0 1.0 Universal
#
###############################################################################


import glob
import math
import os
import shutil
import struct
import sys
import csv

from optparse import OptionParser
from multiprocessing import Process, Queue

from osgeo import gdalconst
from osgeo import gdal
from osgeo import osr

from numpy import *
import numpy as np

import utilities
import SpatialUtilities

from PARC import PARC

def parse_options(args_in):
    # Process command-line args.  
    usageStmt = "usage:  %prog [options] <template image> <input dir or list of input files>"
    desc = "This application projects, aggregates, resamples, and clips imagery."
    
    parser = OptionParser(usage=usageStmt, description=desc)
    parser.add_option("-s", dest="source", help="print the names of all known aggregation methods")
    parser.add_option("-c", dest="categorical")
    parser.add_option("-d", dest="dest", default="./", help="directory in which to put processed images, defaults to current directory")
    parser.add_option("-v", dest="verbose", default=False, action="store_true", help="the verbose flag causes diagnostic output to print")
    parser.add_option("-t", dest="template", help="The template raster used for projection, origin, cell size and extent")
    parser.add_option("-r", dest="resampling", help="The CSV containing the list of files to process.  Format is 'FilePath, Categorical, Resampling, Aggreagtion")
    parser.add_option("-a", dest="aggregation") 
    
    parser.add_option('-i', dest='ignoreNonOverlap',default=False, action="store_true")
    
    (options, args) = parser.parse_args(args_in)
    
    return options

def main(options):
    """
    Process commandline Arguments,
    Create an instance of PARC with the Variables,
    Kick off the parkFiles function of our PARC instance
    """
    setupGDAL()

    # Process command-line args.  tin
    ourPARC = PARC()
    ourPARC.verbose = options.verbose
    ourPARC.template = options.template
    ourPARC.templateRaster = SpatialUtilities.SAHMRaster(options.template)
    outDir = os.path.split(options.dest)[0]
    ourPARC.outDir = outDir
    ourPARC.logger = utilities.logger(os.path.dirname(outDir), ourPARC.verbose, False)
    ourPARC.writetolog = ourPARC.logger.writetolog

#     if options.ignoreNonOverlap:
#         ourPARC.shrink_template_extent(SpatialUtilities.SAHMRaster(options.source))
#     if ourPARC.templateRaster.width < 1 or \
#         ourPARC.templateRaster.height < 1:
#         #the sophisticated method of shrinking the raster blew up.  
#         #reset the template raster and try the naive method
#         ourPARC.templateRaster = SpatialUtilities.SAHMRaster(options.template)
#         ourPARC.shrink_template_extent_naive(SpatialUtilities.SAHMRaster(options.source))
    ourPARC.parcFile([options.source, options.categorical, options.resampling, options.aggregation], options.dest)



def setupGDAL():



    parentDir = os.path.split(os.path.dirname(__file__))[0]
    gdal_data = os.path.join(parentDir, "GDAL_Resources", "gdal-data")
    os.environ['GDAL_DATA'] = gdal_data
    projlib = os.path.join(parentDir, "GDAL_Resources", "projlib")
    os.environ['PROJ_LIB'] = projlib
    
    from osgeo import gdal
    gdal.UseExceptions()

if __name__ == "__main__":

    try:
        options = parse_options(sys.argv[1:])
        try:
            source_name = SpatialUtilities.getRasterShortName(
                            SpatialUtilities.getRasterShortName(options.source))
        except:
            source_name = '<<problem getting raster name>>'

        main(options)

        if SpatialUtilities.all_nodata(options.dest):
            sys.stderr.write("PARC processing of {} failed:\n{}".format(source_name,
                                            "all output pixels were nodata"))
        else:
            print "Finished PARC processing of {}".format(source_name)

    except Exception as e:
#          import traceback
#  #          exc_type, exc_value, exc_traceback = sys.exc_info()
#  #  #          print traceback.format_exc()
#  #          sys.stderr.write(traceback.format_exc())
#          print sys.exc_info()
        sys.stderr.write("PARC processing of {} failed:\n{}".format(source_name,
                                            e))


