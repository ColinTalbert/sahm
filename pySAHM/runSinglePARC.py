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

from optparse import OptionParser
from multiprocessing import Process, Queue

from osgeo import gdalconst
from osgeo import gdal
from osgeo import osr

from numpy import *
import numpy as np

import utilities
from PARC import PARC

def main(args_in):
    """
    Process commandline Arguments, 
    Create an instance of PARC with the Variables,
    Kick off the parkFiles function of our PARC instance
    """
    setupGDAL()
    
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
    parser.add_option('--gt0', dest='gt0')
    parser.add_option('--gt3', dest='gt3')
    parser.add_option('--tNorth', dest='tNorth')
    parser.add_option('--tSouth', dest='tSouth')
    parser.add_option('--tEast', dest='tEast')
    parser.add_option('--tWest', dest='tWest')
    parser.add_option('--tHeight', dest='height')
    parser.add_option('--tWidth', dest='width')
    
    (options, args) = parser.parse_args(args_in)
    
    ourPARC = PARC()
    ourPARC.verbose = options.verbose
    ourPARC.template = options.template
    outDir = os.path.split(options.dest)[0]
    ourPARC.outDir = outDir
    ourPARC.logger = utilities.logger(outDir, ourPARC.verbose)
    ourPARC.writetolog = ourPARC.logger.writetolog
    ourPARC.template_params = ourPARC.getRasterParams(options.template)
    
    if options.ignoreNonOverlap:
        gt = list(ourPARC.template_params['gt'])
        gt[0] = float(options.gt0)
        gt[3] = float(options.gt3)
        ourPARC.template_params["gt"] = tuple(gt)
        
        ourPARC.template_params['tNorth'] = float(options.tNorth)
        ourPARC.template_params['tSouth'] = float(options.tSouth)
        ourPARC.template_params['tEast'] = float(options.tEast)
        ourPARC.template_params['tWest'] = float(options.tWest)
        ourPARC.template_params['height'] = int(options.height)
        ourPARC.template_params['width'] = int(options.width)
    
    ourPARC.parcFile([options.source, options.categorical, options.resampling, options.aggregation], options.dest)

#    print "Finished successfully!"

def setupGDAL():
    parentDir = os.path.split(os.path.dirname(__file__))[0]
    gdal_data = os.path.join(parentDir, "GDAL_Resources", "gdal-data")
    os.environ['GDAL_DATA'] = gdal_data
    projlib = os.path.join(parentDir, "GDAL_Resources", "projlib")
    os.environ['PROJ_LIB'] = projlib
    

if __name__ == "__main__":

    try:
        main(sys.argv[1:])
    except:
        print "Job failed!", sys.exc_info()[0]
    

