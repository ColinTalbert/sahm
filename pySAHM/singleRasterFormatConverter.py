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
from RasterFormatConverter import FormatConverter

def main(argv):
    usageStmt = "usage:  options: -m --MDSFile -o --outputDir -f --format -v --verbose"
    desc = "Converts all of the tif files specified in an MDS to ASCII format (or optionally other formats)"
    parser = OptionParser(usage=usageStmt, description=desc)
    
    parser.add_option("-v", 
                      dest="verbose", 
                      default=False, 
                      action="store_true", 
                      help="the verbose flag causes diagnostic output to print.")
    parser.add_option("-i", "--inputfile", 
                      dest="input", 
                      help="")           
    parser.add_option("-o", "--outputDir", 
                      dest="outputDir", 
                      help="Output directory to save files in.")
    parser.add_option("-f", "--format", 
                      dest="format",
                      default='asc', 
                      help="The format to convert into. 'bil', 'img', 'tif', 'jpg', 'bmp', 'asc'")
    
    (options, args) = parser.parse_args(argv)
    
    ourFC = FormatConverter()
    ourFC.verbose = options.verbose
    ourFC.logger = utilities.logger(options.outputDir, ourFC.verbose)
    ourFC.writetolog = ourFC.logger.writetolog
    ourFC.outputDir = options.outputDir
    ourFC.format = options.format
    ourFC.convertEnvironmentalLayers([options.input, ], options.outputDir, options.format)
    
if __name__ == "__main__":
    main(sys.argv[1:])
#    try:
##        PARC().testing()
#        sys.exit(PARC().main(sys.argv[1:]))
#    except Exception as e:
#        print e
#        sys.exit(1)
