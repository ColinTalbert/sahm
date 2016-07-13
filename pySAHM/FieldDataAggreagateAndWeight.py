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

import sys, os
import math
import csv

from optparse import OptionParser

from osgeo import osr
import utilities
import SpatialUtilities

def main(argv):
    usageStmt = "usage:  options: -t --template -f --fieldData -o --output"
    desc = "Aggregates sample points by pixel and/or year."

    parser = OptionParser(usage = usageStmt, description = desc)
    parser.add_option("-t", "--template",
                      dest = "template",
                      help = "The template grid in Tif, ESRI grid, or ASC format")
    parser.add_option("-f", "--fieldData",
                      dest = "csv",
                      help = "The CSV of field data")
    parser.add_option("-o", "--output",
                      dest = "output",
                      help = "The output CSV file with appended frequency and numPresence")
    parser.add_option("-v", "--verbose",
                      dest = "verbose",
                      default = False,
                      action = "store_true",
                      help = "the verbose flag causes diagnostic output to print")

    (options, args) = parser.parse_args(argv)

    ourFDQ = FieldDataQuery()
    ourFDQ.template = options.template
    ourFDQ.csv = options.csv
    ourFDQ.output = options.output
    ourFDQ.AggByPixel = options.bAgg
    ourFDQ.verbose = options.verbose
    ourFDQ.processCSV()

class FieldDataQuery(object):

    def __init__(self):
        # instance level variables
        self.csv = None
        self.templatefName = None
        self.output = None
        self.template = None
        self.aggMethod = 'Collapse In Pixel'
        self.verbose = False
        self.countdata = False
        self.epsg = None
        self.pointsSpatialRef = None
        self.logger = None
        self.drop_nodata_points = True

    def validateArgs(self):
        """
        Make sure the user sent us some stuff we can work with
        """

        # Validate template image.
        if self.templatefName is None:
            raise Exception, "template raster not provided (-t command line argument missing)"

        if not os.path.exists(self.templatefName):
            raise Exception, "Template file, " + self.template + ", does not exist on file system"

        self.template = SpatialUtilities.SAHMRaster(self.templatefName)
        if len(self.template.Error) <> 0:
            print ("There was a problem with the provided template: \n    " +
                                    "    " + "\n    ".join(self.template.Error))
            raise Exception, ("There was a problem with the provided template: \n    " +
                                    "    " + "\n    ".join(self.template.Error))

        # Ensure the template has square pixels.
        if abs(abs(self.template.xScale) - abs(self.template.yScale)) > 1e-6:
            print "The template raster must have square pixels."
            print "x pixel scale = " + str(self.template.xScale)
            print "y pixel scale = " + str(self.template.yScale)
            raise Exception, "template image must have square pixels."

        # Validate the CSV
        if self.csv is None:
            raise Exception, "No csv provided"

        if not os.path.exists(self.csv):
            raise Exception, "CSV file, " + self.csv + ", does not exist on file system"

        # make sure the directory the mds file is going into exists:
        outDir = os.path.split(self.output)[0]
        if not os.path.exists(outDir):
            raise RuntimeError, "The directory of the supplied MDS output file path, " + self.output + ", does not appear to exist on the filesystem"

        if self.epsg:
            try:
                self.pointsSpatialRef = osr.SpatialReference()
                self.pointsSpatialRef.ImportFromEPSG(self.epsg)
            except:
                raise RuntimeError, "The EPSG code provided, " + str(self.epsg) + ", is not known to the current installation of GDAL."

        if self.logger is None:
            self.logger = utilities.logger(outDir, self.verbose)
        self.writetolog = self.logger.writetolog


    def processCSV(self):

        self.validateArgs()
        if self.verbose:
            self.writetolog("Starting on Field Data Query for " + os.path.split(self.csv)[1])
            self.writetolog("  using template " + os.path.split(self.templatefName)[1])

        templatename = os.path.split(self.templatefName)[1]
        if templatename == 'hdr.adf':
            templatename = os.path.split(os.path.split(self.templatefName)[0])[1]

        csvfile = open(self.csv, "r")
        reader = csv.reader(csvfile)
        usedPixels = {}
        header = reader.next()
        if header[2].lower() == 'responsecount':
            self.countdata = True

        if header[-1].startswith('input='):
            # this file was produced with FDQ
            origCSV = header[-1].replace('input=', '')
            header = header[:-1]
        else:
            origCSV = self.csv

        if self.aggMethod == 'Collapse In Pixel':
            header.append("frequency")
            header.append("numPresence")
            header.append("pixelColumn")
            header.append("pixelRow")
        else:
            header.append("Weights")

        header.append(os.path.abspath(SpatialUtilities.get_raster_name(self.templatefName)))
        header.append(os.path.abspath(origCSV))

        # loop through each row (observation) and
        # if that particular pixel hasn't been encountered before
        # add it to a dictionary containing a key of the pixel X,Y
        # and values of each row encountered for that pixel
        # if pixel
        lineCount = linesInFile(self.csv)
        extraPoints = []
        nodata_points = []
        pointCount = 0
        pcntDone = 0
        line = 1
        for row in reader:
            line += 1
            # make sure x, and y are numbers!
            try:
                x = float(row[0])
                y = float(row[1])
            except ValueError:
                raise Exception, "Non-numeric X, Y used.\nLine number " + str(line)

            if self.pointsSpatialRef:
                try:
                    x, y = SpatialUtilities.transformPoint(x, y,
                                 self.pointsSpatialRef, self.template.srs)
                except:
                    raise Exception, "Problem transforming point: " + str(line)

            if self.template.pointInExtent(x, y):
                pixelColumn, pixelRow = self.template.convertCoordsToColRow(x, y)
                
                if self.template.getPixelValueFromIndex(pixelColumn, pixelRow) == self.template.NoData and \
                    self.drop_nodata_points:
                    nodata_points.append([x, y, row[2]])
                else:
                    pixel = (pixelColumn, pixelRow)
                    # if verbose == True:
                    if not pixel in usedPixels:
                        usedPixels[pixel] = [row]
                    else:
                        curVal = usedPixels[pixel]
                        curVal.append(row)
                        usedPixels[pixel] = curVal
            else:
                extraPoints.append([x, y, row[2]])
            pointCount += 1
            if self.verbose:
                if float(pointCount) / lineCount > float(pcntDone) / 100:
                    pcntDone += 10
                    if self.verbose:
                        print str(pcntDone) + "...",

        # Open up and write to an output file
        oFile = open(self.output, 'wb')
        fOut = csv.writer(oFile, delimiter = ',', quotechar = '"', quoting = csv.QUOTE_MINIMAL)
        fOut.writerow(header)

        # Add each used pixel to the output file
        for k, v in usedPixels.iteritems():

            if self.aggMethod == 'Collapse In Pixel':
                outputLine = v[0]
                col = k[0]
                row = k[1]
                x, y = self.template.convertColRowToCoords(col, row)

                frequency = len(v)

                # loop though the 'points' in each pixel
                # for count data the value is the sum of the 'points'
                #    if there were any 'hits' not equal to 0
                #    otherwise if there were any absences the value is 0
                #    what's left is background points
                # for presence/absence data
                #    The value is 1 if there were any 1s in our points
                #    Or 0 if there were any zeros (absences)
                #    Otherwise it's a background pixel.
                total = 0
                count = 0
                numAbsense = 0
                for i in range (frequency):
                    if int(float(v[i][2])) > 0:
                        total += int(float(v[i][2]))
                        count += 1
                    if int(float(v[i][2])) == 0:
                        numAbsense += 1
                    else:
                        # outValue can be either -9999 (background) or
                        # -9998 (psuedoabsence) the last one
                        outValue = int(float(v[i][2]))

                outputLine[0] = x
                outputLine[1] = y

                if self.countdata and total > 0:
                    outputLine[2] = total
                elif total > 0:
                    outputLine[2] = 1
                elif numAbsense > 0:
                    outputLine[2] = 0
                else:
                    outputLine[2] = outValue

                outputLine.append(frequency)
                outputLine.append(total)
                outputLine.append(pixelColumn)
                outputLine.append(pixelRow)
                fOut.writerow(outputLine)
            else:
                for point in v:
                    outputLine = point
                    if self.pointsSpatialRef:
                        try:
                            x, y = float(outputLine[0]), float(outputLine[1])
                            x, y = SpatialUtilities.transformPoint(x, y,
                                         self.pointsSpatialRef, self.template.srs)
                            outputLine[0], outputLine[1] = x, y
                        except:
                            raise Exception, "Problem transforming point: " + str(line)
                    outputLine[2] = str(1.0 / len(v))
                    outputLine.append(len(v))
                    fOut.writerow(outputLine)

        oFile.close
        if self.verbose:
            self.writetolog("Done\nFinished creating field data query output.\n")
            if len(extraPoints) + len(nodata_points) == pointCount:
                msg = "All " + str(pointCount) + " points were outside of the template extent\n"
                msg += "This might indicate a mismatch between the template and field data projection.\n"
                msg += "\nProcessing cannot continue"
                raise Exception, "All points were outside of the template extent"

            if len(extraPoints) > 0:
                self.writetolog ("  WARNING: " + str(len(extraPoints)) + " points" +
                    " out of " + str(pointCount) + " total points in the " +
                    "original CSV were outside the template extent and WERE NOT " +
                    "INCLUDED IN THE FDAW OUTPUT.")

            if len(nodata_points) > 0:
                self.writetolog ("  WARNING: " + str(len(nodata_points)) + " points" +
                    " out of " + str(pointCount) + " total points in the " +
                    "original CSV were in NoData areas of the template and WERE NOT " +
                    "INCLUDED IN THE FDAW OUTPUT.")


def linesInFile(filename):
    f = open(filename)
    lines = 0
    buf_size = 1024 * 1024
    read_f = f.read  # loop optimization

    buf = read_f(buf_size)
    while buf:
        lines += buf.count('\n')
        buf = read_f(buf_size)

    return lines

if __name__ == '__main__':
    main(sys.argv)
