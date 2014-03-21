#!/usr/bin/python
###############################################################################
#  #
#  # Copyright (C) 2010-2012, USGS Fort Collins Science Center.
#  # All rights reserved.
#  # Contact: talbertc@usgs.gov
#  #
#  # This file is part of the Software for Assisted Habitat Modeling package
#  # for VisTrails.
#  #
#  # "Redistribution and use in source and binary forms, with or without
#  # modification, are permitted provided that the following conditions are met:
#  #
#  #  - Redistributions of source code must retain the above copyright notice,
#  #    this list of conditions and the following disclaimer.
#  #  - Redistributions in binary form must reproduce the above copyright
#  #    notice, this list of conditions and the following disclaimer in the
#  #    documentation and/or other materials provided with the distribution.
#  #  - Neither the name of the University of Utah nor the names of its
#  #    contributors may be used to endorse or promote products derived from
#  #    this software without specific prior written permission.
#  #
#  # THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
#  # AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
#  # THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
#  # PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
#  # CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
#  # EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
#  # PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
#  # OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
#  # WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
#  # OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
#  # ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
#  #
#  # Although this program has been used by the U.S. Geological Survey (USGS),
#  # no warranty, expressed or implied, is made by the USGS or the
#  # U.S. Government as to the accuracy and functioning of the program and
#  # related program material nor shall the fact of distribution constitute
#  # any such warranty, and no responsibility is assumed by the USGS
#  # in connection therewith.
#  #
#  # Any use of trade, firm, or product names is for descriptive purposes only
#  # and does not imply endorsement by the U.S. Government.
###############################################################################

import os
import sys
import csv

from optparse import OptionParser

from osgeo import gdalconst
from osgeo import gdal

from numpy import *
import numpy as np
import scipy.stats.stats as stats

import utilities
import SpatialUtilities
import multiprocessing

def main(args_in):
    """
    Process commandline Arguments,
    Create an instance of PARC with the Variables,
    Kick off the parkFiles function of our PARC instance
    """
    #  Process command-line args.
    usage_stmt = "usage:  %prog [options] <template image> <input dir or " + \
        "list of input files>"
    desc = "This application projects, aggregates, resamples, and clips" + \
        " imagery."

    parser = OptionParser(usage=usage_stmt, description=desc)
    parser.add_option("-l", dest="listMethodFlag", default=False, action="store_true", help="print the names of all known aggregation methods")
    parser.add_option("-o", dest="out_dir", default="./", help="directory in which to put processed images, defaults to current directory")
    parser.add_option("-v", dest="verbose", default=False, action="store_true", help="the verbose flag causes diagnostic output to print")
    parser.add_option("-t", dest="templateRaster", help="The template raster used for projection, origin, cell size and extent")
    parser.add_option("-i", dest="inputs_CSV", help="The CSV containing the list of files to process.  Format is 'FilePath, Categorical, Resampling, Aggreagtion")
    parser.add_option("-m", dest="multicore", default=False, action="store_true", help="Flag indicating to use multiple cores")
    parser.add_option("-n", dest="ignoreNonOverlap", default=False, action="store_true", help="Flag indicating to use ignore non-overlapping area")

    (options, args) = parser.parse_args(args_in)

    our_PARC = PARC()
    our_PARC.verbose = options.verbose
    our_PARC.template = options.templateRaster
    our_PARC.out_dir = options.out_dir
    our_PARC.inputs_CSV = options.inputs_CSV
    our_PARC.multicores = options.multicore
    our_PARC.ignoreNonOverlap = options.ignoreNonOverlap
    our_PARC.logger = utilities.logger(os.path.join(our_PARC.out_dir,
                                                    "logfile.txt"), True)
    our_PARC.parcFiles()

class PARC(object):
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
        #  instance level variables
        self.verbose = False
        self.template = ""
        self.templateRaster = None
        self.out_dir = ""
        self.inputs_CSV = ''
        self.inputs = []
        self.agg_methods = ['Min', 'Mean', 'Max', 'Majority', 'STD']
        self.resample_methods = ['NearestNeighbor', 'Bilinear',
                                 'Cubic', 'CubicSpline', 'Lanczos']
        self.logger = None
        self.multicores = True
        self.processingMode = ""
        self.ignoreNonOverlap = False
        self.module = None
        self.pool_processes = []

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
        self.logger.writetolog("    Arguments validated successfully",
                               True, True)
        self.processFiles()

        self.logger.writetolog("Finished PARC", True, True)

    def processFiles(self):
        if self.processingMode == "FORT Condor":
            self.process_pool = multiprocessing.Pool(multiprocessing.cpu_count() - 1)
        else:
            self.process_pool = multiprocessing.Pool(multiprocessing.cpu_count() - 1)
        self.pool_processes = []

        #  Clip and reproject each source image.
        for image in self.inputs:
            in_fname = SpatialUtilities.getRasterShortName(image[0])
            out_file = os.path.join(self.out_dir, in_fname + ".tif")

            process_queue = []
            if os.path.exists(out_file):
                try:
                    gdal.Open(out_file)
                    msg = "The output " + in_fname + \
                        " already exists. \tSkipping this file."
                    self.logger.writetolog(msg, True, True)
                except:
                    #  we bombed trying to open the outFile with gdal. Lets rerun it.
                    process_queue.append(self.gen_singlePARC_thread(image, out_file))

            else:
                process_queue.append(self.gen_singlePARC_thread(image, out_file))

        #  wait for the last set of processes to finish up
        if self.processingMode == "FORT Condor":
            self.waitForCondorProcessesToFinish(process_queue)
        else:
            for process in self.pool_processes:
                process.get()

        print "done"

    def gen_singlePARC_thread(self, image, outFile):
        command_arr = self.gen_singlePARC_cmd(image, outFile)

        if self.processingMode == "FORT Condor":
            workspace, fname = os.path.split(os.path.abspath(outFile))
            prefix = os.path.splitext(fname)[0]
            utilities.runCondorPythonJob(command_arr, workspace, prefix)
        else:
            self.pool_processes.append(self.process_pool.apply_async(
                                utilities.launch_cmd,
                                [self.gen_singlePARC_cmd(image, outFile)]))

            return os.path.abspath(outFile)

    def gen_singlePARC_cmd(self, image, outFile):
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
        self.logger.writetolog(command, False, False)
        return command_arr

    def log_result(self, result):
        print result

    def waitForCondorProcessesToFinish(self, outputs):
        errors = []
        originalCount = len(outputs)
        while outputs:
            for process in outputs:
                result = self.jobFinished(process)
                if result == "finished":
                    outputs.remove(process)

                    #  cleanup some condor files
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
                    print str(originalCount - len(outputs)) + \
                        " PARC layers finished"

        if len(errors) > 0:
            msg = "There were problems with one or more runs."
            for process in errors:
                msg += "\n" + process + " did not run correctly"
            raise utilities.TrappedError(msg)

    def jobFinished(self, output):
        stdout = output.replace(".tif", "_stdOut.txt")
        try:
            lastline = open(stdout, "r").readlines()[-1]
            if lastline.startswith("Finished successfully!"):
                return "finished"
            elif lastline.startswith("Job failed!"):
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

        short_name = os.path.split(os.path.splitext(source[0])[0])[1]
        self.logger.writetolog("    Starting processing of " + source[0])
        source_raster = SpatialUtilities.SAHMRaster(source[0])

        gdal_type = self.get_gdal_type_from_string(source[2])

        template_src_cellsize = SpatialUtilities.getTemplateSRSCellSize(
                            source_raster, self.templateRaster)
        cell_ratio = template_src_cellsize / self.templateRaster.xScale
        msg = "  ratio of source cell size to template cell size = " + str(cell_ratio)
        msg += "    template cell size = " + str(self.templateRaster.xScale)
        msg += "    " + short_name + " cell size = " + str(template_src_cellsize)
        self.writetolog(msg)

        if cell_ratio > 0.5:
            #  The source cell size is close enough to our template cell size,
            #  or smaller so that all we need to do is reproject and resample.
            self.logger.writetolog("  cell ratio > .5: reprojecting and resampling to template parameters only")
            SpatialUtilities.intermediaryReprojection(source_raster,
                            self.templateRaster, dest, gdal_type, True)
        else:
            #  Our Target cell size is much bigger than our source we need to do
            #  some aggregation to make things work.
            msg = '  cell ratio <= .5: reprojecting and resampling to template parameters'
            msg += '    then aggregating the reprojected raster to match template parameters'
            self.writetolog(msg)

            target_cell_size, num_source_per_target = \
                SpatialUtilities.getAggregateTargetCellSize(source_raster,
                                                            self.templateRaster)
            tmpOutput = os.path.join(os.path.dirname(dest), "tmp_" +
                                         os.path.basename(dest))

            SpatialUtilities.intermediaryReprojection(source_raster,
                            self.templateRaster, tmpOutput, gdal_type, False)
            self.writetolog("   Starting on Aggregating: " + short_name)

            tmp_output_raster = SpatialUtilities.SAHMRaster(tmpOutput)
            self.Aggregate(tmp_output_raster, dest, source[3],
                           num_source_per_target)

            self.writetolog("   Finished Aggregating: " + short_name)
            try:
                os.remove(tmpOutput)
            except WindowsError:
                pass

    def get_gdal_type_from_string(self, gdal_gra_description):
        '''Returns a gdal resampling type based on the string passed
        gdal_GRA_description must be one of: nearestneighbor, bilinear,
        cubic, cubicspline, or lanczos.
        Otherwise will default and return nearestneighbor
        Note gdal_GRA_description is not case sensitive
        '''
        gdal_type = None
        if gdal_gra_description.lower() == "nearestneighbor":
            gdal_type = gdalconst.GRA_NearestNeighbour
        if gdal_gra_description.lower() == "bilinear":
            gdal_type = gdalconst.GRA_Bilinear
        if gdal_gra_description.lower() == "cubic":
            gdal_type = gdalconst.GRA_Cubic
        if gdal_gra_description.lower() == "cubicspline":
            gdal_type = gdalconst.GRA_CubicSpline
        if gdal_gra_description.lower() == "lanczos":
            gdal_type = gdalconst.GRA_Lanczos
        if gdal_type == None:
            self.logger.writetolog("   Specified resampling method ("
                + gdal_gra_description + ") not one of 'NearestNeighbor', " +
                "'Bilinear', 'Cubic', 'CubicSpline', or 'Lanczos'.  " +
                "Defaulting to 'NearestNeighbor'")
            gdal_type = gdalconst.GRA_NearestNeighbour
        return gdal_type

    def Aggregate(self, sourceRaster, outFName,
                  method=None, numSourcePerTarget=10):

        tmpOutput = os.path.splitext(outFName)[0] + ".tif"
        tmpOutDataset = SpatialUtilities.SAHMRaster(tmpOutput)
        tmpOutDataset.pullParamsFromRaster(self.templateRaster.source)
        tmpOutDataset.createNewRaster()

        rows = int(sourceRaster.height)
        cols = int(sourceRaster.width)

        #  loop of 'blocks' of data maybe.
        bSize = 2048  #  source pixels
        #  convert this to the nearest whole number of target pixels
        bSize = int(round(bSize / numSourcePerTarget) * numSourcePerTarget)
        if bSize == 0:
            bSize = int(numSourcePerTarget)


        for i in range(0, rows, bSize):
            if i + bSize < rows:
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
                    ans = self.rebin(data, (numRows / numSourcePerTarget, numCols / numSourcePerTarget), method)
                else:
                    X, Y = data.shape
                    x = X // numSourcePerTarget
                    y = Y // numSourcePerTarget
                    ndMask = data.reshape((x, numSourcePerTarget, y, numSourcePerTarget))
                    ndMask = ndMask.transpose([0, 2, 1, 3])
                    ndMask = ndMask.reshape((x * y, numSourcePerTarget * numSourcePerTarget))
                    ans = np.array(stats.mode(ndMask, 1)[0]).reshape(x, y)

                tmpOutDataset.putBlock(ans, int(j / numSourcePerTarget), int(i / numSourcePerTarget))

        tmpOutDataset.calcStats()
        tmpOutDataset.close()


    def rebin(self, a, shape, method):
        sh = shape[0], a.shape[0] // shape[0], shape[1], a.shape[1] // shape[1]
        if method == "Mean":
            return a.reshape(sh).mean(-1).mean(1)
        elif method == "Min":
            return a.reshape(sh).data_min(-1).data_min(1)
        elif method == "Max":
            return a.reshape(sh).data_max(-1).data_max(1)
        elif method == "STD":
            sh2 = sh[0], sh[2], sh[1] * sh[3]
            return np.rollaxis(a.reshape(sh), 1, -1).reshape(sh2).std(-1)

    def image_covers_template(self, source_raster):
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
        testPoints = []

        for x in range(n + 1):
            for y in range(n + 1):
                testPoints.append(SpatialUtilities.transformPoint(curX, curY,
                                self.templateRaster.srs, source_raster.srs))
                curY -= yOffset

            curX += xOffset
            curY = self.templateRaster.north

        bad_point = False
        for point in testPoints:
            try:
                xOffset = int((point[0] - source_raster.west) / source_raster.xScale)
                yOffset = int((point[1] - source_raster.north) / source_raster.yScale)
                data = source_raster.getBlock(xOffset, yOffset, 1, 1)
                value = data[0, 0]
            except:
                bad_point = True

        #  if valid values were returned from each of our points then
        #  the template falls entirely within the Source image.
        return not bad_point

    def maxNorth(self, sourceRaster):
        northWidth = sourceRaster.east - sourceRaster.west
        steps = 10
        maxNorth = -999999
        for step in range(steps + 1):
            curWest = sourceRaster.west + step * (northWidth / steps)
            transPoint = SpatialUtilities.transformPoint(curWest, sourceRaster.north,
                        sourceRaster.srs, self.templateRaster.srs)
#            print curWest, sourceParams['north'], " = ", transPoint
            if transPoint[1] > maxNorth:
                maxNorth = transPoint[1]
        return maxNorth

    def minSouth(self, sourceRaster):
        southWidth = sourceRaster.east - sourceRaster.west
        steps = 10
        minSouth = 999999
        for step in range(steps + 1):
            curWest = sourceRaster.west + step * (southWidth / steps)
            transPoint = SpatialUtilities.transformPoint(curWest, sourceRaster.south,
                        sourceRaster.srs, self.templateRaster.srs)
#            print curWest, sourceParams['south'], " = ", transPoint
            if transPoint[1] < minSouth:
                minSouth = transPoint[1]
        return minSouth

    def maxEast(self, sourceRaster):
        eastHeight = sourceRaster.north - sourceRaster.south
        steps = 10
        maxEast = -999999
        for step in range(steps + 1):
            curNorth = sourceRaster.south + step * (eastHeight / steps)
            transformed_point = SpatialUtilities.transformPoint(
                        sourceRaster.east, curNorth,
                        sourceRaster.srs, self.templateRaster.srs)
#            print curWest, sourceParams['south'], " = ", transformed_point
            if transformed_point[0] > maxEast:
                maxEast = transformed_point[0]
        return maxEast

    def minWest(self, sourceRaster):
        westHeight = sourceRaster.north - sourceRaster.south
        steps = 10
        minWest = 999999
        for step in range(steps + 1):
            curNorth = sourceRaster.south + step * (westHeight / steps)
            transPoint = SpatialUtilities.transformPoint(sourceRaster.west,
                        curNorth, sourceRaster.srs, self.templateRaster.srs)
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

        #  Validate template image.
        if self.template is None:
            raise utilities.TrappedError("template raster not provided.")

        if not os.path.exists(self.template):
            raise utilities.TrappedError("Template file, " + self.template + ", does not exist on file system")

        self.templateRaster = SpatialUtilities.SAHMRaster(self.template)
        if len(self.templateRaster.Error) != 0:
            raise utilities.TrappedError("There was a problem with the provided template: \n    " +
                                    "    " + "\n    ".join(self.templateRaster.Error))

        #  Ensure the template has square pixels.
        if abs(abs(self.templateRaster.xScale) - abs(self.templateRaster.yScale)) > 1e-6:
            raise utilities.TrappedError("template image must have square pixels." +
                            "/n    x pixel scale = " + str(abs(self.templateRaster.xScale)) +
                            "/n    y pixel scale = " + str(abs(self.templateRaster.yScale)))


        #  Validate input rasters
        if not os.path.exists(self.inputs_CSV):
            raise utilities.TrappedError("Inputs CSV, " + self.inputs_CSV + ", does not exist on file system.")

        inputs_csv = csv.reader(open(self.inputs_CSV, 'r'))
        header = inputs_csv.next()
        input_file_errors = ""

        output_csv = os.path.join(self.out_dir, "PARC_Files.csv")
        if os.path.exists(output_csv):
            existing_files = np.genfromtxt(output_csv, dtype='S1000', delimiter=",", skip_header=True)[:, 4]
            existing_files = [os.path.abspath(f) for f in existing_files]
            output = csv.writer(open(output_csv, "ab"))
        else:
            existing_files = []
            output = csv.writer(open(output_csv, "wb"))
            output.writerow(["PARCOutputFile", "Categorical", "Resampling",
                             "Aggregation", "OriginalFile",
                             os.path.abspath(self.template),
                             os.path.abspath(self.out_dir)])

        inputs = []
        for row in inputs_csv:
            input_file = row[0]
            input_just_file = SpatialUtilities.getRasterShortName(input_file)

            if not utilities.covariate_name_is_ok(input_just_file):
                input_file_errors += "\n  Input Covariate, " + input_just_file
                input_file_errors += "begins with a number or has a special character in it"

            if input_just_file in inputs:
                input_file_errors += "\n  PARC not currently set up to handle identically named inputs."
                input_file_errors += "\n\t" + input_just_file + " used multiple times"
            else:
                inputs.append(input_just_file)


            sourceRaster = SpatialUtilities.SAHMRaster(input_file)
            if len(sourceRaster.Error) > 0:
                input_file_errors += ("  " + os.path.split(input_file)[1] + " had the following errors:\n" +
                                    "    " + "\n    ".join(sourceRaster.Error)) + "\n"
            else:
                if not self.ignoreNonOverlap and not self.image_covers_template(sourceRaster):
                    input_file_errors += "\n  Some part of the template image falls outside of " + input_just_file
                    input_file_errors += "\n        template upper left  = (" + str(self.templateRaster.west) + ", " + str(self.templateRaster.north) + ")"
                    input_file_errors += "\n        template lower right = (" + str(self.templateRaster.east) + ", " + str(self.templateRaster.south) + ")"
                    input_file_errors += "\n        image    upper left  = (" + \
                        str(sourceRaster.west) + ", " + str(sourceRaster.north) + ")"
                    input_file_errors += "\n        image    lower right = (" + str(sourceRaster.east) + ", " + str(sourceRaster.south) + ")"

            if len(row) < 2 or not row[1] in ['0', '1']:
                self.writetolog("  " + os.path.split(input_file)[1] +
                       " categorical either missing or not 0 or 1:"
                       + "\n   Defaulting to 0 (continuous)")
                if len(row) < 2:
                    row.append('0')
                else:
                    row[1] = '0'

            resample_methods = [item.lower() for item in self.resample_methods]
            if len(row) < 3 or not row[2].lower() in resample_methods:
                self.writetolog("  " + os.path.split(input_file)[1] +
                            " resample method either missing or not one of " +
                            ", ".join(self.resample_methods) +
                            "\n  Defaulting to 'Bilinear'")

                if row[1] == '0':
                    default = 'Bilinear'
                else:
                    default = 'NearestNeighbor'
                if len(row) < 3:
                    row.append(default)
                else:
                    row[2] = default

            lower_agg_methods = [item.lower() for item in self.agg_methods]
            if len(row) < 4 or not row[3].lower() in lower_agg_methods:
                self.writetolog("  " + os.path.split(input_file)[1] +
                        " aggregation method either missing or not one of " +
                        ", ".join(self.agg_methods) +
                        "\n  Defaulting to 'Mean'")
                if row[1] == '0':
                    default = 'Mean'
                else:
                    default = 'Majority'
                if len(row) < 4:
                    row.append(default)
                else:
                    row[3] = default

            self.inputs.append(row)
            #  also write the output row, reconfigured to our output file
            short_name = SpatialUtilities.getRasterShortName(row[0])
            file_name = os.path.abspath(os.path.join(self.out_dir,
                                                    short_name + ".tif"))
            outputrow = [file_name] + row[1:4] + [os.path.abspath(row[0]),
                                                 os.path.abspath(self.out_dir)]
            if input_file not in existing_files:
                output.writerow(outputrow)
        del output

        if input_file_errors != "":
            self.writetolog(input_file_errors, False, False)
            raise utilities.TrappedError("There was one or more problems with "
                               + "your input rasters: \n" + input_file_errors)

if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))

