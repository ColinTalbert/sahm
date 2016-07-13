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
    our_PARC.logger = utilities.Logger(os.path.join(our_PARC.out_dir,
                                                    "logfile.txt"), True)
    our_PARC.parcFiles()

class PARC(object):
    """
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
    """

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
        """
            1: Parse the inputs_CSV into our inputs list
            2: Make sure all of our instance variables are good and proper
            3: Loop through the list of sourceImages and PARC each one.
            4: The outputs will be stored in the output directory
            5: Additionally an output CSV will be produced that lists the
            inputs, parameters used, and outputs
        """

        self.logger.write_to_log("Starting PARC", True, True)
        self.validateArgs()
        self.logger.write_to_log("    Arguments validated successfully",
                                 True, True)
        self.processFiles()

        self.logger.write_to_log("Finished PARC", True, True)

    def processFiles(self):
        self.process_pool = multiprocessing.Pool(multiprocessing.cpu_count() - 1)
        self.pool_processes = []

        header_row = ["PARCOutputFile", "Categorical", "Resampling",
                      "Aggregation", "OriginalFile",
                      os.path.abspath(self.template), os.path.abspath(self.out_dir)]
        cur_output = csv.writer(open(self.inputs_CSV, "wb"))
        cur_output.writerow(header_row)


        #  Clip and reproject each source image.
        for image in self.inputs:
            in_fname = SpatialUtilities.getRasterShortName(image[0])
            out_file = os.path.join(self.out_dir, in_fname + ".tif")

            raster_files = utilities.get_raster_files(image[0])
            raster_hash = utilities.hash_file(raster_files)

            process_queue = []
            if os.path.exists(out_file):
                try:
                    gdal.Open(out_file)

                    old_source = utilities.get_fname_from_hash_pickle(raster_hash, self.out_dir)
                    if old_source is None:  #  we've previously run a file with this output name from a different source
                        out_file = out_file.replace('.tif', "_" + raster_hash + '.tif')
                        msg = "The output " + in_fname + \
                            " already exists, but was derived from a different source."

                        msg += " renaming output to {}".format(out_file)
                        self.logger.write_to_log(msg, True, True)
                        process_queue.append(self.gen_singlePARC_thread(image, out_file))
                    else:
                        msg = "The output " + in_fname + \
                            " already exists. \tSkipping this file."
                        self.logger.write_to_log(msg, True, True)
                except:
                    #  we bombed trying to open the outFile with gdal. Lets rerun it.
                    process_queue.append(self.gen_singlePARC_thread(image, out_file))

            else:
                process_queue.append(self.gen_singlePARC_thread(image, out_file))


            #  also write the output row, reconfigured to our output file
            outputrow = [out_file] + image[1:4] + [os.path.abspath(image[0]),
                                                 os.path.abspath(self.out_dir), in_fname]
            cur_output.writerow(outputrow)

            out_finfo = {'source':image[0], 'result':out_file}
            utilities.write_hash_entry_pickle(raster_hash,
                                        out_finfo, self.out_dir)
        del cur_output

        #  wait for the last set of processes to finish up
        error_msgs = ''
        for process in self.pool_processes:
            msg = process.get()
            print msg[0]
            if msg[1] != '':
                error_msgs += ("\n" + msg[1])
        if error_msgs != '':
            raise utilities.TrappedError(error_msgs)

        print "done"

    def gen_singlePARC_thread(self, image, outFile):

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
        self.logger.write_to_log(command, False, False)
        return command_arr

    def log_result(self, result):
        print result

    def parcFile(self, source, dest):
        """
        Processes a single file
        """
        gdal.UseExceptions()

        short_name = os.path.split(os.path.splitext(source[0])[0])[1]
        self.logger.write_to_log("    Starting processing of " + source[0])
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
            self.logger.write_to_log("  cell ratio > .5: reprojecting and resampling to template parameters only")
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
                tmp_output_raster.close()
                os.remove(tmpOutput)
            except WindowsError:
                pass

    def get_gdal_type_from_string(self, gdal_gra_description):
        """Returns a gdal resampling type based on the string passed
        gdal_GRA_description must be one of: nearestneighbor, bilinear,
        cubic, cubicspline, or lanczos.
        Otherwise will default and return nearestneighbor
        Note gdal_GRA_description is not case sensitive
        """
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
            self.logger.write_to_log("   Specified resampling method ("
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
        tmpOutDataset.pixelType = sourceRaster.pixelType
        tmpOutDataset.NoData = sourceRaster.NoData
        tmpOutDataset.signedByte = sourceRaster.signedByte
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
                num_rows = bSize
            else:
                num_rows = rows - i

            for j in range(0, cols, bSize):
                if j + bSize < cols:
                    num_cols = bSize
                else:
                    num_cols = cols - j

                data = sourceRaster.getBlock(col=j, row=i,
                                             numCols=num_cols, numRows=num_rows)

                if method == None:
                    method = "Mean"
                if method in ["Mean", "Max", "Min", "STD"]:
                    ans = self.rebin(data, (num_rows / numSourcePerTarget, num_cols / numSourcePerTarget), method)
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
                data = source_raster.getBlock(col=xOffset, row=yOffset,
                                                        numCols=1, numRows=1)
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
            self.logger = utilities.Logger(self.out_dir, self.verbose)
        self.writetolog = self.logger.write_to_log

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

        current_inputs = np.genfromtxt(self.inputs_CSV, dtype='S1000',
                           delimiter=",", skip_header=True, invalid_raise=False)
        if len(current_inputs.shape) == 1:
            #  there was only a single item in the input file, reshape the array
            current_inputs = np.array([current_inputs])

        input_file_errors = ""


#          all_previous_output = os.path.join(self.out_dir, "PARC_Files.csv")
#          try:
#              previous_inputs = np.genfromtxt(all_previous_output, dtype='S1000', delimiter=",", skip_header=True)
#              if len(previous_inputs.shape) == 1:
#              #  pound there was only a single item in the input file, reshape the array
#                  previous_inputs = previous_inputs = np.array([previous_inputs])
#
#              previous_inputs = [SpatialUtilities.getRasterShortName(os.path.abspath(f))
#                                                           for f in previous_inputs[:, 4]]
#              prev_output = csv.writer(open(all_previous_output, "ab"))
#          except (IndexError, IOError, StopIteration):  #  IndexError=only header, IOError=File doesn't exist, StopIteration=Empty file
#              previous_inputs = []
#              prev_output = csv.writer(open(all_previous_output, "wb"))
#              prev_output.writerow(header_row)



        inputs = []
        for row in [list(r) for r in current_inputs]:
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


        if input_file_errors != "":
            self.writetolog(input_file_errors, False, False)
            raise utilities.TrappedError("There was one or more problems with "
                               + "your input rasters: \n" + input_file_errors)

if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))

