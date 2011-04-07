#!/usr/bin/python

import glob
import math
import os
import shutil
import struct
import sys

from optparse import OptionParser

from osgeo import gdalconst
from osgeo import gdal
from osgeo import osr

from numpy import *
import numpy as np

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
	parser.add_option("-m", dest="aggMethod", default="Mean", help="the method of aggregation to apply: 'Mean', 'Max', 'Min'.  Defaults to 'Mean'")
	parser.add_option("-o", dest="outDir", default="./", help="directory in which to put processed images, defaults to current directory")
	parser.add_option("-r", dest="resampleMethod", default="NearestNeighbour", help="the method of resampling to apply: 'NearestNeighbour', 'Bilinear', 'Cubic', 'CubicSpline', or 'Lanczos'")
	parser.add_option("-v", dest="verbose", default=False, action="store_true", help="the verbose flag causes diagnostic output to print")
	parser.add_option("-t", dest="templateRaster", help="The template raster used for projection, origin, cell size and extent")
	
	(options, args) = parser.parse_args(args_in)
	
	ourPARC = PARC()
	ourPARC.verbose = options.verbose
	ourPARC.template = options.templateRaster
	ourPARC.resampleMethod = options.resampleMethod
	ourPARC.aggMethod = options.aggMethod
	ourPARC.outDir = options.outDir
	ourPARC.inputs = args
	
	ourPARC.parcFiles()

class PARC:
##############################################
# PARC:  Project, Aggregate, Resample, Clip
#  The workflow on this beast is as follows:
#		For each dataset
#		Step 1: RePrject the source raster into a tmp raster using
#			the projection info from the template and the method if 
#			supplied or the default of nearest if not.
#			At this stage the tmp output will have a cell size about
#			the same as the input.  We just use the default for this 
#			setting.
#		Step 2: Aggregate the tmpRaster to have the same origin, 
#			cell size and extent as our template.
##############################################
	def __init__(self):
		#instance level variables
		self.verbose = False
		self.template = ""
		self.templateParams ={}
		self.aggMethod = "Mean" 
		self.outDir = ""
		self.resampleMethod = "NearestNeighbour"
		self.inputs = []

	def parcFiles(self):
		"""
			1: Parse the input files
			2: Make sure all of our instance variables are good and proper
			3: Loop through the list of sourceImages and PARC each one.
			4: The outputs will be stored in the output directory
		"""
		
		self.InputFiles = self.getInputFiles()
		self.validateArgs()
		
		# Clip and reproject each source image.
		for image in self.inputs:
			# Ensure source is different from template.
			#if not os.path.samefile(template, image):
			inPath, inFileName = os.path.split(image)
			outFile, ext = os.path.splitext(inFileName) 
			outFile = os.path.join(self.outDir, outFile + ".asc")
			
			# os.path.samefile(image, outFile):
			if os.path.exists(outFile) and \
			   os.path.abspath(image) == os.path.abspath(outFile):

				baseName, extension = os.path.splitext(outFile)
				outFile = baseName + "-PARC.tif"
				
			if os.path.abspath(self.template) != os.path.abspath(image):
				self.parcFile(image, outFile)
			elif os.path.abspath(self.template) == os.path.abspath(image): 
				shutil.copyfile(self.template, outFile)


	def parcFile(self, source, dest):
		"""
		Processes a single file
		"""
		self.shortName = os.path.split(os.path.splitext(source)[0])[1]
		if self.verbose:
			print "Starting processing of " + source
		sourceParams = self.getRasterParams(source)
				
		gdalType = None
		if self.resampleMethod == "NearestNeighbour":
			gdalType = gdalconst.GRA_NearestNeighbour
		if self.resampleMethod == "Bilinear":
			gdalType = gdalconst.GRA_Bilinear
		if self.resampleMethod == "Cubic":
			gdalType = gdalconst.GRA_Cubic
		if self.resampleMethod == "CubicSpline":
			gdalType = gdalconst.GRA_CubicSpline
		if self.resampleMethod == "Lanczos":
			gdalType = gdalconst.GRA_Lanczos
		if gdalType == None:
			print "Specified resampling method (" + self.resampleMethod + ") not one of 'NearestNeighbour', 'Bilinear', 'Cubic', 'CubicSpline', or 'Lanczos'.  Defaulting to 'NearestNeighbor'"
			gdalType = gdalconst.GRA_NearestNeighbour
		
		#Open dgal dataset of the source to pull some values from
		srcDs = gdal.Open(source)
		
		cellRatio = self.getTemplateSRSCellSize(sourceParams)/self.templateParams["xScale"]
		if self.verbose:
			print "  ratio of source cell size to template cell size = " + str(cellRatio)
			print "    template cell size = " + str(self.templateParams["xScale"])
			print "    " + self.shortName + " cell size = " + str(self.getTemplateSRSCellSize(sourceParams))
			
		if cellRatio > .5:
			#The source cell size is close enough to our template cell size,
			#or smaller so
			#that all we need to do is reproject and resample.
			if self.verbose:
				print "  cell ratio > .5: reprojecting and resampling to template parameters only"
			self.reprojectRaster(srcDs, sourceParams, self.templateParams, dest, 
								gdalType, self.templateParams["xScale"])
		else:
			#Our Target cell size is much bigger than our source we need to do 
			#some aggregation to make things work.
			if self.verbose:
				print "  cell ratio <= .5: reprojecting and resampling to template parameters"
				print "    then aggregating the reprojected raster to match template parameters"
				
			targetCellSize, numSourcePerTarget = self.getAggregateTargetCellSize(sourceParams)
			tmpOutput = os.path.join(os.path.dirname(dest), "tmp_" + os.path.basename(dest))
			
			self.reprojectRaster(srcDs, sourceParams, self.templateParams,
								tmpOutput, gdalType,  targetCellSize)
			if self.verbose:
				print "   Stating on Aggregating: " + self.shortName
				
			tmpOutput2 = os.path.splitext(tmpOutput)[0] + ".tif"
			self.Aggregate(tmpOutput2, dest, 
						sourceParams, self.templateParams,
						self.aggMethod, numSourcePerTarget)
			
			os.remove(tmpOutput2)
			if self.verbose:
				print "   Finished with Aggregating: " + self.shortName
			
	def getTemplateSRSCellSize(self, sourceParams):
		"""
		Calculate what size our source image pixels would be in the template SRS
		"""
		#first convert our template origin into the source srs
		tOriginX, tOriginY = self.transformPoint(self.templateParams["west"], self.templateParams["north"], 
										self.templateParams["srs"], sourceParams["srs"])
		#next add the source xScale to the converted origin x and convert that back to template srs
		tOriginX1 = self.transformPoint (tOriginX + sourceParams["xScale"], tOriginY, 
												sourceParams["srs"], self.templateParams["srs"])[0]						
		
		
#		templateCellXCorner1 = (self.templateParams["west"], self.templateParams["north"], 
#										self.templateParams["srs"], sourceParams["srs"])[0]
#		
#		targetCellXCorner1 = (sourceParams["west"], sourceParams["north"], 
#												sourceParams["srs"], self.templateParams["srs"])[0]
#		targetCellXCorner2 = self.transformPoint(sourceParams["west"] + sourceParams["xScale"], 
#												sourceParams["north"], sourceParams["srs"], self.templateParams["srs"])[0]
		templateSRSCellSize = abs(abs(tOriginX1) - abs(self.templateParams["west"]))
		return templateSRSCellSize

	def getAggregateTargetCellSize(self, sourceParams):
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
		#first determine what cell size we are going to use for the initial reproject/resample 
		#step 1:  Determine the native cell size in the template coordinate system.
		templateSRSCellSize = self.getTemplateSRSCellSize(sourceParams)
		#step 2:  round this up or down to an even fraction of the template cell size
		# for example source = 30, target = 250 resampledSource = 250/round(250/30)
		sourcePixelsPerTarget = round(self.templateParams["xScale"]/templateSRSCellSize)
		nearestWholeCellSize = (self.templateParams["xScale"] / 
							sourcePixelsPerTarget)
		return nearestWholeCellSize, sourcePixelsPerTarget
		
		
	def Aggregate(self, inFile, outFile, sourceParams, templateParams, method, numSourcePerTarget):
		sourceDs = gdal.Open(inFile, gdalconst.GA_ReadOnly)
		sourceBand  = sourceDs.GetRasterBand(1)
		
		tmpOutput = os.path.splitext(outFile)[0] + ".tif"
		tmpOutDataset = self.generateOutputDS(sourceParams, templateParams, tmpOutput)
		outBand = tmpOutDataset.GetRasterBand(1)
		
		rows = int(sourceParams["height"])
		cols = int(sourceParams["width"])

		row = 0
		col = 0
		
		
		pcntDone = 0
		while row < templateParams["width"]:
			while col < templateParams["height"]:
				sourceRow = row * numSourcePerTarget
				sourceCol = col * numSourcePerTarget

				#kernel = self.getKernel(sourceRow, sourceCol, numSourcePerTarget, sourceDs)
				kernel = sourceDs.GetRasterBand(1).ReadAsArray(int(sourceRow), 
													int(sourceCol), 
													int(numSourcePerTarget),
													int(numSourcePerTarget))
				#convert kenel values of our nodata to nan
				ndMask = ma.masked_array(kernel, mask=(kernel==sourceParams["NoData"]))
				#print kernel
				if self.aggMethod == "Min":
					ans = ndMask.min()
				elif self.aggMethod == "Max":
					ans = ndMask.max()
				elif self.aggMethod == "Majority":
					uniques = np.unique(ndMask)
					histogram = np.histogram(ndMask, uniques)
					ans = histogram[1][histogram[0].argmax()]
				else:
					ans = ndMask.mean()
				
#				print ndMask
#				print ans
				#special case real ugly
				if ans < 0 and sourceParams["signedByte"]:
					ans = ans + 255
				
				ansArray = empty([1, 1])
				if type(ans) == ma.core.MaskedArray:
					ansArray[0, 0] = sourceParams["NoData"]
				else:
					ansArray[0, 0] = ans

				outBand.WriteArray(ansArray, row, col)
				
				col += 1
				
			row += 1
			col  = 0
			if float(row)/templateParams["width"] > float(pcntDone)/100:
				pcntDone += 10
				if self.verbose:
					print str(pcntDone) + "...",
		if self.verbose:
			print "Done"
#		if self.verbose:
#			print "Done\nSaving to ASCII format"
#							
#		driver = gdal.GetDriverByName("AAIGrid")
#		driver.Register()
#		
#		dst_ds = driver.CreateCopy(outFile, tmpOutDataset, 0)
#		if self.verbose:
#			print "    Finished Saving ", self.shortName
		
		dst_ds = None
		tmpOutDataset=None
		
	

		

	def getRasterParams(self, rasterFile):
		"""
		Extracts a series of bits of information from a passed raster
		All values are stored in a dictionary which is returned.
		If errors are encountered along the way the error messages will
		be returned as a list in the Error element.
		"""
		try:
			#initialize our params dictionary to have None for all parma
			params = {}
			allRasterParams = ["Error", "xScale", "yScale", "width", "height",
							"east", "north", "west", "south",  
							"tEast", "tNorth", "tWest", "tSouth",
							"gEast", "gNorth", "gWest", "gSouth",  
							"Wkt", "srs", "gt", "prj", "NoData", "PixelType"]
			
			for param in allRasterParams:
				params[param] = None
			params["Error"] = []
			
			# Get the PARC parameters from the rasterFile.
			dataset = gdal.Open(rasterFile, gdalconst.GA_ReadOnly)
			if dataset is None:
				params["Error"].append("Unable to open file")
				return params
				
				#print "Unable to open " + rasterFile
				#raise Exception, "Unable to open specifed file " + rasterFile
				
			
			xform  = dataset.GetGeoTransform()
			params["xScale"] = xform[1]
			params["yScale"] = xform[5]
	
			params["width"]  = dataset.RasterXSize
			params["height"] = dataset.RasterYSize
	
			params["west"] = xform[0]
			params["north"] = xform[3]
			params["east"] = params["west"] + params["width"]  * params["xScale"]
			params["south"] = params["north"] + params["height"] * params["yScale"]
	
			try:
				wkt = dataset.GetProjection()
				params["gt"] = dataset.GetGeoTransform()
				params["prj"] = dataset.GetProjectionRef()
				params["srs"] = osr.SpatialReference(wkt)
				if wkt == '':
					params["Error"].append("Undefined projection")
				else:
					
					if rasterFile == self.template:
						params["tWest"], params["tNorth"] = params["west"], params["north"]
						params["tEast"], params["tSouth"] = params["east"], params["south"]
					elif params["srs"].ExportToWkt() == self.templateParams["srs"].ExportToWkt():
						params["tWest"], params["tNorth"] = params["west"], params["north"]
						params["tEast"], params["tSouth"] = params["east"], params["south"]
					else:
						try:
							params["tWest"], params["tNorth"] = self.transformPoint(params["west"], params["north"], params["srs"], self.templateParams["srs"])
							params["tEast"], params["tSouth"] = self.transformPoint(params["east"], params["south"], params["srs"], self.templateParams["srs"])
						except:
							params["Error"].append("Could not transform extent coordinates to template spatial reference")
							#params["Error"] = "We ran into problems converting projected coordinates to template for " +  rasterFile
					try:
						geographic = osr.SpatialReference()
						geographic.ImportFromEPSG(4326)
						params["gWest"], params["gNorth"] = self.transformPoint(params["west"], params["north"], params["srs"], geographic)
						params["gEast"], params["gSouth"] = self.transformPoint(params["east"], params["south"], params["srs"], geographic)
					except:
						pass
					
			except:
				#print "We ran into problems getting the projection information for " +  rasterFile
				params["Error"].append("Undefined problems extracting the projection information")
				
			try:
				params["signedByte"] = dataset.GetRasterBand(1).GetMetadata('IMAGE_STRUCTURE')['PIXELTYPE'] == 'SIGNEDBYTE'
			except KeyError:
				params["signedByte"] = False
			
			params["NoData"] = dataset.GetRasterBand(1).GetNoDataValue()
			if params["NoData"] == None:
				driver = dataset.GetDriver()
				if dataset.GetRasterBand(1).DataType == 1:
					print "Warning:  Could not extract NoData value.  Using assumed nodata value of 255"
					params["NoData"] = 255
				if dataset.GetRasterBand(1).DataType == 2:
					print "Warning:  Could not extract NoData value.  Using assumed nodata value of 65536"
					params["NoData"] = 65536
				if dataset.GetRasterBand(1).DataType == 3:
					print "Warning:  Could not extract NoData value.  Using assumed nodata value of 32767"
					params["NoData"] = 32767
				if dataset.GetRasterBand(1).DataType == 4:
					print "Warning:  Could not extract NoData value.  Using assumed nodata value of 2147483647"
					params["NoData"] = 2147483647
				if dataset.GetRasterBand(1).DataType == 5:
					print "Warning:  Could not extract NoData value.  Using assumed nodata value of 2147483647"
					params["NoData"] = 2147483647
				if dataset.GetRasterBand(1).DataType == 6:
					print "Warning:  Could not extract NoData value.  Using assumed nodata value of -3.40282346639e+038"
					params["NoData"] = -3.40282346639e+038
				else:
					params["Error"].append("Could not identify nodata value")
			params["PixelType"] = dataset.GetRasterBand(1).DataType
			if params["PixelType"] == None:
				params["Error"].append("Could not identify pixel type (bit depth)")
			
		except:
			#print "We ran into problems extracting raster parameters from " + rasterFile
			params["Error"].append("Some untrapped error was encountered")
		finally:
			del dataset
			return params

	def transformPoint(self, x, y, from_srs, to_srs):
		"""
		Transforms a point from one srs to another
		"""
		coordXform = osr.CoordinateTransformation(from_srs, to_srs)
		yRound = round(y, 4)
		xRound = round(x, 4)

		result = coordXform.TransformPoint(xRound, yRound)
		
		gx = result[0]
		gy = result[1]

		return gx, gy
		
	def TemplateCoversImage(self, sourceParams):
		"""
		Checks to see if the templatate images 
		falls completely inside the source raster
		"""
		inside = False
		if (sourceParams["tWest"] <= self.templateParams["tWest"] and 
			sourceParams["tNorth"] >= self.templateParams["tNorth"] and 									
		   	sourceParams["tEast"] >=  self.templateParams["tEast"] and 
		   	sourceParams["tSouth"] <=  self.templateParams["tSouth"]):
			inside = True

		if (sourceParams["gWest"] <= self.templateParams["gWest"] and 
			sourceParams["gNorth"] >= self.templateParams["gNorth"] and 									
		   	sourceParams["gEast"] >=  self.templateParams["gEast"] and 
		   	sourceParams["gSouth"] <=  self.templateParams["gSouth"]):
			inside = True


		return inside

	##############################################
	# validateArgs
	##############################################
	
	def validateArgs(self):
		"""
		Make sure the user sent us some stuff we can work with
		"""

		# Validate template image.
		if self.template is None:
			raise Exception, "template raster not provided (-t command line argument missing)"
		
		if not os.path.exists(self.template):
			raise Exception, "Template file, " + self.template + ", does not exist on file system"

		self.templateParams = self.getRasterParams(self.template)
		if len(self.templateParams["Error"]) <> 0:
			raise Exception, ("There was a problem with the provided template: \n    " + 
									"    " + "\n    ".join(self.templateParams["Error"]))
		
		# Ensure the template has square pixels.
		if abs(abs(self.templateParams["xScale"]) - abs(self.templateParams["yScale"])) > 1e-6:
			raise Exception, ("template image must have square pixels." + 
							"/n    x pixel scale = " + str(xScale) +
							"/n    y pixel scale = " + str(yScale))

		
		#Validate input rasters
		if len(self.inputs) < 1:
			raise Exception, "Directory of input rasters or list of input rasters not provided"

		strInputFileErrors = ""
		for inputFile in self.InputFiles:
			# Verify the source image falls within the template.
			sourceParams = self.getRasterParams(inputFile)

			if len(sourceParams["Error"]) > 0:
				strInputFileErrors += ("  " + os.path.split(inputFile)[1] + " had the following errors:\n" + 
									"    " + "\n    ".join(sourceParams["Error"])) + "\n"
			else:
				if not self.TemplateCoversImage(sourceParams):
					strInputFileErrors += ("\n  Some part of the template image falls outside of " + os.path.split(inputFile)[1])
					strInputFileErrors += "\n        template upper left  = (" + str(self.templateParams["west"]) + ", " + str(self.templateParams["north"]) + ")"
					strInputFileErrors += "\n        template lower right = (" + str(self.templateParams["east"]) + ", " + str(self.templateParams["south"]) + ")"
					strInputFileErrors += "\n        image    upper left  = (" + str(sourceParams["west"]) + ", " + str(sourceParams["north"]) + ")"
					strInputFileErrors += "\n        image    lower right = (" + str(sourceParams["east"]) + ", " + str(sourceParams["south"]) + ")"
					strInputFileErrors += "\n        points are given in projected coordinates."
					strInputFileErrors += "\n        template upper left  = (" + str(self.templateParams["tWest"]) + ", " + str(self.templateParams["tNorth"]) + ")"
					strInputFileErrors += "\n        template lower right = (" + str(self.templateParams["tEast"]) + ", " + str(self.templateParams["tSouth"]) + ")"
					strInputFileErrors += "\n        image    upper left  = (" + str(sourceParams["tWest"]) + ", " + str(sourceParams["tNorth"]) + ")"
					strInputFileErrors += "\n        image    lower right = (" + str(sourceParams["tEast"]) + ", " + str(sourceParams["tSouth"]) + ")"
					strInputFileErrors += "\n        Note: points are given in the template coordinates." + "\n"
				
		if strInputFileErrors <> "":
			raise Exception, "There was one or more problems with your input raters: \n" + strInputFileErrors

		# Validate output directory.
		if not os.path.exists(self.outDir):
			raise Exception, "Specified Output directory " + self.outDir + " not found on file system"

		if not os.path.isdir(self.outDir):
			raise Exception, "Specified Output directory " + self.outDir + " is not a directory"

	def getInputFiles(self):
		""" Parses the arguments to generate a list
		of covariate files that we will be processing 
		against the template
		"""
		sourceImages = None

		# Validate input directory or files.
		if (os.path.isdir(self.inputs[0]) and
			not os.path.exists(os.path.join(self.inputs[0], "hdr.adf"))):
			if self.verbose == True:  print "Input directory found..."

			if not os.path.exists(self.inputs[0]):
				raise Exception, "Input directory, " + self.inputs[0] + ", does not exist."

			sourceImages = glob.glob(self.inputs[0] + "/*.tif")
			sourceImages += glob.glob(self.inputs[0] + "/*.img")
			sourceImages += glob.glob(self.inputs[0] + "/*.asc")
			hdrs = glob.glob(self.inputs[0] + "/*/hdr.adf")
			for hdr in hdrs:
				folder,hdr = os.path.split(hdr)
				sourceImages.append(folder)

			if len(sourceImages) == 0:
				raise Exception, "Input directory (" + self.inputs[0] + ") did not contain any rasters in one of the recognized formats (tif, img, asc, grid)"

		else:
			sourceImages = self.inputs
			#if self.verbose == True:  print "source images: " + str(sourceImages)

		return sourceImages

	def reprojectRaster(self, srcDs, sourceParams, templateParams, 
					destFile, resamplingType, outputCellSize = None):
		"""
		Reprojects a raster to match the templateParams
		if outputCellSize is not provided defaults to the template cellSize
		"""
#		driver = gdal.GetDriverByName("AAIGrid")
#		driver.Register()
		
		tmpOutput = os.path.splitext(destFile)[0] + ".tif"
		
		tmpOutDataset = self.generateOutputDS(sourceParams, templateParams, tmpOutput, outputCellSize)

		err = gdal.ReprojectImage(srcDs, tmpOutDataset, sourceParams["srs"].ExportToWkt(), 
								templateParams["srs"].ExportToWkt(), resamplingType)
		if self.verbose:
			print "    Saving ", self.shortName
#		dst_ds = driver.CreateCopy(destFile, tmpOutDataset, 0)
		if self.verbose:
			print "    Finished Saving ", self.shortName
		dst_ds = None
		tmpOutDataset = None
		
	def generateOutputDS(self, sourceParams, templateParams, 
						tmpOutput, outputCellSize = None):
		"""
		Creates an output dataset (tiff format) that
		  has the nodata value of the sourceParams but
		  all other attributes from the templateParams
		This output is saved to tmpOutput.
		
		The optional cell size will override the cell size 
			specified in the templateParams
		"""
		tifDriver = gdal.GetDriverByName("GTiff")
		
		if outputCellSize == None:
			width = templateParams["width"]
			height = templateParams["height"]
		else:
			width = templateParams["width"] * int(templateParams["xScale"]/outputCellSize)
			height = templateParams["height"] * int(templateParams["xScale"]/outputCellSize)
		
		if sourceParams["signedByte"]: 
			tmpOutDataset = tifDriver.Create(tmpOutput, 
											width,
											height,
											1, sourceParams["PixelType"], ["PIXELTYPE=SIGNEDBYTE"])
		else:
			tmpOutDataset = tifDriver.Create(tmpOutput, 
											width,
											height,
											1, sourceParams["PixelType"])
		
			
		if outputCellSize == None:
			outputCellSize = templateParams["xScale"]
		gtList = list(templateParams["gt"])
		if templateParams["xScale"] < 0:
			gtList[1] = -1 * outputCellSize
		else:
			gtList[1] = outputCellSize
		if templateParams["yScale"] < 0:
			gtList[5] = -1 * outputCellSize
		else:
			gtList[5] = outputCellSize
		gt = tuple(gtList)
		
		tmpOutDataset.SetGeoTransform(gt)
		tmpOutDataset.SetProjection(templateParams["prj"])
		tmpOutDataset.GetRasterBand(1).SetNoDataValue(sourceParams["NoData"])
		if sourceParams["signedByte"]:
			#tmpOutDataset.GetRasterBand(1).SetMetadataItem('PIXELTYPE', "SIGNEDBYTE")
			tmpOutDataset.GetRasterBand(1).PixelType = "SIGNEDBYTE"
			tmpOutDataset.GetRasterBand(1).SetMetadata({'PIXELTYPE': 'SIGNEDBYTE'}, 'IMAGE_STRUCTURE')
			
		if self.verbose:
			print tmpOutput
			print "noDataValue = ", tmpOutDataset.GetRasterBand(1).GetNoDataValue()
			print "Pixel type = ", gdal.GetDataTypeName(tmpOutDataset.GetRasterBand(1).DataType)
		return tmpOutDataset

if __name__ == "__main__":
	sys.exit(main(sys.argv[1:]))
#	try:
##		PARC().testing()
#		sys.exit(PARC().main(sys.argv[1:]))
#	except Exception as e:
#		print e
#		sys.exit(1)
