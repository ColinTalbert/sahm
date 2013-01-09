import time
import os
import sys
import csv
import itertools
import traceback
import subprocess
from optparse import OptionParser
import utilities
from osgeo import gdalconst
from osgeo import gdal
from osgeo import osr

class MAXENTRunner(object):

    def __init__(self):
        self.verbose = False
        self.maxentpath = ''
        self.mdsFile = ''
        self.projectionlayers = ''
        self.environmentallayers = ''
        self.testCSV = ''
        self.trainingCSV = ''
        self.backgroundCSV = ''
        self.outputDir = ''
        self.categoricals = []
        self.argsCSV = ''
        self.logger = None
        self.testKey = 'test'
        self.subRun = False



    def run(self):
        self.loadArgs()
        self.args['outputdirectory'] = self.outputDir
        if self.subRun:
            self.args['plots'] = 'false'
            self.args['responsecurves'] = 'false'
            self.args['outputgrids'] = 'false'
        self.validateInputs()
        if self.mdsFile != '':
            self.prepInputs()
        else:
            raise Exception, 'No MDS supplied.'
        if not self.args.has_key('projectionlayers'):
            self.args['projectionlayers'] = ''
        if self.trainingCSV != '':
            self.args['samplesfile'] = '"' + self.trainingCSV + '"'
        else:
            raise Exception, 'No Samples file supplied'
        if self.testCSV != '':
            self.args['testsamplesfile'] = '"' + self.testCSV + '"'
        if self.args['environmentallayers'] != '':
            self.args['environmentallayers'] = '"' + self.args['environmentallayers'] + '"'
            if self.args.has_key('biasfile'):
                self.args['biasfile'] = '"' + self.args['biasfile'] + '"'
        elif self.backgroundCSV != '':
            self.args['environmentallayers'] = '"' + self.backgroundCSV + '"'
            if self.args.has_key('biasfile'):
                self.writetolog('The supplied biasfile will be ignored since no environmentallayers were provided', True, True)
                self.args.pop('biasfile')
        self.args['autorun'] = 'true'
        if ' ' in self.args['species_name']:
            self.args['species_name'] = self.args['species_name'].replace(' ', '_')
        self.args['outputdirectory'] = '"' + self.args['outputdirectory'] + '"'
        self.args['projectionlayers'] = '"' + self.args['projectionlayers'] + '"'
        strargs = [ '='.join((str(k), str(v))) for (k, v,) in self.args.iteritems() if k != 'species_name' if k != 'mdsFile' ]
        for categorical in self.categoricals:
            strargs += ['togglelayertype=' + categorical.replace('_categorical', '')]

        if not self.maxentpath.endswith('.jar'):
            jar = '"' + os.path.join(self.maxentpath, 'maxent.jar') + '"'
        else:
            jar = '"' + self.maxentpath + '"'
        self.run_cmd_line_jar(jar, strargs)



    def run_cmd_line_jar(self, jar_name, args):
        cmd = ' '.join(['java',
         '-mx512m',
         '-jar',
         jar_name] + args)
        self.writetolog('    running:  ' + cmd, True, False)
        p = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        self.writetolog('    Finished running:  ', True, False)
        ret = p.communicate()
        self.writetolog('    Maxent strOut:  ' + str(ret[0]))
        if ret[1] is not None:
            msg = 'An error was encountered running the Maxent jar file.  The error message is below - \n'
            msg += ret[1]
            writetolog(msg)
            raise RuntimeError, msg
        del ret



    def loadArgs(self):
        argsReader = csv.reader(open(self.argsCSV, 'r'))
        header = argsReader.next()
        self.args = {}
        for row in argsReader:
            self.args[row[0]] = row[1]




    def validateInputs(self):
        if not os.path.exists(self.argsCSV):
            raise RuntimeError(self, 'Input argsFile, ' + self.argsCSV + ', could not be found on file system')
        if not os.path.exists(self.mdsFile):
            raise RuntimeError(self, 'Input MDS, ' + self.mdsFile + ', could not be found on file system')
        if not self.args.has_key('projectionlayers'):
            self.args['projectionlayers'] = ''
        if not self.args.has_key('environmentallayers'):
            self.args['environmentallayers'] = ''
        elif not os.path.isdir(self.args['environmentallayers']):
            raise RuntimeError(self, 'Input environmentallayers directory, ' + self.args['environmentallayers'] + ', could not be found on file system')
        if self.args['projectionlayers'] != '':
            dirs = self.args['projectionlayers'].split(',')
            for dir in dirs:
                if not os.path.isdir(dir):
                    raise RuntimeError(self, "Input 'projectionlayers' must be a directory")

        if not utilities.isMDSFile(self.mdsFile):
            raise RuntimeError(self, 'Input MDS, ' + self.mdsFile + ', does not appear to be formated as an MDS file.')
        if not os.path.exists(self.outputDir):
            raise RuntimeError(self, 'Output directory, ' + self.outputDir + ', could not be found on file system')
        if self.logger is None:
            self.logger = utilities.logger(outDir, self.verbose)
        self.writetolog = self.logger.writetolog



    def prepInputs(self):
        """parses out input MDS file into the 1 to 3 SWD files that Maxent requires.
        """
        self.testCSV = os.path.join(self.outputDir, 'testSamples.csv')
        self.trainingCSV = os.path.join(self.outputDir, 'trainingSamples.csv')
        self.backgroundCSV = os.path.join(self.outputDir, 'backgroundPoints.csv')
        testWriter = csv.writer(open(self.testCSV, 'wb'))
        trainingWriter = csv.writer(open(self.trainingCSV, 'wb'))
        backgroundWriter = csv.writer(open(self.backgroundCSV, 'wb'))
        MDSreader = csv.reader(open(self.mdsFile, 'r'))
        header1 = MDSreader.next()
        header2 = MDSreader.next()
        header3 = MDSreader.next()
        self.pullCategoricals(header1)
        splitcol = None
        evalsplit = None
        if 'EvalSplit' in header1:
            evalsplit = header1.index('EvalSplit')
        try:
            splitcol = header1.index('Split')
            deleteTest = False
        except ValueError:
            self.writetolog("    The supplied MDS does not have a 'Split' column defaulting to having Maxent apply test/training split.")
            deleteTest = True
        covariateIndexes = self.usedIndexes(header1, header2)
        covariateNames = self.usedValues(header1, covariateIndexes)
        covariateNamesClean = [ name.replace('_categorical', '') for name in covariateNames ]
        usedCovariateFiles = self.usedValues(header3, covariateIndexes)
        self.writetolog('    Used covariates:' + ', '.join(covariateNames), False, False)
        testWriter.writerow(['full_name', 'x', 'y'] + covariateNamesClean)
        trainingWriter.writerow(['full_name', 'x', 'y'] + covariateNamesClean)
        backgroundWriter.writerow(['full_name', 'x', 'y'] + covariateNamesClean)
        hasBackground = False
        absencePointCount = 0
        for row in MDSreader:
            self.convertNA(row)
            if not evalsplit and row[evalsplit] == 'test':
                pass
            elif row[2] == '0':
                absencePointCount += 1
            elif row[2] == '-9999' or row[2] == '-9998' and not row[splitcol] == self.testKey:
                hasBackground = True
                vals = self.usedValues(row, covariateIndexes)
                backgroundWriter.writerow([''] + row[:2] + vals)
            elif splitcol is None and str(row[2]) != '0':
                vals = self.usedValues(row, covariateIndexes)
                trainingWriter.writerow([self.args['species_name']] + row[:2] + vals)
            elif row[splitcol] == self.testKey and str(row[2]) != '0' or self.testCSV == '':
                vals = self.usedValues(row, covariateIndexes)
                testWriter.writerow([self.args['species_name']] + row[:2] + vals)
            elif str(row[2]) != '0':
                vals = self.usedValues(row, covariateIndexes)
                trainingWriter.writerow([self.args['species_name']] + row[:2] + vals)

        if not hasBackground and self.args['environmentallayers'] == '':
            msg = '    No environmental layers were provided and '
            msg += '\n no background points were detected in the input file.'
            msg += '\n    Maxent requires pregenerated background points or environmental layer'
            msg += '\nfrom which to extract values.'
            self.writetolog(msg)
            raise RuntimeError(msg)
        elif hasBackground and self.args['environmentallayers'] != '':
            msg = '    Both background points in the MDS file and a '
            msg += '\n folder of environmental layers were specified.'
            msg += '\n    Either of these could be used by Maxent to specify/create background points.\n\n'
            msg += 'Remove either the background points from the MDSBuilder or the environmental layer.'
            self.writetolog(msg)
            raise RuntimeError(msg)
        try:
            del testWriter
            if deleteTest:
                os.remove(self.testCSV)
                self.testCSV = ''
            del backgroundWriter
            if not hasBackground:
                os.remove(self.backgroundCSV)
                self.backgroundCSV = ''
            del trainingWriter
        except:
            print ' '.join([ str(i) for i in sys.exc_info()[:2] ])
        if not self.args['projectionlayers'] != '':
            if self.args['environmentallayers'] != '':
                pass
            else:
                self.args['outputgrids'] = 'false'
            return 



    def convertNA(self, vals):
        """Switches the NA value used in our R models
        to the value expected by Maxent
        """
        for (index, item,) in enumerate(vals):
            if item == 'NA':
                vals[index] = '-9999'




    def usedIndexes(self, header1, header2):
        covariateIndexes = []
        for i in range(len(header1)):
            if header2[i] == '1' and header1[i] not in ('Weights', 'Split', 'EvalSplit'):
                covariateIndexes.append(i)

        return covariateIndexes



    def usedValues(self, values, indexes):
        usedvals = []
        for i in indexes:
            usedvals.append(values[i])

        return usedvals



    def pullCategoricals(self, headerline):
        for item in headerline:
            if item.endswith('_categorical'):
                self.categoricals.append(item)




    def isSWD(self, file):
        """Checks the format of a file to see if it is in the 
        Maxent samples with data (SWD) format.
        """
        if os.path.exists(file):
            reader = csv.reader(open(file, 'r'))
            header = reader.next()
            if header[0].lower() in ('species', 'full_name', 'fullname'):
                return True
        return False




def main(argv):
    """Process our command line args and initiate a Maxent run
    """
    usageStmt = 'usage:  -m --MDSFile -a --argsCSV -o --outputDir'
    desc = 'Formats and prepares input for running the Maxent Jar in a SAHM workflow'
    parser = OptionParser(usage=usageStmt, description=desc)
    parser.add_option('-m', '--MDSFile', dest='MDSFile', help='The MDS file with our sample data.')
    parser.add_option('-a', '--argsCSV', dest='argsCSV', help="A CSV with each Maxent argument name and it's value on separate lines.")
    parser.add_option('-e', '--maxentExecutable', dest='maxentExecutable', help='The full path to the maxent executable jar file.')
    parser.add_option('-o', '--outputDir', dest='outputDir', help='The directory to save output files.')
    parser.add_option('-v', '--verbose', dest='verbose', default=False, action='store_true', help='the verbose flag causes diagnostic output to print')
    (options, args,) = parser.parse_args(argv)
    ourMaxent = MAXENTRunner()
    ourMaxent.verbose = options.verbose
    ourMaxent.maxentpath = options.maxentExecutable
    ourMaxent.mdsFile = options.mdsFile
    ourMaxent.outputDir = options.outputDir
    ou