#!/usr/bin/env python
###############################################################################
##
## Copyright (C) 2006-2011, University of Utah. 
## All rights reserved.
## Contact: contact@vistrails.org
##
## This file is part of VisTrails.
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
###############################################################################

"""Runs all tests available in VisTrails modules by importing all of
them, stealing the classes that look like unit tests, and running
all of them.

runtestsuite.py also reports all VisTrails modules that don't export
any unit tests, as a crude measure of code coverage.

"""

import os
import fnmatch
import sys
import unittest
import os.path
import optparse
import shutil
import filecmp
import re
import csv

from cStringIO import StringIO

class SahmTestSuiteRunner(object):
    
    def __init__(self):
        self.verbose = 2
        self.vistrailsRoot = ''
        self.SAHMRoot = ''
        self.TempSahmDir = ''
        self.testVTs = []
        self.tests = {}
        
    def run(self):
        self.setInitializePaths()
        
        vtFiles = self.findVtTestFiles()
        
        import core.db.locator
        testsDir = os.path.join(self.test_root, "tests")
        for vtFile in vtFiles:
            vtdir, vtfname = os.path.split(vtFile)
            vtjustname = os.path.splitext(vtfname)[0]
            expectedOutput = os.path.join(vtdir, "expectedOutputs")
            locator = core.db.locator.FileLocator(vtFile)
            (v, abstractions, thumbnails, mashups) = core.db.io.load_vistrail(locator)
            w_list = []
            for version,tag in v.get_tagMap().iteritems():
                #If the tag has "NOT A TEST" in the notes we're ignoring it.
                notes = v.get_notes(version)
                if notes is None or \
                    v.get_notes(version).find("NOT A TEST") == -1:
                    expectedOutputForTest = os.path.join(expectedOutput, tag)
                    self.runTest(locator, version, tag, expectedOutputForTest)
        
        self.printResults()
        
    def setInitializePaths(self):
        # Makes sure we can import modules as if we were running VisTrails
        # from the root directory
        self.test_root = os.path.split(__file__)[0]
        self.SAHMRoot = os.path.split(self.test_root)[0]
        self.vistrailsRoot = os.path.abspath(os.path.join(self.test_root, '..', '..', '..'))
        
        sys.path.append(self.vistrailsRoot)
#        packagesDir = os.path.join(self.vistrailsRoot, 'packages')
#        sys.path.append(packagesDir)
#        ssPackageDir = os.path.join(packagesDir, 'spreadsheet')
#        sys.path.append(ssPackageDir)
        
        # creates the app so that testing can happen
        import gui.application
        
        # We need the windows so we can test events, etc.
        gui.application.start_application({'interactiveMode': False,
                                           'nologger': True})
        
        self.initializeSAHMVars()
        
        #some imports that require the above path
        import tests
        import packages.sahm.utils as utils
        import packages.sahm.init as sahm
        self.sahm = sahm
        import core.console_mode as console_mode
        self.console_mode = console_mode
        from core.db.locator import FileLocator
        
        self.tempSahmDir = os.path.join(self.test_root, "tempWorkspace")
        self.setTempSahmDir(self.tempSahmDir)
        
    def initializeSAHMVars(self):
        centralinstallroot = os.path.abspath(os.path.join(self.vistrailsRoot, '..', '..'))
        #Proj, GDAL, and GDAL data
        currentPath = os.environ['Path']
        gdal_path = os.path.join(centralinstallroot, 'Central_GDAL', 'GDAL')
        gdalpluggins_path = os.path.join(centralinstallroot, 'Central_GDAL', 'GDAL', 'gdalplugins')
        os.environ['Path'] = ";".join([gdal_path, gdalpluggins_path, currentPath])
        
        
    
        gdal_data = os.path.join(centralinstallroot, 'Central_GDAL', 'GDAL', "gdal-data")
        os.putenv("GDAL_DATA", gdal_data)
    
        proj_lib = os.path.join(centralinstallroot, 'Central_GDAL', 'GDAL', 'projlib')
        os.putenv("PROJ_LIB", proj_lib)
        
    def findVtTestFiles(self):

        testsDir = os.path.join(self.test_root, "tests")
        return self.filesInDir(testsDir, "*.vt")
        
    def filesInDir(self, directory, filter= "*.*"):
        files = []
        for root, dirnames, filenames in os.walk(directory):
            for filename in fnmatch.filter(filenames, filter):
                files.append(os.path.join(root, filename))
        return files
        
    def runTest(self, locator, version, name, expectedOutput):
        vt = os.path.splitext(os.path.split(locator.name)[1])[0]
        testname = vt + "_" + name
        self.tests[testname] = {"locator":locator, 
                        "version":version, 
                        "name":name}
        
        print "running test:  " + testname
        
        
        #make temporary outputworkspace and change our sahm workspace here
        runWorkspace = os.path.join(self.tempSahmDir, name)
        if os.path.exists(runWorkspace):
            shutil.rmtree(runWorkspace)
        os.mkdir(runWorkspace)
        
        self.setTempSahmDir(runWorkspace)
        
#        original_stdout = sys.stdout
#        sys.stdout = stdout = StringIO()
        errs = self.console_mode.run([[locator, version]], update_vistrail=False)
#        sys.stdout = original_stdout
        
        
        if errs:
            print "    test failed\n    errors:\n" + errs
            self.tests[testname]["Result"] = "Failed"
            self.tests[testname]["errs"] = errs
        else:
            #No errors check if output is different from expectation.
            onlyInLeft, onlyInRight, diffCommon = self.dirCompare(runWorkspace, expectedOutput, ignoreFiles)
            if onlyInLeft and onlyInRight and diffCommon:
                print "    test failed\n    output did not match previous output."
                diffreport = 'Found differences:\n'
                for item in onlyInRight:
                    diffreport += "\n  Missing output: " + item   
                for item in onlyInLeft:
                    diffreport += "\n  Unexpected output: " + item
                for item in diffCommon:
                    diffreport += "\n  different output file: " + item                   
                print "    Output differs from expected:\n\n" + diffreport
                self.tests[testname]["Result"] = "Failed"
                self.tests[testname]["errs"] = errs
            else:
                self.tests[testname]["Result"] = "Passed"
                self.tests[testname]["errs"] = ''

        self.setTempSahmDir(self.tempSahmDir)
        shutil.rmtree(runWorkspace)
        
    def runUnitTests(self):
        for (p, subdirs, files) in os.walk(self.test_root):
            # skip subversion subdirectories
            if p.find('.svn') != -1:
                continue
            for filename in files:
                # skip files that don't look like VisTrails python modules
                if not filename.endswith('.py'):
                    continue
        #        module = p[5:] + '/' + filename[:-3]
                module = p[len(self.test_root)+1:] + os.sep + filename[:-3]
                if (module.startswith('tests') or
                    module.startswith(os.sep) or
                    module.startswith('\\') or
                    ('#' in module)):
                    continue
                if ('system' in module and not
                    module.endswith('__init__')):
                    continue
                if test_modules and not module in test_modules:
                    continue
                msg = ("%s %s |" % (" " * (40 - len(module)), module))
        
                # use qualified import names with periods instead of
                # slashes to avoid duplicates in sys.modules
                module = module.replace('/','.')
                module = module.replace('\\','.')
                if module.endswith('__init__'):
                    module = module[:-9]
                try:
                    if '.' in module:
                        m = __import__(module, globals(), locals(), ['foo'])
                    else:
                        m = __import__(module)
                except tests.NotModule:
                    if verbose >= 1:
                        print "Skipping %s, not an importable module" % filename
                except:
                    print msg, "ERROR: Could not import module!"
                    continue
        
                test_cases = get_test_cases(m)
                for test_case in test_cases:
                    suite = unittest.TestLoader().loadTestsFromTestCase(test_case)
                    main_test_suite.addTests(suite)
        
                if not test_cases and verbose >= 1:
                    print msg, "WARNING: %s has no tests!" % filename
                elif verbose >= 2:
                    print msg, "Ok: %s test cases." % len(test_cases)

    def printResults(self):
        print "\n" * 10
        for test, results in self.tests.iteritems():
            print "*" * 80
            print "      test:  " + test
            vtfile = results["locator"].name
            vtfile = os.path.split(vtfile)[1]
            print "   vt file:  " + vtfile
            print "      node:  " + results["name"]
            print "\n   Result:  " + results["Result"]
            if results["Result"] == 'Failed':
                print "   Errors:  " + str(results["errs"])
    
        print "*" * 80
        print "*" * 80 
        print "*" * 80 
        
        print "Number of VT workflow tests run:  " + str(len(self.tests))
        print "-"* 80
        results = [i["Result"] for i in self.tests.itervalues()]
        print "Passed:  " + str(results.count('Passed'))
        print "Failed:  " + str(results.count('Failed'))

    def setTempSahmDir(self, dirname):
        self.sahm.session_dir = dirname
        self.sahm.utils.setrootdir(dirname)
        self.sahm.utils.createLogger(self.sahm.session_dir, self.sahm.configuration.output_dir)
        
        self.sahm.configuration.cur_session_folder = dirname

################################################################################
## Testing Examples
#
#EXAMPLES_PATH = os.path.join(_this_dir, '..', '..', 'examples')
##dictionary of examples that will be run with the workflows that will be ignored
#VT_EXAMPLES = { 'EMBOSS_webservices.vt': [],
#                'KEGGPathway.vt': [],
#                'KEGG_SearchEntities_webservice.vt': [],
#                'KEGG_webservices.vt': [],
#                'brain_vistrail.vt': [],
#                'chebi_webservice.vt': [],
#                'head.vt': [],
#                'infovis.vt': [],
#                'noaa_webservices.vt': [],
#                'offscreen.vt': [],
#                'plot.vt': [],
#                'spx.vt': [],
#                'structure_or_id_webservice.vt': [],
#                'terminator.vt': ["Isosurface Script"],
#                'triangle_area.vt': [],
#                'vtk.vt': [],
#                'vtk_book_3rd_p189.vt': ["quadric", "SmapleFunction",
#                                         "Almost there"],
#                'vtk_book_3rd_p193.vt': ["modules", "connections",
#                                         "lookup table"],
#                'vtk_http.vt': [],
#    }


###############################################################################
# Utility

    def dirCompare(self, leftDir, rightDir, ignore=[]):
        leftFiles = set([f.replace(leftDir, "")[1:] for f in self.filesInDir(leftDir) if ignore.count(os.path.split(f)[1]) == 0])
        rightFiles = set([f.replace(rightDir, "")[1:] for f in self.filesInDir(rightDir) if ignore.count(os.path.split(f)[1]) == 0])
        
        diffString = ''
        onlyInLeft = list(leftFiles - rightFiles)
        onlyInRight = list(rightFiles - leftFiles)
        common = list(leftFiles & rightFiles)
        
        diffCommon = []
        for item in common:
            leftfname = os.path.join(leftDir, item)
            rightfname = os.path.join(rightDir, item)
            same = None
            for k, v in specialFileComparisons.iteritems():
                if re.search(k, leftfname, re.I):
                    same = v(leftfname, rightfname)
            
            if same == None:
                same = filecmp.cmp(leftfname, rightfname, False)
                
            if not same:
                diffCommon.append(item)
        
        return onlyInLeft, onlyInRight, diffCommon
        

def sub_print(s, overline=False):
    """Prints line with underline (and optionally overline) ASCII dashes."""
    if overline:
        print "-" * len(s)
    print s
    print "-" * len(s)

def get_test_cases(module):
    """Return all test cases from the module. Test cases are classes derived
    from unittest.TestCase"""
    result = []
    import inspect
    for member_name in dir(module):
        member = getattr(module, member_name)
        if inspect.isclass(member) and issubclass(member, unittest.TestCase):
            result.append(member)
    return result


def compareMDS(mds1, mds2):
    filemds1 = csv.reader(open(mds1, "r"))
    filemds2 = csv.reader(open(mds2, "r"))
    
    if filemds1.next() != filemds2.next():
        return False
    if filemds1.next()[2:] != filemds2.next()[2:]:
        return False
    if filemds1.next()!= filemds2.next():
        pass
        #The third line will always be different for files in different session folders.
    
    for line in filemds1:
        if line != filemds2.next():
            return False
        
    return True
        
#these are files that require a special means of comparing for equality to
#account for differences in paths in the file.  The key is a regex expression 
specialFileComparisons = {".*MergedDataset_[0-9]*.csv":compareMDS}
#these are files that we will not compare for output equality
ignoreFiles = ["PARC_Files.csv", "sessionLog.txt","Thumbs.db"]


def main(argv):
    from optparse import OptionParser
    usage = "Usage: %prog [options] [module1 module2 ...]"
    parser = OptionParser(usage=usage)
    parser.add_option("-V", "--verbose", action="store", type="int",
                      default=None, dest="verbose",
                      help="set verboseness level(0--2, default=0, "
                      "higher means more verbose)")
    parser.add_option("-e", "--examples", action="store_true",
                      default=None,
                      help="will run vistrails examples")
    
    (options, args) = parser.parse_args()
    verbose = 0
    if options.verbose:
        verbose = options.verbose
    test_examples = False 
    if options.examples:
        test_examples = True
    test_modules = None
    if len(args) > 0:
        test_modules = set(args)
    
    parser = OptionParser()
    
    parser.add_option("-v", 
                      dest="verbose", 
                      default=False, 
                      action="store_true", 
                      help="the verbose flag causes diagnostic output to print")
    parser.add_option("-f", "--fieldData", 
                      dest="fieldData", 
                      help="The input CSV of field data points")
    parser.add_option("-i", "--inCSV", 
                      dest="inputsCSV", 
                      help="The input CSV containing a list of our inputs, one per line.")              
    parser.add_option("-o", "--output", 
                      dest="outputMDS", 
                      help="Output MDS file to save to.")
    parser.add_option("-p", "--probSurface", 
                      dest="probSurface",
                      default='', 
                      help="Probability surface to use for generation of background points (optional)")
    parser.add_option("-c", "--pointCount", 
                      dest="pointCount",
                      default=0, 
                      help="Number of random background points to add(optional)")
    
    (options, args) = parser.parse_args(argv)
    
    ourMDS = SahmTestSuiteRunner()
    ourMDS.run()
   

if __name__ == '__main__':
    sys.exit(main(sys.argv))