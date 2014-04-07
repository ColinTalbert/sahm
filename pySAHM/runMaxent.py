import os
import sys
import csv

import utilities

import copy

import multiprocessing


class MAXENTRunner(object):
    '''This is a class to run the maxent jar.  The output from will need
    to be sent through WrapMaxent.r to generate outputs.
    '''
    def __init__(self):
        self.verbose = False
        self.maxent_path = ''
        self.java_path = 'java'
        self.mdsfile = ''
        self.projectionlayers = ''
        self.environmentallayers = ''
        self.testCSV = ''
        self.trainingCSV = ''
        self.backgroundCSV = ''
        self.outputdir = ''
        self.categoricals = []
        self.logger = None
        self.test_key = 'test'
        self.sub_run = False

        self.processing_mode = "single models sequentially (n - 1 cores each)"
        self.args_dict = {}
        self.maxent_args = {}

    def run(self):
        if self.sub_run:
            self.maxent_args['plots'] = 'false'
            self.maxent_args['responsecurves'] = 'false'
            self.maxent_args['outputgrids'] = 'false'

        self.validateInputs()

        self.start_pool()

        if self.args_dict['c'] != '':
            self.prepInputs()
        else:
            raise Exception, 'No MDS supplied.'
        if not self.maxent_args.has_key('projectionlayers'):
            self.maxent_args['projectionlayers'] = ''
        if self.trainingCSV != '':
            self.maxent_args['samplesfile'] = self.trainingCSV
        else:
            raise Exception, 'No Samples file supplied'
        if self.testCSV != '':
            self.maxent_args['testsamplesfile'] = self.testCSV
        if self.maxent_args['environmentallayers'] != '':
            self.maxent_args['environmentallayers'] = self.maxent_args['environmentallayers']
            if self.maxent_args.has_key('biasfile'):
                self.maxent_args['biasfile'] = self.maxent_args['biasfile']
        elif self.backgroundCSV != '':
            self.maxent_args['environmentallayers'] = self.backgroundCSV
            if self.maxent_args.has_key('biasfile'):
                self.writetolog('The supplied biasfile will be ignored since no environmentallayers were provided', True, True)
                self.maxent_args.pop('biasfile')
        self.maxent_args['autorun'] = 'true'
        if ' ' in self.args_dict['species_name']:
            self.args_dict['species_name'] = self.args_dict['species_name'].replace(' ', '_')
        if(not self.test_key):  #  if there is no testsplit (ie cross validation, remove the empty csv
            del self.maxent_args['testsamplesfile']

        if self.categoricals:
            catstr = ",".join([cat.replace('_categorical', '')
                               for cat in self.categoricals])
            self.maxent_args['togglelayertype'] = catstr

        if not self.maxent_path.endswith('.jar'):
            self.maxent_path = os.path.join(self.maxent_path, 'maxent.jar')

        cmd = self.gen_maxent_cmd()
        stdout_fname = os.path.join(self.outputdir, "stdOut.txt")
        stderr_fname = os.path.join(self.outputdir, "stdErr.txt")
        self.writetolog('    running command:  \n' +
                        utilities.convert_list_to_cmd_str(cmd) + "\n", True, False)
        utilities.add_process_to_pool(utilities.launch_cmd,
                                [cmd, stdout_fname, stderr_fname])
        utilities.wait_for_pool_to_finish()

        r_cmd = [sys.executable]
        r_cmd.extend(sys.argv)
        r_cmd[1] = r_cmd[1].replace('runMaxent.py', 'runRModel.py')
        r_cmd.append('lam=' + self.outputdir)
        self.writetolog('    running command:  \n' +
                    utilities.convert_list_to_cmd_str(r_cmd) + "\n", True, False)
        utilities.add_process_to_pool(utilities.launch_cmd,
                                [r_cmd, stdout_fname, stderr_fname])
        utilities.wait_for_pool_to_finish()


    def start_pool(self):
        #  note that except for Condor this is the inverse of how the main
        #  application allocates cores.
        if self.cur_processing_mode == "FORT Condor":
            process_count = 2 ** 32
        elif self.cur_processing_mode == "multiple models simultaneously (1 core each)":
            #  there might be multiple models running so we only get one core
            process_count = 1
        else:
            #  no other models should be running, we get to use all the cores
            process_count = multiprocessing.cpu_count() - 1

        if process_count < 1:
            process_count = 1

        utilities.start_new_pool(process_count)

    def gen_maxent_cmd(self):
        cmd = [self.java_path, '-mx512m', '-jar', self.maxent_path]
        for k, v in self.maxent_args.iteritems():
            if v in [True, False]:
                cmd.append(k + "=" + str(v).lower())
            else:
                cmd.append(k + "=" + str(v))
        return cmd

    def validateInputs(self):
        #  first manually set some instance variables from the args
        self.mdsfile = self.args_dict['c']
        self.outputdir = self.args_dict['o']
        self.maxent_args['outputdirectory'] = self.outputdir
        self.cur_processing_mode = self.args_dict['cur_processing_mode']
        self.test_key = self.args_dict.get('test_key', 'test')
        self.sub_run = self.args_dict.get('sub_run', False)
        self.maxent_path = self.args_dict.get('maxent_path', "")
        self.java_path = self.args_dict.get('java_path', self.java_path)

        #  next run our valid inputs checks.
        if not os.path.exists(self.mdsfile):
            raise RuntimeError(self, 'Input MDS, ' + self.mdsfile + ', could not be found on file system')
        if not self.maxent_args.has_key('projectionlayers'):
            self.maxent_args['projectionlayers'] = ''
        if not self.maxent_args.has_key('environmentallayers'):
            self.maxent_args['environmentallayers'] = ''
        elif self.maxent_args['environmentallayers'] != '' and \
            not os.path.isdir(self.maxent_args['environmentallayers']):
            raise RuntimeError(self, 'Input environmentallayers directory, ' + self.maxent_args['environmentallayers'] + ', could not be found on file system')
        if self.maxent_args['projectionlayers'] != '':
            dirs = self.maxent_args['projectionlayers'].split(',')
            for d in dirs:
                if not os.path.isdir(d):
                    raise RuntimeError(self, "Input 'projectionlayers' must be a directory")

        if not utilities.isMDSFile(self.mdsfile):
            raise RuntimeError(self, 'Input MDS, ' + self.mdsfile + ', does not appear to be formated as an MDS file.')
        if not os.path.exists(self.outputdir):
            raise RuntimeError(self, 'Output directory, ' + self.outputdir + ', could not be found on file system')
        if self.logger is None:
            self.logger = utilities.logger(self.outputdir, self.verbose)
        self.writetolog = self.logger.writetolog

    def prepInputs(self):
        if not self.sub_run:
            self.handleCrossValidations()

        """parses out input MDS file into the 1 to 3 SWD files that Maxent requires.
        """
        self.testCSV = os.path.join(self.outputdir, 'testSamples.csv')
        self.trainingCSV = os.path.join(self.outputdir, 'trainingSamples.csv')
        self.backgroundCSV = os.path.join(self.outputdir, 'backgroundPoints.csv')

        testWriter = csv.writer(open(self.testCSV, 'wb'))
        trainingWriter = csv.writer(open(self.trainingCSV, 'wb'))
        backgroundWriter = csv.writer(open(self.backgroundCSV, 'wb'))
        MDSreader = csv.reader(open(self.args_dict['c'], 'r'))
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
        for row in MDSreader:
            self.convertNA(row)
            vals = self.usedValues(row, covariateIndexes)
            if evalsplit and row[evalsplit] == 'test':  #  handle evaluation split
                pass
            elif splitcol is None:  #  no split column
                if row[2] == '1':
                    trainingWriter.writerow([self.args_dict['species_name']] + row[:2] + vals)
                elif row[2] in ['-9999', '-9998', '0']:
                    hasBackground = True
                    backgroundWriter.writerow([''] + row[:2] + vals)
            elif not row[splitcol] == self.test_key:  #  train split
                if row[2] in ['-9999', '-9998', '0']:
                    hasBackground = True
                    backgroundWriter.writerow([''] + row[:2] + vals)
                elif row[2] == '1':
                    trainingWriter.writerow([self.args_dict['species_name']] + row[:2] + vals)
            elif row[splitcol] == self.test_key or self.testCSV == '':  #  test split only used by Maxent and only using pres data
                if row[2] == '1':
                    testWriter.writerow([self.args_dict['species_name']] + row[:2] + vals)
            else :
                pass

        if not hasBackground and self.maxent_args['environmentallayers'] == '':
            msg = '    No environmental layers were provided and '
            msg += '\n no background points were detected in the input file.'
            msg += '\n    Maxent requires pregenerated background points or environmental layer'
            msg += '\nfrom which to extract values.'
            self.writetolog(msg)
            raise RuntimeError(msg)
        elif hasBackground and self.maxent_args['environmentallayers'] != '':
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
        if not self.maxent_args['projectionlayers'] != '':
            if self.maxent_args['environmentallayers'] != '':
                pass
            else:
                self.maxent_args['outputgrids'] = 'false'
            return

    def handleCrossValidations(self):
        #  Start marian adding junk to the code
        MDSreader = csv.reader(open(self.args_dict['c'], 'r'))
        header1 = MDSreader.next()
        header2 = MDSreader.next()
        header3 = MDSreader.next()

        if 'Split' in header1:

            newLine = MDSreader.next()
            ttList = ["test", "train"]
            cvList = ["NA"]
            #  find the first line of the mds that isn't na to determine if test/train or cv split
            while newLine[header1.index("Split")] == "NA":
                newLine = MDSreader.next()

            #  loop through the mds and fit a maxent model withholding each new cv fold
            if (not newLine[header1.index("Split")] in ttList):
                subrun_args = copy.deepcopy(self.args_dict)
                subrun_args["sub_run"] = 'True'
#                cvMaxent = copy.deepcopy(self)
#                cvMaxent.sub_run=True
                for row in MDSreader:
                    if (not row[header1.index("Split")] in cvList):
                        subrun_args["test_key"] = row[header1.index("Split")]
                        outdir = os.path.join(self.outputdir,
                                        "cvSplit" + row[header1.index("Split")])
                        subrun_args["o"] = outdir
                        stdout_fname = os.path.join(outdir, "stdOut.txt")
                        stderr_fname = os.path.join(outdir, "stdErr.txt")
                        os.mkdir(subrun_args["o"])

                        cvList.append(row[header1.index("Split")])
                        try:
                            cmd = [sys.executable]
                            cmd += sys.argv[:sys.argv.index("--args") + 1]

                            for k, v in subrun_args.iteritems():
                                if v in [True, False]:
                                    cmd.append(k + "=" + str(v).upper())
                                else:
                                    cmd.append(k + "=" + str(v))
                            cmd.append("maxent_args=" + str(self.maxent_args))

                            utilities.add_process_to_pool(utilities.launch_cmd,
                                [cmd, stdout_fname, stderr_fname])
                        except utilities.TrappedError as e:
                            raise RuntimeError(self, e.message)

                    #  here we need to run Maxent without the test split csv which breaks it
                    self.test_key = None
            utilities.wait_for_pool_to_finish()

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
                if item not in self.categoricals:
                    self.categoricals.append(item)

    def isSWD(self, f):
        """Checks the format of a file to see if it is in the
        Maxent samples with data (SWD) format.
        """
        if os.path.exists(f):
            reader = csv.reader(open(f, 'r'))
            header = reader.next()
            if header[0].lower() in ('species', 'full_name', 'fullname'):
                return True
        return False


def main(argv):
    """Process our command line args and initiate a Maxent run
    """

    ourMaxent = MAXENTRunner()

    args = sys.argv[sys.argv.index("--args") + 1:]
    for arg in args:
        k, v = arg.split("=")
        if k == "maxent_args":
            ourMaxent.maxent_args = eval(v)
        elif v.lower() in ['true', 'false']:
            ourMaxent.args_dict[k] = eval(v.title())
        else:
            ourMaxent.args_dict[k] = v

    ourMaxent.run()

if __name__ == "__main__":

#    try:
    main(sys.argv)
