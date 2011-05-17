import csv
from datetime import datetime
import glob
import itertools
import os
import shutil
import sys
import subprocess

from core.modules.vistrails_module import Module, ModuleError, ModuleConnector
from core.modules.basic_modules import File, Directory, new_constant, Constant
from core.modules.basic_modules import List
from core.system import list2cmdline, execute_cmdline


from widgets import get_predictor_widget, get_predictor_config
from SelectPredictorsLayers import SelectListDialog
from utils import map_ports, path_value, create_file_module, createrootdir 
from utils import create_dir_module, mktempfile, mktempdir, cleantemps
from utils import dir_path_value, collapse_dictionary, tif_to_color_jpeg

#import our python SAHM Processing files
import packages.sahm.pySAHM.FieldDataQuery as FDQ
import packages.sahm.pySAHM.MDSBuilder as MDSB
import packages.sahm.pySAHM.PARC as parc

identifier = 'gov.usgs.sahm'

#def run_cmd_line_jar(jar_name, args):
#    arg_items = list(itertools.chain(*args.items()))
#    output = []
#    jar_name = os.path.join(sahm_path, jar_name)
#    cmdline = ['java', '-jar', jar_name] + arg_items
#    print 'running', cmdline
#    res = execute_cmdline(['java', '-jar', jar_name] + arg_items, output)
#    return res, output

#def run_cmd_line_py(jar_name, args):
#    arg_items = list(itertools.chain(*args.items()))
#    output = []
#    jar_name = os.path.join(sahm_path, jar_name)
#    cmdline = ['java', '-jar', jar_name] + arg_items
#    print 'running', cmdline
#    res = execute_cmdline(['java', '-jar', jar_name] + arg_items, output)
#    return res, output

def expand_ports(port_list):
    new_port_list = []
    for port in port_list:
        port_spec = port[1]
        if type(port_spec) == str: # or unicode...
            if port_spec.startswith('('):
                port_spec = port_spec[1:]
            if port_spec.endswith(')'):
                port_spec = port_spec[:-1]
            new_spec_list = []
            for spec in port_spec.split(','):
                spec = spec.strip()
                parts = spec.split(':', 1)
#                print 'parts:', parts
                namespace = None
                if len(parts) > 1:
                    mod_parts = parts[1].rsplit('|', 1)
                    if len(mod_parts) > 1:
                        namespace, module_name = mod_parts
                    else:
                        module_name = parts[1]
                    if len(parts[0].split('.')) == 1:
                        id_str = 'edu.utah.sci.vistrails.' + parts[0]
                    else:
                        id_str = parts[0]
                else:
                    mod_parts = spec.rsplit('|', 1)
                    if len(mod_parts) > 1:
                        namespace, module_name = mod_parts
                    else:
                        module_name = spec
                    id_str = identifier
                if namespace:
                    new_spec_list.append(id_str + ':' + module_name + ':' + \
                                             namespace)
                else:
                    new_spec_list.append(id_str + ':' + module_name)
            port_spec = '(' + ','.join(new_spec_list) + ')'
        new_port_list.append((port[0], port_spec) + port[2:])
#    print new_port_list
    return new_port_list

class FieldData(File):
    _input_ports = [('csvFile', '(edu.utah.sci.vistrails.basic:File)')]
    _output_ports = [('value', '(gov.usgs.sahm:FieldData:DataInput)'),
                     ('value_as_string', 
                      '(edu.utah.sci.vistrails.basic:String)', True)]
    
class Predictor(File):
    _input_ports = [('categorical', '(edu.utah.sci.vistrails.basic:Boolean)')]
    _output_ports = [('value', '(gov.usgs.sahm:Predictor:DataInput)'),
                     ('value_as_string', 
                      '(edu.utah.sci.vistrails.basic:String)', True)]

class TemplateLayer(File):
    _input_ports = [('FilePath', '(edu.utah.sci.vistrails.basic:File)')]
    _output_ports = [('value', '(gov.usgs.sahm:TemplateLayer:DataInput)'),
                     ('value_as_string', '(edu.utah.sci.vistrails.basic:String)', True)]
#    def compute(self):
#        output_file = create_file_module(self.forceGetInputFromPort('FilePath', []))
#        self.setResult('value', output_file)

#class SingleInputPredictor(Predictor):
#    pass

class SpatialDef(Module):
    _output_ports = [('spatialDef', '(gov.usgs.sahm:SpatialDef:DataInput)')]

class MergedDataSet(File):
    _input_ports = expand_ports([('mdsFile', '(edu.utah.sci.vistrails.basic:File)')])
    _output_ports = expand_ports([('value', '(gov.usgs.sahm:MergedDataSet:DataInput)')])
    
    True

class Model(Module):
    _input_ports = [('mdsFile', '(gov.usgs.sahm:MergedDataSet:DataInput)')]
    _output_ports = [('BinaryMap', '(edu.utah.sci.vistrails.basic:File)'), 
                     ('ProbabilityMap', '(edu.utah.sci.vistrails.basic:File)'),
                     ('AUC_plot', '(edu.utah.sci.vistrails.basic:File)'),
                     ('ResponseCurves', '(edu.utah.sci.vistrails.basic:File)'),
                     ('Text_Output', '(edu.utah.sci.vistrails.basic:File)')]

    def compute(self):
        mdsFile = dir_path_value(self.forceGetInputFromPort('mdsFile', []))

        global models_path
        global color_breaks_csv
        
        r_path = configuration.r_path
        program = os.path.join(r_path, "i386", "Rterm.exe") #-q prevents program from running
        Script = os.path.join(models_path, self.name)
        
        ModelOutput = {"FIT_BRT_pluggable.r":"brt",
                       "FIT_GLM_pluggable.r":"glm",
                       "FIT_RF_pluggable.r":"rf",
                       "FIT_MARS_pluggable.r":"mars"}
        ModelAbbrev = ModelOutput[self.name]
        
        output_dname = mktempdir(prefix='output_')
        
        args = "c=" + mdsFile + " o=" + output_dname + " rc=ResponseBinary"
        
        command = program + " --vanilla -f " + Script + " --args " + args
#        print command
#        
#        print subprocess.PIPE
        
        p = subprocess.Popen(command, stderr=subprocess.PIPE, stdout=subprocess.PIPE)

        # Second, use communicate to run the command; communicate() returns a
        #   tuple (stdoutdata, stderrdata)
        if configuration.verbose:
            print "starting R Processing of " + ModelAbbrev,

        ret = p.communicate()
        if ret[1]:
            print ret[1]
        del(ret)
        
        
        input_fname = os.path.join(output_dname, ModelAbbrev + "_prob_map.tif")
        output_fname = mktempfile(prefix=ModelAbbrev + '_prob_map_', suffix='.jpeg')
        tif_to_color_jpeg(input_fname, output_fname, color_breaks_csv)
        
        outFileName = os.path.join(output_dname, ModelAbbrev + "_bin_map.tif")
        output_file1 = create_file_module(outFileName)
        self.setResult('BinaryMap', output_file1)
        
        outFileName = os.path.join(output_dname, ModelAbbrev + "_output.txt")
        output_file2 = create_file_module(outFileName)
        self.setResult('Text_Output', output_file2)
        
        outFileName = os.path.join(output_dname, ModelAbbrev + "_auc_plot.jpg")
#        print "out auc: ", outFileName
        output_file3 = create_file_module(outFileName)
        self.setResult('AUC_plot', output_file3)
        
        outFileName = output_fname
#        print ModelAbbrev + "_prob_map.tif: ", outFileName
        output_file4 = create_file_module(outFileName)
        self.setResult('ProbabilityMap', output_file4)
        
        outFileName = os.path.join(output_dname, ModelAbbrev + "_response_curves.pdf")
        output_file5 = create_file_module(outFileName)
        self.setResult('ResponseCurves', output_file5)
        
        if configuration.verbose:
            print "Finished " + ModelAbbrev   +  " builder\n"
        




#class Model(File):
#    _input_ports = [('value', '(edu.utah.sci.vistrails.basic:File)', True)]
#    _output_ports = [('value', '(gov.usgs.sahm:Model:Models)'),
#                     ('value_as_string', 
#                      '(edu.utah.sci.vistrails.basic:String)', True)]
#    
#    def compute(self):
#        self.upToDate = True
#        self.setResult('value', self)
        
class GLM(Model):
    def __init__(self):
        global models_path
        Model.__init__(self)
        self.name = 'FIT_GLM_pluggable.r'

class RandomForest(Model):
    def __init__(self):
        global models_path
        Model.__init__(self)
        self.name = 'FIT_RF_pluggable.r'

class MARS(Model):
    def __init__(self):
        global models_path
        Model.__init__(self)
        self.name = 'FIT_MARS_pluggable.r'

class MAXENT(Model):
    def __init__(self):
        global models_path
        Model.__init__(self)
        self.name = 'RunMaxEnt.jar'

class BoostedRegressionTree(Model):
    def __init__(self):
        global models_path
        Model.__init__(self)
        self.name = 'FIT_BRT_pluggable.r'

class MDSBuilder(Module):

    _input_ports = expand_ports([('PredictorsDir', 'basic:Directory'),
                                 ('fieldData', '(gov.usgs.sahm:FieldData:DataInput)'),
                                 ('minValue', 'basic:Float')]
                                 )
    _output_ports = expand_ports([('mdsFile', '(gov.usgs.sahm:MergedDataSet:DataInput)')])

    def compute(self):
        if configuration.verbose:
            print "Running MDSBuilder  ",
        port_map = {'fieldData': ('-f', dir_path_value, True),
                    'PredictorsDir': ('-d', path_value, True),
                    'minValue': ('-m', None, False)}
         
        args = map_ports(self, port_map)

        output_fname = mktempfile(prefix='sahm', suffix='.mds')
        args['-o'] = output_fname

#        print args
        
        cmd_args = collapse_dictionary(args)
#        print cmd_args

        MDSB.run(cmd_args)

        output_file = create_file_module(output_fname)
        
        if configuration.verbose:
            print "Finished running MDS builder\n"
        
        self.setResult('mdsFile', output_file)

class FieldDataQuery(Module):
    _input_ports = expand_ports([('templateLayer', '(gov.usgs.sahm:TemplateLayer:DataInput)'),
                                 ('fieldData', '(gov.usgs.sahm:FieldData:DataInput)'),
                                 ('aggregateRows', 'basic:Boolean'),
                                 ('aggregateRowsByYear', 'basic:Boolean')])
    _output_ports = expand_ports([('fieldData', '(gov.usgs.sahm:FieldData:DataInput)')])
    
    def compute(self):

        output_fname = mktempfile(prefix='FDQ_', suffix='.csv')
        
        ourFDQ = FDQ.FieldDataQuery()
        
        if configuration.verbose:
            ourFDQ.verbose = True
            
        ourFDQ.template = self.forceGetInputFromPort('templateLayer').name
        ourFDQ.csv = self.forceGetInputFromPort('fieldData').name
        ourFDQ.output = output_fname
        if self.hasInputFromPort('aggregateRows'):
            ourFDQ.AggByPixel = self.getInputFromPort('aggregateRows')
        #not implemented yet
        #ourFDQ.AggByYear = options.bAggYears
        ourFDQ.processCSV()
        

        output_file = utils.create_file_module(output_fname)
        writetolog("Finished running FieldDataQuery", True)
        self.setResult('fieldData', output_file)

class PARC(Module):
    '''
    A widget to run the PARC module which
    provides functionality to sync raster layer properties
    with a template dataset's properties.
    '''

    configuration = []
    _input_ports = [('predictor', "(gov.usgs.sahm:Predictor:DataInput)"),
                                ('PredictorList', '(gov.usgs.sahm:PredictorList:Other)'),
                                ('FileListCSV', '(edu.utah.sci.vistrails.basic:File)'),
                                ('templateLayer', '(gov.usgs.sahm:TemplateLayer:DataInput)')]

    _output_ports = [('PredictorListCSV', '(edu.utah.sci.vistrails.basic:File)')]

    def compute(self):
        writetolog("\nRunning PARC", True)
        
        ourPARC = parc.PARC()
        output_dname = utils.mknextdir(prefix='PARC_')
        
        if configuration.verbose:
            ourPARC.verbose = True
        ourPARC.loggert = utils.getLogger()
        writetolog("    output_dname=" + output_dname, False, False)
        ourPARC.outDir = output_dname

        workingCSV = utils.mknextfile(prefix='tmpFilesToPARC_', suffix='.csv')
        outputCSV = utils.mknextfile(prefix='PARCOutput_', suffix='.csv')
        writetolog("    workingCSV=" + workingCSV, False, False)
        #append additional inputs to the existing CSV if one was supplied
        #otherwise start a new CSV
        if self.hasInputFromPort("csvFileList"):
            inputCSV = self.getInputFromPort("csvFileList").name
            shutil.copy(inputCSV, workingCSV)
            f = open(workingCSV, "ab")
            csvWriter = csv.writer(f)
        else:
            f = open(workingCSV, "wb")
            csvWriter = csv.writer(f)
            csvWriter.writerow(["FilePath", "Categorical", "Resampling", "Aggregation"])
        
        if self.hasInputFromPort("PredictorList"):
            predictor_lists = self.forceGetInputListFromPort('PredictorList')
            for predictor_list in predictor_lists:
                for predictor in predictor_list:
                    csvWriter.writerow(list(predictor))
        
        if self.hasInputFromPort("predictor"):
            predictor_list = self.forceGetInputListFromPort('predictor')
            for predictor in predictor_list:
                csvWriter.writerow(list(predictor))
        f.close()
        del csvWriter
        ourPARC.inputsCSV = workingCSV
        ourPARC.template = self.forceGetInputFromPort('templateLayer').name

        try:
            ourPARC.parcFiles()
        except:
            utils.informative_untrapped_error(self, "PARC")
        
        #read through our tmp csv of files to parc. 
        #append the output file location to a new (fourth) column for each input.
        working = csv.reader(open(workingCSV, "r"))
        working.next()
        output = csv.writer(open(outputCSV, "wb"))
        output.writerow(["PARCOutputFile", "Categorical", "Resampling", "Aggregation", "OriginalFile"])
        for row in working:
            fileName = os.path.split(row[0])[1]
            fileName = os.path.join(output_dname, fileName + ".tif")
            outputrow = [fileName] + row[1:4] + [row[0]]
            output.writerow(outputrow)
        del working
        del output
        #delete our temp working file
        os.remove(workingCSV)
        
        
        predictorsDir = utils.create_dir_module(output_dname)
        output_file = utils.create_file_module(outputCSV)
         
        writetolog("Finished running PARC", True)
        self.setResult('PredictorListCSV', output_file)

class TiffConverter(Module):
    '''
    A widget to run the TiffConverter module which
    provides functionality to convert the tiffs specified 
    in an MDS header into ASCII format for Maxent.
    '''

    configuration = []
    _input_ports = [("inputMDS", "(gov.usgs.sahm:MergedDataSet:DataInput)"),
                                ('format', '(edu.utah.sci.vistrails.basic:String)'),]

    _output_ports = [('outputDir', '(edu.utah.sci.vistrails.basic:Directory)')]

    def compute(self):
        writetolog("\nRunning TiffConverter", True)
        #utils.breakpoint()
        ourTC = TC.FormatConverter()
        ourTC.MDSFile = self.forceGetInputFromPort('inputMDS').name   
        ourTC.outputDir = utils.mknextdir(prefix='ConvertedTifs_')
        if configuration.verbose:
            ourTC.verbose = True
        ourTC.logger = utils.getLogger()
        writetolog("    output directory = " + ourTC.outputDir, False, False)
        ourTC.run()
        writetolog("\nFinished running TiffConverter", True)
        
class TestTrainingSplit(Module):
    _input_ports = [("inputMDS", "(gov.usgs.sahm:MergedDataSet:DataInput)"),
                    ('trainingProportion', '(edu.utah.sci.vistrails.basic:String)'),
                    ('RatioPresAbs', '(edu.utah.sci.vistrails.basic:String)')]
    _output_ports = [("outputMDS", "(gov.usgs.sahm:MergedDataSet:DataInput)")]
    
    def compute(self):
        writetolog("\nGenerating Test Training split ", True)
        inputMDS = utils.dir_path_value(self.forceGetInputFromPort('inputMDS', []))
        outputMDS = utils.mknextfile(prefix='TestTrainingSplit_', suffix='.csv')

        global models_path
        
        args = "i=" + inputMDS + " o=" + outputMDS 
        args += " rc=" + utils.MDSresponseCol(inputMDS)
        if (self.hasInputFromPort("trainingProportion")):
            try:
                trainingProportion = float(self.getInputFromPort("resampleMethod"))
                if trainingProportion <= 0 or trainingProportion > 1:
                    raise runtimeError
                args += " p=" + str(trainingProportion)
            except:
                raise ModuleError(self, "Train Proportion (trainProp) must be a number between 0 and 1 excluding 0")
        if (self.hasInputFromPort("RatioPresAbs")):
            try:
                RatioPresAbs = float(self.getInputFromPort("RatioPresAbs"))
                if RatioPresAbs <= 0 or RatioPresAbs >= 1:
                    raise runtimeError
                args += " m=" + str(trainingProportion)
            except:
                raise ModuleError(self, "The ratio of presence to absence (RatioPresAbs) must be a \nnumber between 0 and 1 excluding both 0 and 1") 
        
        
        r_path = configuration.r_path
        program = os.path.join(r_path, "i386", "Rterm.exe") #-q prevents program from running
        Script = os.path.join(models_path, "TestTrainSplit.r")

        command = program + " --vanilla -f " + Script + " --args " + args
        writetolog("    " + command, False, False)
#        print command
#        
#        print subprocess.PIPE
        
        p = subprocess.Popen(command, stderr=subprocess.PIPE, stdout=subprocess.PIPE)

        ret = p.communicate()
        if ret[1]:
            msg = "An error was encountered in the R script for this module.  The R error message is below - \n"
            msg += ret[1]
            writetolog(msg)
            raise ModuleError(self, msg)
        del(ret)

        output = os.path.join(outputMDS)
        if os.path.exists(output):
            output_file = utils.create_file_module(output)
            writetolog("Finished Test Training split ", True)
        else:
            msg = "Problem encountered generating Test Training split.  Expected output file not found."
            writetolog(msg, False)
            raise ModuleError(self, msg)
        self.setResult("outputMDS", output_file)
        

class CovariateCoorelationAndSelection(Module):
    '''
    select from a list of processessed predictor layers for inclusion in the module
    '''
    kwargs = {}
    kwargs['defaults'] = str(['initial'])
    _input_ports = [("inputMDS", "(gov.usgs.sahm:MergedDataSet:DataInput)"),
                    ('selectionName', '(edu.utah.sci.vistrails.basic:String)', kwargs)]
    _output_ports = [("outputMDS", "(gov.usgs.sahm:MergedDataSet:DataInput)")]

    def compute(self):
        writetolog("\nOpening Select Predictors Layers widget", True)
        inputMDS = utils.dir_path_value(self.forceGetInputFromPort('inputMDS'))
        selectionName = self.forceGetInputFromPort('selectionName', 'initial')
        outputMDS = utils.mknextfile(prefix='SelectPredictorsLayers_' + selectionName + "_", suffix='.csv')
        displayJPEG = utils.mknextfile(prefix='PredictorCorrelation_' + selectionName + "_", suffix='.jpg')
        writetolog("    inputMDS = " + inputMDS, False, False)
        writetolog("    displayJPEG = " + displayJPEG, False, False)
        writetolog("    outputMDS = " + outputMDS, False, False)
        
        self.callDisplayMDS(inputMDS, outputMDS, displayJPEG)

        output_file = utils.create_file_module(outputMDS)
        writetolog("Finished Select Predictors Layers widget", True)
        self.setResult("outputMDS", output_file)

    def callDisplayMDS(self, inputMDS, outputMDS, displayJPEG):
        global r_path
        global models_path
        dialog = SelectListDialog(inputMDS, outputMDS, displayJPEG, r_path, models_path)
        #dialog.setWindowFlags(QtCore.Qt.WindowMaximizeButtonHint)
#        print " ... finished with dialog "  
        retVal = dialog.exec_()
        #outputPredictorList = dialog.outputList

        return inputMDS


class ProjectionLayers(Module):
    _input_ports = [('fileListCSV', '(edu.utah.sci.vistrails.basic:File)'),
                    ('templateLayer', '(gov.usgs.sahm:TemplateLayer:DataInput)'),
                    ('model', '(edu.utah.sci.vistrails.basic:String)'),
                    ('scenario', '(edu.utah.sci.vistrails.basic:String)'),
                    ('year', '(edu.utah.sci.vistrails.basic:String)'),
                    ('directoryCrosswalkCSV', '(edu.utah.sci.vistrails.basic:File)')
                    ]
    _output_ports = [("outputMDS", "(gov.usgs.sahm:MergedDataSet:DataInput)")]

    def compute(self):
        models = ['CCCMA', 'CSIRO', 'hadcm3']
        scenarioss = ['A2a', 'B2b']
        years = ['2020', '2050', '2080']
        
        writetolog("\nRunning make Projection Layers", True)
        
        
        
        
        inputCSV = self.forceGetInputFromPort('fileListCSV').name
    
        if self.hasInputFromPort('templateLayer'):
            template = self.forceGetInputFromPort('templateLayer').name
        else:
            template = '' #we'll get a template below
            
        fromto = []
        climargs = {}
        
        for input in ['model', 'scenario', 'year']:
            if self.hasInputFromPort(input):
                climargs[input] = self.forceGetInputFromPort(input)
        if climargs <> {} and climargs.keys() <> ['model', 'scenario', 'year']:
            #they did not add in one of each, Not going to fly
            raise ModuleError(self, "All of model, scenario, and year must be supplied if any are used.")
        elif climargs <> {} and climargs.keys <> ['model', 'scenario', 'year']:
            #they specified a alt climate scenario add this to our list to search for
            fromto.append([r'K:\GIS_LIBRARY\Climate\WorldClim\BioclimaticVariables\bio_30s_esri\bio',
                           os.path.join('I:\WorldClim_Future_Climate\RenamedBILs', 
                                        climargs['model'], climargs['scenario'], climargs['year'])])
        
        if self.hasInputFromPort('directoryCrosswalkCSV'):
            crosswalkCSV = csv.reader(open(self.forceGetInputFromPort('directoryCrosswalkCSV'), 'r'))
            header = crosswalkCSV
            for row in crosswalkCSV:
                fromto.append(row[0], row[1])
            del crosswalkCSV    
            
        #write out the outputs to an empty MDS file (just the header is needed to PARC the outputs)
            
        
        inCSV = csv.reader(open(inputCSV, 'r'))
        inCSV.next() #skip header
        workingCSV = utils.mknextfile(prefix='tmpFilesToPARC_', suffix='.csv')
        tmpCSV = csv.writer(open(workingCSV, 'wb'))
        tmpCSV.writerow(["FilePath", "Categorical", "Resampling", "Aggregation"])
        outHeader1 = ['x', 'y', 'response']
        outHeader2 = ['', '', '']
        outHeader3 = ['', '', '']
        
        output_dname = utils.mknextdir(prefix='ProjectionLayers_')
        
        for row in inCSV:
            if template == '':
                template = row[0]
            fileShortName = utils.getShortName(row[0])
            if row[1] == 1:
                outHeader1.append(fileShortName + '_categorical')
            else:
                outHeader1.append(fileShortName)
            outHeader2.append('1')
            outHeader3.append(os.path.join(output_dname, fileShortName + '.tif'))

            origFile = row[4]
            newOrigFile = origFile
            for lookup in fromto:
               if lookup[0] in origFile:
                   newOrigFile = origFile.replace(lookup[0], lookup[1])
            tmpCSV.writerow([newOrigFile,] + row[1:4])
        
        #PARC the files here
        ourPARC = parc.PARC()
        
        
        if configuration.verbose:
            ourPARC.verbose = True
        writetolog("    output_dname=" + output_dname, False, False)
        ourPARC.outDir = output_dname
        
        ourPARC.inputsCSV = workingCSV
        ourPARC.template = template

        try:
            ourPARC.parcFiles()
        except:
            utils.informative_untrapped_error(self, "PARC")
        
        #loop through our workingCSV and format it into an MDS header
        
        output_MDSname = utils.mknextfile(prefix='ProjectionLayersMDS', suffix = '.csv')
        outCSV = csv.writer(open(output_MDSname), 'wb')
        outCSV.writerow(outHeader1)
        outCSV.writerow(outHeader2)
        outCSV.writerow(outHeader3)
        
        
        
        
        #at a minimum we need input MDS, template layer
        # additionally we need a specified , year and model and scenario, 
        #    and/or a csvFile that has a find replace of path names.

#class ClimateModel(String):
#    _input_ports = [('value', '(gov.usgs.sahm:ClimateModel:Other)')]
#    _output_ports = [('value_as_string', '(edu.utah.sci.vistrails.basic:String)', True)]
#    _widget_class = build_enum_widget('ClimateModel', 
#                                      ['CCCMA', 'CSIRO', 'hadcm3'])
#
#    @staticmethod
#    def get_widget_class():
#        return ClimateModel._widget_class
#
#class ClimateScenario(String):
#    _input_ports = [('value', '(gov.usgs.sahm:ClimateScenario:Other)')]
#    _output_ports = [('value_as_string', '(edu.utah.sci.vistrails.basic:String)', True)]
#    _widget_class = build_enum_widget('ClimateScenario', 
#                                      ['A2a', 'B2b'])
#
#    @staticmethod
#    def get_widget_class():
#        return ClimateScenario._widget_class
#
#class ClimateYear(String):
#    _input_ports = [('value', '(gov.usgs.sahm:ClimateYear:Other)')]
#    _output_ports = [('value_as_string', '(edu.utah.sci.vistrails.basic:String)', True)]
#    _widget_class = build_enum_widget('ClimateYear', 
#                                      ['2020', '2050', '2080'])
#
#    @staticmethod
#    def get_widget_class():
#        return ClimateYear._widget_class

class MAXENT(Module):

    _output_ports = [("lambdas", "(edu.utah.sci.vistrails.basic:File)"),
                     ("report", "(edu.utah.sci.vistrails.basic:File)"),
                     ("roc", "(edu.utah.sci.vistrails.basic:File)")]

    def compute(self):
        global maxent_path
        #get input MDS
        
        
        ourMaxent = MaxentRunner.MAXENTRunner()
        ourMaxent.outputDir = utils.mknextdir(prefix='maxentFiles_')
        
        if self.hasInputFromPort('inputMDS'):
            ourMaxent.inputMDS = self.forceGetInputFromPort('inputMDS').name

        ourMaxent.maxentpath = maxent_path
        
        MaxentArgsCSV = utils.mknextfile(prefix='MaxentArgs', suffix='.csv')
        argWriter = csv.writer(open(MaxentArgsCSV, 'wb'))
        for port in self._input_ports:
            #print port
            if port[0] <> 'inputMDS':
                if self.hasInputFromPort(port[0]):
                    port_val = self.getInputFromPort(port[0])
                    #print "   has input " + str(port_val)
                    if port[1] == "(edu.utah.sci.vistrails.basic:Boolean)":
                        port_val = str(port_val).lower()
                        #port_val = port_val[0].lower() + port_val[1:]
                    elif port[1] == "(edu.utah.sci.vistrails.basic:Path)" or \
                        port[1] == "(edu.utah.sci.vistrails.basic:File)":
                        port_val = port_val.name
                    argWriter.writerow([port[0],port_val])
                else:
                    #print "   has no input "
                    kwargs = port[2]
                    #print kwargs
                    try:
                        if port[1] == "(edu.utah.sci.vistrails.basic:Boolean)":
                            default = kwargs['defaults'][2:-2].lower()
                        else:
                            default = kwargs['defaults'][2:-2]
                        #args[port[0]] = default
                        argWriter.writerow([port[0], default])
                    except KeyError:
                        pass
        del argWriter
        ourMaxent.argsCSV = MaxentArgsCSV
        ourMaxent.logger = utils.getLogger()
        try:
            ourMaxent.run()
        except:
            utils.informative_untrapped_error(self, "Maxent")
#        
        #set outputs
        lambdasfile = os.path.join(outputDir, args["species_name"] + ".lambdas")
        print lambdasfile
        output_file = utils.create_file_module(lambdasfile)
        self.setResult("lambdas", output_file)
        
        rocfile = os.path.join(outputDir, 'plots', args["species_name"] + "_roc.png")
        print rocfile
        output_file = utils.create_file_module(rocfile)
        self.setResult("roc", output_file)

        htmlfile = os.path.join(outputDir, args["species_name"] + ".html")
        print htmlfile
        output_file = utils.create_file_module(htmlfile)
        self.setResult("report", output_file)

        writetolog("Finished Maxent widget", True)
        
def load_max_ent_params():    
    maxent_fname = os.path.join(os.path.dirname(__file__), 'maxent.csv')
    csv_reader = csv.reader(open(maxent_fname, 'rU'))
    # pass on header
    csv_reader.next()
    input_ports = []
    
    input_ports.append(('inputMDS', '(gov.usgs.sahm:MergedDataSet:DataInput)'))
    
    docs = {}
    basic_pkg = 'edu.utah.sci.vistrails.basic'
    p_type_map = {'file/directory': 'Path',
                  'double': 'Float'}
    for row in csv_reader:
        [name, flag, p_type, default, doc, notes] = row
        name = name.strip()
        p_type = p_type.strip()
        if p_type in p_type_map:
            p_type = p_type_map[str(p_type)]
        else:
            p_type = str(p_type).capitalize()
        kwargs = {}
        default = default.strip()
        if default:
            if p_type == 'Boolean':
                default = default.capitalize()
            kwargs['defaults'] = str([default])
        if p_type == 'Boolean':
            kwargs['optional'] = True
        input_ports.append((name, '(' + basic_pkg + ':' + p_type + ')', kwargs))
        # FIXME set documentation
        #print 'port:', (name, '(' + basic_pkg + ':' + p_type + ')', kwargs)
        docs[name] = doc

    #print 'MAXENT:', input_ports
    MAXENT._input_ports = input_ports
    MAXENT._port_docs = docs

    def provide_input_port_documentation(cls, port_name):
        return cls._port_docs[port_name]
    MAXENT.provide_input_port_documentation = \
        classmethod(provide_input_port_documentation)

class SelectPredictorsLayers(Module):
    '''
    select from a list of processessed predictor layers for inclusion in the module
    '''

    _input_ports = [("inputMDS", "(gov.usgs.sahm:MergedDataSet:DataInput)")]
    _output_ports = [("outputMDS", "(gov.usgs.sahm:MergedDataSet:DataInput)")]

    def compute(self):
        print "Starting compute ",
        inputMDS = dir_path_value(self.forceGetInputFromPort('inputMDS', []))
        outputMDS = mktempfile(prefix='sahm', suffix='.mds')
        
        print "inputMDS = ", inputMDS, "outputMDS = ", outputMDS
        
        self.callDisplayMDS(inputMDS, outputMDS)

        output_file = create_file_module(outputMDS)
        
        self.setResult("outputMDS", output_file)
        print " ... Finished compute",

    def callDisplayMDS(self, inputMDS, outputMDS):
        global r_path
        global models_path
        dialog = SelectListDialog(inputMDS, outputMDS, r_path, models_path)
        #dialog.setWindowFlags(QtCore.Qt.WindowMaximizeButtonHint)
        print " ... finished with dialog ",  
        retVal = dialog.exec_()
        #outputPredictorList = dialog.outputList
        print " ... finished with callDisplayMDS"
        return inputMDS

def initialize():
    global models_path, r_path, color_breaks_csv
    
    r_path = configuration.r_path
    
    #append to our path variable the location of the GDAL dependencies
    #Proj, GDAL, and GDAL data
    proj_path = os.path.join(configuration.gdal_path, "proj", "bin")
    currentPath = os.environ['Path']
    appendedPath = currentPath + ";" + proj_path
    os.environ['Path'] = appendedPath

    gdal_data = os.path.join(configuration.gdal_path, "gdal-data")
    os.putenv("GDAL_DATA", gdal_data)

    gdal_folder = os.path.join(configuration.gdal_path, "GDAL")
    currentPath = os.environ['Path']
    appendedPath = currentPath + ";" + gdal_folder
    os.environ['Path'] = appendedPath
    
    #store the path to the directory containing our R code in a g
    models_path = os.path.join(os.path.dirname(__file__), "pySAHM", "Resources", "R_Modules")  

    rootdir = createrootdir(configuration.output_dir)
    
    color_breaks_csv = os.path.join(os.path.dirname(__file__),  "ColorBreaks.csv")
    
    load_max_ent_params()
    
    if configuration.verbose:
        print "*" * 79
        print "Initialize:"
        print "  Locations of dependencies"
        print "   layers csv = " + os.path.join(os.path.dirname(__file__), "layers.csv")
        print "   ColorBreaks csv = " + color_breaks_csv
        print "   R path = " + configuration.r_path
        print "   R models directory = " + models_path
        print "   GDAL folder = " + configuration.gdal_path
        print "        Must contain subfolders proj, gdal-data, GDAL"
        print "    "
        print "*" * 79
    
    print "*" * 79
    print " output directory:   " + rootdir
    print "*" * 79
    print "*" * 79
    
def finalize():
    cleantemps()
    

# FIXME: no need for generate_namespaces on trunk, this is built in to the
#        registry

def generate_namespaces(modules):
    module_list = []
    for namespace, m_list in modules.iteritems():
        for module in m_list:
            m_dict = {'namespace': namespace}
            if type(module) == tuple:
                m_dict.update(module[1])
                module_list.append((module[0], m_dict))
                #print 'm_dict:', m_dict
            else:
                module_list.append((module, m_dict))
    return module_list

def build_available_trees():
    trees = {}

    layers_fname = os.path.join(os.path.dirname(__file__), 'layers.csv')
    csv_reader = csv.reader(open(layers_fname, 'rU'))
    # pass on header
    csv_reader.next()
    for row in csv_reader:
        if row[2] not in trees:
            trees[row[2]] = {}
        available_dict = trees[row[2]]
#        if 'Daymet' not in available_dict:
#            available_dict['Daymet'] = []
#        available_dict['Daymet'].append((row[0], row[1], row[3]))            
        if row[3] not in available_dict:
            available_dict[row[3]] = []
        available_dict[row[3]].append((row[0], row[1], row[4]))
       
    return trees

def build_predictor_modules():
    available_trees = build_available_trees()
    modules = []
    for name, tree in available_trees.iteritems():
        name_arr = name.strip().split()
        class_base = ''.join(n.capitalize() for n in name_arr)
        widget_class = get_predictor_widget(class_base, tree)
        config_class = get_predictor_config(class_base, tree)
        class_name = class_base + "Predictors"
        def get_widget_method(w_class):
            @staticmethod
            def get_widget_class():
                return w_class
            return get_widget_class
        module = type(class_name, (PredictorList,),
                      {'get_widget_class': get_widget_method(widget_class),
                       '_input_ports': \
                           [('value',
                             '(gov.usgs.sahm:%s:DataInput)' % class_name)]})
        modules.append((module, {'configureWidgetType': config_class}))
    return modules

_modules = generate_namespaces({'DataInput': [
                                              Predictor,
                                              PredictorList,
                                              FieldData,
                                              TemplateLayer,
                                              MergedDataSet] + \
                                              build_predictor_modules(),
                                'Tools': [FieldDataQuery,
                                          MDSBuilder,
                                          PARC,
                                          SelectPredictorsLayers],
                                'Models': [Model,
                                           GLM,
                                           RandomForest,
                                           MARS,
                                           MAXENT,
                                           BoostedRegressionTree],
                                })
