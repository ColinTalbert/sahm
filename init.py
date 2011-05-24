import csv
from datetime import datetime
import glob
import itertools
import os
import shutil
import sys
import subprocess
import traceback

from core.modules.vistrails_module import Module, ModuleError, ModuleConnector
from core.modules.basic_modules import File, Directory, Path, new_constant, Constant
from core.modules.basic_modules import List
from core.modules.basic_modules import String

#from enum_modules import myEnum

from widgets import get_predictor_widget, get_predictor_config
from enum_widget import build_enum_widget

from SelectPredictorsLayers import SelectListDialog
#from maxent_module import MAXENTRunner
import utils
#import packages.sahm.pySAHM.Utilites


#import our python SAHM Processing files
import packages.sahm.pySAHM.FieldDataQuery as FDQ
import packages.sahm.pySAHM.MDSBuilder as MDSB
import packages.sahm.pySAHM.PARC as parc
import packages.sahm.pySAHM.TiffConverter as TC
import packages.sahm.pySAHM.MaxentRunner as MaxentRunner

from utils import writetolog

identifier = 'gov.usgs.sahm' 


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
    '''
    FieldData

    The FieldData module allows a user to add presence/absence points recorded across a landscape
    for the phenomenon being modeled (e.g. plant sightings, evidence of animal presence, etc.).
    The input data for this module must be in the form of a .csv file that follows one of two formats: 

    Format 1
    A .csv file with the following column headings, in order: "X," "Y," and "responseBinary".
    In this case, the "X" field should be populated with the horizontal (longitudinal) positional
    data for a sample point. The "Y" field should be populated with the vertical (latitudinal) data
    for a sample point. These values must be in the same coordinate system/units as the template
    layer used in the workflow. The column "responseBinary" should be populated with either a '0'
    (indicating absence at the point) or a '1' (indicating presence at the point).

    Format 2
    A .csv file with the following column headings, in order: "X," "Y," and "responseCount".
    In this case, the "X" field should be populated with the horizontal (longitudinal) positional
    data for a sample point. The "Y" field should be populated with the vertical (latitudinal) data
    for a sample point. These values must be in the same coordinate system/units as the template
    layer used in the workflow. The column "responseCount" should be populated with either a '-999'
    (indicating that the point is a background point) or a numerical value (either '0' or a positive integer)
    indicating the number of a incidences of the phenomenon recorded at that point.
    '''   
#    _input_ports = [('csvFile', '(edu.utah.sci.vistrails.basic:File)')]
    _output_ports = [('value', '(gov.usgs.sahm:FieldData:DataInput)'),
                     ('value_as_string', '(edu.utah.sci.vistrails.basic:String)', True)]
    
class AggregationMethod(String):
    _input_ports = [('value', '(gov.usgs.sahm:AggregationMethod:Other)')]
    _output_ports = [('value_as_string', '(edu.utah.sci.vistrails.basic:String)', True)]
    _widget_class = build_enum_widget('AggregationMethod', 
                                      ['Mean', 'Max', 'Min', 'Majority', 'None'])

    @staticmethod
    def get_widget_class():
        return AggregationMethod._widget_class

class ResampleMethod(String):
    _input_ports = [('value', '(gov.usgs.sahm:ResampleMethod:Other)')]
    _output_ports = [('value_as_string', '(edu.utah.sci.vistrails.basic:String)', True)]
    _widget_class = build_enum_widget('ResampleMethod', 
                                      ['NearestNeighbor', 'Bilinear', 'Cubic', 'CubicSpline', 'Lanczos'])

    @staticmethod
    def get_widget_class():
        return ResampleMethod._widget_class

class Predictor(Constant):
    _input_ports = [('categorical', '(edu.utah.sci.vistrails.basic:Boolean)'),
                    ('ResampleMethod', '(gov.usgs.sahm:ResampleMethod:Other)'),
                    ('AggregationMethod', '(gov.usgs.sahm:AggregationMethod:Other)'),
                    ('file', '(edu.utah.sci.vistrails.basic:Path)')]
    _output_ports = [('value', '(gov.usgs.sahm:Predictor:DataInput)'),
                     ('value_as_string', '(edu.utah.sci.vistrails.basic:String)', True)]

    def compute(self):
        #utils.breakpoint()
        if (self.hasInputFromPort("ResampleMethod")):
            resampleMethod = self.getInputFromPort("ResampleMethod")
            if resampleMethod.lower() not in ['nearestneighbor', 'bilinear', 'cubic', 'cubicspline', 'lanczos']:
                raise ModuleError(self, 
                                  "Resample Method not one of 'nearestneighbor', 'bilinear', 'cubic', 'cubicspline', or 'lanczos'")
        else:
            resampleMethod = 'Bilinear'
        
        if (self.hasInputFromPort("AggregationMethod")):
            aggregationMethod = self.getInputFromPort("AggregationMethod")
            if self.getInputFromPort("AggregationMethod").lower() not in ['mean', 'max', 'min', 'majority', 'none']:
                raise ModuleError(self, "No Aggregation Method specified")
        else:
            aggregationMethod = "Mean"
        
        if (self.hasInputFromPort("categorical")):
            if self.getInputFromPort("categorical") == True:
                categorical = '1'
            else:
                categorical = '0'
        else:
            categorical = '0'
        
        if (self.hasInputFromPort("file")):
            inFile = utils.getRasterName(self.getInputFromPort("file").name)
        else:
            raise ModuleError(self, "No input file specified")
        self.setResult('value', (inFile, categorical, resampleMethod, aggregationMethod))
   
class PredictorList(Constant):
    _input_ports = expand_ports([('value', 'Other|PredictorList'),
                                 ('addPredictor', 'DataInput|Predictor')])
    _output_ports = expand_ports([('value', 'Other|PredictorList')])
    
    @staticmethod
    def translate_to_string(v):
        return str(v)

    @staticmethod
    def translate_to_python(v):
        v_list = eval(v)
        return v_list

    @staticmethod
    def validate(x):
        return type(x) == list

    def compute(self):
        p_list = self.forceGetInputListFromPort("addPredictor")
        v = self.forceGetInputFromPort("value", [])
        
        b = self.validate(v)
        if not b:
            raise ModuleError(self, "Internal Error: Constant failed validation")
        if len(v) > 0 and type(v[0]) == tuple:
            f_list = [utils.create_file_module(v_elt[1]) for v_elt in v]
        else:
            f_list = v
        p_list += f_list
        #self.setResult("value", p_list)
        self.setResult("value", v)     

class PredictorListFile(Module):
    _input_ports = expand_ports([('csvFileList', '(edu.utah.sci.vistrails.basic:File)'),
                                 ('addPredictor', 'DataInput|Predictor')])
    _output_ports = expand_ports([('csvFileList', '(edu.utah.sci.vistrails.basic:File)')])
    '''
    copies the input predictor list csv to our working directory
    and appends any additionally added predictors
    '''
    @staticmethod
    def translate_to_string(v):
        return str(v)

    @staticmethod
    def translate_to_python(v):
        v_list = eval(v)
        return v_list

    @staticmethod
    def validate(x):
        return type(x) == list

    def compute(self):
        if not (self.hasInputFromPort("csvFileList") or
                self.hasInputFromPort("addPredictor")):
            raise ModuleError(self, "No inputs or CSV file provided")

        output_fname = utils.mknextfile(prefix='PredictorList_', suffix='.csv')
        if (self.hasInputFromPort("csvFileList") and 
            os.path.exists(self.getInputFromPort("csvFileList").name)):
            shutil.copy(self.getInputFromPort("csvFileList").name, 
                output_fname)
            csv_writer = csv.writer(open(output_fname, 'ab'))
        else:
            #create an empty file to start with.
            csv_writer = csv.writer(open(output_fname, 'wb'))
            csv_writer.writerow(["file", "Resampling", "Aggregation"])
        
        if self.hasInputFromPort("addPredictor"):
            p_list = self.forceGetInputListFromPort("addPredictor")
            for p in p_list:
                if p.hasInputFromPort('resampleMethod'):
                    resMethod = p.getInputFromPort('resampleMethod')
                else:
                    resMethod = "NearestNeighbor"
                if p.hasInputFromPort('aggregationMethod'):
                    aggMethod = p.getInputFromPort('aggregationMethod')
                else:
                    aggMethod = "Mean"  
                csv_writer.writerow([os.path.normpath(p.name), resMethod, aggMethod])

        del csv_writer
        
        output_file = utils.create_file_module(output_fname)
        self.setResult('csvFileList', output_file)
        
#        v = self.forceGetInputFromPort("value", [])
#        b = self.validate(v)
#        if not b:
#            raise ModuleError(self, "Internal Error: Constant failed validation")
#        if len(v) > 0 and type(v[0]) == tuple:
#            f_list = [create_file_module(v_elt[1]) for v_elt in v]
#        else:
#            f_list = v
#        p_list += f_list
#        self.setResult("value", p_list)

class TemplateLayer(Path):
#    _input_ports = [('FilePath', '(edu.utah.sci.vistrails.basic:File)')]
    _output_ports = [('value', '(gov.usgs.sahm:TemplateLayer:DataInput)')]
                     #('value_as_string', '(edu.utah.sci.vistrails.basic:String)', True)]
#    def compute(self):
#        output_file = create_file_module(self.forceGetInputFromPort('FilePath', []))
#        self.setResult('value', output_file)

#class SingleInputPredictor(Predictor):
#    pass
#
#class SpatialDef(Module):
#    _output_ports = [('spatialDef', '(gov.usgs.sahm:SpatialDef:DataInput)')]

class MergedDataSet(File):
    _input_ports = expand_ports([('mdsFile', '(edu.utah.sci.vistrails.basic:File)')])
    _output_ports = expand_ports([('value', '(gov.usgs.sahm:MergedDataSet:DataInput)')])
    
    pass

class ApplyModel(Module):
    _input_ports = [('mdsFile', '(gov.usgs.sahm:MergedDataSet:DataInput)'),
                    ('modelWorkspace', '(edu.utah.sci.vistrails.basic:File)'),
                    ('makeBinMap', '(edu.utah.sci.vistrails.basic:Boolean)'),
                    ('makeProbabilityMap', '(edu.utah.sci.vistrails.basic:Boolean)'),]
    _output_ports = [('BinaryMap', '(edu.utah.sci.vistrails.basic:File)'), 
                     ('ProbabilityMap', '(edu.utah.sci.vistrails.basic:File)')]
    
    
    
    def compute(self):
        
        workspace = self.forceGetInputFromPort('modelWorkspace').name
        output_dname = utils.mknextdir(prefix='AppliedModel_')
        if self.hasInputFromPort('mdsFile'):
            mdsFile = self.forceGetInputFromPort('mdsFile').name
            args = "ws=" + workspace + " c=" + mdsFile + " o=" + output_dname
        else:
            args = "ws=" + workspace + " o=" + output_dname 
        
        if self.hasInputFromPort('makeBinMap'):
            makeBinMap = self.forceGetInputFromPort('makeBinMap')
            args += ' mbt=' + str(makeBinMap).upper()
        else:
            args += ' mbt=TRUE'
            
        if self.hasInputFromPort('makeProbabilityMap'):
            makeProbabilityMap = self.forceGetInputFromPort('makeProbabilityMap')
            args += ' mpt=' + str(makeProbabilityMap).upper()
        else:
             args += ' mpt=TRUE'
                
        
        utils.runRScript('PredictModel.r', args, self)
        
        input_fname = os.path.join(output_dname, "prob_map.tif")
        output_fname = os.path.join(output_dname, 'prob_map.jpeg')
        if os.path.exists(input_fname):
            utils.tif_to_color_jpeg(input_fname, output_fname, color_breaks_csv)
            output_file1 = utils.create_file_module(output_fname)
            self.setResult('ProbabilityMap', output_file1)
        else:
            msg = "Expected output from ApplyModel was not found."
            msg += "\nThis likely indicates problems with the inputs to the R module."
            writetolog(msg, False, True)
            raise ModuleError(self, msg)
        
        if  os.path.exists(os.path.join(output_dname, "bin_map.tif")):
            outFileName = os.path.join(output_dname, "bin_map.tif")
            output_file2 = utils.create_file_module(outFileName)
            self.setResult('BinaryMap', output_file2)
        
        
        

class Model(Module):
    _input_ports = [('mdsFile', '(gov.usgs.sahm:MergedDataSet:DataInput)'),
                    ('makeBinMap', '(edu.utah.sci.vistrails.basic:Boolean)'),
                    ('makeProbabilityMap', '(edu.utah.sci.vistrails.basic:Boolean)'),
                    ('seed', '(edu.utah.sci.vistrails.basic:Integer)') 
                    ]
    _output_ports = [('modelWorkspace', '(edu.utah.sci.vistrails.basic:File)'), 
                     ('BinaryMap', '(edu.utah.sci.vistrails.basic:File)'), 
                     ('ProbabilityMap', '(edu.utah.sci.vistrails.basic:File)'),
                     ('AUC_plot', '(edu.utah.sci.vistrails.basic:File)'),
                     ('ResponseCurves', '(edu.utah.sci.vistrails.basic:File)'),
                     ('Text_Output', '(edu.utah.sci.vistrails.basic:File)')]

    def compute(self):
        global color_breaks_csv
        
        mdsFile = utils.dir_path_value(self.forceGetInputFromPort('mdsFile', []))
        
        ModelOutput = {"FIT_BRT_pluggable.r":"brt",
                       "FIT_GLM_pluggable.r":"glm",
                       "FIT_RF_pluggable.r":"rf",
                       "FIT_MARS_pluggable.r":"mars"}
        ModelAbbrev = ModelOutput[self.name]
        
        output_dname = utils.mknextdir(prefix=ModelAbbrev + 'output_')
        args = "c=" + mdsFile + " o=" + output_dname 
        args += " rc=" + utils.MDSresponseCol(mdsFile)
        if self.hasInputFromPort('makeBinMap'):
            makeBinMap = self.forceGetInputFromPort('makeBinMap')
            args += ' mbt=' + str(makeBinMap).upper()
        else:
            makeBinMap = True
            args += ' mbt=TRUE'
            
        if self.hasInputFromPort('makeProbabilityMap'):
            makeProbabilityMap = self.forceGetInputFromPort('makeProbabilityMap')
            args += ' mpt=' + str(makeProbabilityMap).upper()
        else:
            makeProbabilityMap = True
            args += ' mpt=TRUE'  
        
        if self.hasInputFromPort('seed'):
            args += ' seed=' + str(self.forceGetInputFromPort('seed'))
        
        utils.runRScript(self.name, args)
#        utils.breakpoint()
#        utils.runRScript('FIT_BRT_pluggableErrorMessage.r', args, self)
        
        input_fname = os.path.join(output_dname, ModelAbbrev + "_prob_map.tif")
        output_fname = os.path.join(output_dname, ModelAbbrev + "_prob_map.jpeg")
        if os.path.exists(input_fname):
            utils.tif_to_color_jpeg(input_fname, output_fname, color_breaks_csv)
            output_file4 = utils.create_file_module(output_fname)
            self.setResult('ProbabilityMap', output_file4)
        elif makeProbabilityMap == True:
            msg = "Expected output from " + ModelAbbrev + " was not found."
            msg += "\nThis likely indicates problems with the inputs to the R module."
            writetolog(msg, False, True)
            raise ModuleError(self, msg)
        
        if makeBinMap == True:
            outFileName = os.path.join(output_dname, ModelAbbrev + "_bin_map.tif")
            output_file1 = utils.create_file_module(outFileName)
            self.setResult('BinaryMap', output_file1)
        
        outFileName = os.path.join(output_dname, ModelAbbrev + "_output.txt")
        output_file2 = utils.create_file_module(outFileName)
        self.setResult('Text_Output', output_file2)
        
        outFileName = os.path.join(output_dname, ModelAbbrev + "_auc_plot.jpg")
#        print "out auc: ", outFileName
        output_file3 = utils.create_file_module(outFileName)
        self.setResult('AUC_plot', output_file3)
        
        outFileName = os.path.join(output_dname, ModelAbbrev + "_response_curves.pdf")
        output_file5 = utils.create_file_module(outFileName)
        self.setResult('ResponseCurves', output_file5)
        
        outFileName = os.path.join(output_dname, "modelWorkspace")
#        print "out auc: ", outFileName
        output_file6 = utils.create_file_module(outFileName)
        self.setResult('modelWorkspace', output_file6)
        
        writetolog("Finished " + ModelAbbrev   +  " builder\n", True, True) 
        
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

#class MAXENT(Model):
#    def __init__(self):
#        global models_path
#        Model.__init__(self)
#        self.name = 'RunMaxEnt.jar'

class BoostedRegressionTree(Model):
    def __init__(self):
        global models_path
        Model.__init__(self)
        self.name = 'FIT_BRT_pluggable.r'


    
class MDSBuilder(Module):

    _input_ports = expand_ports([('PredictorListCSV', '(edu.utah.sci.vistrails.basic:File)'),
                                 ('fieldData', '(gov.usgs.sahm:FieldData:DataInput)'),
                                 ('backgroundPointCount', '(edu.utah.sci.vistrails.basic:Integer)'),
                                 ('backgroundProbSurf', '(edu.utah.sci.vistrails.basic:File)')]
                                 )
    _output_ports = expand_ports([('mdsFile', '(gov.usgs.sahm:MergedDataSet:DataInput)')])

    def compute(self):

        if self.hasInputFromPort('PredictorListCSV'):
            inputsCSV = self.forceGetInputFromPort('PredictorListCSV').name
        else:
            raise ModuleError(self, "Missing required input from PredictorListCSV")
        #check to see if this is a simple list (default) format 
        #or if this has come from PARC and needs to be rearanged
        ourMDSBuilder = MDSB.MDSBuilder()
        ourMDSBuilder.logger = utils.getLogger()
        ourMDSBuilder.inputsCSV = inputsCSV
        ourMDSBuilder.fieldData = self.forceGetInputFromPort('fieldData').name
        ourMDSBuilder.outputMDS = utils.mknextfile(prefix='MergedDataset_', suffix='.csv')

        if self.hasInputFromPort('backgroundPointCount'):
            ourMDSBuilder.pointcount = self.forceGetInputFromPort('backgroundPointCount')

        if self.hasInputFromPort('backgroundProbSurf'):
            ourMDSBuilder.probsurf = self.forceGetInputFromPort('backgroundProbSurf').name

        if configuration.verbose:
            ourMDSBuilder.verbose = True

        writetolog("    inputsCSV=" + ourMDSBuilder.inputsCSV, False, False)
        writetolog("    fieldData=" + ourMDSBuilder.fieldData, False, False)
        writetolog("    outputMDS=" + ourMDSBuilder.outputMDS, False, False)
        
        try:
            ourMDSBuilder.run()
        except:
            utils.informative_untrapped_error(self, "MDSBuilder")

        output_file = utils.create_file_module(ourMDSBuilder.outputMDS)
        
        self.setResult('mdsFile', output_file)

class FieldDataQuery(Module):
    '''A widget implementation of the SAHM FieldDataQuery'''
    _input_ports = expand_ports([('templateLayer', '(gov.usgs.sahm:TemplateLayer:DataInput)'),
                                 ('fieldData', '(gov.usgs.sahm:FieldData:DataInput)'),
                                 ('aggregateRows', 'basic:Boolean'),
                                 ('aggregateRowsByYear', 'basic:Boolean')])
    _output_ports = expand_ports([('fieldData', '(gov.usgs.sahm:FieldData:DataInput)')])
    
    def compute(self):
        writetolog("\nRunning FieldDataQuery", True)
        output_fname = utils.mknextfile(prefix='FDQ_', suffix='.csv')
        writetolog("    output_fname=" + output_fname, True, False)
        ourFDQ = FDQ.FieldDataQuery()
        
        if configuration.verbose:
            ourFDQ.verbose = True
        ourFDQ.loggger = utils.getLogger()
            
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
    with a template dataset's properties. DI Test....
    '''

    #configuration = []
    _input_ports = [('predictor', "(gov.usgs.sahm:Predictor:DataInput)"),
                                ('PredictorList', '(gov.usgs.sahm:PredictorList:Other)'),
                                ('csvFileList', '(edu.utah.sci.vistrails.basic:File)'),
                                ('templateLayer', '(gov.usgs.sahm:TemplateLayer:DataInput)')]

    _output_ports = [('PredictorListCSV', '(edu.utah.sci.vistrails.basic:File)')]

    def compute(self):
        writetolog("\nRunning PARC", True)
        
        ourPARC = parc.PARC()
        output_dname = utils.mknextdir(prefix='PARC_')
        
        if configuration.verbose:
            ourPARC.verbose = True
        ourPARC.logger = utils.getLogger()
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
        #utils.breakpoint()
        ourPARC.inputsCSV = workingCSV
        ourPARC.template = self.forceGetInputFromPort('templateLayer').name
        writetolog('    template layer = ' + self.forceGetInputFromPort('templateLayer').name)

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

    #configuration = []
    _input_ports = [("inputMDS", "(gov.usgs.sahm:MergedDataSet:DataInput)"),
                                ('format', '(edu.utah.sci.vistrails.basic:String)'),]

    _output_ports = [('outputDir', '(edu.utah.sci.vistrails.basic:Directory)')]

    def compute(self):
        writetolog("\nRunning TiffConverter", True)
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
        
        utils.runRScript("TestTrainSplit.r", args, self)

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
        dialog = SelectListDialog(inputMDS, outputMDS, displayJPEG, configuration.r_path)
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
        del tmpCSV
        
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
        
        outputMDS = utils.mknextfile(prefix='ProjectionLayersMDS', suffix = '.csv')
        outCSV = csv.writer(open(outputMDS, 'wb'))
        outCSV.writerow(outHeader1)
        outCSV.writerow(outHeader2)
        outCSV.writerow(outHeader3)
        
        output_file = utils.create_file_module(outputMDS)
        writetolog("Finished Select Projection Layers widget", True)
        self.setResult("outputMDS", output_file)
        
        
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


def initialize():
    global maxent_path, color_breaks_csv
    global session_dir
    utils.config = configuration
    
    r_path = configuration.r_path
    maxent_path = configuration.maxent_path
    
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

    session_dir = utils.createrootdir(configuration.output_dir)
    utils.createLogger(session_dir, configuration.output_dir)
    #log_file = Utilities.createsessionlog(session_dir, configuration.verbose)
    
    color_breaks_csv = os.path.join(os.path.dirname(__file__),  "ColorBreaks.csv")
    
    load_max_ent_params()
    
    writetolog("*" * 79)
    writetolog("Initializing:", True, True)
    writetolog("  Locations of dependencies")
    writetolog("   layers csv = " + os.path.join(os.path.dirname(__file__), "layers.csv"))
    writetolog("   ColorBreaks csv = " + color_breaks_csv)
    writetolog("   R path = " + configuration.r_path)
    writetolog("   GDAL folder = " + configuration.gdal_path)
    writetolog("        Must contain subfolders proj, gdal-data, GDAL")
    writetolog("    ")
    writetolog("*" * 79)
    
    print "*" * 79
    print " output directory:   " + session_dir
    print "*" * 79
    print "*" * 79
    
def finalize():
    utils.cleantemps()#No longer used
    

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
#        print class_name
#        print PredictorList
#        print dir(PredictorList)
#        print widget_class
#        print config_class
#        print get_widget_method(widget_class)
        module = type(class_name, (PredictorList,),
                      {'get_widget_class': get_widget_method(widget_class),
                       '_input_ports': \
                           [('value',
                             '(gov.usgs.sahm:%s:DataInput)' % class_name, True)]})
        modules.append((module, {'configureWidgetType': config_class}))
    return modules

_modules = generate_namespaces({'DataInput': [
                                              Predictor,
                                              PredictorListFile,
                                              FieldData,
                                              TemplateLayer,
                                              MergedDataSet] + \
                                              build_predictor_modules(),
                                'Tools': [FieldDataQuery,
                                          MDSBuilder,
                                          PARC,
                                          TiffConverter,
                                          ProjectionLayers,
                                          TestTrainingSplit,
                                          CovariateCoorelationAndSelection,
                                          ApplyModel],
                                'Models': [GLM,
                                           RandomForest,
                                           MARS,
                                           MAXENT,
                                           BoostedRegressionTree],
                                'Other':  [Model,
                                           ResampleMethod,
                                           AggregationMethod,
                                           PredictorList,
#                                           ClimateModel,
#                                           ClimateScenario,
#                                           ClimateYear
],
                                })
