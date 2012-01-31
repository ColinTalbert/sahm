'''
'''
import csv
from datetime import datetime
import glob
import itertools
import os
import shutil
import sys
import subprocess
import traceback
import random

from core.modules.vistrails_module import Module, ModuleError, ModuleConnector
from core.modules.basic_modules import File, Directory, Path, new_constant, Constant
from packages.spreadsheet.basic_widgets import SpreadsheetCell, CellLocation
from packages.spreadsheet.spreadsheet_cell import QCellWidget, QCellToolBar

from core.modules.basic_modules import String

from PyQt4 import QtCore, QtGui

from widgets import get_predictor_widget, get_predictor_config

from SelectPredictorsLayers import SelectListDialog
from SelectAndTestFinalModel import SelectAndTestFinalModel

import utils
#import our python SAHM Processing files
import packages.sahm.pySAHM.FieldDataAggreagateAndWeight as FDAW
import packages.sahm.pySAHM.MDSBuilder as MDSB
import packages.sahm.pySAHM.PARC as parc
import packages.sahm.pySAHM.RasterFormatConverter as RFC
import packages.sahm.pySAHM.MaxentRunner as MaxentRunner
from packages.sahm.SahmOutputViewer import SAHMModelOutputViewerCell
from packages.sahm.SahmSpatialOutputViewer import SAHMSpatialOutputViewerCell
from packages.sahm.sahm_picklists import ResponseType, AggregationMethod, \
        ResampleMethod, PointAggregationMethod, ModelOutputType

from utils import writetolog
from pySAHM.utilities import TrappedError

identifier = 'gov.usgs.sahm' 

def menu_items():
    """ Add a menu item which allows users to specify their session directory
    and select and test the final model
    """
    def change_session_folder():
        global session_dir
        
        path = str(QtGui.QFileDialog.getExistingDirectory(None,
                                        'Browse to new session folder -'))
        session_dir = path
        utils.setrootdir(path)
        utils.createLogger(session_dir, configuration.output_dir)
        
        writetolog("*" * 79 + "\n" + "*" * 79)
        writetolog(" output directory:   " + session_dir)
        writetolog("*" * 79 + "\n" + "*" * 79)
    
    def select_test_final_model():
        global session_dir
        
        STFM  = SelectAndTestFinalModel(session_dir, configuration.r_path) 
        retVal = STFM.exec_()
        if retVal == 1:
            raise ModuleError(self, "Cancel or Close selected (not OK) workflow halted.")
    
    lst = []
    lst.append(("Change session folder", change_session_folder))
    lst.append(("Select and test the Final Model", select_test_final_model))
    return(lst)


#
#def expand_ports(port_list):
#    new_port_list = []
#    for port in port_list:
#        port_spec = port[1]
#        if type(port_spec) == str: # or unicode...
#            if port_spec.startswith('('):
#                port_spec = port_spec[1:]
#            if port_spec.endswith(')'):
#                port_spec = port_spec[:-1]
#            new_spec_list = []
#            for spec in port_spec.split(','):
#                spec = spec.strip()
#                parts = spec.split(':', 1)
##                print 'parts:', parts
#                namespace = None
#                if len(parts) > 1:
#                    mod_parts = parts[1].rsplit('|', 1)
#                    if len(mod_parts) > 1:
#                        namespace, module_name = mod_parts
#                    else:
#                        module_name = parts[1]
#                    if len(parts[0].split('.')) == 1:
#                        id_str = 'edu.utah.sci.vistrails.' + parts[0]
#                    else:
#                        id_str = parts[0]
#                else:
#                    mod_parts = spec.rsplit('|', 1)
#                    if len(mod_parts) > 1:
#                        namespace, module_name = mod_parts
#                    else:
#                        module_name = spec
#                    id_str = identifier
#                if namespace:
#                    new_spec_list.append(id_str + ':' + module_name + ':' + \
#                                             namespace)
#                else:
#                    new_spec_list.append(id_str + ':' + module_name)
#            port_spec = '(' + ','.join(new_spec_list) + ')'
#        new_port_list.append((port[0], port_spec) + port[2:])
##    print new_port_list
#    return new_port_list

class FieldData(Path): 
    '''
    Field Data

    The FieldData module allows a user to add presence/absence points or count data recorded across a
    landscape for the phenomenon being modeled (e.g., plant sightings, evidence of animal presence, etc.).
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
    layer used in the workflow. The column "responseCount" should be populated with either a '-9999'
    (indicating that the point is a background point) or a numerical value (either '0' or a positive integer)
    indicating the number of incidences of the phenomenon recorded at that point.
    '''   
#    _input_ports = [('csvFile', '(edu.utah.sci.vistrails.basic:File)')]
    _output_ports = [('value', '(gov.usgs.sahm:FieldData:DataInput)'),
                     ('value_as_string', '(edu.utah.sci.vistrails.basic:String)', True)]
    
    @classmethod
    def provide_input_port_documentation(cls, port_name):
        return utils.construct_port_msg(cls, port_name, 'in')
    @classmethod
    def provide_output_port_documentation(cls, port_name):
         return utils.construct_port_msg(cls, port_name, 'out') 
    
    def setDocstring(self, docstring=None):
        setattr(self, '__doc__', docstring)
    
class Predictor(Constant):
    '''
    Predictor
    
    The Predictor module allows a user to select a single raster layer for consideration in the
    modeled analysis. Four parameters must be specified by the user:
    
    1. Aggregation Method: The aggregation method to be used in the event that the raster layer
    must be up-scaled to match the template layer (e.g., generalizing a 10 m input layer to a
    100 m output layer). Care should be taken to ensure that the aggregation method that
    best preserves the integrity of the data is used.
    
    2. Resample Method: The resample method employed to interpolate new cell values when
    transforming the raster layer to the coordinate space or cell size of the template layer,
    if necessary. 
    
    3. Categorical (Boolean): Checking this box indicates that the data contained in the raster
    layer is categorical (e.g. landcover categories). Leaving this box unchecked indicates that
    the data contained in the raster is continuous (e.g., a DEM layer). This distinction is important
    in determining an appropriate resampling method.
    
    4. File Path: The location of the raster predictor file, which a user can specify by navigating to the
    file's location on their file system. When a user is selecting an ESRI grid raster, the user should navigate
    to the 'hdr.adf' file contained within the grid folder. 
    
    Accepted formats include:
        ESRI Grid ('.bil', or the folder, or 'hdr.adf' file in the folder, see above)
        GeoTiffs('.tif')
        Imagine('.img')
        ASCII('.asc')
        Bitmap('.bmp')
        Jpeg('.jpg')
        For additional formats see: the Compiled by default options at
                                    http://www.gdal.org/formats_list.html
        
    '''
    _input_ports = [('categorical', '(edu.utah.sci.vistrails.basic:Boolean)'),
                    ('ResampleMethod', '(gov.usgs.sahm:ResampleMethod:Other)', {'defaults':'Bilinear'}),
                    ('AggregationMethod', '(gov.usgs.sahm:AggregationMethod:Other)', {'defaults':'Mean'}),
                    ('file', '(edu.utah.sci.vistrails.basic:Path)')]
    _output_ports = [('value', '(gov.usgs.sahm:Predictor:DataInput)'),
                     ('value_as_string', '(edu.utah.sci.vistrails.basic:String)', True)]

    @classmethod
    def provide_input_port_documentation(cls, port_name):
        return utils.construct_port_msg(cls, port_name, 'in')
    @classmethod
    def provide_output_port_documentation(cls, port_name):
         return utils.construct_port_msg(cls, port_name, 'out') 

    def compute(self):
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
    '''
    This module is a required class for other modules and scripts within the
    SAHM package. It is not intended for direct use or incorporation into
    the VisTrails workflow by the user.
    '''
    _input_ports = [('value', '(gov.usgs.sahm:PredictorList:Other)'),
                                 ('addPredictor', '(gov.usgs.sahm:Predictor:DataInput)')]
    _output_ports = [('value', '(gov.usgs.sahm:PredictorList:Other)')]
    
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
    '''
    Predictor List File

    The PredictorListFile module allows a user to load a .csv file containing a list
    of rasters for consideration in the modeled analysis. The .csv file should contain
    a header row and four columns containing the following information, in order, for
    each raster input. 
    
    Column 1: The full file path to the input raster layer.
    
    Column 2: A binary value indicating whether the input layer is categorical or not.
    A value of "0" indicates that an input raster is non-categorical data (continuous),
    while a value of "1" indicates that an input raster is categorical data.
    
    Column 3: The resampling method employed to interpolate new cell values when
    transforming the raster layer to the coordinate space or cell size of the template
    layer, if necessary. The resampling type should be specified using one of the following
    values: "nearestneighbor," "bilinear," "cubic," or "lanczos."
    
    Column 4: The aggregation method to be used in the event that the raster layer
    must be up-scaled to match the template layer (e.g., generalizing a 10 m input layer to a
    100 m output layer). Care should be taken to ensure that the aggregation method that
    best preserves the integrity of the data is used. The aggregation should be specified
    using one of the following values: "Min," "Mean," "Max," "Majority," or "None."

    In formatting the list of predictor files, the titles assigned to each of the columns
    are unimportant as the module retrieves the information based on the order of the
    values in the .csv file (the ordering of the information and the permissible values
    in the file however, are strictly enforced). The module also anticipates a header row
    and will ignore the first row in the .csv file.

    '''
    _input_ports = [('csvFileList', '(edu.utah.sci.vistrails.basic:File)'),
                                 ('predictor', "(gov.usgs.sahm:Predictor:DataInput)")]
    _output_ports = [('RastersWithPARCInfoCSV', '(gov.usgs.sahm:RastersWithPARCInfoCSV:Other)')]

    #copies the input predictor list csv to our working directory
    #and appends any additionally added predictors

    @classmethod
    def provide_input_port_documentation(cls, port_name):
        return utils.construct_port_msg(cls, port_name, 'in')
    @classmethod
    def provide_output_port_documentation(cls, port_name):
         return utils.construct_port_msg(cls, port_name, 'out') 

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
        self.setResult('RastersWithPARCInfoCSV', output_file)
        
class TemplateLayer(Path):
    '''
    Template Layer

    The TemplateLayer is a raster data layer with a defined coordinate system, a known cell size,
    and an extent that defines the study area. This raster layer serves as the template for all
    the other inputs in the analysis. All additional raster layers used in the analysis will be
    resampled and reprojected as needed to match the template, snapped to the template, and
    clipped to have an extent that matches the template. Users should ensure that any additional
    layers considered in the analysis have coverage within the extent of the template layer.

    The TemplateLayer is a required input for the PARC module.

    '''
#    _input_ports = [('FilePath', '(edu.utah.sci.vistrails.basic:File)')]
    _output_ports = [('value', '(gov.usgs.sahm:TemplateLayer:DataInput)'),
                     ('value_as_string', '(edu.utah.sci.vistrails.basic:String)', True)]
    
    @classmethod
    def provide_input_port_documentation(cls, port_name):
        return utils.construct_port_msg(cls, port_name, 'in')
    @classmethod
    def provide_output_port_documentation(cls, port_name):
         return utils.construct_port_msg(cls, port_name, 'out') 
    
#    def compute(self):
#        output_file = create_file_module(self.forceGetInputFromPort('FilePath', []))
#        self.setResult('value', output_file)

#class SingleInputPredictor(Predictor):
#    pass
#
#class SpatialDef(Module):
#    _output_ports = [('spatialDef', '(gov.usgs.sahm:SpatialDef:DataInput)')]

class MergedDataSet(File):
    '''
    This module is a required class for other modules and scripts within the
    SAHM package. It is not intended for direct use or incorporation into
    the VisTrails workflow by the user.
    '''
    _input_ports = [('mdsFile', '(edu.utah.sci.vistrails.basic:File)'),]
    _output_ports = [('value', '(gov.usgs.sahm:MergedDataSet:Other)'),]
    
    pass
    
class RastersWithPARCInfoCSV(File):
    '''
    This module is a required class for other modules and scripts within the
    SAHM package. It is not intended for direct use or incorporation into
    the VisTrails workflow by the user.
    '''
    _input_ports = [('mdsFile', '(edu.utah.sci.vistrails.basic:File)'),]
    _output_ports = [('value', '(gov.usgs.sahm:MergedDataSet:Other)'),]
    
    pass
#    def compute(self, is_input=None):
#        PersistentPath.compute(self, is_input, 'blob')

#removed for the V1.0 release
#class ApplyModel(Module):
#    '''
#    Apply Model
#
#    The ApplyModel module allows the user to apply a model developed using a
#    particular package within the workflow and generate output probability and binary
#    maps. The process of creating an output probability map and binary map based on
#    a particular model can be time-consuming, depending on the input data. By
#    existing as a stand-alone process in the workflow, the ApplyModel module allows
#    a user to investigate the performance metrics for a particular model (such as
#    the ROC curve or the AUC) before dedicating the processing time needed to generate
#    the output maps. In most cases, a user will "fine tune" a model by exploring the
#    performance metrics of different model iterations before applying the model and
#    generating the actual maps as a final step.
#    
#    The ApplyModel module also provides the user with the option of projecting the results
#    of a model developed from one set of environmental predictors onto a new modeled space
#    containing that same set of environmental predictors but representing data captured at
#    a different temporal or spatial location. For example, a user could generate a model
#    predicting habitat suitability using recorded presence points and certain environmental
#    predictors such as elevation, landcover, and proximity to water in one geographic
#    location. Based on the training from this information, the modeled results could be
#    generated for (or "projected to") a new location based on the range of values seen in
#    elevation, landcover, and proximity to water in the second geographic area. Similarly,
#    modeling predicted results through time is also possible. A model trained using field
#    data and a set of predictor layers representative of one time period could be projected
#    onto the same geographical area using a new set of layers corresponding to the same
#    predictors but representing data from a different time period (e.g., different climate
#    data).
#
#    The ApplyModel module accepts two inputs from the user:
#
#    1. Model Workspace: The model workspace field accepts as an input a modeling
#    package element (complete with all required parameters) from upstream in the workflow.
#    The inputs and specifications provided in the dialogue fields of the model will be applied
#    and used to generate the output maps. A user should populate the model workspace field by
#    connecting a model element to the appropriate input port of the ApplyModel module in the
#    visual display of the model workflow.
#    
#    2. Projection Target: The projection target is an optional parameter that allows a user to
#    apply a model to a particular geographic area and or set of predictors (other than those
#    used to train the model) and create output maps within the spatial extent of the projection
#    target layers. This input field should be populated by connecting either the output of the
#    ProjectionLayers module or a separate MDSBuilder element to the appropriate input port of
#    the ApplyModel module.
#    
#    '''
#    
#    _input_ports = [('projectionTarget', '(gov.usgs.sahm:MergedDataSet:Other)'),
#                    ('modelWorkspace', '(edu.utah.sci.vistrails.basic:File)'),
#                    ('makeBinMap', '(edu.utah.sci.vistrails.basic:Boolean)'),
#                    ('makeProbabilityMap', '(edu.utah.sci.vistrails.basic:Boolean)'),]
#    _output_ports = [('BinaryMap', '(edu.utah.sci.vistrails.basic:File)'), 
#                     ('ProbabilityMap', '(edu.utah.sci.vistrails.basic:File)')]
#    
#    
#    
#    def compute(self):
#        
#        workspace = self.forceGetInputFromPort('modelWorkspace').name
#        output_dname = utils.mknextdir(prefix='AppliedModel_')
#        if self.hasInputFromPort('projectionTarget'):
#            mdsFile = self.forceGetInputFromPort('projectionTarget').name
#            args = "ws=" + '"' + workspace + '"' + " c=" + '"' + mdsFile + '"' + " o=" + '"' + output_dname + '"'
#        else:
#            args = "ws=" + '"' + workspace + '"' + " o=" + '"' + output_dname + '"'
#        
#        if self.hasInputFromPort('makeBinMap'):
#            makeBinMap = self.forceGetInputFromPort('makeBinMap')
#            args += ' mbt=' + str(makeBinMap).upper()
#        else:
#            args += ' mbt=TRUE'
#            
#        if self.hasInputFromPort('makeProbabilityMap'):
#            makeProbabilityMap = self.forceGetInputFromPort('makeProbabilityMap')
#            args += ' mpt=' + str(makeProbabilityMap).upper()
#        else:
#             args += ' mpt=TRUE'
#                
#        
#        utils.runRScript('PredictModel.r', args, self)
#        
#        input_fname = os.path.join(output_dname, "prob_map.tif")
#        output_fname = os.path.join(output_dname, 'prob_map.jpeg')
#        if os.path.exists(input_fname):
#            utils.tif_to_color_jpeg(input_fname, output_fname, color_breaks_csv)
#            output_file1 = utils.create_file_module(output_fname)
#            self.setResult('ProbabilityMap', output_file1)
#        else:
#            msg = "Expected output from ApplyModel was not found."
#            msg += "\nThis likely indicates problems with the inputs to the R module."
#            writetolog(msg, False, True)
#            raise ModuleError(self, msg)
#        
#        if  os.path.exists(os.path.join(output_dname, "bin_map.tif")):
#            outFileName = os.path.join(output_dname, "bin_map.tif")
#            output_file2 = utils.create_file_module(outFileName)
#            self.setResult('BinaryMap', output_file2)
        
class Model(Module):
    '''
    This module is a required class for other modules and scripts within the
    SAHM package. It is not intended for direct use or incorporation into
    the VisTrails workflow by the user.
    '''
    _input_ports = [('mdsFile', '(gov.usgs.sahm:MergedDataSet:Other)'),
                    ('makeBinMap', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'True', 'optional':False}),
                    ('makeProbabilityMap', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'True', 'optional':False}),
                    ('makeMESMap', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'False', 'optional':False}),
                    ('ThresholdOptimizationMethod', '(edu.utah.sci.vistrails.basic:Integer)', {'defaults':'2', 'optional':False})
                    ]
    _output_ports = [('modelWorkspace', '(edu.utah.sci.vistrails.basic:File)'), 
                     ('BinaryMap', '(edu.utah.sci.vistrails.basic:File)'), 
                     ('ProbabilityMap', '(edu.utah.sci.vistrails.basic:File)'),
                     ('ResidualsMap', '(edu.utah.sci.vistrails.basic:File)'),
                     ('MessMap', '(edu.utah.sci.vistrails.basic:File)'),
                     ('MoDMap', '(edu.utah.sci.vistrails.basic:File)'),
                     ('modelEvalPlot', '(edu.utah.sci.vistrails.basic:File)'),
                     ('ResponseCurves', '(edu.utah.sci.vistrails.basic:File)'),
                     ('Text_Output', '(edu.utah.sci.vistrails.basic:File)')]

    @classmethod
    def provide_input_port_documentation(cls, port_name):
        return utils.construct_port_msg(cls, port_name, 'in')
    @classmethod
    def provide_output_port_documentation(cls, port_name):
         return utils.construct_port_msg(cls, port_name, 'out') 

    def compute(self):
        
        ModelOutput = {"FIT_BRT_pluggable.r":"brt",
                       "FIT_GLM_pluggable.r":"glm",
                       "FIT_RF_pluggable.r":"rf",
                       "FIT_MARS_pluggable.r":"mars"}
        self.ModelAbbrev = ModelOutput[self.name]
        
        self.output_dname = utils.mknextdir(prefix=self.ModelAbbrev + '_')
        self.argsDict = utils.map_ports(self, self.port_map)

        mdsFile = self.forceGetInputFromPort('mdsFile').name
        
        args = ''
        if self.ModelAbbrev == 'brt' or \
            self.ModelAbbrev == 'rf':
            if not "seed" in self.argsDict.keys():
                self.argsDict['seed'] = random.randint(-1 * ((2**32)/2 - 1), (2**32)/2 - 1)
            writetolog("    seed used for " + self.ModelAbbrev + " = " + str(self.argsDict['seed']))
            args += " seed=" + str(str(self.argsDict['seed']))
        
        for k, v in self.argsDict.iteritems():
            if k == 'c':
                args += ' ' + '='.join([str(k),'"' + str(v) + '"'])
            else:
                args += ' ' + '='.join([str(k),str(v)])
        args += " o=" + '"' + self.output_dname + '"'
        args += " rc=" + utils.MDSresponseCol(mdsFile)
                
        utils.runRScript(self.name, args, self)
        
        if not self.argsDict.has_key('mes'):
            self.argsDict['mes'] = 'FALSE'
        self.setModelResult(self.ModelAbbrev + "_prob_map.tif", 'ProbabilityMap', 'mpt')
        self.setModelResult(self.ModelAbbrev + "_bin_map.tif", 'BinaryMap', 'mbt')
        self.setModelResult(self.ModelAbbrev + "_resid_map.tif", 'ResidualsMap', 'mes')
        self.setModelResult(self.ModelAbbrev + "_mess_map.tif", 'MessMap', 'mes')
        self.setModelResult(self.ModelAbbrev + "_MoD_map.tif", 'MoDMap', 'mes')
        self.setModelResult(self.ModelAbbrev + "_output.txt", 'Text_Output')
        self.setModelResult(self.ModelAbbrev + "_modelEvalPlot.jpg", 'modelEvalPlot') 
        self.setModelResult(self.ModelAbbrev + "_response_curves.pdf", 'ResponseCurves')
        self.setModelResult("modelWorkspace", 'modelWorkspace')        
        writetolog("Finished " + self.ModelAbbrev   +  " builder\n", True, True)
        
    def setModelResult(self, filename, portname, arg_key=None):
        outFileName = os.path.join(self.output_dname, filename)
        required = not (self.argsDict.has_key(arg_key) and 
                        self.argsDict[arg_key].lower() == 'false')
        if required and not os.path.exists(outFileName):
            msg = "Expected output from " + self.ModelAbbrev + " was not found."
            msg += "\nSpecifically " + outFileName + " was missing."
            msg += "\nThis might indicate problems with the inputs to the R module."
            msg += "\nCheck the console output for additional R warnings "
            writetolog(msg, False, True)
            raise ModuleError(self, msg)
            
        output_file = utils.create_file_module(outFileName)
        self.setResult(portname, output_file)
        
class GLM(Model):
    '''
     Generalized Linear Model (GLM)
     
        This is basically linear regression adapted to binary presence-absence or count data.  We used a bidirectional 
    stepwise procedure to select covariates to be used in the model.  That is, we began with a null model and calculated 
    the AIC (Akaike Information Criterion) score for each covariate which could be added to the model.  AIC is a measure 
    of how well the model fits the data with a penalty based on the number of covariates in the model.  In the first step, 
    we add the covariate with the best AIC score.  In the next step we calculate AIC scores for all two-covariate models 
    and again add the covariate that most improves the AIC, and so on.  At each step, we also look at the change in AIC 
    from dropping each covariate currently in the model.  The stepwise procedure ends when no additions or removals 
    result in an improvement in AIC.  
    
    Inputs:  
            1.  mdsFile:  This is the input data set consisting of locational data for each sample point, the values of each predictor variable at those points. 
               This input file is almost always generated by the upstream steps.  The following information regarding the file contents is fairly technical and 
               can be ignored by most users.   Several names are reserved and have special meaning in the model fitting algorithms and not be used as predictors.  
               These names appear in the first row of the csv.     
    
            2.  MakeProbabilityMap: Indicate whether a probability map is to be produced for the model fit. 
            3.	makeBinMap: Indicate whether to discretize the continues probability map into presence absence 
                see the ThresholdOptimizationMethod for how this is done.  
            4.	makeMESMap: Indicate whether to produce a multivariate Environmental similarity surface (MESS) 
                and a map of which factor is limiting at each point see Elith et. al. 2010 for more details.  
                If time is a concern and many models are to be fit and assessed  maps can be produced after model 
                selection for only the best models using the Select and Test the Final Model tool.  Options are 
                available for producing Probability, Binary and MESS maps there as well.    
            5.	ThresholdOptimizationMethod:  determines how the threshold is set between to discretize continuous predictions into binary for evaluation 
                metrics calculated based on the confusion matrix as well as for the binary map. The options, directly from the PresenceAbsences package in R are: 
                   1:  Threshold=0.5
                   2:  Sens=Spec sensitivity=specificity
                   3:  MaxSens+Spec maximizes (sensitivity+specificity)/2
                   4:  MaxKappa maximizes Kappa
                   5:  MaxPCC maximizes PCC (percent correctly classified)
                   6:  PredPrev=Obs predicted prevalence=observed prevalence
                   7:  ObsPrev threshold=observed prevalence
                   8:  MeanProb mean predicted probability
                   9:  MinROCdist minimizes distance between ROC plot and (0,1)
    
                This is set to 2 by default so it uses the threshold that results in the same sensitivity and specificity as nearly as possible.  The value 
                calculated for the train portion of the data will be applied to the test portion and if cross validation was specified, the value is calculated 
                separately for each fold using the threshold from the training data and applying it to the test data for each combination of folds.  
            6. Simplification Method: This can be set either to AIC or BIC and alters the decision rule governing how the model is pruned in the stepwise model selection step.   
    
    Outputs: 
    Several files should be produced by a successful model run while an unsuccessful model run will produce a log file which will indicate the issue that caused execution 
    to halt and possibly some other files depending on where in the workflow execution was halted.  When a model is successful the following files are produce in a folder 
    in the vistrails workspace:
            1.	model_output.txt : This file contains a summary of  the model fit.  The information contained here includes the number of presence observations 
               (counts equal to or greater than 1 for count models), the number of absence points, the number of covariates that were considered by the model selection 
               algorithm.  Note all of these can differ from the numbers in the original .mds due to incomplete records being deleted, and predictors with only one unique 
               value being removed.  The random number seed is recorded if applicable which allows completely reproducible results as well as a summary of the model fit.  
               Evaluation Statistics are reported for the data used to fit the model as well as for the test or cross-validation split if applicable.  References for 
               how to interpret most of these are ubiquitous in the literature but it is worth mentioning that interpretation of the calibration statistics is described 
               by Pearce and Ferrier 2000 as well as Miller and Hui 1991.  
            2.	model_modelEvalPlot.jpg : For binary data this will be a Receiver operating characteristic curve.  Which shows the relationship between sensitivity 
               and specificity as threshold for discretizing continuous predictions into presence absence is varied.  The threshold selected using the specified 
               ThresholdOptimizationMethod is shown.  If a model selection test\training split was specified the ROC curve for this will be shown in red and if a 
               cross validation split was specified a blue region will show the standard deviation for the cross validation folds.  If the model fits well both 
               sensitivity and specificity should be well above the diagonal line.  If there is a strong disparity between the curves for the training data and either 
               the testing split or cross validation standard deviation curves this can be indicative of model overfitting.  These plots and the evaluation metrics based 
               on the confusion matrix describe the models ability to discriminate between presence and absence points.  The AUC value, or area under the ROC curve, is the 
               probability that the model will rank a randomly chosen presence observation higher than a randomly chosen absence observation.  For count data this display 
               will show several standard plots for assessment of model residuals .     
            3.	model_CalibrationPlot: Calibration plots and statistics describe the goodness-of-fit between the predicted values and the actual observations.  These are 
               especially usefull for identifying problems with overfitting or underfitting when separate data is used for model fitting and model evaluation.  These plots 
               and statistics can be used to determine how reliably the model will predict if a site is occupied or unoccupied (Pearce and Ferrier 2000).  The calibration 
               plot shows the predicted probability of occurrence plotted against the actual proportions of occurrence for each of 5 bins along the probability axis.  
               A logistic regression model is fit to the logits of the predicted probabilities of occurrence and is shown on the plot.  This line is a logistic curve because we 
               are not using the logit transform of the predicted and observed.  The intercept and slope of this line should be 0 and 1 respectively.  Test statistics are reported 
               and significant values indicate that the predicted values have a different mean or spread than the observed data.  The five plots below show several different patterns 
               that can be identified using calibration plots and statistics.  Currently calibration plots are only produced for presence absence models.      
            4.	model.confusion.matrix.jpg:  The confusion matrix shows the percent of predicted and observed values in each of the presence and absence classes.  For predicted values 
               this is  based on the threshold used to discritize the predicted values.  If a test\training split or cross validation was specified then  the percentages for the training 
               split and for the test or total for each evaluation set fold will be shown in the same plot.   Several evaluation metrics are based on the discritization of the continuous 
               predictions and could be seen as accompanying this plot.  These include the percent correctly classified, sensitivity, specificity, Cohen's kappa, and the true skill statistic.    
               The calibration plot and related statisticics  are only reported for  presence absence models.
            5.	model_response_curves.pdf:   Model response curves show the relationship between each predictor included in the model, while holding all other predictors constant at 
               their means, and the fitted values.  MARS response curves are shown on a logit scale thus the response axis will not necessarily be bounded on the 0 to 1 interval.  
               BRT response curves will show response surfaces for any interaction terms included in the final model along with the percent relative influence.  
            6.	model.resid.plot:  Model residual plots show the spatial relationship between the model deviance residuals.  Most models assume residuals will be independent thus 
               spatial pattern in the deviance residuals can be indicative of a problem with the model fit and inference based on the fit.  It can for example indicate that important 
               predictors were not included in the model and can be compared with the spatial pattern of predictors that were not included in the model.  Whether or not a significant 
               spatial pattern exists in model residuals can at times be difficult to assess visually.  We hope to add correlograms of Moran's I soon.  Unfortunately statistical tests 
               based on the Moran's I statistic for residuals of binary response models lack statistical justification and thus cannot be used to test for a significant spatial pattern 
               (Bivand 2008).   See Dormann 2007 for more discussion on evaluation of model residuals and spatial models that are appropriate for species distribution modeling.  
               Residual plots can also be used to determine if certain observations contribute disproportionately to the deviance of the fitted model.  For a binary response model 
               deviance residuals with absolute values greater than 2 can be indicative of a problem.      
            7.	model_prob_map.tif:  If specified using MakeProbabilityMap=TRUE then a surface of predicted values is produced based on the tiffs in the input .mds file and the 
               fitted model.  These can but do not always indicate the probability of finding the species at a given site.  For example if model calibration is poor then these 
               will not agree well with the true probabilities though discrimination between presence and absences might still be good.     
            8.	model_bin_map.tif:  If specified using MakeBinaryMap=TRUE then a surface of binary observations is produced by discretizing  the probability map based on the 
               selected threshold.  This map indicates whether one could expect each site to be occupied or unoccupied based on the model.    
            9.	model_MESS_map and  model_Mod_Map: If specified by selecting makeMESMap=TRUE the the MESS and MOD surfaces will be produced.  The MESS surface is the multivariate 
               environment similarity surface and shows how well each point fits into the univariate ranges of the points for which the model was fit.  Negative values in this map 
               indicate that the point is out of the range of the training data.  The MoD map is related and indicates which variable was furthest from the range over which the model 
               was.  The MESS map takes the minimum value of a statistic calculate for each predictor and thus cannot diagnose hidden extrapolation as one might do using a hat matrix.  
               This surface is only calculated for variables that are selected in the model selection step within each model fitting algorithm so that variables that do not significantly 
               affect the occurrence of the organism over the range of the training data will not be included in the MESS map even though these predictors might be significant outside 
               the range in which the model was fit.  Random Forest never drops predictors so if one wishes to compare the MESS and Mod map before and after insignificant predictors 
               were dropped, one can compare the MESS map of a Random Forest fit to that produced from the other model fit algorithms as long as they were fit using the same dataset.  
               See Elith et. al. 2010 for details on how the MESS map calculations are performed.    
            10.	Evaluation metrics appended to AppendOutput .csv and .jpg: An appended output csv is produced to track several evaluation metrics across model runs and if at least 
               two models have been run then a .jpg will accompany this.  The name of the csv indicates the type of response as well as the type of model selection split that was specified 
               and separate .csv's and .jpg's will be produced for each combination so that a folder might contain any combination of:
                 AcrossModelNoSplitBinom
                 AcrossModelNoSplitCount
                 AcrossModelTestTrainBinom
                 AcrossModelTestTrainCount
                 AcrossModelCrossValBinom 
                 AcrossModelCrossValCount
               This plot will be used to select and evaluate the models and is discussed in more detail in  the Select and Test the Final Model section.  
    '''

    
    _input_ports = list(Model._input_ports)
    _input_ports.extend([('SimplificationMethod', '(edu.utah.sci.vistrails.basic:String)', {'defaults':'AIC', 'optional':True}),
                         ]) 
    def __init__(self):
        global models_path
        Model.__init__(self) 
        self.name = 'FIT_GLM_pluggable.r'
        self.port_map = {'mdsFile':('c', None, True),#These ports are for all Models
                         'makeProbabilityMap':('mpt', utils.R_boolean, False),
                         'makeBinMap':('mbt', utils.R_boolean, False),
                         'makeMESMap':('mes', utils.R_boolean, False),
                         'ThresholdOptimizationMethod':('om', None, False),
                         'SimplificationMethod':('sm', None, False) #This is a GLM specific port
                         }

class RandomForest(Model):
    _input_ports = list(Model._input_ports)
    _input_ports.extend([('Seed', '(edu.utah.sci.vistrails.basic:Integer)', {'optional':True}),
                         ('mTry', '(edu.utah.sci.vistrails.basic:Integer)', {'defaults':'1', 'optional':True}),
                         ('nTrees', '(edu.utah.sci.vistrails.basic:Integer)', {'optional':True}),
                         ('nodesize', '(edu.utah.sci.vistrails.basic:Integer)', {'optional':True}),
                         ('replace', '(edu.utah.sci.vistrails.basic:Boolean)', {'optional':True}),
                         ('maxnodes', '(edu.utah.sci.vistrails.basic:Integer)', {'optional':True}),
                         ('importance', '(edu.utah.sci.vistrails.basic:Boolean)', {'optional':True}),
                         ('localImp', '(edu.utah.sci.vistrails.basic:Boolean)', {'optional':True}),
                         ('proximity', '(edu.utah.sci.vistrails.basic:Boolean)', {'optional':True}),
                         ('oobProx', '(edu.utah.sci.vistrails.basic:Boolean)', {'optional':True}),
                         ('normVotes', '(edu.utah.sci.vistrails.basic:Boolean)', {'optional':True}),
                         ('doTrace', '(edu.utah.sci.vistrails.basic:Boolean)', {'optional':True}),
                         ('keepForest', '(edu.utah.sci.vistrails.basic:Boolean)', {'optional':True}),
                         ]) 
    def __init__(self):
        global models_path
        Model.__init__(self)
        self.name = 'FIT_RF_pluggable.r'
        self.port_map = {'mdsFile':('c', None, True),#These ports are for all Models
                         'makeProbabilityMap':('mpt', utils.R_boolean, False),
                         'makeBinMap':('mbt', utils.R_boolean, False),
                         'makeMESMap':('mes', utils.R_boolean, False), 
                         'ThresholdOptimizationMethod':('om', None, False),
                         'Seed':('seed', None, False), #This is a BRT specific port
                         'mTry': ('mtry', None, False), #This is a Random Forest specific port
                         'nodesize': ('nodeS', None, False), #This is a Random Forest specific port
                         'replace': ('sampR', utils.R_boolean, False), #This is a Random Forest specific port
                         'maxnodes': ('maxN', None, False), #This is a Random Forest specific port
                         'importance': ('impt', utils.R_boolean, False), #This is a Random Forest specific port
                         'localImp': ('locImp', utils.R_boolean, False), #This is a Random Forest specific port
                         'proximity': ('prox', utils.R_boolean, False), #This is a Random Forest specific port
                         'oobPorx': ('oopp', utils.R_boolean, False), #This is a Random Forest specific port
                         'normVotes': ('nVot', utils.R_boolean, False), #This is a Random Forest specific port
                         'doTrace': ('Trce', utils.R_boolean, False), #This is a Random Forest specific port
                         'keepForest': ('kf', utils.R_boolean, False), #This is a Random Forest specific port
                         }

class MARS(Model):
    _input_ports = list(Model._input_ports)
    _input_ports.extend([('MarsDegree', '(edu.utah.sci.vistrails.basic:Integer)', {'defaults':'1', 'optional':True}),
                          ('MarsPenalty', '(edu.utah.sci.vistrails.basic:Integer)', {'defaults':'2', 'optional':True}),
                          ])
    def __init__(self):
        global models_path        
        Model.__init__(self)
        self.name = 'FIT_MARS_pluggable.r'
        self.port_map = {'mdsFile':('c', None, True),#These ports are for all Models
                         'makeProbabilityMap':('mpt', utils.R_boolean, False),
                         'makeBinMap':('mbt', utils.R_boolean, False),
                         'makeMESMap':('mes', utils.R_boolean, False), 
                         'ThresholdOptimizationMethod':('om', None, False),
                         'MarsDegree':('deg', None, False), #This is a MARS specific port
                         'MarsPenalty':('pen', None, False), #This is a MARS specific port
                         }

class BoostedRegressionTree(Model):
    _input_ports = list(Model._input_ports)
    _input_ports.extend([('Seed', '(edu.utah.sci.vistrails.basic:Integer)', {'optional':True}),
                              ('TreeComplexity', '(edu.utah.sci.vistrails.basic:Integer)', {'optional':True}),
                              ('BagFraction', '(edu.utah.sci.vistrails.basic:Float)', {'defaults':'0.5', 'optional':True}),
                              ('NumberOfFolds', '(edu.utah.sci.vistrails.basic:Integer)', {'defaults':'3', 'optional':True}),
                              ('Alpha', '(edu.utah.sci.vistrails.basic:Float)', {'defaults':'1', 'optional':True}),
                              ('PrevalenceStratify', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'True', 'optional':True}),
                              ('ToleranceMethod', '(edu.utah.sci.vistrails.basic:String)', {'defaults':'auto', 'optional':True}),
                              ('Tolerance', '(edu.utah.sci.vistrails.basic:Float)', {'defaults':'0.001', 'optional':True}),
                              ('LearningRate', '(edu.utah.sci.vistrails.basic:Float)', {'optional':True}),
                              ('MaximumTrees', '(edu.utah.sci.vistrails.basic:Integer)', {'optional':True}),
                              ])
    def __init__(self):
        global models_path
        Model.__init__(self)
        self.name = 'FIT_BRT_pluggable.r'
        self.port_map = {'mdsFile':('c', None, True),#These ports are for all Models
                         'makeProbabilityMap':('mpt', utils.R_boolean, False),
                         'makeBinMap':('mbt', utils.R_boolean, False),
                         'makeMESMap':('mes', utils.R_boolean, False), 
                         'ThresholdOptimizationMethod':('om', None, False),
                         'Seed':('seed', None, False), #This is a BRT specific port
                         'TreeComplexity':('tc', None, False), #This is a BRT specific port
                         'BagFraction':('bf', None, False), #This is a BRT specific port
                         'NumberOfFolds':('nf', None, False), #This is a BRT specific port
                         'Alpha':('alp', None, False), #This is a BRT specific port
                         'PrevalenceStratify':('ps', None, False), #This is a BRT specific port
                         'ToleranceMethod':('tolm', None, False), #This is a BRT specific port
                         'Tolerance':('tol', None, False), #This is a BRT specific port
                         'LearningRate':('lr', None, False), #This is a BRT specific port
                         'MaximumTrees':('mt', None, False), #This is a BRT specific port
                         }
   
class MDSBuilder(Module):
    '''
    MDS Builder

    The Merged Data Set (MDS) Builder module is a utility that extracts the values of each predictor
    layer to the point locations included in the field data set. The module produces a .csv file that
    contains the x and y locations of the sample points and a column indicating whether each point
    represents a presence recording, an absence recording, a presence count, or a background point.
    Following these first three columns, each environmental predictor layer is appended as a column
    with row entries representing the value present in the raster layer at each field sample point.
    There are a total of three header rows in the output .csv of the MDSBuilder. The first row contains
    the columns "x," "y," "ResponseBinary" or "ResponseCount," and the names of each of the raster
    predictor files that were passed to the MDS Builder. The second row contains a binary value
    indicating whether the column should be included when the model is finally applied; these values
    are later modified during the Covariate Correlation and Selection process that takes place downstream
    in the workflow. The final header row contains the full path on the file system to each of the raster
    predictor files.

    The MDSBuilder accepts four inputs from the user:

    1. Rasters with PARC Info .csv File: The raster layers listed in this file will have their values
    extracted to the points in the field data .csv file. This parameter should be supplied by connecting
    the output of a PARC module within the workflow to the MDSBuilder module. (In order to properly
    extract the values of each predictor layer to the field data points, all the layers must have matching
    coordinate systems and cell sizes; outputs from the PARC module will have had the prerequisite processing).
    
    2. Background Point Count: This is an optional value that applies only to workflows that employ the
    Maxent modeling package. The dialogue box provides the option of specifying a number of background points
    to be randomly scattered throughout the study area (the extent of the template layer) to capture a more
    complete sample of the range of values present for each predictor layer. These points will be added to
    the field data .csv file with a value of "-9999" denoting them as background points.
    
    3. Background Probability Surface: This is an optional parameter that applies only to workflows that
    employ the Maxent modeling package. In some analyses, it may be appropriate to spatially limit background
    points to a particular subset of the study area (e.g., islands within a study area polygon, particular
    regions within a study area polygon, or a region determined by the known bias present in the field data).
    Specifying a background probability surface raster allows a user to control where random points will be
    scattered within the extent of the study area. The raster layer specified by a user should have the same
    projection and extent as the template layer and contain values ranging from 0 to 100. These values represent
    the probability that a randomly generated point will be retained should it fall within a particular cell.
    That is, randomly generated points will not be generated in any part of the probability grid with a value
    of "0" while all points falling in an area with a value of "100" will be retained. A point falling in an
    area with a value of "50" will be kept as a background point 50% of the time.
    
    4. Field Data: The field data input corresponds to a .csv file containing presence/absence points or count
    data recorded across a landscape for the phenomenon being modeled (e.g., plant sightings, evidence of
    animal presence, etc.). This input file must be in a particular format, and in most cases, a user should
    populate this field by connecting a FieldData element to the MDSBuilder in the visual display within VisTrails.
    Please see the documention for the FieldData module for more details.

    '''

    _input_ports = [('RastersWithPARCInfoCSV', '(gov.usgs.sahm:RastersWithPARCInfoCSV:Other)'),
                                 ('fieldData', '(gov.usgs.sahm:FieldData:DataInput)'),
                                 ('backgroundPointCount', '(edu.utah.sci.vistrails.basic:Integer)'),
                                 ('backgroundProbSurf', '(edu.utah.sci.vistrails.basic:File)'),
                                 ('Seed', '(edu.utah.sci.vistrails.basic:Integer)')]
                            
    
    _output_ports = [('mdsFile', '(gov.usgs.sahm:MergedDataSet:Other)')]

    def compute(self):
        port_map = {'fieldData': ('fieldData', None, True),
                    'backgroundPointCount': ('pointcount', None, False),
                    'backgroundProbSurf': ('probsurf', None, False),
                    'Seed': ('seed', None, False)}
        
        MDSParams = utils.map_ports(self, port_map)            
        MDSParams['outputMDS'] = utils.mknextfile(prefix='MergedDataset_', suffix='.csv')
        
        #allow multiple CSV of inputs to be provided.  
        #if more than one then combine into a single CSV before sending to MDSBuilder
        inputs_csvs = self.forceGetInputListFromPort('RastersWithPARCInfoCSV')
        if len(inputs_csvs) == 0:
            raise ModuleError(self, "Must supply at least one 'RastersWithPARCInfoCSV'/nThis is the output from the PARC module")
        if len(inputs_csvs) > 1:
            inputs_csv = utils.mknextfile(prefix='CombinedPARCFiles_', suffix='.csv')
            inputs_names = [f.name for f in inputs_csvs]
            utils.merge_inputs_csvs(inputs_names, inputs_csv)
        else:
            inputs_csv = inputs_csvs[0].name
        MDSParams['inputsCSV'] = inputs_csv
        
        #inputsCSV = utils.path_port(self, 'RastersWithPARCInfoCSV')
        
        ourMDSBuilder = MDSB.MDSBuilder()
        utils.PySAHM_instance_params(ourMDSBuilder, MDSParams)

        writetolog("    inputsCSV=" + ourMDSBuilder.inputsCSV, False, False)
        writetolog("    fieldData=" + ourMDSBuilder.fieldData, False, False)
        writetolog("    outputMDS=" + ourMDSBuilder.outputMDS, False, False)
        
        try:
            ourMDSBuilder.run()
        except TrappedError as e:
            raise ModuleError(self, e.message)
        except:
            utils.informative_untrapped_error(self, "MDSBuilder")

        output_file = utils.create_file_module(ourMDSBuilder.outputMDS) 
        self.setResult('mdsFile', output_file)

class FieldDataQuery(Module):
    '''
    FieldDataQuery
        
    This module provides convienence functions for selecting the pertinent columns in a
    raw field data file.  Also provided are functions for selecting the rows in the file
    to include based on the values in another column.
    
    columns can be specified with either a number (1 based) or the header string name.
    The string is not case sensitive and does not need to be enclosed in quotes.
    if the name or number of any of the columns cannot be found an error will be thrown.
    
    For the Query column you can either enter an equality statement with x used as a 
        placeholder to represent the values in the query column or you can construct 
        more involved queries using Python syntax.
        
        For example:
            x < 2005 (would return values less than 2005)
            x == 2000 or x == 2009 (would return 2000 or 2009)
            The syntax is python in case you want to create an involved query.
    '''    
    _input_ports = [('fieldData_file', '(gov.usgs.sahm:FieldData:DataInput)'),
                                 ('x_column', '(edu.utah.sci.vistrails.basic:String)', {'defaults':'1'}),
                                 ('y_column', '(edu.utah.sci.vistrails.basic:String)', {'defaults':'2'}),
                                 ('Response_column', '(edu.utah.sci.vistrails.basic:String)', {'defaults':'3'}),
                                 ('ResponseType', '(gov.usgs.sahm:ResponseType:Other)', {'defaults':'Presence(Absence)'}),
                                  ('Query_column', '(edu.utah.sci.vistrails.basic:String)'),
                                  ('Query', '(edu.utah.sci.vistrails.basic:String)')]
    _output_ports = [('fieldData', '(gov.usgs.sahm:FieldData:DataInput)'),]
    
    @classmethod
    def provide_input_port_documentation(cls, port_name):
        return utils.construct_port_msg(cls, port_name, 'in')
    @classmethod
    def provide_output_port_documentation(cls, port_name):
         return utils.construct_port_msg(cls, port_name, 'out') 
    
    def compute(self):
        writetolog("\nRunning FieldDataQuery", True)
        port_map = {'fieldData_file': ('fieldData', None, True),
            'x_column': ('x_col', None, True),
            'y_column': ('y_col', None, True),
            'Response_column': ('res_col', None, True),
            'ResponseType': ('response_type', None, True),
            'Query_column': ('query_col', None, False),
            'Query': ('query', None, False),}
        
        FDQParams = utils.map_ports(self, port_map)
        FDQOutput = utils.mknextfile(prefix='FDQ_', suffix='.csv')
        
        infile = open(FDQParams['fieldData'], "rb")
        csvReader = csv.reader(infile)
        header = csvReader.next()
        

        outfile = open(FDQOutput, "wb")
        csvwriter = csv.writer(outfile)
        if FDQParams["response_type"] == 'Count':
            responsetype = 'responseCount'
        else:
            responsetype = 'responseBinary'
            
        csvwriter.writerow(['X','Y',responsetype,"input=" + infile.name])
        x_index = self.find_column(header,FDQParams['x_col'])
        y_index = self.find_column(header,FDQParams['y_col'])
        res_index = self.find_column(header,FDQParams['res_col'])
        
        use_query = False
        if self.hasInputFromPort('Query_column'):
            use_query = True
            query_col_index = self.find_column(header,FDQParams['query_col'])
        
        for row in csvReader:
            if not use_query or \
             FDQParams['query'] == row[query_col_index] or \
             self.check_query(row[query_col_index], FDQParams['query']):
                response = row[res_index]
                if response.lower() == 'present':
                    response = '1'
                elif response.lower() == 'absent':
                    response = '0'
                csvwriter.writerow([row[x_index], row[y_index], response])
        
        del infile
        del outfile
        
        output_file = utils.create_file_module(FDQOutput) 
        self.setResult('fieldData', output_file) 
    
    def find_column(self, header, column_name):
        try:
            index = int(column_name) - 1
            if index > len(header) - 1:
                msg = "Field data input contains fewer columns than the number specified\n"
                msg += str(index + 1) + " is greater than " + str(len(header))
                raise ModuleError(self, msg)
        except:
            try:
                all_lowers = [item.lower() for item in header]
                index = all_lowers.index(column_name.lower())
            except:
                msg = "The specified column wasn't in the input file\n"
                msg += column_name + " not in " + str(header)
                raise ModuleError(self, msg)
        return index

    def check_query(self, value, query):
        try:
            #this works with numeric queries
            toevaluate = query.replace('x', value)
            return eval(toevaluate)
        except (NameError,SyntaxError):
            #this works with string queries
            toevaluate = query.replace('x', "'" + value + "'")
            return eval(toevaluate)

            
     
class FieldDataAggregateAndWeight(Module):
    '''
    Documentation to be updated when module finalized.
    '''
    _input_ports = [('templateLayer', '(gov.usgs.sahm:TemplateLayer:DataInput)'),
                                 ('fieldData', '(gov.usgs.sahm:FieldData:DataInput)'),
                                 ('PointAggregationOrWeightMethod', '(gov.usgs.sahm:PointAggregationMethod:Other)', {'defaults':'Collapse In Pixel'}),
                                 ('SDofGaussianKernel', '(edu.utah.sci.vistrails.basic:Float)')
                                 ]
    _output_ports = [('fieldData', '(gov.usgs.sahm:FieldData:DataInput)')]
    
    @classmethod
    def provide_input_port_documentation(cls, port_name):
        return utils.construct_port_msg(cls, port_name, 'in')
    @classmethod
    def provide_output_port_documentation(cls, port_name):
         return utils.construct_port_msg(cls, port_name, 'out')  
    
    def compute(self):
        writetolog("\nFieldDataAggregateAndWeight", True)
        port_map = {'templateLayer': ('template', None, True),
            'fieldData': ('csv', None, True),
            'PointAggregationOrWeightMethod': ('aggMethod', None, True),
            'SDofGaussianKernel': ('sd', None, False),}
        
        FDAWParams = utils.map_ports(self, port_map)
        output_fname = utils.mknextfile(prefix='FDAW_', suffix='.csv')
        writetolog("    output_fname=" + output_fname, True, False)
        FDAWParams['output'] = output_fname
        
        output_fname = utils.mknextfile(prefix='FDAW_', suffix='.csv')
        writetolog("    output_fname=" + output_fname, True, False)
        
        if FDAWParams['aggMethod'] == 'Inverse Density' or \
            FDAWParams['aggMethod'] == 'Total Presence=Total Absence':
            args = "o=" + FDAWParams['output']
            args += " i=" + FDAWParams['csv']
            args += " rc=" + utils.MDSresponseCol(FDAWParams['csv'])
            
            if FDAWParams['aggMethod'] == 'Inverse Density':
                args += " met=Density"
                if FDAWParams.has_key('sd'):
                    #uste the supplied SD of Gausian Kernel
                    args += " sig=" + str(FDAWParams['sd'])
                else:
                    #default to 1/2 the pixel width
                    args += " sig=" + str(float(utils.getpixelsize(FDAWParams['template']))/2)
            else:
                args += " met=PresAbs"
                
                
            
            utils.runRScript("SetWeights.r", args, self)
        else:
            ourFDAW = FDAW.FieldDataQuery()
            utils.PySAHM_instance_params(ourFDAW, FDAWParams) 
            ourFDAW.processCSV()
        
        output_file = utils.create_file_module(output_fname)
        writetolog("Finished running FieldDataQuery", True)
        self.setResult('fieldData', output_file)

class PARC(Module):
    '''
PARC

Description:
        The Projection, Aggregation, Resampling, and Clipping (PARC) module is a powerful
    utility that automates the preparation steps required for using raster layers in most
    geospatial modeling packages. In order to successfully consider multiple environmental
    predictors in raster format, each layer must have coincident cells (pixels) of the
    same size, have the same coordinate system (and projection, if applicable), and the
    same geographic extent. The PARC module ensures that all of these conditions are met
    for the input layers by transforming and or reprojecting each raster to match the
    coordinate system of the template layer. This process usually involves aggregation
    (necessary when an input raster layer must be up-scaled to match the template layer--
    e.g., generalizing a 10 m input layer to a 100 m output layer), and or resampling
    (necessary for interpolating new cell values when transforming the raster layer to the
    coordinate space or cell size of the template layer). Lastly, each raster predictor
    layer is clipped to match the extent of the template layer.

        The settings used during these processing steps follow a particular set of
    decision rules designed to preserve the integrity of data as much as possible.
    However, it is important for a user to understand how these processing steps may
    modify the data inputs. For additional information about the PARC module, please see
    the extended help and documentation for the SAHM package.


Input Ports:
    predictor:  (optional)
         A single raster with resampling, aggregation, and categorical options.

        Common connections:
            value' port of a Predictor module
            Note - Multiple single Predictor modules can be connected to this single input
                    port.


    PredictorList:  (optional)
         This is an in memory data construct that contains a list of predictors each with
        resampling, aggregation, and categorical options.

        Common connections:
            value' port of any of the 'Individual Predictors selector' modules
            Note - Multiple single Predictors selectors modules can be connected to this
                    single input port.


    RastersWithPARCInfoCSV:  (optional)
         This is a CSV containing a list of files to include in the PARC operation.

        The format of this list conforms to the 'PredictorListFile' specs:
            Column 1: The full file path to the input raster layer including the drive.
            Column 2: A binary value indicating whether the input layer is categorical or
        not. A value of "0" indicates that an input raster is non-categorical data
        (continuous), while a value of "1" indicates that an input raster is categorical
        data.
            Column 3: The resampling method employed to interpolate new cell values when
        transforming the raster layer to the coordinate space or cell size of the template
        layer, if necessary. The resampling type should be specified using one of the
        following values: "nearestneighbor," "bilinear," "cubic," or "lanczos."
            Column 4: The aggregation method to be used in the event that the raster layer
        must be up-scaled to match the template layer (e.g., generalizing a 10 m input
        layer to a 100 m output layer). Care should be taken to ensure that the
        aggregation method that best preserves the integrity of the data is used. The
        aggregation should be specified using one of the following values: "Min," "Mean,"
        "Max," "Majority," or "None."

        In formatting the list of predictor files, the titles assigned to each of the
        columns are unimportant as the module retrieves the information based on the order
        of the values in the .csv file (the ordering of the information and the
        permissible values in the file however, are strictly enforced). The module also
        anticipates a header row and will ignore the first row in the .csv file.

        Common connections:
            'value' port of PredictorListFile module


    templateLayer:  (mandatory)
         The templat layer raster file used to define the Extent, Cell size, Projection,
        raster snap, and coordinate system of the outputs.

        Common connections:
            'value' port of TemplateLayer module

            Be specified directly in the Module Information pane.


    ignoreNonOverlap:  (optional)
         Option of using the intersection of all covariates and template or enforcing the
        template extent.

        Options are:
            True (checked) = Use intersection of all covariates extents.  Area of template
                    extent will be reduce such all covariate layers extents can be
                    completely covered by the new extent.

            False (Unchecked) = The template extent will be used for all outputs and an
                    error will be raised if any of the covariates are not completely
                    covered by the template.


    multipleCores:  (optional)
         Option of running processing on multiple threads/cores.

        Options are:
            True (checked) = Individual layers will be run consectively on separate
                    threads.

            False (Unchecked) = All processing will occur on the same thread as the main
                    program. '''

    #configuration = []
    _input_ports = [('predictor', "(gov.usgs.sahm:Predictor:DataInput)"),
                                ('PredictorList', '(gov.usgs.sahm:PredictorList:Other)'),
                                ('RastersWithPARCInfoCSV', '(gov.usgs.sahm:RastersWithPARCInfoCSV:Other)'),
                                ('templateLayer', '(gov.usgs.sahm:TemplateLayer:DataInput)'),
                                ('ignoreNonOverlap', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'False', 'optional':True}),
                                ('multipleCores', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'True', 'optional':True})]

    _output_ports = [('RastersWithPARCInfoCSV', '(gov.usgs.sahm:RastersWithPARCInfoCSV:Other)')]
    
    def compute(self):
        #writetolog("\nRunning PARC", True)
        
        ourPARC = parc.PARC()
        template = self.forceGetInputFromPort('templateLayer').name
        template_path, template_fname = os.path.split(template)
        template_fname = os.path.splitext(template_fname)[0]
        if template_fname == 'hdr':
            template_fname = os.path.split(template_path)[1]
        
        output_dname = os.path.join(utils.getrootdir(), 'PARC_' + template_fname)
        if not os.path.exists(output_dname):
            os.mkdir(output_dname)
        
        if configuration.verbose:
            ourPARC.verbose = True
        ourPARC.logger = utils.getLogger()
        
        ourPARC.out_dir = output_dname

        if self.hasInputFromPort("multipleCores"):
            ourPARC.multicores = self.getInputFromPort("multipleCores")         

        if self.hasInputFromPort("ignoreNonOverlap"):
            ourPARC.ignoreNonOverlap = self.getInputFromPort("ignoreNonOverlap")

        workingCSV = os.path.join(output_dname, "tmpFilesToPARC.csv")

        #append additional inputs to the existing CSV if one was supplied
        #otherwise start a new CSV
        if self.hasInputFromPort("RastersWithPARCInfoCSV"):
            inputCSV = self.forceGetInputFromPort("RastersWithPARCInfoCSV").name
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
        ourPARC.inputs_CSV = workingCSV
        ourPARC.template = template
        writetolog('    template layer = ' + template)
        writetolog("    output_dname=" + output_dname, False, False)
        writetolog("    workingCSV=" + workingCSV, False, False)
        try:
            ourPARC.parcFiles()
        except TrappedError as e:
            writetolog(e.message)
            raise ModuleError(self, e.message)
        except:
            utils.informative_untrapped_error(self, "PARC")        
        
        #delete our temp working file
        os.remove(workingCSV)
        
        predictorsDir = utils.create_dir_module(output_dname)
        outputCSV = os.path.join(output_dname, "PARC_Files.csv")
        output_file = utils.create_file_module(outputCSV)
        
        
#        writetolog("Finished running PARC", True)
        self.setResult('RastersWithPARCInfoCSV', output_file)
        

class RasterFormatConverter(Module):
    '''
    Raster Format Converter

    The RasterFormatConverter module allows a user to easily convert .tif raster layers
    into a different raster format for use and display in other software packages. The
    module accepts as an input either a list of rasters in a Merged Dataset File (MDS)
    or the location of a directory containing multiple raster files. All outputs will be
    sent to a folder named "ConvertedRasters" (followed by an underscore and a number
    corresponding to the run sequence of the module) within the user's current VisTrail
    session folder.

    Three parameters can be specified by the user:

    1. Format: The format corresponds to the desired raster output format. The following
    output file formats are supported: Arc/Info ASCII Grid, ESRI BIL, ERDAS Imagine, and JPEG.
    
    To specify the desired output, users should enter the values shown below.
    For an ASCII (.asc) output, enter: "asc"
    For an ESRI BIL output, enter: "bil"
    For an Erdas Imagine (.img) output, enter: "img"
    For a JPEG (.jpg) output, enter: "jpg"
    
    If no value is entered by the user, the module will default to an ASCII (.asc) output
    format.

    2. Input Directory: The input directory allows a user to point to an entire folder as
    an input to the RasterFormatConverter. The contents of the specified folder will be
    checked for raster files and all the raster files contained within the directory will
    be converted to the format specified in the "Format" dialogue box. The module will
    identify and convert files of the following raster types: .bil, .img, .tif, .jpg, and .asc. 

    3. Input Merged Dataset (MDS): The input merged dataset allows a user to specify a .csv
    file created in the VisTrails workflow (containing a list of .tif raster files) as an
    input to the raster converter. All of the files listed in the MDS will be converted to
    the raster format specified in the "Format" dialogue box.

    '''

    #configuration = []
    _input_ports = [("inputMDS", "(gov.usgs.sahm:MergedDataSet:Other)"),
                    ('inputDir', '(edu.utah.sci.vistrails.basic:Directory)'),
                    ('format', '(edu.utah.sci.vistrails.basic:String)'),
                    ('multipleCores', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'True', 'optional':True})]

    _output_ports = [('outputDir', '(edu.utah.sci.vistrails.basic:Directory)')]

    def compute(self):
        writetolog("\nRunning TiffConverter", True)
        ourRFC = RFC.FormatConverter()
        if self.hasInputFromPort('inputMDS'):
            ourRFC.MDSFile = self.forceGetInputFromPort('inputMDS').name
        elif self.hasInputFromPort('inputDir'):
            ourRFC.inputDir = self.forceGetInputFromPort('inputDir').name
            
        if self.hasInputFromPort('format'):
            format = self.forceGetInputFromPort('format')
            if format == '':
                format = 'asc'
            ourRFC.format = format
             
        if self.hasInputFromPort("multipleCores"):
             if self.getInputFromPort("multipleCores"):
                ourRFC.multicores = "True"
        
        ourRFC.outputDir = utils.mknextdir(prefix='ConvertedRasters_')
        if configuration.verbose:
            ourRFC.verbose = True
        ourRFC.logger = utils.getLogger()
        writetolog("    output directory = " + ourRFC.outputDir, False, False)
        
        try:
            ourRFC.run()
        except TrappedError as e:
            raise ModuleError(self, e.message)
        except:
            utils.informative_untrapped_error(self, "RasterFormatConverter") 
        
        
        outputDir = utils.create_dir_module(ourRFC.outputDir)
        self.setResult('outputDir', outputDir)
        writetolog("\nFinished running TiffConverter", True)
        
class ModelEvaluationSplit(Module):
    '''
     The ModelEvaluationSplit module provides the opportunity to reserve a specified portion of the data for 
    producing and reporting evaluation metrics on an independent test set following model exploration and selection.  
    The ModelEvaluationSplit must be applied before the CovariateCorrelationAndSelection module.  
    The nearly identical ModelSelectionSplit reserves a portion of the data from the model fitting 
    process but reports the evaluation metrics on all models not just the those selected as the 
    final models to be reported in the analysis.  This module can be placed either directly before 
    or directly after the CovariateCorrelationAndSelection.  If both a ModelEvaluationSplit and a 
    ModelSelectionSplit are specified then the training portion of the ModelEvalutationSplit will 
    be further partitioned by the ModelSelectionSplit thus the ModelEvalutationSplit should come 
    first in the workflow.  Both of these algorithms stratify the splits by the response.  
    That is, the ratio of presence to absence points should be nearly equal in the testing 
    and training split.  If a ModelSelectionSplit is included evaluation metrics applied to 
    the reserved data will be reported in the textual output, model evaluation plots including 
    AUC plots as well as the across model plots and the csv.  Both of these modules ignore 
    background points and treat all observations with values greater than 0 as presence 
    for the purpose of stratification by response. Three parameters can be set by the user for each of these modules: 

		1.	Ratio of Presence/Absence Points: This optional field is populated with a number corresponding 
		to the desired ratio of presence to  absence points to be used in the analysis. If not populated 
		then all occurrence records (not background points) will be portioned into either the test or 
		training split with no reduction in the total number of points.  If populated, this entry should 
		be a number greater than zero. (A value of '1' will result in an equal number of both presence 
		and absence points being used, a value of '2' indicates that twice as many presence points 
		will be used, a value of '0.5' indicates that twice as many absence points will be used, etc.). 
		All field data points with a value equal to or greater than 1 are interpreted as presence points. 
		Although the original field data is unmodified, this option will reduce the sample size as the 
		merged dataset containing sample points will have points deleted from it to achieve the 
		specified ratio as such it should be used with caution. A warning will be generated if 
		more than 50% of either the presence or absence points will be deleted based on the ratio 
		specified by the user. Background points are ignored by this module (they are read in 
		and written out, but not assigned to either the test or training split).
		
		2.	Input Merged Data Set (MDS): This is the input data set consisting of location data for each sample 
	    point, the values of each predictor variable at those points, and if established, a field denoting the 
	    weight that will be assigned to each point in modeling. This input is usually provided by the upstream 
	    steps that precede the Test Training Split module. Any value entered here (e.g., specifying another 
	    existing MDS on the file system) will override the input specified by a model connection in the visual display.

		3.	 Training Proportion: This is the proportion of the sample points that will be used to train the model, 
		relative to the total number of points. Entered values should be greater than 0 but less than 1. For example, 
		a value of '0.9' will result in 90% of the sample points being used to train the model, with 10% of the sample 
		being held out to test the model's performance. Choosing an appropriate training proportion can depend on various 
		factors, such as the total number of sample points available.  Selecting an appropriate value for the training 
		proportion is a complex issue that depends on many factors including the total number of observations, the 
		complexity of the models that will be fit, and the signal to noise ratio in the data (Hastie et. al. 2009).  

	 	
    Hastie T, Tibshirani R, Friedman JH. 2009. The Elements of Statistical Learning: Data Mining, Inference, and Prediction. New York: Springer-Verlag. 744 pp. 2nd ed. 

    '''

    _input_ports = [("inputMDS", "(gov.usgs.sahm:MergedDataSet:Other)"),
                    ('trainingProportion', '(edu.utah.sci.vistrails.basic:Float)', 
                        {'defaults':'0.7'}),
                    ('RatioPresAbs', '(edu.utah.sci.vistrails.basic:Float)'),
                    ('Seed', '(edu.utah.sci.vistrails.basic:Integer)'),]
    _output_ports = [("outputMDS", "(gov.usgs.sahm:MergedDataSet:Other)")]
    
    def compute(self):
        writetolog("\nGenerating Model Evaluation split ", True)
        inputMDS = utils.dir_path_value(self.forceGetInputFromPort('inputMDS', []))
        outputMDS = utils.mknextfile(prefix='ModelEvaluation_Split_', suffix='.csv')

        global models_path
        
        args = "i=" + '"' + inputMDS + '"' + " o=" + '"' + outputMDS + '"'
        args += " rc=" + utils.MDSresponseCol(inputMDS) 
        if (self.hasInputFromPort("trainingProportion")):
            try:
                trainingProportion = float(self.getInputFromPort("trainingProportion"))
                if trainingProportion <= 0 or trainingProportion > 1:
                    raise ModuleError(self, "Train Proportion (trainProp) must be a number between 0 and 1 excluding 0")
                args += " p=" + str(trainingProportion)
            except:
                raise ModuleError(self, "Train Proportion (trainProp) must be a number between 0 and 1 excluding 0")
        if (self.hasInputFromPort("RatioPresAbs")):
            try:
                RatioPresAbs = float(self.getInputFromPort("RatioPresAbs"))
                if RatioPresAbs <= 0:
                    raise ModuleError(self, "The ratio of presence to absence (RatioPresAbs) must be a number greater than 0") 
                args += " m=" + str(trainingProportion) 
            except:
                raise ModuleError(self, "The ratio of presence to absence (RatioPresAbs) must be a number greater than 0") 

        args += " es=TRUE"

        if self.hasInputFromPort("Seed"):
            seed = str(self.getInputFromPort("Seed"))
        else:
            seed = random.randint(-1 * ((2**32)/2 - 1), (2**32)/2 - 1)
        writetolog("    seed used for Split = " + str(seed))
        args += " seed=" + str(seed)

        utils.runRScript("TestTrainSplit.r", args, self)
        
        output = os.path.join(outputMDS)
        if os.path.exists(output):
            output_file = utils.create_file_module(output)
            writetolog("Finished Model Evaluation split ", True)
        else:
            msg = "Problem encountered generating Model Evaluation split.  Expected output file not found."
            writetolog(msg, False)
            raise ModuleError(self, msg)
        self.setResult("outputMDS", output_file)
        
class ModelSelectionSplit(Module):
    '''
    The ModelSelectionSplit reserves a portion of the data from the model fitting 
process but reports the evaluation metrics on all models not just the those selected as the 
final models to be reported in the analysis (in contrast to the ModelEvaluationSplit).  This module 
can be placed either directly before or directly after the CovariateCorrelationAndSelection.  If both a 
ModelEvaluationSplit and a ModelSelectionSplit are specified then the training portion of the ModelEvalutationSplit will 
be further partitioned by the ModelSelectionSplit thus the ModelEvalutationSplit should come 
first in the workflow.  Both of these algorithms stratify the splits by the response.  
That is, the ratio of presence to absence points should be nearly equal in the testing 
and training split.  If a ModelSelectionSplit is included evaluation metrics applied to 
the reserved data will be reported in the textual output, model evaluation plots including 
AUC plots as well as the across model plots and the csv.  Both of these modules ignore 
background points and treat all observations with values greater than 0 as presence 
for the purpose of stratification by response. Three parameters can be set by the user for each of these modules:

       1.	Ratio of Presence/Absence Points: This optional field is populated with a number corresponding 
         to the desired ratio of presence to  absence points to be used in the analysis. If not populated 
         then all occurrence records (not background points) will be portioned into either the test or 
         training split with no reduction in the total number of points.  If populated, this entry should 
         be a number greater than zero. (A value of '1' will result in an equal number of both presence 
         and absence points being used, a value of '2' indicates that twice as many presence points 
         will be used, a value of '0.5' indicates that twice as many absence points will be used, etc.). 
         All field data points with a value equal to or greater than 1 are interpreted as presence points. 
         Although the original field data is unmodified, this option will reduce the sample size as the 
         merged dataset containing sample points will have points deleted from it to achieve the 
         specified ratio as such it should be used with caution. A warning will be generated if 
         more than 50% of either the presence or absence points will be deleted based on the ratio 
         specified by the user. Background points are ignored by this module (they are read in 
         and written out, but not assigned to either the test or training split).
		
         2.	Input Merged Data Set (MDS): This is the input data set consisting of location data for each sample 
         point, the values of each predictor variable at those points, and if established, a field denoting the 
         weight that will be assigned to each point in modeling. This input is usually provided by the upstream 
         steps that precede the Test Training Split module. Any value entered here (e.g., specifying another 
         existing MDS on the file system) will override the input specified by a model connection in the visual display.

         3.	 Training Proportion: This is the proportion of the sample points that will be used to train the model, 
         relative to the total number of points. Entered values should be greater than 0 but less than 1. For example, 
         a value of '0.9' will result in 90% of the sample points being used to train the model, with 10% of the sample 
         being held out to test the model's performance. Choosing an appropriate training proportion can depend on various 
         factors, such as the total number of sample points available.  Selecting an appropriate value for the training 
         proportion is a complex issue that depends on many factors including the total number of observations, the 
         complexity of the models that will be fit, and the signal to noise ratio in the data (Hastie et. al. 2009). 
		
    It is not valid to select models based on their performance on the reserved portion of the data and then report these 
metrics only for the top performing models claiming that we would expect similar performance on an independent dataset see Hastie 2009 for 
this discussion.  If one desires metrics for how the models might be expected to perform on an independent dataset then the ModelEvaluationSplit 
must be used. 

    Hastie T, Tibshirani R, Friedman JH. 2009. The Elements of Statistical Learning: Data Mining, Inference, and Prediction. New York: Springer-Verlag. 744 pp. 2nd ed. 
    
    '''        

    _input_ports = [("inputMDS", "(gov.usgs.sahm:MergedDataSet:Other)"),
                    ('trainingProportion', '(edu.utah.sci.vistrails.basic:Float)', 
                        {'defaults':'0.7'}),
                    ('RatioPresAbs', '(edu.utah.sci.vistrails.basic:Float)'),
                    ('Seed', '(edu.utah.sci.vistrails.basic:Integer)'),]
    _output_ports = [("outputMDS", "(gov.usgs.sahm:MergedDataSet:Other)")]
    
    def compute(self):
        writetolog("\nGenerating Model Selection split ", True)
        inputMDS = utils.dir_path_value(self.forceGetInputFromPort('inputMDS', []))
        outputMDS = utils.mknextfile(prefix='modelSelection_split_', suffix='.csv')

        global models_path
        
        args = "i=" + '"' + inputMDS + '"' + " o=" + '"' + outputMDS + '"'
        args += " rc=" + utils.MDSresponseCol(inputMDS) 
        if (self.hasInputFromPort("trainingProportion")):
            try:
                trainingProportion = float(self.getInputFromPort("trainingProportion"))
                if trainingProportion <= 0 or trainingProportion > 1:
                    raise ModuleError(self, "Train Proportion (trainProp) must be a number between 0 and 1 excluding 0")
                args += " p=" + str(trainingProportion)
            except:
                raise ModuleError(self, "Train Proportion (trainProp) must be a number between 0 and 1 excluding 0")
        if (self.hasInputFromPort("RatioPresAbs")):
            try:
                RatioPresAbs = float(self.getInputFromPort("RatioPresAbs"))
                if RatioPresAbs <= 0:
                    raise ModuleError(self, "The ratio of presence to absence (RatioPresAbs) must be a number greater than 0") 
                args += " m=" + str(trainingProportion) 
            except:
                raise ModuleError(self, "The ratio of presence to absence (RatioPresAbs) must be a number greater than 0") 

        args += " es=FALSE"

        if self.hasInputFromPort("Seed"):
            seed = str(self.getInputFromPort("Seed"))
        else:
            seed = random.randint(-1 * ((2**32)/2 - 1), (2**32)/2 - 1)
        writetolog("    seed used for Split = " + str(seed))
        args += " seed=" + str(seed)

        utils.runRScript("TestTrainSplit.r", args, self)
        
        output = os.path.join(outputMDS)
        if os.path.exists(output):
            output_file = utils.create_file_module(output)
            writetolog("Finished Model Selection split ", True)
        else:
            msg = "Problem encountered generating Model Selection split.  Expected output file not found."
            writetolog(msg, False)
            raise ModuleError(self, msg)
        self.setResult("outputMDS", output_file)

class ModelSelectionCrossValidation(Module):
    '''
    The ModelSelectionCrossValidation module provides another tool for model selection by splitting the field data observations into cross validation folds.  
    This should not be used with the ModelSelectionSplit but can be used with the ModelEvaluationSplit in which case only the training portion of the ModelEvalutationSplit 
    is partitioned into folds.  If specified then the individual models will fit a model using all of the data and report this as the training results.  Following the model 
    fitting step sub-models with be fit to each set of n-1 folds and then evaluation metrics calculated on the remaining fold.  These will show up as ranges in the AUC plot, 
    means and standard deviations are reported in textual output and box plots in across model comparison plots.  Evaluation metrics for each individual fold are reported 
    in the across model comparison csv.  The cross validation method incorporated here was originally written for evaluation of MARS models by Leathwick et. al. 2006.  
    
    The current implementation does not attempt any sort of model averaging but rather is only used for calculation of evaluation metrics.  The ModelSelectionCrossValidation 
    module makes better use of data then the ModelSelectionSplit as it uses all of the data to fit the final model but can be substantially more time consuming.   

       Under most circumstances the cross validation evaluation metrics reported by this module do not indicate how the the model might perform if applied to an independent 
       set of data but rather are to be used only for model selection purposes.  The first issue is that when cross validation is applied any feature selection based on the 
       relationship between the response and the predictors must be carried out on each cross validation training set.  The CovariateCorrelationAndSelection module includes 
       an exploration of the relationship between the predictors and the response and thus would need to be carried out for each for each cross validation training set.  
       The second issue is that it is invalid to use an evaluation metric for model selection and then report that metric for only the best performing model without 
       acknowledgement to the total number of models that were considered and the range of the evaluation metrics.  This module ignores background points.  
       
       Three parameters can be set by the user:
        1. Stratify:  Whether to stratify the folds by the response the default is to true
        
        2. Input Merged Data Set (MDS): This is the input data set consisting of locational data for each sample point, the values of each predictor variable at those points, 
        and if established, a field denoting the weight that will be assigned to each point in modeling. This input is usually provided by the upstream steps that precede the 
        Test Training Split module. Any value entered here (e.g., specifying another existing MDS on the file system) will override the input specified by a model connection 
        in the visual display.
        
        3. nFolds: The number of folds into which the data should be partitioned.  The default is 10.   A trade-off exists in selecting the number of folds to use for cross-validation.  
        When nFolds is close to the total number of observations the prediction error is nearly unbiased as the cross validation sample size is nearly equal to the total sample 
        size but because the training sets are nearly identical in this case variance of the prediction error can be quite high (Hastie et. al 2009).  

Hastie T, Tibshirani R, Friedman JH. 2009. The Elements of Statistical Learning: Data Mining, Inference, and Prediction. New York: Springer-Verlag. 744 pp. 2nd ed. 
    '''        

    _input_ports = [("inputMDS", "(gov.usgs.sahm:MergedDataSet:Other)"),
                    ('nFolds', '(edu.utah.sci.vistrails.basic:Integer)', 
                        {'defaults':'10'}),
                    ('Stratify', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'True', 'optional':True}),
                    ('Seed', '(edu.utah.sci.vistrails.basic:Integer)'),]
    _output_ports = [("outputMDS", "(gov.usgs.sahm:MergedDataSet:Other)")]
    
    def compute(self):
        writetolog("\nGenerating Cross Validation split ", True)
        port_map = {'inputMDS':('i', utils.dir_path_value, True),
                    'nFolds':('nf', None, True),
                    'Stratify':('stra', utils.R_boolean, True)}
        
        argsDict = utils.map_ports(self, port_map)

        outputMDS = utils.mknextfile(prefix='modelSelection_cv_', suffix='.csv')

        args = "i=" + '"' + argsDict["i"] + '"'
        args += " o=" + '"' + outputMDS + '"'
        args += " rc=" + utils.MDSresponseCol(argsDict["i"]) 

        if argsDict["nf"] <= 0:
            raise ModuleError(self, "Number of Folds must be greater than 0")
        args += " nf=" + str(argsDict["nf"])

        args += " stra=" + argsDict["stra"]
        
        args += " es=TRUE"

        if self.hasInputFromPort("Seed"):
            seed = str(self.getInputFromPort("Seed"))
        else:
            seed = random.randint(-1 * ((2**32)/2 - 1), (2**32)/2 - 1)
        writetolog("    seed used for Split = " + str(seed))
        args += " seed=" + str(seed)

        utils.runRScript("CrossValidationSplit.r", args, self)
        
        output = os.path.join(outputMDS)
        if os.path.exists(output):
            output_file = utils.create_file_module(output)
            writetolog("Finished Cross Validation split ", True)
        else:
            msg = "Problem encountered generating Cross Validation split.  Expected output file not found."
            writetolog(msg, False)
            raise ModuleError(self, msg)
        self.setResult("outputMDS", output_file)


class CovariateCorrelationAndSelection(Module):
    '''
    The CovariateCorrelationAndSelection view provides a breakpoint in the modeling workflow 
    for the user to assess how well each variable explains the distribution of the sampled 
    data points and to remove any variables that may exhibit high correlation with others. 
	The display shows the n variables that have the highest total number of correlations above 
	a threshold with other predictors using the maximum of the Pearson, Spearman and Kendall 
	coefficient. The column heading over each variable displays the number of other variables with which 
	the environmental predictor is correlated using the user supplied threshold which defaults to .7.  
	Radio buttons are available to limit the display and correlation calculations to any combination 
	of presence, absence, or background points.  The first column in the plot shows the relationship 
	between the response and each predictor.  Row labels indicate the maximum of the Spearman and Pearson 
	correlation coefficient and a locally weighted smooth has been added to help distinguish the nature of 
	the relationship.  

	The remaining plots make up a square with histograms for each variable displayed on the diagonal.  
	Their respective graphical display and correlation with other variables can be found by locating 
	the row/column intersection between each (above and below the diagonal).  The scatter plot along 
	with a locally weight smooth is shown below the diagonal.  Presence records are represented by 
	red points, absence by green, and background are yellow.  Above the diagonal is the correlation 
	coefficient between the two predictors.  If Spearman or Kendall correlation coefficient is larger 
	than the Pearson correlation coefficient then an s or k will show up in the bottom right corner of 
	this box.   

	A user is provided with the opportunity to select a new set of the environmental predictor variables 
	and "Update" the Covariate Correlation screen to investigate the relationships among the new variables 
	selected.  Variables with a high degree of correlation with other variables should generally be unchecked 
	in their respective radio buttons, and will be excluded from subsequent analysis steps in the model workflow.

	Multiple iterations can be run at this screen, allowing the user to investigate the relationships among the 
	environmental predictor variables and choose the most appropriate set to be used in the subsequent modeling. 
	When the desired set of variables has been chosen, the "OK" button is selected and processing will resume 
	in the VisTrails workflow.  Three parameters can/must be specified by the user:
	
	1.	ShowGUI : This Boolean indicates whether to stop execution and display the GUI for user interaction.  
	In some cases such as exploration you might want to make a selection in a previous run and then change this 
	to false so that the selection will be apply to subsequent runs without interrupting execution.
	
	2.	corsWithHighest: If one desires to view only other parameters that have a correlation above the specified 
	threshold with the parameter than has the highest number of total correlations with other parameters then this 
	should be set to true.  Otherwise, by default, the parameters that are selected for display will be the set of 
	parameters that have the highest number of correlations with other parameters above the given threshold.  
	
	3.	inputMDS : The file to select from.  If this file contains unselected layers (0 in the second header line) 
	these will initially appear deselected in the GUI.
	
	4.	minCor: The minimum correlation used to summarize the number of other variables each variable is highly 
	correlated with.
	
	5.	numPlots: The number of variables to display at a time in the plot frame.  
	
	6.	selectionName: This serves two purposes.  First to uniquely identify a given selection.  This unique name 
	is used to determine if a selection has been previously made, to apply for example.  And secondly to provide 
	something that can be changed to trigger VisTrails to rerun this module even if nothing upstream has changed.
    Covariate Correlation And Selection

    Multiple iterations can be run at this screen, allowing the user to investigate the
    relationships among the environmental predictor variables and choose the most appropriate
    set to be used in the subsequent modeling. When the desired set of variables has been chosen,
    the "OK" button is selected and processing will resume in the VisTrails workflow.

    '''
    _input_ports = [("inputMDS", "(gov.usgs.sahm:MergedDataSet:Other)"),
                    ('selectionName', '(edu.utah.sci.vistrails.basic:String)', {'defaults':'initial'}),
                    ('ShowGUI', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'True'}),
                    ('numPlots', '(edu.utah.sci.vistrails.basic:Integer)', {'defaults':'8', 'optional':True}),
                    ('minCor', '(edu.utah.sci.vistrails.basic:Float)', {'defaults':'0.7', 'optional':True}),
                    ('corsWithHighest', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'False', 'optional':True})]
    _output_ports = [("outputMDS", "(gov.usgs.sahm:MergedDataSet:Other)")]

    
    def compute(self):
        writetolog("\nOpening Select Predictors Layers widget", True)
        
        port_map = {'inputMDS': ('inputMDS', None, True),
                    'selectionName': ('selectionName', None, True),
                    'ShowGUI': ('ShowGUI', None, True),
                    'numPlots': ('numPlots', None, False),
                    'minCor': ('minCor', None, False),
                    'corsWithHighest': ('corsWithHighest', utils.R_boolean, False),}
        
        params = utils.map_ports(self, port_map)

        global session_dir
        params['outputMDS'] = os.path.join(session_dir, "CovariateCorrelationOutputMDS_" + params['selectionName'] + ".csv")
        params['displayJPEG'] = os.path.join(session_dir, "CovariateCorrelationDisplay.jpg")
        params['r_path'] = configuration.r_path
        writetolog("    inputMDS = " + params['inputMDS'], False, False)
        writetolog("    displayJPEG = " + params['displayJPEG'], False, False)
        writetolog("    outputMDS = " + params['outputMDS'], False, False)
        
        if os.path.exists(params['outputMDS']) and params['ShowGUI']:
            utils.applyMDS_selection(params['outputMDS'], params['inputMDS'])
            os.remove(params['outputMDS'])
            self.callDisplayMDS(params)
        elif os.path.exists(params['outputMDS']) and not params['ShowGUI']:
            utils.applyMDS_selection(params['outputMDS'], params['inputMDS'])
            os.remove(params['outputMDS'])
            shutil.copy2(params['inputMDS'], params['outputMDS'])
            writetolog("    Applying previous selection but not showing GUI", False, True)
        elif not os.path.exists(params['outputMDS']) and not params['ShowGUI']:
            raise ModuleError(self, "Show GUI deselected but no previous output detected.\n\nCan not continue!")
        else:
            self.callDisplayMDS(params)
                    
        
        output_file = utils.create_file_module(params['outputMDS'])
        writetolog("Finished Select Predictors Layers widget", True)
        self.setResult("outputMDS", output_file)

    def callDisplayMDS(self, kwargs):
        dialog = SelectListDialog(kwargs)
        #dialog.setWindowFlags(QtCore.Qt.WindowMaximizeButtonHint)
#        print " ... finished with dialog "  
        retVal = dialog.exec_()
        #outputPredictorList = dialog.outputList
        if retVal == 1:
            raise ModuleError(self, "Cancel or Close selected (not OK) workflow halted.")

#removed for the V1.0 release
#class ProjectionLayers(Module):
#    '''
#    Projection Layers
#
#    Note: as of June 2011, this module offers some functionality that is only available
#    to users running the SAHM package within the USGS Fort Collins Science Center (FORT).
#
#    The ProjectionLayers module provides the option to prepare a separate set of predictor
#    layers so that the results of a model developed from one set of environmental predictors
#    can be projected onto a new modeled space. This second set of environmental predictors
#    (corresponding to the "projection target") most often contains the same environmental
#    predictors but represents data captured at a different temporal or spatial location. For
#    example, a user could generate a model predicting habitat suitability using recorded
#    presence points and certain environmental predictors such as elevation, landcover, and
#    proximity to water in one geographic location. Based on the training from this information,
#    the modeled results could be generated for (or "projected to") a new location based on the
#    range of values seen in elevation, landcover, and proximity to water in the second geographic
#    area. Similarly, modeling predicted results through time is also possible. A model trained
#    using field data and a set of predictor layers representative of one time period could be
#    projected onto the same geographical area using a new set of predictor layers corresponding
#    to the same predictors but representing data from a different time period (e.g., different
#    climate data). 
#
#    The output of this module is subsequently used as the projection target in the ApplyModel module.
#
#    (As part of the process of preparing the layers for modeling, the ProjectionLayers module runs
#    the PARC module internally on the inputs. Outputs from the ProjectionLayers module will possess
#    matching coordinate systems, cell sizes, and extents and do not need to be run through PARC
#    before being used downstream in the workflow.)
#
#    Six parameters can be set by the user:
#
#    1. Directory Crosswalk CSV: This is a .csv file containing two columns designating
#    the layers that should be swapped out in the projected model. The first column
#    contains a list of the full paths to the predictor layers used to develop the original
#    model that will be replaced in the projection process. The second column contains the
#    full paths to the new predictor layers that will substitute the respective layers used
#    in the original model. Each original layer in the first column should be paired with
#    its replacement in the second column (e.g., Column 1 = C:\ModelLayers\Precipitation1980.tif,
#    Column 2 = C:\ModelLayers\Precipitation2000.tif). In the case of any file used to develop
#    the first model that is not expressly listed in the Directory Crosswalk CSV with a
#    replacement, the original file will be used in the new model projection. The module
#    anticipates a header row in this .csv file (thus, the first row of data will be ignored).
#    
#    2. File List CSV: This is a .csv file containing the list of predictor files used to
#    develop the first model. Effectively, this file will be updated based on the information
#    provided in the directory crosswalk .csv and used as the input to the training process
#    for the projected model. The output of the PARC module from the first model iteration
#    should be used as the input to this parameter.
#    
#    3. Model (available only to users at the FORT): This parameter allows VisTrail users
#    running the SAHM package on site at the USGS Science Center in Fort Collins (FORT) to
#    specify one of three models to use for the projected model run ("CCCMA," "CSIRO,"
#    or "hadcm3").
#    
#    4. Scenario (available only to users at the FORT): This parameter allows VisTrail
#    users running the SAHM package on site at the USGS Science Center in Fort Collins 
#    FORT) to specify one of two scenarios for the projected model run ("A2a" or "B2b"). 
#    
#    5. Template: This parameter allows a user to specify the new template layer to be used
#    in the projected model run. The template layer is a raster data layer with a defined
#    coordinate system, a known cell size, and an extent that defines the (new) study area.
#    This raster layer serves as the template for all the other inputs in the analysis. All
#    additional raster layers used in the analysis will be resampled and reprojected as
#    needed to match the template, snapped to the template, and clipped to have an extent
#    that matches the template. Users should ensure that all the layers used for the projected
#    analysis have coverage within the extent of the template layer.
#    
#    6. Year (available only to users at the FORT): This parameter allows VisTrail users
#    running the SAHM package on site at the USGS Science Center in Fort Collins (FORT)
#    to specify one of three years to use for the projected model run ("2020," "2050," or "2080").
#
#    '''
#    _input_ports = [('RastersWithPARCInfoCSV', '(gov.usgs.sahm:RastersWithPARCInfoCSV:Other)'),
#                    ('templateLayer', '(gov.usgs.sahm:TemplateLayer:DataInput)'),
#                    ('model', '(edu.utah.sci.vistrails.basic:String)'),
#                    ('scenario', '(edu.utah.sci.vistrails.basic:String)'),
#                    ('year', '(edu.utah.sci.vistrails.basic:String)'),
#                    ('directoryCrosswalkCSV', '(edu.utah.sci.vistrails.basic:File)')
#                    ]
#    _output_ports = [("MDS", "(gov.usgs.sahm:MergedDataSet:Other)")]
#
#    def compute(self):
#        models = ['CCCMA', 'CSIRO', 'hadcm3']
#        scenarioss = ['A2a', 'B2b']
#        years = ['2020', '2050', '2080']
#        
#        writetolog("\nRunning make Projection Layers", True)
#        
#        inputCSV = self.forceGetInputFromPort('RastersWithPARCInfoCSV').name
#    
#        if self.hasInputFromPort('templateLayer'):
#            template = self.forceGetInputFromPort('templateLayer').name
#        else:
#            template = '' #we'll get a template below
#            
#        fromto = []
#        climargs = {}
#        
#        for input in ['model', 'scenario', 'year']:
#            if self.hasInputFromPort(input):
#                climargs[input] = self.forceGetInputFromPort(input)
#        if climargs <> {} and climargs.keys() <> ['model', 'scenario', 'year']:
#            #they did not add in one of each, Not going to fly
#            raise ModuleError(self, "All of model, scenario, and year must be supplied if any are used.")
#        elif climargs <> {} and climargs.keys <> ['model', 'scenario', 'year']:
#            #they specified a alt climate scenario add this to our list to search for
#            fromto.append([r'K:\GIS_LIBRARY\Climate\WorldClim\BioclimaticVariables\bio_30s_esri\bio',
#                           os.path.join('I:\WorldClim_Future_Climate\RenamedBILs', 
#                                        climargs['model'], climargs['scenario'], climargs['year'])])
#        
#        if self.hasInputFromPort('directoryCrosswalkCSV'):
#            crosswalkCSV = csv.reader(open(self.forceGetInputFromPort('directoryCrosswalkCSV'), 'r'))
#            header = crosswalkCSV.next()
#            for row in crosswalkCSV:
#                fromto.append(row[0], row[1])
#            del crosswalkCSV    
#            
#        #write out the outputs to an empty MDS file (just the header is needed to PARC the outputs)
#            
#        
#        inCSV = csv.reader(open(inputCSV, 'r'))
#        inCSV.next() #skip header
#        workingCSV = utils.mknextfile(prefix='tmpFilesToPARC_', suffix='.csv')
#        tmpCSV = csv.writer(open(workingCSV, 'wb'))
#        tmpCSV.writerow(["FilePath", "Categorical", "Resampling", "Aggregation"])
#        outHeader1 = ['X', 'Y', 'response']
#        outHeader2 = ['', '', '']
#        outHeader3 = ['', '', '']
#        
#        output_dname = utils.mknextdir(prefix='ProjectionLayers_')
#        
#        for row in inCSV:
#            if template == '':
#                template = row[0]
#            fileShortName = utils.getShortName(row[0])
#            if row[1] == 1:
#                outHeader1.append(fileShortName + '_categorical')
#            else:
#                outHeader1.append(fileShortName)
#            outHeader2.append('1')
#            outHeader3.append(os.path.join(output_dname, fileShortName + '.tif'))
#
#            origFile = row[4]
#            newOrigFile = origFile
#            for lookup in fromto:
#               if lookup[0] in origFile:
#                   newOrigFile = origFile.replace(lookup[0], lookup[1])
#            tmpCSV.writerow([newOrigFile,] + row[1:4])
#        del tmpCSV
#        
#        #PARC the files here
#        ourPARC = parc.PARC()
#        
#        
#        if configuration.verbose:
#            ourPARC.verbose = True
#        writetolog("    output_dname=" + output_dname, False, False)
#        ourPARC.outDir = output_dname
#        ourPARC.inputsCSV = workingCSV
#        ourPARC.template = template
#
#        try:
#            ourPARC.parcFiles()
#        except TrappedError as e:
#            raise ModuleError(self, e.message)
#        except :
#            utils.informative_untrapped_error(self, "PARC")
#        
#        #loop through our workingCSV and format it into an MDS header
#        
#        #outputMDS = utils.mknextfile(prefix='ProjectionLayersMDS_', suffix = '.csv')
#        outputMDS = os.path.join(output_dname, 'ProjectionLayersMDS.csv')
#        outCSV = csv.writer(open(outputMDS, 'wb'))
#        outCSV.writerow(outHeader1)
#        outCSV.writerow(outHeader2)
#        outCSV.writerow(outHeader3)
#        
#        output_file = utils.create_file_module(outputMDS)
#        self.setResult("MDS", output_file)
#        writetolog("Finished Select Projection Layers widget", True)

class MAXENT(Module):

    _output_ports = [("lambdas", "(edu.utah.sci.vistrails.basic:File)"),
                     ("report", "(edu.utah.sci.vistrails.basic:File)"),
                     ("roc", "(edu.utah.sci.vistrails.basic:File)")]

    def compute(self):
        global maxent_path

        ourMaxent = MaxentRunner.MAXENTRunner()
        ourMaxent.outputDir = utils.mknextdir(prefix='maxentFiles_')
        
        ourMaxent.inputMDS = self.forceGetInputFromPort('inputMDS').name
        
        ourMaxent.maxentpath = maxent_path
        
        MaxentArgsCSV = os.path.join(ourMaxent.outputDir, "MaxentArgs.csv")
        
        argWriter = csv.writer(open(MaxentArgsCSV, 'wb'))
        argWriter.writerow(['parameter','value'])
        for port in self._input_ports:
            #print port
            if port[0] <> 'inputMDS' and port[0] <> 'projectionlayers':
                if self.hasInputFromPort(port[0]):
                    port_val = self.getInputFromPort(port[0])
                    if port[1] == "(edu.utah.sci.vistrails.basic:Boolean)":
                        port_val = str(port_val).lower()
                    elif (port[1] == "(edu.utah.sci.vistrails.basic:Path)" or \
                        port[1] == "(edu.utah.sci.vistrails.basic:File)"):
                        port_val = port_val.name
                    argWriter.writerow([port[0], port_val])
                else:
                    kwargs = port[2]
                    try:
                        if port[1] == "(edu.utah.sci.vistrails.basic:Boolean)":
                            default = kwargs['defaults'][2:-2].lower()
                        else:
                            default = kwargs['defaults'][2:-2]
                        #args[port[0]] = default
                        argWriter.writerow([port[0], default])
                    except KeyError:
                        pass
        if self.hasInputFromPort('projectionlayers'):
            value = self.forceGetInputListFromPort('projectionlayers')
            projlayers = ','.join([path.name for path in value])
            argWriter.writerow(['projectionlayers', projlayers])
            
        argWriter.writerow(['inputMDS', ourMaxent.inputMDS])
        del argWriter
        ourMaxent.argsCSV = MaxentArgsCSV
        ourMaxent.logger = utils.getLogger()
        try:
            ourMaxent.run()
        except TrappedError as e:
            raise ModuleError(self, e.message)  
        except:
            utils.informative_untrapped_error(self, "Maxent")
        
         #set outputs
        lambdasfile = os.path.join(ourMaxent.outputDir, ourMaxent.args["species_name"] + ".lambdas")
        output_file = utils.create_file_module(lambdasfile)
        self.setResult("lambdas", output_file)
        
        
        rocfile = os.path.join(ourMaxent.outputDir, 'plots', ourMaxent.args["species_name"] + "_roc.png")
        output_file = utils.create_file_module(rocfile)
        self.setResult("roc", output_file)

        htmlfile = os.path.join(ourMaxent.outputDir, ourMaxent.args["species_name"] + ".html")
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
    
    input_ports.append(('inputMDS', '(gov.usgs.sahm:MergedDataSet:Other)'))
    
    docs = {}
    basic_pkg = 'edu.utah.sci.vistrails.basic'
    p_type_map = {'file/directory': 'Path',
                  'directory':'Directory',
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
    
    load_port_docs()   
    
    r_path = os.path.abspath(configuration.r_path)
    maxent_path = os.path.abspath(configuration.maxent_path)
    utils.r_path = r_path    
    
    session_dir = utils.createrootdir(configuration.output_dir)
    utils.createLogger(session_dir, configuration.verbose)

    color_breaks_csv = os.path.abspath(os.path.join(os.path.dirname(__file__),  "ColorBreaks.csv"))
    
    load_max_ent_params()
    
    global layers_csv_fname
    
    writetolog("*" * 79)
    writetolog("Initializing:", True, True)
    writetolog("  Locations of dependencies")
#    writetolog("   Layers CSV = " + os.path.join(os.path.dirname(__file__), 'layers.csv'))
    writetolog("   Layers CSV = " + layers_csv_fname)
    writetolog("   R path = " + r_path)
    writetolog("   GDAL folder = " + os.path.abspath(configuration.gdal_path))
#    writetolog("        Must contain subfolders proj, gdal-data, GDAL")
    writetolog("   Maxent folder = " + maxent_path)
#    writetolog("   QGIS folder = " + os.path.abspath(configuration.qgis_path))
#    writetolog("        Must contain subfolders qgis1.7.0, OSGeo4W")
    writetolog("    ")
    writetolog("*" * 79)
    
    writetolog("*" * 79)
    writetolog(" output directory:   " + session_dir)
    writetolog("*" * 79)
    writetolog("*" * 79)
    
def finalize():
    pass
    #utils.cleantemps()#No longer used  

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
    global layers_csv_fname
    layers_csv_fname = os.path.join(os.path.dirname(__file__), 'layers.csv')
    csv_reader = csv.reader(open(layers_csv_fname, 'rU'))
    csv_reader.next()
    first_file = csv_reader.next()[0]
    
    #if the first file in the layers file does not exist assume that none
    #of them do and use the exampledata version
    if not os.path.exists(first_file):
        print (("!" * 30) + " WARNING " + ("!" * 30) + "\n")*3
        print "The first grid in your layers CSV could not be found."
        print "Defaulting to the example data csv."
        print "fix/set paths in file " + layers_csv_fname + " to enable this functionality."
        print "See documentation for more information on setting up the layers.csv\n"
        print (("!" * 30) + " WARNING " + ("!" * 30) + "\n")*3
        layers_csv_fname = os.path.join(os.path.dirname(__file__), 'layers.exampledata.csv')
    
#    #####Only for testing tutorial data
#    layers_csv_fname = os.path.join(os.path.dirname(__file__), 'layers.exampledata.csv')
    
    csv_reader = csv.reader(open(layers_csv_fname, 'rU'))
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
                             '(gov.usgs.sahm:%s:DataInput)' % class_name, True)]})
        modules.append((module, {'configureWidgetType': config_class, 
                                 'moduleColor':input_color,
                                 'moduleFringe':input_fringe}))
        for module in modules:
            module[0]._output_ports.append(('value_as_string', '(edu.utah.sci.vistrails.basic:String)', True))
            
    return modules

def load_port_docs():
    csv_file = os.path.abspath(os.path.join(os.path.dirname(__file__),  "PortDocs.csv"))
    csvReader = csv.DictReader(open(csv_file, "r"))
    port_docs = {}
    for row in csvReader:
        k = row['Module'] + row['Port'] + row['Direction']
        port_docs[k] = row
        
    utils.port_docs = port_docs

input_color = (0.76, 0.76, 0.8)
input_fringe = [(0.0, 0.0),
                    (0.25, 0.0),
                    (0.0, 1.0)]
  
model_color = (0.76, 0.8, 0.76)
model_fringe = [(0.0, 0.0),
                    (0.25, 0.5),
                    (0.0, 1.0)] 

output_color = (0.8, 0.8, 0.76)
output_fringe = [(0.0, 0.0),
                    (0.25, 0.0),
                    (0.0, 1.0)]

_modules = generate_namespaces({'DataInput': [
                                              (Predictor, {'moduleColor':input_color,
                                                           'moduleFringe':input_fringe}),
                                              (PredictorListFile, {'moduleColor':input_color,
                                                           'moduleFringe':input_fringe}),
                                              (FieldData, {'moduleColor':input_color,
                                                           'moduleFringe':input_fringe}),
                                              (TemplateLayer, {'moduleColor':input_color,
                                                           'moduleFringe':input_fringe}),] + \
                                              build_predictor_modules(),
                                'Tools': [FieldDataQuery,
                                          FieldDataAggregateAndWeight,
                                          MDSBuilder,
                                          PARC,
                                          RasterFormatConverter,
#                                          ProjectionLayers,
                                          ModelEvaluationSplit,
                                          ModelSelectionSplit,
                                          ModelSelectionCrossValidation,
                                          CovariateCorrelationAndSelection,
#                                          ApplyModel
                                          ],                                          
                                'Models': [(GLM, {'moduleColor':model_color,
                                                           'moduleFringe':model_fringe}),
                                           (RandomForest, {'moduleColor':model_color,
                                                           'moduleFringe':model_fringe}),
                                           (MARS, {'moduleColor':model_color,
                                                           'moduleFringe':model_fringe}),
                                           (MAXENT, {'moduleColor':model_color,
                                                           'moduleFringe':model_fringe}),
                                           (BoostedRegressionTree, {'moduleColor':model_color,
                                                           'moduleFringe':model_fringe}),],
                                'Other':  [(Model, {'abstract': True}),
                                           (ResampleMethod, {'abstract': True}),
                                           (AggregationMethod, {'abstract': True}),
                                           (PredictorList, {'abstract': True}),
                                           (MergedDataSet, {'abstract': True}),
                                           (ResponseType, {'abstract': True}),
                                           (RastersWithPARCInfoCSV, {'abstract': True}),
                                           (PointAggregationMethod, {'abstract': True}),
                                           (ModelOutputType, {'abstract': True}),
                                           ],
                                'Output': [(SAHMModelOutputViewerCell, {'moduleColor':output_color,
                                                           'moduleFringe':output_fringe}),
                                          (SAHMSpatialOutputViewerCell, {'moduleColor':output_color,
                                                           'moduleFringe':output_fringe})
                                          ]
                                })


#
##ad hoc code to convert the current docs to xml
#load_port_docs()  
#
##the XML file we're writing it all out to
#from xml.etree import ElementTree as ET
#root_element = ET.Element("Documentation")
#
#
#for k in locals().keys():
#    print locals()[k]
#    try:
#        testinstance = locals()[k]()
#        if isinstance(testinstance, Module) and \
#            "sahm" in testinstance.__class__.__module__:
#            #We have a sahm module to add to the doc
#            mod = ET.SubElement(root_element, "Module")
#            title = ET.SubElement(mod, "Title")
#            title.text = k
#            desc = ET.SubElement(mod, "Description")
#            desc.text = testinstance.__doc__
#            inPorts = ET.SubElement(mod, "InputPorts")
#            for inPort in testinstance._input_ports:
#                port = ET.SubElement(inPorts, "Port")
#                portName = ET.SubElement(port, "PortName")
#                portName.text = inPort[0]
#                definition = ET.SubElement(port, "Definition")
#                try:
#                    definition.text = utils.port_docs[k+portName.text+"in"]["Definition"]
#                except:
#                    definition.text = "NA"
#                manditory = ET.SubElement(port, "Manditory")
#                try:
#                    manditory.text = utils.port_docs[k+portName.text+"in"]["Mandatory"]
#                except:
#                    manditory.text = "False"
#                default = ET.SubElement(port, "Default")
#                try:
#                    default.text = utils.port_docs[k+portName.text+"out"]["Default"]
#                except:
#                    default.text = "NA"   
#                options = ET.SubElement(port, "Options")
#                try:
#                    options.text = utils.port_docs[k+portName.text+"in"]["Options"]
#                except:
#                    options.text = "NA"
#                connections = ET.SubElement(port, "Connections")
#                try:
#                    connections.text = utils.port_docs[k+portName.text+"in"]["ConnectsTo"]
#                except:
#                    connections.text = "NA"
#            outPorts = ET.SubElement(mod, "OutputPorts")          
#            for outPort in testinstance._output_ports:
#                port = ET.SubElement(outPorts, "Port")
#                portName = ET.SubElement(port, "PortName")
#                portName.text = outPort[0]
#                definition = ET.SubElement(port, "Definition")
#                try:
#                    definition.text = utils.port_docs[k+portName.text+"out"]["Definition"]
#                except:
#                    definition.text = "ToDo"
#                manditory = ET.SubElement(port, "Manditory")
#                try:
#                    manditory.text = utils.port_docs[k+portName.text+"out"]["Mandatory"]
#                except:
#                    manditory.text = "False"  
#                default = ET.SubElement(port, "Default")
#                try:
#                    default.text = utils.port_docs[k+portName.text+"out"]["Default"]
#                except:
#                    default.text = "NA"     
#                options = ET.SubElement(port, "Options")
#                try:
#                    options.text = utils.port_docs[k+portName.text+"out"]["Options"]
#                except:
#                    options.text = "NA"
#                connections = ET.SubElement(port, "Connections")
#                try:
#                    connections.text = utils.port_docs[k+portName.text+"out"]["ConnectsTo"]
#                except:
#                    connections.text = "NA" 
#                    
#                    
#                    
#            refs = ET.SubElement(mod, "References")
#            ref = ET.SubElement(refs, "Reference")
#            ref.text = "NA"
#            seeAlso = ET.SubElement(mod, "SeeAlso")
#            seAlso.text = "NA"
#    except:
#        pass
#
#outfile = open(r"C:\temp\sahmdoc.xml", "w")
#outfile.write(ET.tostring(root_element))
#outfile.close()