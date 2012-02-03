# -*- coding: latin-1 -*-

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
    FieldData

Description:
        The FieldData module allows a user to add presence/absence points or count data recorded
    across a landscape for the phenomenon being modeled (e.g., plant sightings, evidence of animal
    presence, etc.).  The input data for this module must be in the form of a .csv file that follows
    one of two formats:

        Format 1:
        A .csv file with the following column headings, in order: "X," "Y," and "responseBinary".
    In this case, the "X" field should be populated with the horizontal (longitudinal) positional
    data for a sample point. The "Y" field should be populated with the vertical (latitudinal) data
    for a sample point. These values must be in the same coordinate system/units as the template
    layer used in the workflow. The column "responseBinary" should be populated with either a '0'
    (indicating absence at the point) or a '1' (indicating presence at the point).

        Format 2:
        A .csv file with the following column headings, in order: "X," "Y," and "responseCount".  In
    this case, the "X" field should be populated with the horizontal (longitudinal) positional data
    for a sample point. The "Y" field should be populated with the vertical (latitudinal) data for a
    sample point. These values must be in the same coordinate system/units as the template layer
    used in the workflow. The column "responseCount" should be populated with either a '-9999'
    (indicating that the point is a background point) or a numerical value (either '0' or a positive
    integer) indicating the number of incidences of the phenomenon recorded at that point.

Input Ports:
    None

Output Ports:
    value:  (mandatory)
        This is the actual file object that is being passed to other modules in the workflow.
        Common connections:
            The 'fieldData_file' input port of the FieldDataQuery Module if the field data needs
                    subsetting or aggregation.
            The 'fieldData' input port of the FieldDataAggregateAndWeight Module if the field data
                    needs to be aggregated or weighted to match the spatial resolution of the
                    template layer.
            The 'fieldData' input port of the MDS builder Module if the field data needs no further
                    pre-processing prior to modeling.

    value_as_string:  (optional)
        This is a VisTrails port that is not used in general SAHM workflows.
        Common connections:
            This does not commonly connect to other SAHM modules.
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
    
class Predictor(Constant):
    '''
    Description:
        The Predictor module allows a user to select a single raster layer for consideration in the
    modeled analysis. Besides selecting the file the user also specifies the parameters to use for
    resampling, aggregation, and whether the data is categorical.

Input Ports:
    categorical:  (optional)
        This paramater allows a user to indicate the type of data represented.  The distinction
        between continuous and categorical data will maintained throught a workflow by appending the
        word '_categorical' to categorical layer names in the resulting MDS file.  It is also import
        to select the nearest neighbor resampling option for categorical layers.
        
        Default value = False (Unchecked)
        
        Options are:
            True (Checked) - The data contained in the raster layer is categorical (e.g., landcover
                    categories).
            False(Unchecked) - The data contained in the raster is continuous (e.g., a DEM layer).

    ResampleMethod:  (mandatory)
        The resample method employed to interpolate new cell values when transforming the raster
        layer to the coordinate space or cell size of the template layer.
        
        Options are:
            near:  nearest neighbour resampling Fastest algorithm, worst interpolation quality, but
                    best choice for categorical data.
            bilinear:  bilinear resampling, good choice for continuous data.
            cubic:   cubic resampling.
            cubicspline:  cubic spline resampling.
            lanczos:  Lanczos windowed sinc resampling.
            see: http://www.gdal.org/gdalwarp.html for context

    AggregationMethod:  (mandatory)
        The aggregation method to be used in the event that the raster layer must be up-scaled to
        match the template layer (e.g., generalizing a 10 m input layer to a 100 m output layer).
        Care should be taken to ensure that the aggregation method that best preserves the integrity
        of the data is used.  See the PARC module documentation for more information on how
        resampling and aggregation are performed.
        
        Options are:
            Mean:  Average value of all constituent pixels used.
            Max:   Maximum value of all constituent pixels used.
            Min:   Minimum value of all constituent pixels used.
            Majority:   The value occuring most frequently in constituent pixels used.
            None:   No Aggregation used.

    file:  (mandatory)
        The location of the raster file. A user can navigate to the location on their file system.
        When a user is selecting an ESRI grid raster, the user should navigate to the 'hdr.adf' file
        contained within the grid folder

Output Ports:
    value:  (mandatory)

        
        Common connections:
            The output from this port only connects to the PARC input port 'predictor'.

    value_as_string:  (optional)
        This is a VisTrails port that is not used in general SAHM workflows.
        
        Common connections:
            Does not generally connect to other SAHM modules.
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
    PredictorListFile

Description:
        The PredictorListFile module allows a user to load a .csv file containing a list of rasters
    for consideration in the modeled analysis. The .csv file should contain a header row and four
    columns containing the following information, in order, for each raster input.

        Column 1: The full file path to the input raster layer.

        Column 2: A binary value indicating whether the input layer is categorical or not.  A value
    of "0" indicates that an input raster is non-categorical data (continuous), while a value of "1"
    indicates that an input raster is categorical data.

        Column 3: The resampling method employed to interpolate new cell values when transforming
    the raster layer to the coordinate space or cell size of the template layer, if necessary. The
    resampling type should be specified using one of the following values: "nearestneighbor,"
    "bilinear," "cubic," or "lanczos."

        Column 4: The aggregation method to be used in the event that the raster layer must be up-
    scaled to match the template layer (e.g., generalizing a 10 m input layer to a 100 m output
    layer). Care should be taken to ensure that the aggregation method that best preserves the
    integrity of the data is used. The aggregation should be specified using one of the following
    values: "Min," "Mean," "Max," "Majority," or "None."

        In formatting the list of predictor files, the titles assigned to each of the columns are
    unimportant as the module retrieves the information based on the order of the values in the .csv
    file (the ordering of the information and the permissible values in the file however, are
    strictly enforced). The module also anticipates a header row and will ignore the first row in
    the .csv file.



Input Ports:
    csvFileList:  (optional)
        This is the CSV file on the file system.  While not strictly manditory this port will almost
        always have an input.

    predictor:  (optional)
        Allows a user to add individual Predictor modules to a PredictorListFile
        
        Common connections:
            The output port 'value' of a Predictor module.

Output Ports:
    RastersWithPARCInfoCSV:  (mandatory)
        This port generally connects to the input port 'RastersWithPARCInfoCSV' on the PARC module.
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
    TemplateLayer

Description:
        The second fundamental input in an analysis is the template layer.  It is used to define the
    extent and resolution that will be used in all subsequent analyses.  The TemplateLayer is a
    raster data layer with a defined coordinate system, a known cell size, and an extent that
    defines the study area. The data type and values in this raster are not important.  All
    additional raster layers used in the analysis will be resampled and reprojected as needed to
    match the template, snapped to the template, and clipped to have an extent that matches the
    template. Users should ensure that additional covariates considered in the analysis have
    complete coverage of the template layer used.

Input Ports:
    None

Output Ports:
    value:  (mandatory)
        This is the actual file object that is being passed to other modules in the workflow.
        
        Common connections:
            The 'TemplateLayer' input port of the FieldDataAggregationAndWeight Module.
            The 'TemplateLayer' input port of the PARC Module.

    value_as_string:  (optional)
        This is a VisTrails port that is not used in general SAHM workflows.
        
        Common connections:
            This does not commonly connect to other SAHM modules.
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
MDSBuilder

Description:
        The Merged Data Set (MDS) Builder module is a utility that extracts the values of each
    predictor layer to the point locations included in the field data set. The module produces a
    .csv file that contains the x and y locations of the sample points and a column indicating
    whether each point represents a presence recording, an absence recording, a presence count, or a
    background point.  Following these first three columns, each environmental predictor layer is
    appended as a column with row entries representing the value present in the raster layer at each
    field sample point.  There are a total of three header rows in the output .csv of the
    MDSBuilder. The first row contains the columns "x," "y," "ResponseBinary" or "ResponseCount,"
    and the names of each of the raster predictor files that were passed to the MDS Builder. The
    second row contains a binary value indicating whether the column should be included when the
    model is finally applied; these values are later modified during the Covariate Correlation and
    Selection process that takes place downstream in the workflow. The final header row contains the
    full path on the file system to each of the raster predictor files.

        The output from this module is in the format expected by most of the pre-modeling data
    manipulation modules, all of the model modules, as well as the RasterFormatConverter module.  As
    such it can reasonabily be connected to numerous other modules depending on the type of modeling
    being conducted.  A typical workflow would linearly connect MDSBuilder -> ModleEvaluationSplit
    -> ModelSelectionSplit or ModelSelectionCrossValidation -> CovariateCorrelationAndSelection ->
    any or all of the models (BoostedRegressionTree, GLM, MARS, RandomForest, Maxent).  If using
    Maxent the output from CovariateCorrelationAndSelection would also go into the
    RasterFormatConverter module which woulc connect to the projectionlayers of the maxent module.

Input Ports:
    RastersWithPARCInfoCSV:  (mandatory)
        This is a csv file which contains information about all of the predictors used. The user
        will not generally need to create or edit this file as it is an output of the PARC module.

        The folowing columns are in a RastersWithPARCInfoCSV:
          PARCOutputFile - The raster file produced by PARC
          Categorical - 0=not categorical data, 1=categorical data
          Resampling - One of NearestNeighbor, bilinear, cubic, cubicspline, or lanczos
          Aggregation - One of min, max, mean, or majority
          OriginalFile - The location and name of the input file used by PARC
        
        Common connections:
            This port will generally connect with the ouput from the PARC module.

    fieldData:  (mandatory)
        The field data input corresponds to a .csv file containing presence/absence points or count
        data recorded across a landscape for the phenomenon being modeled (e.g., plant sightings,
        evidence of animal presence, etc.).
        
        Common connections:
            The output port of the FieldData Module
            The output port of the FieldDataQuery Module
            The output port of the FieldDataAggregationAndWeight Module

    backgroundPointCount:  (optional)
        This is an optional value that specifies how many randomly placed packground points to add
        to the output.  These points will be randomly placed at pixel centroids within the template
        extent with no more than one point assigned to any one pixel. In typical SAHM workflows
        these points are only used by the Maxent modeling package.  These points will be added to
        the output .csv file with a value of "-9999" denoting them as background points.
        
        Default value = 0, which is to say that no background point are added to the output.

    backgroundProbSurf:  (optional)
        Background Probability Surface: This is an optional parameter that applies only to workflows
        that employ the Maxent modeling package. In some analyses, it may be appropriate to
        spatially limit background points to a particular subset of the study area (e.g., islands
        within a study area polygon, particular regions within a study area polygon, or a region
        determined by the known bias present in the field data).  Specifying a background
        probability surface raster allows a user to control where random points will be scattered
        within the extent of the study area. The raster layer specified by a user should have the
        same projection and extent as the template layer and contain values ranging from 0 to 100.
        These values represent the probability that a randomly generated point will be retained
        should it fall within a particular cell.  That is, randomly generated points will not be
        generated in any part of the probability grid with a value of "0" while all points falling
        in an area with a value of "100" will be retained. A point falling in an area with a value
        of "50" will be kept as a background point 50% of the time.
        
        Default value = 0 (No background points are added to the output.)
        
        Options are:
            Any positive integer.

    Seed:  (optional)
        The seed is used to be able recreate a specific output.  The seed used in each run will be
        noted on the console output and saved in the output log in the session folder.
        
        Default value = If no seed is specified a random seed between -1*((2^32)/2) and ((2^32)/2) will be generated and used.
        
        Options are:
            Any integer between -1*((2^32)/2) and ((2^32)/2)

Output Ports:
    mdsFile:  (optional)
        This is the CSV flat file containing the location data and values extracted from each of the
        covariates.
        
        Common connections:
            The input port 'InputMDS' in the ModelEvaluationSplit module.
            The input port 'InputMDS' in the ModelSelectionSplit module.
            The input port 'InputMDS' in the ModelSelectionCrossValidation module.
            The input port 'InputMDS' in the CovariateCorrelationAndSelection module.
            The input port 'InputMDS' in the RasterFormatConverter module.
            The input port 'InputMDS' in the Maxent module.
            The input port 'InputMDS' in the BoostedRegressionTree module.
            The input port 'InputMDS' in the GLM module.
            The input port 'InputMDS' in the MARS module.
            The input port 'InputMDS' in the RandomForest module.
    '''

    _input_ports = [('RastersWithPARCInfoCSV', '(gov.usgs.sahm:RastersWithPARCInfoCSV:Other)'),
                                 ('fieldData', '(gov.usgs.sahm:FieldData:DataInput)'),
                                 ('backgroundPointCount', '(edu.utah.sci.vistrails.basic:Integer)'),
                                 ('backgroundProbSurf', '(edu.utah.sci.vistrails.basic:File)'),
                                 ('Seed', '(edu.utah.sci.vistrails.basic:Integer)')]
                            
    
    _output_ports = [('mdsFile', '(gov.usgs.sahm:MergedDataSet:Other)')]

    @classmethod
    def provide_input_port_documentation(cls, port_name):
        return utils.construct_port_msg(cls, port_name, 'in')
    @classmethod
    def provide_output_port_documentation(cls, port_name):
         return utils.construct_port_msg(cls, port_name, 'out')

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

Description:
        Often raw field data come to us in a format that contains more information than we need to
    include in any single model.  This can take the form of additional columns that contain
    extraneous information, additional columns that contain occurrence data for additional species,
    or rows that from a time period, collection method, or species that we are not interested in
    modeling.  The Field Data Query module contains functionality to subset and reformat this output
    into the format used by the SAHM package.
            Columns can be specified with either a positional argument (1, 2, 3, etc) if you want to
    select the first, second, third etc column.  Note these numbers start from 1.  Alternatively you
    can select a column based on name by entering the text of the column name found in the header.

            When selecting rows there are two types of queries that can be specified:
               Simple - Select a Query_Column and enter a value in the Query port.  If the value for
    a row in the selected column equals the value entered in the Query that row will be kept.  For
    example you might have a 'year' column and you would want to select all 2009 entries.  NOTE:  DO
    NOT ENCLOSE THE QUERY TEXT IN QUOTES IF YOU ARE TRYING TO MATCH A STRING!

               Complex - Optionally you can construct complex queries using Python syntax.  To do
    this enclose the column name in square brackets as part of a line of Python code.  Since the
    columns used are specified in the query string there is no need to use the Query_column port and
    it will be ignored.  For example to select years greater than 2005 you would use:  [year] > 2005
    To include string equality make sure you enclose the entire bracketed field name in quotes as
    well.  For example "[Observer]" == "Colin"  Complex queries involving multiple columns are
    possible as well, for example "[Observer]" != "Colin" and [year] > 2005.

Input Ports:
    fieldData_file:  (mandatory)
        The file containing Field data.  The acceptable formats vary but it must have a column with
        X, Y, and response values.  Additional columns are permissible.
        
        Common connections:
            This port can be connected to a FieldData module or the FieldData file can be specified
                    directly in the module information pane.

    x_column:  (optional)
        The column that contains the 'X' coordinates.  These values must be in the same coordinates,
        projection, and units as those defined in the template layer.

        Columns can be specified with either a positional argument (1, 2, 3, etc) if you want to
        select the first, second, third etc column.  Note these numbers start from 1.  Alternatively
        you can select a column based on name by entering the text of the column name found in the
        header.
        
        Default value = 1, which is to say the first column in the input field data file.
        
        Common connections:
            NA

    y_column:  (optional)
        The column that contains the 'Y' coordinates.  These values must be in the same coordinates,
        projection, and units as those defined in the template layer.

        Columns can be specified with either a positional argument (1, 2, 3, etc) if you want to
        select the first, second, third etc column.  Note these numbers start from 1.  Alternatively
        you can select a column based on name by entering the text of the column name found in the
        header.
        
        Default value = 2, which is to say the second column in the input field data file.

    Response_column:  (optional)
        The column that contains the response of interest.

        Columns can be specified with either a positional argument (1, 2, 3, etc) if you want to
        select the first, second, third etc column.  Note these numbers start from 1.  Alternatively
        you can select a column based on name by entering the text of the column name found in the
        header.
        
        Default value = 3, which is to say the third column in the input field data file.

    ResponseType:  (optional)
        The type of response recorded in the response column
        
        Default value = 'Presence(Absence)'
        
        Options are:
            'Presence(Absence)' = 1 for Presense, optionally also 0 for Absence and -9999 for
                    background points.
            'Count' = 0, 1, 2, 3 etc. observed count data.  Optionally also -9999 for background
                    points

    Response_Presence_value:  (optional)
        The value in the response column that will be taken to indicate a presense.
        
        Default value = By default any value in the list '1', 'True', 'T', 'Present', 'Presence' will be assigned a value of 1 (presence) in the output. Note: not case sensitive.
        
        Options are:
            And number or string can be entered, quotes are not required.

    Response_Absence_value:  (optional)
        The value in the response column that will be taken to indicate an absence.
        
        Default value = By default any value in the list '0', 'False', 'F', 'Absent', 'Absence' will be assigned a value of 0 (absence) in the output. Note: not case sensitive.
        
        Options are:
            And number or string can be entered, quotes are not required.

    Query_column:  (optional)
        The column which contains values you would like to use to with the simple equality query
        option.  The values in this column will be checked against the value entered in the query
        port.

    Query:  (optional)
        If using the simple equality query functionality simply enter the value you would like to
        filter on here.  NOTE:  DO NOT ENCLOSE THE QUERY TEXT IN QUOTES IF YOU ARE TRYING TO MATCH A
        STRING!  Also do not include any additional spaces.

        If using the complex Python syntax query a valid Python equality statement with the values
        from each individual row indicated with square bracketed field (header row) names.
        
        Options are:
            Simple - Select a Query_Column and enter a value in the Query port.  If the value for a
                    row in the selected column equals the value entered in the Query that row will
                    be kept.  For example you might have a 'year' column selected in the
                    Query_column port and enter 2009 here to to select all 2009 entries.  NOTE:  DO
                    NOT ENCLOSE THE QUERY TEXT IN QUOTES IF YOU ARE TRYING TO MATCH A STRING!
            Complex - Optionally you can construct complex queries using Python syntax.  To do this
                    enclose the column name in square brackets as part of a line of Python code.
                    Since the columns used are specified in the query string there is no need to use
                    the Query_column port and it will be ignored.  For example to select years
                    greater than 2005 you would use:  [year] > 2005  To include string equality make
                    sure you enclose the entire bracketed field name in quotes as well.  For example
                    "[Observer]" == "Colin"  Complex queries involving multiple columns are possible
                    as well, for example "[Observer]" != "Colin" and [year] > 2005.

Output Ports:
    fieldData:  (mandatory)
        The file containing field data.
        This output file will be in: X, Y, ResponseBinary/ResponseCount format.
        
        Common connections:
            FieldDataAggregateAndWeight FieldData - For collapsing or weighting points relative to
                    the template pixels.
            MDS_builder - fieldData - for modeling without using the FieldDataAggregateAndWeight
                    functionality.



    '''    
    _input_ports = [('fieldData_file', '(gov.usgs.sahm:FieldData:DataInput)'),
                                 ('x_column', '(edu.utah.sci.vistrails.basic:String)', {'defaults':'1'}),
                                 ('y_column', '(edu.utah.sci.vistrails.basic:String)', {'defaults':'2'}),
                                 ('Response_column', '(edu.utah.sci.vistrails.basic:String)', {'defaults':'3'}),
                                 ('Response_Presence_value', '(edu.utah.sci.vistrails.basic:String)', {'defaults':'1'}),
                                 ('Response_Absence_value', '(edu.utah.sci.vistrails.basic:String)', {'defaults':'0'}),
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
            'Response_Presence_value': ('res_pres_val', None, True),
            'Response_Absence_value': ('res_abs_val', None, True),
            'ResponseType': ('response_type', None, True),
            'Query_column': ('query_col', None, False),
            'Query': ('query', None, False),}
        
        FDQParams = utils.map_ports(self, port_map)
        FDQOutput = utils.mknextfile(prefix='FDQ_', suffix='.csv')
        
        infile = open(FDQParams['fieldData'], "rb")
        csvReader = csv.DictReader(infile)      

        outfile = open(FDQOutput, "wb")
        csvwriter = csv.writer(outfile)
        if FDQParams["response_type"] == 'Count':
            responsetype = 'responseCount'
        else:
            responsetype = 'responseBinary'
            
        csvwriter.writerow(['X','Y',responsetype,"input=" + infile.name])

        header = csvReader.fieldnames
        x_key = self.find_column(header,FDQParams['x_col'])
        y_key = self.find_column(header,FDQParams['y_col'])
        res_key = self.find_column(header,FDQParams['res_col'])
        
        use_query = False
        if self.hasInputFromPort('Query'):
            use_query = True
            query  = FDQParams['query']
            #check if we're using a simple (equality) or complex (python syntax) query
            use_complex = any(str in query for str in ['[' + str + ']' for str in header] )
            
        if self.hasInputFromPort('Query_column'):
            query_col_key = self.find_column(header,FDQParams['query_col'])
        else:
            query_col_key = None
        
        
        
        
        for row in csvReader:
            if not use_query:
                include_row = True
            elif use_complex:
                include_row = self.complex_query(row, query)
            else:
                include_row = self.simple_query(row, query, query_col_key)
                
            if include_row:
                response = row[res_key]
                if response.lower() in ["1", "true", "t", "present", "presence", FDQParams['res_pres_val']]:
                    response = 1
                elif response.lower() in ["0", "false", "f", "absent", "absense", FDQParams['res_abs_val']]:
                    response = 0
                else:
                    response = row[res_key]
                    
                csvwriter.writerow([row[x_key],
                                    row[y_key],
                                    response])
        
        del infile
        del outfile
        
        output_file = utils.create_file_module(FDQOutput) 
        self.setResult('fieldData', output_file) 
    
    
    def find_column(self, header, column):
        try:
            index = int(column) - 1
            if index > len(header) - 1:
                msg = "Field data input contains fewer columns than the number specified\n"
                msg += str(index + 1) + " is greater than " + str(len(header))
                writetolog(msg, True, True)
                raise ModuleError(self, msg)
            return header[index]
        except ValueError:
            if column in header:
                return column
            else:
                msg = "The specified column wasn't in the input file\n"
                msg += column_name + " not in " + str(header)
                writetolog(msg, True, True)
                raise ModuleError(self, msg)

    def simple_query(self, row, query, query_col):
        return row[query_col] == query
        
    def complex_query(self, row, query):
            
        for key in row.keys():
            query = query.replace('[' + key + ']', row[key])
        try:   
            return eval(query)
        except NameError:
            msg = "There was a 'NameError' in the complex query you entered.\n"
            msg += "This is often an indication that strings are not being properly quoted in the python syntax.\n"
            msg += "Try enclosing the [fieldName] item in quotes.\n\n"
            msg += 'For example:  "[SourceType]" == "Expert"  instead of  [SourceType] == "Expert"' 
            writetolog(msg, True, True)
            raise ModuleError(self, msg)

            
     
class FieldDataAggregateAndWeight(Module):
    '''
FieldDataAggregateAndWeight

Description:
        In many instances data collected in the field can be at a different spatial resolution than
    we are modeling at.  For example we might have observations collected every five meters along a
    200 m. transect when we are modeling with covariates with 1000 m. cells.  When running species
    distribution models (SDMs) such as those contained in SAHM, spatial issues need to be addressed
    in order to avoid introduction of pseudo-replication.   For instance, considering multiple field
    data observations which are all spatially located in the same modeled pixel will generate
    replicate values or redundant information.  When running a model, this redundancy causes pseudo-
    replication and can negatively influence model development.  The FieldDataAggregateAndWeight
    tool helps aggregate field data locations so only one field data observation is represented per
    pixel or multiple points are downweighted proportionately.

        Currently only GLM, MARS, and Boosted Regression Trees accept weights.  Any Weights column
    will be ignored by Random Forest.


Input Ports:
    templateLayer:  (mandatory)
        Raster file used to determine cell size and extent.
        Note - The projection and coordinate system used in the template file must match that given
        in the FieldData's X and Y columns.
        
        Common connections:
            This input port generally will connnect to the 'value' port of a TemplateLayer Module.

    fieldData:  (mandatory)
        The file containing field data.  Must be in X, Y, ResponseBinary/ResponseCount format
        
        Common connections:
            The'value' port of a FieldData module
            The 'fieldData' port of the FieldDataQuery module

    PointAggregationOrWeightMethod:  (mandatory)
        The method used to either weight or aggregate field data points.
        
        Options are:
            Collapse In Pixel : All field data points falling within a single pixel will be collapse
                    into a single point at the center of that pixel.  If using Presense(Absense)
                    data the point will be given a value of 1 if any are presense, otherwise 0 if
                    any are absense, otherwise -9999 if all are background.  If using count data the
                    point value will be the average of all points in a pixel.
            Weight Per Pixel : All field data points are retained but a weight column is added and
                    points in pixels with multiple points are given a weight of 1 over the number of
                    points in that pixel.  For example all the points in a pixel with 4 points would
                    be given a weight of 1/4.

Output Ports:
    fieldData:  (mandatory)
        This is a CSV file in a X,Y,Response,(Weight) format.
        
        Common connections:
            The input port 'fieldData' of the MDSBuilder module.
    '''
    _input_ports = [('templateLayer', '(gov.usgs.sahm:TemplateLayer:DataInput)'),
                                 ('fieldData', '(gov.usgs.sahm:FieldData:DataInput)'),
                                 ('PointAggregationOrWeightMethod', '(gov.usgs.sahm:PointAggregationMethod:Other)', {'defaults':'Collapse In Pixel'}),
                                 ('FD_EPSG_projection', '(edu.utah.sci.vistrails.basic:Integer)'),
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
            'SDofGaussianKernel': ('sd', None, False),
            'FD_EPSG_projection': ('epsg', None, False)}
        
        FDAWParams = utils.map_ports(self, port_map)
        output_fname = utils.mknextfile(prefix='FDAW_', suffix='.csv')
        writetolog("    output_fname=" + output_fname, True, False)
        FDAWParams['output'] = output_fname
        
        output_fname = utils.mknextfile(prefix='FDAW_', suffix='.csv')
        writetolog("    output_fname=" + output_fname, True, False)
        
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
        The Projection, Aggregation, Resampling, and Clipping (PARC) module is a powerful utility
    that automates the preparation steps required for using raster layers in most geospatial
    modeling packages. In order to successfully consider multiple environmental predictors in raster
    format, each layer must have coincident cells (pixels) of the same size, have the same
    coordinate system (and projection, if applicable), and the same geographic extent. The PARC
    module ensures that all of these conditions are met for the input layers by transforming and or
    reprojecting each raster to match the coordinate system of the template layer. This process
    usually involves aggregation (necessary when an input raster layer must be up-scaled to match
    the template layer-- e.g., generalizing a 10 m input layer to a 100 m output layer), and or
    resampling (necessary for interpolating new cell values when transforming the raster layer to
    the coordinate space or cell size of the template layer). Lastly, each raster predictor layer is
    clipped to match the extent of the template layer.

        The settings used during these processing steps follow a particular set of decision rules
    designed to preserve the integrity of data as much as possible. However, it is important for a
    user to understand how these processing steps may modify the data inputs. For additional
    information about the PARC module, please see the extended help and documentation for the SAHM
    package.

Input Ports:
    predictor:  (optional)
        A single raster with resampling, aggregation, and categorical options.
        
        Common connections:
            value' port of a Predictor module
            Note - Multiple single Predictor modules can be connected to this single input port.

    PredictorList:  (optional)
        This is an in memory data construct that contains a list of predictors each with resampling,
        aggregation, and categorical options.
        
        Common connections:
            value' port of any of the 'Individual Predictors selector' modules
            Note - Multiple single Predictors selectors modules can be connected to this single
                    input port.

    RastersWithPARCInfoCSV:  (optional)
        This is a CSV containing a list of files to include in the PARC operation.

        The format of this list conforms to the 'PredictorListFile' specs:
            Column 1: The full file path to the input raster layer including the drive.
            Column 2: A binary value indicating whether the input layer is categorical or not. A
        value of "0" indicates that an input raster is non-categorical data (continuous), while a
        value of "1" indicates that an input raster is categorical data.
            Column 3: The resampling method employed to interpolate new cell values when
        transforming the raster layer to the coordinate space or cell size of the template layer, if
        necessary. The resampling type should be specified using one of the following values:
        "nearestneighbor," "bilinear," "cubic," or "lanczos."
            Column 4: The aggregation method to be used in the event that the raster layer must be
        up-scaled to match the template layer (e.g., generalizing a 10 m input layer to a 100 m
        output layer). Care should be taken to ensure that the aggregation method that best
        preserves the integrity of the data is used. The aggregation should be specified using one
        of the following values: "Min," "Mean," "Max," "Majority," or "None."

        In formatting the list of predictor files, the titles assigned to each of the columns are
        unimportant as the module retrieves the information based on the order of the values in the
        .csv file (the ordering of the information and the permissible values in the file however,
        are strictly enforced). The module also anticipates a header row and will ignore the first
        row in the .csv file.
        
        Common connections:
            'value' port of PredictorListFile module

    templateLayer:  (mandatory)
        The templat layer raster file used to define the Extent, Cell size, Projection, raster snap,
        and coordinate system of the outputs.
        
        Common connections:
            'value' port of TemplateLayer module

    ignoreNonOverlap:  (optional)
        Option of using the intersection of all covariates and template or enforcing the template
        extent.
        
        Options are:
            True (checked) = Use intersection of all covariates extents.  Area of template extent
                    will be reduce such all covariate layers extents can be completely covered by
                    the new extent.
            False (Unchecked) = The template extent will be used for all outputs and an error will
                    be raised if any of the covariates are not completely covered by the template.

    multipleCores:  (optional)
        Option of running processing on multiple threads/cores.
        
        Options are:
            True (checked) = Individual layers will be run consectively on separate threads.
            False (Unchecked) = All processing will occur on the same thread as the main program.

Output Ports:
    RastersWithPARCInfoCSV:  (mandatory)
        The VisTrails output from the PARC module is a interim CSV file that contains information
        about each of the files processed.  This is used by the MDS builder to determine which files
        to extract values from and which layers are categorical.
        
        Common connections:
            Input port 'RastersWithPARCInfoCSV' of MDSBuilder module
    '''

    #configuration = []
    _input_ports = [('predictor', "(gov.usgs.sahm:Predictor:DataInput)"),
                                ('PredictorList', '(gov.usgs.sahm:PredictorList:Other)'),
                                ('RastersWithPARCInfoCSV', '(gov.usgs.sahm:RastersWithPARCInfoCSV:Other)'),
                                ('templateLayer', '(gov.usgs.sahm:TemplateLayer:DataInput)'),
                                ('ignoreNonOverlap', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'False', 'optional':True}),
                                ('multipleCores', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'True', 'optional':True})]

    _output_ports = [('RastersWithPARCInfoCSV', '(gov.usgs.sahm:RastersWithPARCInfoCSV:Other)')]
    
    @classmethod
    def provide_input_port_documentation(cls, port_name):
        return utils.construct_port_msg(cls, port_name, 'in')
    @classmethod
    def provide_output_port_documentation(cls, port_name):
         return utils.construct_port_msg(cls, port_name, 'out')
    
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
RasterFormatConverter

Description:
        The RasterFormatConverter module allows a user to easily convert between raster file types
    for a group of rasters. This group can be specified as either all the rasters in a single
    directory or the rasters specified in a single MDS file (see below).  All outputs will be sent
    to a folder named "ConvertedRasters" (followed by an underscore and a number corresponding to
    the run sequence of the module) within the user's current VisTrail's session folder.  Typically
    this module will be used within a workflow to convert the geotiff format used by the rest of the
    modules to the ascii format needed by Maxent.  But the following file formats are accepted for
    both inputs and outputs: Arc/Info ASCII Grid, ESRI BIL, ERDAS Imagine, and JPEG and others.  See
    the compiled by default options at http://www.gdal.org/formats_list.html for a complete list of
    the accepted file types.

Input Ports:
    inputMDS:  (optional)
        Any merged dataset (MDS) format csv can be used as input to this module.  All of the rasters
        pointed to in the third line of the file will be converted to the output format.
        
        Common connections:
            This can be connected to the output 'mdsFile' port on any of the following modules:
                    MDSBuilder, ModelEvaluationSplit, ModelSelectionSplit,
                    ModelCrossValidationSplit, or CovariateCorrelationAndSelection.

    inputDir:  (optional)
        An directory can be used to specify which files to process.  All of the rasters (of any
        format) will be converted to the output format specified.
        
        Common connections:
            This does not generally connect to any other SAHM modules.

    format:  (optional)
        The format to convert all the input grids into.  For Maxent this will be ASCII.
        
        Default value = Geotif
        
        Options are:
            Geotif
            Arc/Info Grid
            ASCII Grid
            ESRI Bil
            ERDAS Imagine
            JPEG
            BMP
            Additional uncommon file types are supported by GDAL.  For the complete list see the
                    'compiled by default' options at http://www.gdal.org/formats_list.html for a
                    complete list of the accepted file types.

    multipleCores:  (optional)
        Option of running processing on multiple threads/cores.
        
        Options are:
            True (checked) = Individual layers will be run consectively on separate threads.
            False (Unchecked) = All processing will occur on the same thread as the main program.

Output Ports:
    outputDir:  (optional)
        The directory where all output files will be saved to.
        
        Default value = This directory name is created by the module and it will be located in the session folder.
        
        Common connections:
            This port will connect to the maxent input port 'projectionlayers'.
    '''

    #configuration = []
    _input_ports = [("inputMDS", "(gov.usgs.sahm:MergedDataSet:Other)"),
                    ('inputDir', '(edu.utah.sci.vistrails.basic:Directory)'),
                    ('format', '(edu.utah.sci.vistrails.basic:String)'),
                    ('multipleCores', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'True', 'optional':True})]

    _output_ports = [('outputDir', '(edu.utah.sci.vistrails.basic:Directory)')]

    @classmethod
    def provide_input_port_documentation(cls, port_name):
        return utils.construct_port_msg(cls, port_name, 'in')
    @classmethod
    def provide_output_port_documentation(cls, port_name):
         return utils.construct_port_msg(cls, port_name, 'out')

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
    Model Evaluation Split

    The ModelEvaluationSplit module provides the opportunity to establish specific settings
    for how field data will be used in the modeling process. Three parameters can be set
    by the user:

    1. Ratio of Presence/Absence Points:
    This field is populated with a number corresponding to the desired proportion of
    presence and absence points to be used in the analysis. If populated, this entry should
    be a number greater than zero. (A value of '1' will result in an equal number of both
    presence and absence points being used, a value of '2' indicates that twice as many
    presence points will be used, a value of 0.5 indicates that twice as many absence points
    will be used, etc.). All field data points with a value equal to or greater than 1 are
    interpreted as presence points. Although the original field data is unmodified, this
    option will reduce the sample size as the merged dataset containing sample points will
    have points deleted from it to achieve the specified ratio. A warning will be generated
    if more than 50% of either the presence or absence points will be deleted based on the
    ratio specified by the user. Background points are ignored by this module (they are read
    in and written out, but not assigned to either the test or training split).

    When left empty, this field will default to 'null' and the model will use the existing
    presence/absence ratio present in the field data.

    2. Input Merged Data Set (MDS): 
    This is the input data set consisting of locational data for each sample point, the
    values of each predictor variable at those points, and if established, a field denoting
    the weight that will be assigned to each point in modeling. This input is usually provided
    by the upstream steps that precede the Test Training Split module. Any value entered here
    (e.g., specifying another existing MDS on the file system) will override the input
    specified by a model connection in the visual display.

    3. Training Proportion:
    This is the proportion of the sample points that will be used to train the model, relative
    to the total number of points. Entered values should be greater than 0 but less than 1.
    For example, a value of '0.9' will result in 90% of the sample points being used to train
    the model, with 10% of the sample being held out to test the model's performance. Choosing
    an appropriate training proportion can depend on various factors, such as the total number
    of sample points available.

    '''

    _input_ports = [("inputMDS", "(gov.usgs.sahm:MergedDataSet:Other)"),
                    ('trainingProportion', '(edu.utah.sci.vistrails.basic:Float)', 
                        {'defaults':'0.7'}),
                    ('RatioPresAbs', '(edu.utah.sci.vistrails.basic:Float)'),
                    ('Seed', '(edu.utah.sci.vistrails.basic:Integer)'),]
    _output_ports = [("outputMDS", "(gov.usgs.sahm:MergedDataSet:Other)")]
    
    @classmethod
    def provide_input_port_documentation(cls, port_name):
        return utils.construct_port_msg(cls, port_name, 'in')
    @classmethod
    def provide_output_port_documentation(cls, port_name):
         return utils.construct_port_msg(cls, port_name, 'out')
    
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
    ToDo: Marian to write
    '''        

    _input_ports = [("inputMDS", "(gov.usgs.sahm:MergedDataSet:Other)"),
                    ('trainingProportion', '(edu.utah.sci.vistrails.basic:Float)', 
                        {'defaults':'0.7'}),
                    ('RatioPresAbs', '(edu.utah.sci.vistrails.basic:Float)'),
                    ('Seed', '(edu.utah.sci.vistrails.basic:Integer)'),]
    _output_ports = [("outputMDS", "(gov.usgs.sahm:MergedDataSet:Other)")]

    @classmethod
    def provide_input_port_documentation(cls, port_name):
        return utils.construct_port_msg(cls, port_name, 'in')
    @classmethod
    def provide_output_port_documentation(cls, port_name):
         return utils.construct_port_msg(cls, port_name, 'out')
     
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
    ToDo: Marian to write
    '''        

    _input_ports = [("inputMDS", "(gov.usgs.sahm:MergedDataSet:Other)"),
                    ('nFolds', '(edu.utah.sci.vistrails.basic:Integer)', 
                        {'defaults':'10'}),
                    ('Stratify', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'True', 'optional':True}),
                    ('Seed', '(edu.utah.sci.vistrails.basic:Integer)'),]
    _output_ports = [("outputMDS", "(gov.usgs.sahm:MergedDataSet:Other)")]

    @classmethod
    def provide_input_port_documentation(cls, port_name):
        return utils.construct_port_msg(cls, port_name, 'in')
    @classmethod
    def provide_output_port_documentation(cls, port_name):
         return utils.construct_port_msg(cls, port_name, 'out')
         
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
    CovariateCorrelationAndSelection

Description:
        The CovariateCorrelationAndSelection view provides a breakpoint in the modeling workflow for
    the user to assess how well each variable explains the distribution of the sampled data points
    and to remove any variables that may exhibit high correlation with others.
        The display shows the n variables that have the highest total number of correlations above a
    threshold with other predictors using the maximum of the Pearson, Spearman and Kendall
    coefficient. The column heading over each variable displays the number of other variables with
    which the environmental predictor is correlated using the user supplied threshold which defaults
    to .7.  Radio buttons are available to limit the display and correlation calculations to any
    combination of presence, absence, or background points.  The first column in the plot shows the
    relationship between the response and each predictor.  Row labels indicate the maximum of the
    Spearman and Pearson correlation coefficient and a locally weighted smooth has been added to
    help distinguish the nature of the relationship.
        The remaining plots make up a square with histograms for each variable displayed on the
    diagonal.  Their respective graphical display and correlation with other variables can be found
    by locating the row/column intersection between each (above and below the diagonal).  The
    scatter plot along with a locally weight smooth is shown below the diagonal.  Presence records
    are represented by red points, absence by green, and background are yellow.  Above the diagonal
    is the correlation coefficient between the two predictors.  If Spearman or Kendall correlation
    coefficient is larger than the Pearson correlation coefficient then an s or k will show up in
    the bottom right corner of this box.

        A user is provided with the opportunity to select a new set of the environmental predictor
    variables and “Update” the Covariate Correlation screen to investigate the relationships among
    the new variables selected.  Variables with a high degree of correlation with other variables
    should generally be unchecked in their respective radio buttons, and will be excluded from
    subsequent analysis steps in the model workflow.
        Multiple iterations can be run at this screen, allowing the user to investigate the
    relationships among the environmental predictor variables and choose the most appropriate set to
    be used in the subsequent modeling. When the desired set of variables has been chosen, the “OK”
    button is selected and processing will resume in the VisTrails workflow.


Input Ports:
    inputMDS:  (mandatory)
        The file to select from.  If this file contains unselected layers (0 in the second header
        line) these will initially appear deselected in the GUI.
        
        Common connections:
            The inputMDS can come from any module that outpus an MDS file.  These are: MDSBuilder,
                    ModelEvaluationSplit, ModelSelectionSplit, and ModelSelectionCrossValidation.

    selectionName:  (optional)
        This serves two purposes.  First to uniquely identify a given selection.  This unique name
        is used to determine if a selection has been previously made, to apply for example.  And
        secondly to provide something that can be changed to trigger VisTrails to rerun this module
        even if nothing upstream has changed.
        
        Options are:
            Any text can be used.

    ShowGUI:  (optional)
        This Boolean indicates whether to stop execution and display the GUI for user interaction.
        In some cases such as exploration you might want to make a selection in a previous run and
        then change this to false so that the selection will be apply to subsequent runs without
        interrupting execution.
        
        Default value = True
        
        Options are:
            True - The GUI will be shown.
            False - The GUI will not be shown, execution will not be interrupted, but the previous
                    selection made with the specified selectionName will be applied to the current
                    MDS file.

    numPlots:  (optional)
        The number of variables to display at a time in the plot frame.
        
        Default value = 8
        
        Options are:
            An integer greater than 1 and generally no greater than 12

    minCor:  (optional)
        The minimum correlation used to summarize the number of other variables each variable is
        highly correlated with.
        
        Default value = 0.7
        
        Options are:
            A decimal number between 0 and 1.

    corsWithHighest:  (optional)
        If one desires to view only other parameters that have a correlation above the specified
        threshold with the parameter than has the highest number of total correlations with other
        parameters then this should be set to true.  Otherwise, by default, the parameters that are
        selected for display will be the set of parameters that have the highest number of
        correlations with other parameters above the given threshold.
        
        Default value = False
        
        Options are:
            True (Checked)
            False (Unchecked)

Output Ports:
    outputMDS:  
        This is the output MDS file with the user supplied selection applied.
        
        Common connections:
            The output from the CovariateCorrelationAndSelection will generally connect to one of
                    the model modules (BoostedRegressionTree, GLM, MARS, RandomForest, or Maxent)
            If using Maxent the output from CovariateCorrelationAndSelection might also conect to
                    the RasterFormatConverter.

    '''
    _input_ports = [("inputMDS", "(gov.usgs.sahm:MergedDataSet:Other)"),
                    ('selectionName', '(edu.utah.sci.vistrails.basic:String)', {'defaults':'initial'}),
                    ('ShowGUI', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'True'}),
                    ('numPlots', '(edu.utah.sci.vistrails.basic:Integer)', {'defaults':'8', 'optional':True}),
                    ('minCor', '(edu.utah.sci.vistrails.basic:Float)', {'defaults':'0.7', 'optional':True}),
                    ('corsWithHighest', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'False', 'optional':True})]
    _output_ports = [("outputMDS", "(gov.usgs.sahm:MergedDataSet:Other)")]

    @classmethod
    def provide_input_port_documentation(cls, port_name):
        return utils.construct_port_msg(cls, port_name, 'in')
    @classmethod
    def provide_output_port_documentation(cls, port_name):
         return utils.construct_port_msg(cls, port_name, 'out')
     
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
    '''
    
    '''

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
    
    doc_file = os.path.abspath(os.path.join(os.path.dirname(__file__),  "documentation.xml"))
    utils.load_documentation(doc_file)
    
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
#    print "For tutorial tesing uing the layers.exampledata.csv"
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
