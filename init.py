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
        

        output_file = create_file_module(output_fname)
        self.setResult('fieldData', output_file)

class PARC(Module):
    '''
    This class provides a widget to run the PARC module which
    provides functionality to sync raster layer properties
    with a template dataset
    '''

    configuration = []
    _input_ports = [('predictor', "(gov.usgs.sahm:Predictor:DataInput)"),
                                ('PredictorList', '(gov.usgs.sahm:PredictorList:DataInput)'),
                                ('templateLayer', '(gov.usgs.sahm:TemplateLayer:DataInput)'),
                                ('resampleMethod', '(edu.utah.sci.vistrails.basic:String)'),
                                ('aggregationMethod', '(edu.utah.sci.vistrails.basic:String)')]

    _output_ports = [('PredictorLayersDir', '(edu.utah.sci.vistrails.basic:Directory)')]

    def compute(self):
        if configuration.verbose:
            print "Running PARC"
        
        ourPARC = parc.PARC()
        output_dname = mktempdir(prefix='parc')
        
        if configuration.verbose:
            ourPARC.verbose = True
        
        
        ourPARC.template = self.forceGetInputFromPort('templateLayer').name
        if self.hasInputFromPort('resampleMethod'):
            ourPARC.AggByPixel = self.getInputFromPort('resampleMethod')
        if self.hasInputFromPort('aggregationMethod'):
            ourPARC.AggByPixel = self.getInputFromPort('aggregationMethod')
        ourPARC.outDir = output_dname

        predictor_list = self.forceGetInputFromPort('PredictorList', [])
        predictor_list.extend(self.forceGetInputListFromPort('predictor'))

        predictors = []
        for predictor in predictor_list:
            predictors.append(os.path.join(predictor.name))

        ourPARC.inputs = predictors
        
        ourPARC.parcFiles()

#        ourPARCer.main(args)
        predictorsDir = create_dir_module(output_dname)
        if configuration.verbose:
            print "Finished running PARC\n"
        self.setResult('PredictorLayersDir', predictorsDir)


class PredictorList(Constant):
    _input_ports = expand_ports([('value', 'DataInput|PredictorList'),
                                 ('addPredictor', 'DataInput|Predictor')])
    _output_ports = expand_ports([('value', 'DataInput|PredictorList')])
    
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
            f_list = [create_file_module(v_elt[1]) for v_elt in v]
        else:
            f_list = v
        p_list += f_list
        self.setResult("value", p_list)

def load_max_ent_params():    
    maxent_fname = os.path.join(os.path.dirname(__file__), 'maxent.csv')
    csv_reader = csv.reader(open(maxent_fname, 'rU'))
    # pass on header
    csv_reader.next()
    input_ports = []
    docs = {}
    basic_pkg = 'edu.utah.sci.vistrails.basic'
    p_type_map = {'file/directory': 'Path',
                  'double': 'Float'}
    for row in csv_reader:
        [name, flag, p_type, default, doc] = row
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
