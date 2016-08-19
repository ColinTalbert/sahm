"""Utility functions for obtaining, querying, and modifying VisTrails
 workflows and pipelines

These were written for and are used for data management and archiving
"""

import os
import csv

import pandas as pd

from vistrails.core.application import get_vistrails_application
import utils
# assumption Colin has a different method of referencing .utils?

from .. import *  # gets configuration from __init__.py from parent directory
import utils
utils.setrootdir(configuration.cur_session_folder)

def get_current_history_node():
    """
    Queries the current history tree and returns the name (tag)
    and change count of the currently active history node

    Returns
    -------
        tuple (str, int)
            the first item is the name (tag) of the currently selected
                history node.  'root' will be returned if no named node is
                selected
            the second item is the integer count of unnamed nodes (changes)
                since the last named node
    """
    controller = get_vistrails_application().get_current_controller()
    cur_version = controller.current_version
    cur_vt = controller.vistrail
    cur_name = cur_vt.get_pipeline_name(cur_version)
    if "+" in cur_name:
        count = int(cur_name.split(' + ')[-1])
        cur_name = " ".join(cur_name.split()[:-2])
    else:
        count = 0

    return cur_name, count


def _get_current_pipeline():
    """
    return the pipeline on the current workflow canvas, which cooresponds to
        the currently selected history node

    Returns
    -------
        pipeline
    """
    controller = get_vistrails_application().get_current_controller()
    return controller.current_pipeline


def get_current_copy_list():
    """

    Returns
    list
    a list of files and directories that are required to reproduce a workflow
    -------

    """

    cur_pipeline = _get_current_pipeline()

    copy_list = []

    for m in cur_pipeline.modules.itervalues():
        if m.name in ['FieldData', 'templateLayer', 'Predictor']:
            copy_list = _pull_param(m, 'file', copy_list)
        elif m.name in ['ModelEvaluationSplit', 'ModelSelectionSplit',
                        'ModelSelectionCrossValidation',
                        'CovariateCorrelationAndSelection']:
            copy_list = _pull_param(m, 'inputMDS', copy_list)
        elif m.name == 'PredictorListFile':
            copy_list = _pull_param(m, 'csvFileList', copy_list)
        elif m.name == 'MDSBuilder':
            copy_list = _pull_param(m, 'RastersWithPARCInfoCSV', copy_list)
            copy_list = _pull_param(m, 'backgroundProbSurf', copy_list)
        elif m.name == 'BackgroundSurfaceGenerator':
            params = _module_params(m)
            print "Not handled ", m.name, params
        elif m.name == 'EnsembleBuilder':
            params = _module_params(m)
            print "Not handled ", m.name
        elif m.name == 'PARC':
            copy_list = _pull_param(m, 'RastersWithPARCInfoCSV', copy_list)
        elif m.name == 'Reclassifier':
            params = _module_params(m)
            print "Not handled ", m.name
        elif m.name == 'CategoricalToContinuous':
            params = _module_params(m)
            print "Not handled ", m.name
        elif m.name == 'RasterFormatConverter':
            params = _module_params(m)
            print "Not handled ", m.name
        elif m.name in ['GLM', 'RandomForest', 'MARS',
                        'BoostedRegressionTree', 'MAXENT', 'UserDefinedCurve']:
            copy_list = _pull_param(m, 'mdsFile', copy_list)

    output_names = _get_output_names(cur_pipeline)
    subfolder, subname = output_names[0]
    if len(output_names) != 1 and not subfolder:
        pass
        #TODO:implement a msgbox with the message "This functionality is designed to work with workflows that use a single outputname module which specifies a subfolder name"

    session_dname = configuration.cur_session_folder
    subfolder = os.path.join(session_dname, subfolder)

    subdir_contents = [os.path.join(subfolder, f) for f in os.listdir(subfolder)]
    copy_list.extend(subdir_contents)

    return copy_list


def _pull_param(module, param_name, result_list=[]):
    """

    Parameters
    ----------
    module  : vistrails module from pipeline
    module  : str
        the parameter name to be queried on this module
    result_list : list
        The result that will be extended
    Returns
    -------
    list
        The list passed in appended with the querried results
    """
    params = _module_params(module)
    fnames = params.get(param_name, '')
    if fnames:
        for fname in fnames:
            result_list.append(utils.get_relative_path(fname))
    return result_list


def _module_params(module):
    """

    Parameters
    ----------
    module  : vistrails module from pipeline

    Returns
    -------
    dict
        key = parameter(input port) name
        value = list of the str representation of parameter values
    """
    results = {}
    for f in module.functions:
        results[f.db_name] = [p.db_val for p in f.params]
    return results


def _get_output_names(pipeline):
    """
    Query a pipeline to find all OutputName modules and return a list
        of tuples of the subfolder_name and run_name of each
        OutputName module in the pipeline

    Parameters
    ----------
    pipeline : VisTrails pipeline
        A single vistrails pipeline

    Returns
    -------
    list of tuples
        each tuple is (subfolder_name, run_name)

    """
    output_names = []
    for m in pipeline.modules.itervalues():
        if m.name == 'OutputName':
            params = _module_params(m)
            subfolder = params.get('subfolder_name', [''])[0]
            subname = params.get('run_name', '')
            output_names.append((subfolder, subname))
    return output_names


def create_big_inputs_list(big_input_fname):
    """
    creates a csv file which lists all of the 'big' inputs that are required by
    a workflow

    Parameters
    ----------
    big_input_fname  : str
        the filename to create and fill with the list of big inputs

    Returns
    None
    -------

    """
    cur_pipeline = _get_current_pipeline()
    for m in cur_pipeline.modules.itervalues():
        if m.name in ['templateLayer', 'Predictor']:
            template_fnames = _pull_param(m, 'file')
            _append_big_inputs(big_input_fname, template_fnames)
        elif m.name == 'PredictorListFile':
            raster_fnames = _pull_param(m, 'csvFileList')
            for raster_fname in raster_fnames:
                local_raster_fname = utils.get_relative_path(raster_fname)
                df = pd.read_csv(local_raster_fname)
                rasters = list(df.iloc[:, 0])
                _append_big_inputs(big_input_fname, rasters)

        elif m.name == 'PARC':
            raster_fnames = _pull_param(m, 'RastersWithPARCInfoCSV')
            for raster_fname in raster_fnames:
                local_raster_fname = utils.get_relative_path(raster_fname)
                df = pd.read_csv(local_raster_fname)
                rasters = list(df.iloc[:, 0])
                _append_big_inputs(big_input_fname, rasters)


def _append_big_inputs(big_input_fname, file_list):
    """

    Parameters
    ----------
    big_input_fname : str
        The file path to the csv that contains a list of the 'big' data that
        went into this workflow.
    file_list : list
        list of filepaths to add to big_input_fname

    Returns
    -------

    """
    if not os.path.exists(big_input_fname):
        csvfile = open(big_input_fname, 'wb')
        out_csv = csv.writer(csvfile, delimiter=',',
                                quotechar='|', quoting=csv.QUOTE_MINIMAL)
        out_csv.writerow(["workflow_name", "full_path", "hash"])
    else:
        csvfile = open(big_input_fname, 'ab')
        out_csv = csv.writer(csvfile, delimiter=',',
                             quotechar='|', quoting=csv.QUOTE_MINIMAL)

    for fname in file_list:
        full_fname = utils.get_relative_path(fname)
        raster_fnames = utils.get_raster_files(full_fname)
        full_fname = utils.get_raster_name(full_fname)
        hash_value = utils.hash_file(raster_fnames)
        out_csv.writerow([fname, full_fname, hash_value])

    csvfile.close()
