###############################################################################
#  #
#  # Copyright (C) 2010-2012, USGS Fort Collins Science Center.
#  # All rights reserved.
#  # Contact: talbertc@usgs.gov
#  #
#  # This file is part of the Software for Assisted Habitat Modeling package
#  # for VisTrails.
#  #
#  # "Redistribution and use in source and binary forms, with or without
#  # modification, are permitted provided that the following conditions are met:
#  #
#  #  - Redistributions of source code must retain the above copyright notice,
#  #    this list of conditions and the following disclaimer.
#  #  - Redistributions in binary form must reproduce the above copyright
#  #    notice, this list of conditions and the following disclaimer in the
#  #    documentation and/or other materials provided with the distribution.
#  #  - Neither the name of the University of Utah nor the names of its
#  #    contributors may be used to endorse or promote products derived from
#  #    this software without specific prior written permission.
#  #
#  # THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
#  # AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
#  # THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
#  # PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
#  # CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
#  # EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
#  # PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
#  # OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
#  # WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
#  # OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
#  # ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
#  #
#  # Although this program has been used by the U.S. Geological Survey (USGS),
#  # no warranty, expressed or implied, is made by the USGS or the
#  # U.S. Government as to the accuracy and functioning of the program and
#  # related program material nor shall the fact of distribution constitute
#  # any such warranty, and no responsibility is assumed by the USGS
#  # in connection therewith.
#  #
#  # Any use of trade, firm, or product names is for descriptive purposes only
#  # and does not imply endorsement by the U.S. Government.
###############################################################################

################################################################################
#  ImageViewer widgets/toolbar implementation
################################################################################
import os
import csv
import gc
import itertools
import copy

import utils
import math

try:
    import fiona
    from fiona import crs
    from shapely.geometry import shape
    from matplotlib.patches import Polygon
except ImportError:
    fiona = None



from PyQt4 import QtCore, QtGui
from vistrails.core.modules.vistrails_module import Module
from vistrails.packages.spreadsheet.basic_widgets import SpreadsheetCell
from vistrails.packages.spreadsheet.spreadsheet_cell import QCellWidget, QCellToolBar
from vistrails.packages.spreadsheet.spreadsheet_controller import spreadsheetController
from vistrails.core.packagemanager import get_package_manager


from utils import map_ports

from pySAHM.utilities import dbfreader as dbfreader
import pySAHM.utilities as utilities
import pySAHM.SpatialUtilities as SpatialUtilities

import matplotlib
#  from matplotlib.backends.backend_qt4agg import FigureCanvasQTAgg as FigureCanvas
#  from matplotlib.backends.backend_qt4agg import NavigationToolbar2QTAgg as NavigationToolbar
#  from matplotlib.backends.backend_qt4 import FigureCanvasQT as FigureCanvas
#  from matplotlib.backends.backend_qt4 import NavigationToolbar2QT as NavigationToolbar
from matplotlib.figure import Figure
from matplotlib.offsetbox import AnchoredOffsetbox, TextArea
import matplotlib.colors as colors

import numpy as np

from osgeo import gdal, gdalconst, ogr

import GenerateModuleDoc as GenModDoc
doc_file = os.path.abspath(os.path.join(os.path.dirname(__file__), "documentation.xml"))
GenModDoc.load_documentation(doc_file)

import spatial_modules
from spatial_modules import BaseGeoViewerCell, GeoSpatialViewerCell, SpatialViewerCellWidgetBase, \
    GeneralSpatialViewerToolBar, ViewStateBoundariesButton

class ModelMapViewer(BaseGeoViewerCell):
    """
    SAHMModelOutputViewerCell is a VisTrails Module that
    displays the various output from a SAHM Model run in a single cell
    """
    __doc__ = GenModDoc.construct_module_doc('ModelMapViewer')
    _input_ports = copy.deepcopy(BaseGeoViewerCell._input_ports)
    _input_ports.pop(_input_ports.index(('vector_layers', '(edu.utah.sci.vistrails.basic:Dictionary)', {'optional': False})))
    _input_ports.extend([('display_presense_points', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'["False"]', 'optional':True}),
                    ('display_absense_points', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'["False"]', 'optional':True}),
                    ('display_background_points', '(edu.utah.sci.vistrails.basic:Boolean)', {'defaults':'["False"]', 'optional':True}),
                    ('initial_raster_display', '(edu.utah.sci.vistrails.basic:String)',
                     {'entry_types': "['enum']",
                      'values': "[['Probability', 'Binary Probability', 'Residuals', 'Mess', 'MoD']]", 'optional': True,
                      'defaults':'["Probability"]'}),
                    ('model_workspace', '(edu.utah.sci.vistrails.basic:Directory)'),
                    ('vector_layers', '(edu.utah.sci.vistrails.basic:Dictionary)', {'optional': True})])

    #  all inputs are determined relative to the model_workspace
    @classmethod
    def provide_input_port_documentation(cls, port_name):
        return GenModDoc.construct_port_doc(cls, port_name, 'in')
    @classmethod
    def provide_output_port_documentation(cls, port_name):
        return GenModDoc.construct_port_doc(cls, port_name, 'out')

    def __init__(self):
        BaseGeoViewerCell.__init__(self)

        self.port_map = {'display_presense_points': ("display_pres_points", None, True),
            'display_absense_points': ("display_abs_points", None, True),
            'display_background_points': ("display_backs_points", None, True),
            'initial_raster_display': ("initial_raster", None, True),
            "model_workspace": ("model_workspace", utils.get_relative_path, True)}

#    @print_timing
    def compute(self):
        self.inputs = map_ports(self, self.port_map)
        self.inputs.update(self.parse_inputs())

        self.inputs["model_dir"] = self.inputs["model_workspace"]

        for model_output in ['prob', 'bin', 'resid', 'mess', 'MoD']:
            try:
                self.inputs[model_output + "_map"] = os.path.join(self.inputs["model_dir"],
                                utils.find_file(self.inputs["model_dir"], "_" + model_output + "_map.tif"))
            except:
                self.inputs[model_output + "_map"] = ""

        try:
            self.inputs["mds"] = self.find_mds(self.inputs["model_dir"])
        except RuntimeError:
            self.inputs["mds"] = ""

        self.inputs["model_tag"] = os.path.split(self.inputs["model_dir"])[1]

        utils.set_sheet_location(self)

        if utils.check_if_model_finished(self.inputs["model_dir"]):
            self.local_displayAndWait(self.inputs)

#    @print_timing
    def local_displayAndWait(self, inputs):
        self.displayAndWait(SAHMSpatialOutputViewerCellWidget,
                            inputs)

    def find_mds(self, model_dir):
        """returns the path to the mds that was used to generate this
        model output.  While the text file that the R model produces
        has an absolute path to the data this function assumes that
        the mds file is in the session folder that this model output
        folder is in.  That is it looks for an mds with the same
        file name in the parent folder of the model folder.
        """
        model_text = os.path.join(model_dir,
                            utils.find_file(model_dir, "_output.txt"))
        #  assumed to be one level up from model folder.
        session_folder = os.path.split(model_dir)[0]

        f = open(model_text, 'rb')
        lines = f.read().splitlines()

        #  grab the line after "Data:"
        try:
            originalMDS = [lines[i + 1] for i in range(len(lines))
                  if lines[i].startswith("Data:")][0].strip()


            fname = os.path.split(originalMDS)[1]
            mds_in_root = os.path.join(session_folder, fname)
            if os.path.exists(originalMDS):
                return originalMDS
            elif os.path.exists(mds_in_root):
                return mds_in_root
            else:
                raise RuntimeError('Valid input MDS file not found in Model text output.')
        except IndexError:
            raise RuntimeError('Valid input MDS file not found in Model text output.')


class SAHMSpatialOutputViewerCellWidget(SpatialViewerCellWidgetBase):
    """

    """
    def __init__(self, parent=None):
        SpatialViewerCellWidgetBase.__init__(self, parent)
        self.display_title = True

    def set_toolbars(self):
        self.toolBarType = SAHMSpatialViewerToolBar
        self.controlBarType = SAHMSpatialViewerToolBar

    def load_layers(self):
#        print "SAHMSpatialOutputViewerCellWidget load_layers"
        self.all_layers = {"prob_map":{"type":"raster", "title":"Probability" , "categorical":False, "display_min":0, "display_max":1, 'cmap':matplotlib.cm.jet, "displayorder":9999, "displayed":False, "enabled":False, "file":""},
                         "bin_map":{"type":"raster", "title":"Binary probability" , "categorical":False, "display_min":0, "display_max":1, 'cmap':matplotlib.cm.Greys, "displayorder":9999, "displayed":False, "enabled":False, "file":""},
                         "resid_map":{"type":"raster", "title":"Residuals" , "categorical":False, "display_min":"pullfromraster", "display_max":"pullfromraster", 'cmap':matplotlib.cm.Accent, "displayorder":9999, "displayed":False, "enabled":False, "file":""},
                         "mess_map":{"type":"raster", "title":"Mess" , "categorical":False, "display_min":-100, "display_max":100, "categories":False, 'cmap':matplotlib.cm.PiYG, "displayorder":9999, "displayed":False, "enabled":False, "file":""},
                         "MoD_map":{"type":"raster", "title":"MoD" , "categorical":True, "display_min":0, "display_max":"pullfromraster", 'cmap':matplotlib.cm.Spectral, "displayorder":9999, "num_breaks":7, "displayed":False, "enabled":False, "file":""},
                         "pres_points":{"type":"Vector", "title":"Presence", "color":(1, 0, 0), "displayorder":3, "num_breaks":7, "displayed":False, "enabled":True, "file":""},
                         "abs_points":{"type":"Vector", "title":"Absence", "color":(0, 1, 0), "displayorder":2, "num_breaks":7, "displayed":False, "enabled":True, "file":""},
                         "backs_points":{"type":"Vector", "title":"Background", "color":(0, 0, 0), "displayorder":1, "num_breaks":7, "displayed":False, "enabled":True, "file":""}}

        #  set the inital layers to display passed on passed in inputs
        for layer in ["display_pres_points", "display_abs_points", "display_backs_points"]:
            self.all_layers[layer.replace("display_", "")]["displayed"] = self.inputs[layer]

        initial_map_dict = {'Probability':"prob_map", 'Binary Probability':"bin_map",
                                       'Residuals':"resid_map", 'Mess':"mess_map", 'MoD':"MoD_map"}
        self.all_layers[initial_map_dict[self.inputs['initial_raster']]]["displayed"] = True

        for k, v in self.all_layers.items():
            if k in self.inputs:
                if os.path.exists(self.inputs[k]):
                    self.all_layers[k]["file"] = self.inputs[k]
                    self.all_layers[k]["enabled"] = True

        self.set_raster(initial_map_dict[self.inputs['initial_raster']])
        self.vector_layers = self.inputs['vector_layers']

        #  make our specialty colormaps
        if self.all_layers["resid_map"]['enabled']:
            self.all_layers["resid_map"]["cmap"] = self.make_resid_cmap(self.all_layers["resid_map"])

#          if self.all_layers["MoD_map"]['enabled']:
#              self.all_layers["MoD_map"]["cmap"] = self.make_categorical_cmap(self.all_layers["MoD_map"])

#          if self.all_layers["mess_map"]['enabled']:
#              self.all_layers["mess_map"]["cmap"] = self.make_mess_cmap(self.all_layers["mess_map"])

    def set_raster(self, key):
        raster_kwargs = {'raster_file':self.all_layers[key]["file"],
                             'display_min':self.all_layers[key]["display_min"],
                             'display_max':self.all_layers[key]["display_max"],
                             'cmap':self.all_layers[key]["cmap"],
                             'categorical':self.all_layers[key]["categorical"],
                             'threeBand':False
                             }
        if raster_kwargs['categorical']:
            uniques, labels, cmap = spatial_modules.make_categorical_cmap(raster_kwargs)
            raster_kwargs['cmap'] = cmap
            raster_kwargs['unique_vals'] = uniques
            raster_kwargs['unique_labels'] = labels

        SpatialViewerCellWidgetBase.set_raster_base(self, raster_kwargs)

    def loadPoints(self):
        #  initialize our arrays
        for pointType in ['abs_points', 'pres_points', 'backs_points']:
            self.all_layers[pointType]['x'] = []
            self.all_layers[pointType]['y'] = []


        if os.path.exists(self.inputs["mds"]):
            mdsReader = csv.reader(open(self.inputs["mds"], 'r'))
            header = mdsReader.next()
            header2 = mdsReader.next()
            header3 = mdsReader.next()

            for row in mdsReader:
                if int(row[2]) > 0:
                    self.all_layers['pres_points']['x'].append(float(row[0]))
                    self.all_layers['pres_points']['y'].append(float(row[1]))
                elif int(row[2]) == 0:
                    self.all_layers['abs_points']['x'].append(float(row[0]))
                    self.all_layers['abs_points']['y'].append(float(row[1]))
                else:
                    self.all_layers['backs_points']['x'].append(float(row[0]))
                    self.all_layers['backs_points']['y'].append(float(row[1]))

        for pointType in ['abs_points', 'pres_points', 'backs_points']:
            self.all_layers[pointType]["enabled"] = len(self.all_layers[pointType]['x']) > 0

    def add_title(self, title):
        self.title_box = AnchoredText(title,
                          loc=2, frameon=True, pad=.05, borderpad=0.2)
        self.title_box.patch.set_boxstyle("round,rounding_size=0.2")
        self.title_box.set_alpha(0.1)
        self.title_box.set_zorder(9999)  #  put the legend on top
        self.axes.add_artist(self.title_box)

    def make_resid_cmap(self, kwargs):

        vals_min, vals_max = SpatialUtilities.get_raster_minmax(kwargs['file'])
        diff = vals_max - vals_min
        zero_ratio = abs(vals_min) / diff

        cdict = {'red': ((0.0, 1.0, 0.0),
                         (zero_ratio, 0.53, 1.0),
                         (1.0, 1.0, 1.0)),
                 'green': ((0.0, 1.0, 0.0),
                           (zero_ratio, 1, 1.0),
                           (1.0, 0.0, 1.0)),
                 'blue': ((0.0, 0.0, 1.0),
                          (zero_ratio, 1, 0.0),
                          (1.0, 0.0, 1.0))}

        return matplotlib.colors.LinearSegmentedColormap('my_colormap', cdict, 256)

    def make_mess_cmap(self, kwargs):
        vals_min, vals_max = SpatialUtilities.get_raster_minmax(kwargs['file'])
        diff = vals_max - vals_min
        zero_ratio = abs(vals_min) / diff

        cdict = {'red': ((0.0, 1.0, 0.0),
                         (zero_ratio, 0.53, 1.0),
                         (1.0, 1.0, 1.0)),
                 'green': ((0.0, 1.0, 0.0),
                           (zero_ratio, 1, 1.0),
                           (1.0, 0.0, 1.0)),
                 'blue': ((0.0, 0.0, 1.0),
                          (zero_ratio, 1, 0.0),
                          (1.0, 0.0, 1.0))}

        vals_min, vals_max = SpatialUtilities.get_raster_minmax(kwargs['file'])
        diff = vals_max - vals_min
        zero_ratio = abs(vals_min) / diff

        cdict = {'red': ((0.0, 1.0, 0.0),
                         (zero_ratio, 0.53, 1.0),
                         (1.0, 1.0, 1.0)),
                 'green': ((0.0, 1.0, 0.0),
                           (zero_ratio, 1, 1.0),
                           (1.0, 0.0, 1.0)),
                 'blue': ((0.0, 0.0, 1.0),
                          (zero_ratio, 1, 0.0),
                          (1.0, 0.0, 1.0))}

        return matplotlib.colors.LinearSegmentedColormap('my_colormap', cdict, 256)



#    @print_timing
    def on_draw(self, view_extent=None):
        """ Completely clears then redraws the figure
        There's probably a more efficient way to do this.
        """
#          if UseMaxExt:
#              display_extent = self.getMaxExtent()
#
#          if not display_extent:
#              display_extent = self.get_extent()
#
#          self.fig.clear()
#          self.add_axis()

        displayed_keys = [key for key in self.all_layers.keys() if self.all_layers[key]['displayed']]
        displayed_keys = sorted(displayed_keys, key=lambda disp_key: self.all_layers[disp_key]['displayorder'])
        #  loop through all_layers and display the enabled layers
        #  only displayed layers sorted by display order
        title = self.inputs["model_tag"] + "\n"

        try:
            displayed_raster = [r for r in displayed_keys if self.all_layers[r]["type"] == "raster"][0]
            self.set_raster(displayed_raster)
            title += self.all_layers[displayed_raster]['title']
        except IndexError:
            pass

        SpatialViewerCellWidgetBase.on_draw_base(self, view_extent=view_extent)

        displayed_points = [r for r in displayed_keys if self.all_layers[r]["type"] == "Vector"]
        for point_type in displayed_points:
            if self.all_layers[point_type]['enabled']:
                self.add_points(point_type)

        self.add_title(title)
        self.title_box.set_visible(self.display_title)

    def add_points(self, layername):
        kwargs = self.all_layers[layername]
        if not kwargs.has_key("x"):
            self.loadPoints()

        if self.all_layers[layername]['enabled']:
            self.axes.scatter(kwargs['x'], kwargs['y'], s=10, c=kwargs['color'], linewidth=0.5, antialiased=True)


class ViewTitleButton(QtGui.QAction):
    def __init__(self, parent=None):
        icon = os.path.abspath(os.path.join(
                    os.path.dirname(__file__), "data", "Images", "titlelegend.png"))
        QtGui.QAction.__init__(self,
                               QtGui.QIcon(icon),
                               "Show/Hide Title",
                               parent)
        self.setCheckable(True)
        self.setChecked(True)

    def triggeredSlot(self):
        cellWidget = self.toolBar.getSnappedWidget()

        active_cells = cellWidget.get_active_cells()
        for cell in active_cells:
            cell.display_title = self.isChecked()
            cell.title_box.set_visible(cell.display_title)
            cell.fig.canvas.draw()
            cell.update()

class sync_changes(QtGui.QAction):

    def __init__(self, parent=None):
        self.sync_options = itertools.cycle(["all", "sel", "one"])

        QtGui.QAction.__init__(self,
                               self.getIcon(self.sync_options.next()),
                               r"Apply changes to all / selected / single cell",
                               parent)
        self.setCheckable(True)
        self.setChecked(True)


    def getIcon(self, tag):
        icon = os.path.abspath(os.path.join(
                    os.path.dirname(__file__), "data", "Images", tag + ".png"))
        return QtGui.QIcon(icon)

    def triggeredSlot(self):
        cellWidget = self.toolBar.getSnappedWidget()
        next_option = self.sync_options.next()
        self.setIcon(self.getIcon(next_option))
        cellWidget.sync_changes = next_option

class ViewLayerAction(QtGui.QAction):
    def __init__(self, action_dict, parent=None):
        icon = os.path.abspath(os.path.join(
                    os.path.dirname(__file__), "data", "Images", action_dict["icon"]))
        QtGui.QAction.__init__(self,
                               QtGui.QIcon(icon),
                               action_dict["label"],
                               parent)
        self.setCheckable(True)
        self.setChecked(action_dict["checked"])
        self.tag = action_dict["tag"]
        self.group = action_dict["group"]

    def triggeredSlot(self, checked=False):

        self.toggleOthers()
        self.displayLayer()
        self.toolBar.updateToolBar()

    def toggleOthers(self):
        '''Unselect the other raster or vector layers
        '''
        for action in self.toolBar.actions():
            if "group" in dir(action) and \
                action.group == self.group and \
                action.tag <> self.tag:
                action.setChecked(False)

    def displayLayer(self):
        '''Display all the layers that have their
        actions selected in the toolbar
        '''
        cellWidget = self.toolBar.getSnappedWidget()
        active_cells = cellWidget.get_active_cells()

        for cell in active_cells:
            if isinstance(cell, SAHMSpatialOutputViewerCellWidget):
                #  set all to not displayed
                for k, v in cell.all_layers.items():
                    v['displayed'] = False
                #  turn on the selected layers
                for action in self.toolBar.actions():
                    try:
                        if action.isChecked() and cell.all_layers.has_key(action.tag):
                            cell.all_layers[action.tag]['displayed'] = True
                    except AttributeError:
                        pass  #  ignore buttons that don't have a tag set
                cell.on_draw(view_extent=cell.get_extent())
                cell.fig.canvas.draw()

                cell.update()

class MPL_action(QtGui.QAction):

    def __init__(self, action_dict, parent=None):
        icon = os.path.abspath(os.path.join(
                    os.path.dirname(__file__), "data", "Images", action_dict["icon"]))
        QtGui.QAction.__init__(self,
                               QtGui.QIcon(icon),
                               action_dict["label"],
                               parent)
        self.setToolTip(action_dict["tooltip"])
        self.setCheckable(action_dict["checkable"])
        self.setChecked(action_dict["checked"])
        self.actionfunc = action_dict["actionfunc"]
        self.tag = action_dict["label"]

    def triggeredSlot(self, checked=False):

        cellWidget = self.toolBar.getSnappedWidget()

        if self.tag in self.tag in ["Pan", "Zoom"]:
            if self.isChecked():
                cursor = self.tag
            elif self.tag == 'Pan':
                cursor = "Zoom"
            elif self.tag == 'Zoom':
                cursor = "Pan"

        active_cells = cellWidget.get_active_cells()
        for cell in active_cells:

            if self.tag in ["Pan", "Zoom"]:
                zoomaction = cell.getActionByTag("Zoom")
                panaction = cell.getActionByTag("Pan")

                if cursor == "Zoom" and \
                    (not zoomaction.isChecked() or cellWidget is cell):
                    cell.mpl_toolbar.zoom()
                elif cursor == "Pan" and \
                    (not panaction.isChecked() or cellWidget is cell):
                    cell.mpl_toolbar.pan()

                zoomaction.setChecked(cursor == "Zoom")
                panaction.setChecked(cursor == "Pan")

            else:
                eval("cell.mpl_toolbar." + self.actionfunc + "()")

    def getAction(self, name):
        for action in self.parent().actions():
            if hasattr(action, "actionfunc") and \
                action.actionfunc == name:
                return action
        return None


class SAHMSpatialViewerToolBar(GeneralSpatialViewerToolBar):
    """
    The toolbar that allows users to toggle layers on and off
    in the widget

    """
    def add_layers_actions(self):
        '''add the actions (buttons) associated with turning layers on and off
        '''
        lyrs_label = QtGui.QLabel()
        lyrs_label.setText("Layers:")
        self.appendWidget(lyrs_label)


        sw = self.getSnappedWidget()

        actions = [{"tag":"pres_points", "icon":"RedPoints.png",
                     "checked":False, "label":"Display presence points",
                     "group":"pres_points"},
                   {"tag":"abs_points", "icon":"GreenPoints.png",
                     "checked":False, "label":"Display absence points",
                     "group":"abs_points"},
                   {"tag":"backs_points", "icon":"BlackPoints.png",
                     "checked":False, "label":"Display background points",
                     "group":"backs_points"},
                   {"tag":"prob_map", "icon":"ProbMap.png",
                     "checked":True, "label":"Display probability map",
                     "group":"Grids"},
                   {"tag":"bin_map", "icon":"BinMap.png",
                     "checked":False, "label":"Display binary map",
                     "group":"Grids"},
                   {"tag":"resid_map", "icon":"ResMap.png",
                     "checked":False, "label":"Display residuals map",
                     "group":"Grids"},
                   {"tag":"mess_map", "icon":"MesMap.png",
                     "checked":False, "label":"Display Multivariate Environmental Similarity Surface (Mess) map",
                     "group":"Grids"},
                    {"tag":"MoD_map", "icon":"ModMap.png",
                     "checked":False, "label":"Display Most Dissimilar Variable (MoD) map",
                     "group":"Grids"}]

        for action_dict in actions:
            self.appendAction(ViewLayerAction(action_dict, self))

        self.appendAction(ViewStateBoundariesButton(self))

    def add_other_actions(self):
        '''Add the actions(buttons) associated with map navigation
        i.e. zoom, pan, extents
        '''
        self.addSeparator()
        other_label = QtGui.QLabel()
        other_label.setText("  Other:")
        self.appendWidget(other_label)
        self.appendAction(spatial_modules.SyncChangesButton(self))
        self.appendAction(spatial_modules.showColorbarButton(self))
        self.appendAction(ViewTitleButton(self))
        mpl_save = {"icon":"filesave.png", "checked":False, "label":"Save",
                     "tooltip":"Save the figure", "checkable":False,
                     "actionfunc":"save_figure"}
        self.appendAction(spatial_modules.MPLButton(mpl_save, self))

    def updateToolBar(self):
        QCellToolBar.updateToolBar(self)
        sw = self.getSnappedWidget()

        for action in self.actions():
            if type(action) == ViewLayerAction:
                #  disenable all action refering to data we don't have
                action.setEnabled(sw.all_layers[action.tag]['enabled'])
                action.setChecked(sw.all_layers[action.tag]['displayed'])
            elif type(action) == ViewStateBoundariesButton:
                action.setEnabled(not fiona is None)
                action.setChecked(sw.display_states)
        sw.popMenu = self.gen_popup_menu()


class AnchoredText(AnchoredOffsetbox):
    def __init__(self, s, loc, pad=0.4, borderpad=0.5, prop=None, frameon=True):

        self.txt = TextArea(s, minimumdescent=False)

        super(AnchoredText, self).__init__(loc, pad=pad, borderpad=borderpad,
                                           child=self.txt,
                                           prop=prop,
                                           frameon=frameon)
