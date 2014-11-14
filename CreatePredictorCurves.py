###############################################################################
##
## Copyright (C) 2010-2012, USGS Fort Collins Science Center. 
## All rights reserved.
## Contact: talbertc@usgs.gov
##
## This file is part of the Software for Assisted Habitat Modeling package
## for VisTrails.
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
## Although this program has been used by the U.S. Geological Survey (USGS), 
## no warranty, expressed or implied, is made by the USGS or the 
## U.S. Government as to the accuracy and functioning of the program and 
## related program material nor shall the fact of distribution constitute 
## any such warranty, and no responsibility is assumed by the USGS 
## in connection therewith.
##
## Any use of trade, firm, or product names is for descriptive purposes only 
## and does not imply endorsement by the U.S. Government.
###############################################################################


from PyQt4 import QtCore, QtGui
import csv
import numpy as np

import pandas as pd

#  import utils
#  from utils import writetolog
import os

import sys
sys.path.append(r'C:\GoogleDrive\work\Python\VisTrails_SAHM_x64\VisTrails\vistrails\packages')
sys.path.append(r'C:\GoogleDrive\work\Python\VisTrails_SAHM_x64\VisTrails\vistrails')
sys.path.append(r'C:\GoogleDrive\work\Python\VisTrails_SAHM_x64\VisTrails\vistrails\packages\spreadsheet')
from vistrails.core.modules.vistrails_module import Module

import matplotlib.transforms as mtransforms
from matplotlib.mlab import dist_point_to_segment
import matplotlib.colors as colors

import pySAHM.SpatialUtilities as SpatialUtilities
import seaborn as sns
sns.set(style="darkgrid")

import json


#      from core.system import execute_cmdline
    
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
#  to be replaces with an import from spatial modules later
class RasterDisplay(object):
    '''The idea behind this is from
    http://matplotlib.sourceforge.net/examples/event_handling/viewlims.py
    basically we want to only query as much data as we have screen pixels for.
    When the user zooms, pans, resizes we'll go back to the original display
    and get another set of pixels.

    This object has a pointer to the original raster and functions
    for switching the input file or getting an array of pixel values
    '''
    def __init__(self, width=300, height=300):
        self.height = height
        self.width = width
        self.display_min = None
        self.display_max = None
        self.cmap = None


    def switch_raster(self, raster_kwargs):
        '''A new raster fname was provided
        update the instance variables accordingly
        '''
        self.kwargs = raster_kwargs
        self.raster = SpatialUtilities.SAHMRaster(raster_kwargs['raster_file'])

        self.data_min, self.data_max = SpatialUtilities.get_raster_minmax(raster_kwargs['raster_file'])
        self.display_min, self.display_max = self.data_min, self.data_max

        if not raster_kwargs.get("display_max", 'pullfromraster') == 'pullfromraster':
            self.display_max = raster_kwargs['display_max']

        if not raster_kwargs.get("display_min", 'pullfromraster') == 'pullfromraster':
            self.display_min = raster_kwargs['display_min']

        self.three_band = raster_kwargs.get('three_band', False)

        self.cmap = raster_kwargs.get("cmap", mpl.cm.jet)

    def __call__(self, xlim, ylim):
        '''this function is called whenever the view updates
        We pull new pixels from our raster as needed
        '''

        #  reel these values in if they are outside our bounds
        xstart, xend = np.clip(xlim, self.raster.west, self.raster.east)
        ystart, yend = np.clip(ylim, self.raster.south, self.raster.north)

        if (xend - xstart) / self.raster.xScale > self.width:
            x_pixels = self.width
        else:
            x_pixels = None
        if (yend - ystart) / abs(self.raster.yScale) > self.height:
            y_pixels = self.height
        else:
            y_pixels = None

        if self.three_band:
            display_band = [1, 2, 3]
        else:
            display_band = 1

        ary = self.raster.get_block_bbox([xstart, ystart, xend, yend], x_pixels, y_pixels, display_band)
        if ary is None or ary.size == 1:
                print "raster_array is None!!!/n/n"
                return np.empty([1960, 1080])
        else:
                return ary

#    @print_timing
    def setDims(self, ax):
        #  Get the number of points from the number of pixels in the window
        dims = ax.axesPatch.get_window_extent().bounds
        self.width = int(dims[2] + 0.5)
        self.height = int(dims[3] + 0.5)

#    @print_timing
    def ax_update(self, ax):
        ax.set_autoscale_on(False)  #  Otherwise, infinite loop
        self.setDims(ax)

        #  Get the range for the new area
        xstart, ystart, xdelta, ydelta = ax.axes.viewLim.bounds
        xend = xstart + xdelta
        yend = ystart + ydelta

        factor = 0.1  #  we want to pull more pixels than we absolutely need
        #  so that we don't get white edges
        xBuff = (xend - xstart) * factor
        yBuff = (yend - ystart) * factor
        xstart = xstart - xBuff
        ystart = ystart - yBuff
        xend = xend + xBuff
        yend = yend + yBuff

        #  reel these values in if they are outside our bounds
        xstart, xend = np.clip([xstart, xend], self.raster.west, self.raster.east)
        ystart, yend = np.clip([ystart, yend], self.raster.south, self.raster.north)

        #  Update the image object with our new data and extent
        im = ax.images[-1]
        im.set_data(self.__call__((xstart, xend), (ystart, yend)))

        xstart, xend, ystart, yend = self.raster.get_bbox_data_bounds([xstart, ystart, xend, yend])

        im.set_extent((xstart, xend, ystart, yend))
        ax.figure.canvas.draw_idle()
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################











try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    _fromUtf8 = lambda s: s

class CreatePredictorCurvesDialog(QtGui.QDialog):

    def __init__(self, kwargs, parent=None):
        self.kwargs = kwargs

        QtGui.QDialog.__init__(self)  #  , parent)
        
        self.input_mds = kwargs['inputMDS']
        self.output_mds = kwargs['output_json']
        
        self.setWindowFlags(QtCore.Qt.Window)

        self.main_layout = QtGui.QVBoxLayout()
        self.main_layout.setContentsMargins(0, 0, 0, 0)

        self.covariates_viewer = QtGui.QWidget()
        self.covariates_vlayout = QtGui.QFormLayout()  #  .QVBoxLayout()
        self.covariates_viewer.setLayout(self.covariates_vlayout)

        self.PopulateTreeview()

        scroll = QtGui.QScrollArea()
        scroll.setWidget(self.covariates_viewer)
        self.main_layout.addWidget(scroll)  #  self.covariates_viewer)

        self.button_layout = QtGui.QHBoxLayout()
        self.button_layout.setContentsMargins(-1, 3, -1, 3)
        self.button_layout.setObjectName(_fromUtf8("button_layout"))
        self.btnOK = QtGui.QPushButton()
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.btnOK.sizePolicy().hasHeightForWidth())
        self.btnOK.setSizePolicy(sizePolicy)
        self.btnOK.setBaseSize(QtCore.QSize(100, 0))
        self.btnOK.setToolTip(_fromUtf8(""))
        self.btnOK.setObjectName(_fromUtf8("btnOK"))
        self.button_layout.addWidget(self.btnOK)
        self.btnCancel = QtGui.QPushButton()
        sizePolicy = QtGui.QSizePolicy(QtGui.QSizePolicy.Fixed, QtGui.QSizePolicy.Fixed)
        sizePolicy.setHorizontalStretch(0)
        sizePolicy.setVerticalStretch(0)
        sizePolicy.setHeightForWidth(self.btnCancel.sizePolicy().hasHeightForWidth())
        self.btnCancel.setSizePolicy(sizePolicy)
        self.btnCancel.setBaseSize(QtCore.QSize(100, 0))
        self.btnCancel.setObjectName(_fromUtf8("btnCancel"))
        self.button_layout.addWidget(self.btnCancel)
        self.main_layout.addLayout(self.button_layout)

        self.setWindowTitle(_fromUtf8("Habitat Suitability Criteria"))
        self.btnOK.setText(_fromUtf8("OK"))
        self.btnCancel.setText(_fromUtf8("Cancel"))
        self.resize(1100, 800)

        
        self.btnCancel.setShortcut('Esc')
        self.connect(self.btnOK, QtCore.SIGNAL('clicked(bool)'),
                     self.okTriggered)
        self.connect(self.btnCancel, QtCore.SIGNAL('clicked(bool)'),
                     self.cancel)
#          self.connect(self.btnRunR, QtCore.SIGNAL('clicked(bool)'),
#                       self.update_pairs_plot)
#          self.connect(self.lineEdit, QtCore.SIGNAL('textChanged(QString)'),
#                       self.thresholdEdit)
#          self.connect(self.numPlots, QtCore.SIGNAL('textChanged(QString)'),
#               self.numPlotsEdit)
        
#          self.connect(self.treeview, QtCore.SIGNAL('itemDoubleClicked(QTreeWidgetItem*, int)'), self.on_item_doublclick)

        
        
        #code to populate the treeview with the contents of our MDS
        

        self.setLayout(self.main_layout)
        self.repaint()
        
    def okTriggered(self):
        self.SaveMDSFromTreeview()
        self.done(0)

    def cancel(self):
        for cv in self.covariate_viewers:
            cv.fig_hsc.delaxes(cv.ax_hsc)
            cv.fig_hsc.delaxes(cv.ax_hist)
            cv.fig_map.delaxes(cv.ax_map)
            del cv
        self.done(1)
        
    def thresholdEdit(self):
        try:
            self.kwargs['minCor'] = float(str(self.lineEdit.text()))
        except ValueError:
            pass
     
    def numPlotsEdit(self):
        try:
            self.kwargs['numPlots'] = int(str(self.numPlots.text()))
        except ValueError:
            pass   
        
    
    def PopulateTreeview(self):
        ''' Reads in the input MDS and populates the treeview widget
        with the items in covariate columns.  
        Sets the check state to be the same as the 0/1 include flag.
        '''

        if os.path.exists(self.output_mds):
            prev_out = json.load(open(self.output_mds, 'rb'))
        else:
            prev_out = {}

        self.mds_data = pd.read_csv(self.input_mds, na_values=['NA'])
        noncovariate_columns = ['Split', 'EvalSplit', 'CV']

        self.covariate_viewers = []

        first_viewer = self.create_covartiate_viewer(self.mds_data.columns[3],
                    previous_verts=prev_out.get(self.mds_data.columns[3], None))
        self.covariates_vlayout.addWidget(first_viewer)
        self.covariate_viewers.append(first_viewer)

        for col in self.mds_data.columns[4:]:
            if not col in noncovariate_columns:
                print col
                this_viewer = self.create_covartiate_viewer(col, matchax=first_viewer.ax_map,
                    previous_verts=prev_out.get(col, None))
                self.covariates_vlayout.addWidget(this_viewer)
                self.covariate_viewers.append(this_viewer)

    def create_covartiate_viewer(self, col, matchax=None, previous_verts=None):
        mds_subset = self.mds_data[list(self.mds_data.columns[:3].values) + [col]][2:]
        this_item = covariate_viewer(name=col,
                     values=self.mds_data[col][2:].astype(dtype='float'),
                     df=mds_subset,
                     layer_fname=self.mds_data[col][1],
                     include=int(self.mds_data[col][0]) == 1,
                     parent=self,
                     label=col,
                     matchax=matchax,
                     previous_verts=previous_verts)
        this_item.on_draw()
        return this_item
        
    def SaveMDSFromTreeview(self):
        #updates the second header line on the input MDS file 
        #to reflect the checked items in the tree view 
        #and saves the results to the output MDS.
        
        output = {}
        for covariate_viewer in self.covariate_viewers:
            output[covariate_viewer.name] = covariate_viewer.vertices

        with open(self.output_mds, 'wb') as f:
            json.dump(output, f)
        
    def closeEvent(self, event):
        self.cancel()


import matplotlib as mpl
from matplotlib.backends.backend_qt4agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.backends.backend_qt4agg import NavigationToolbar2QT
from matplotlib.figure import Figure

class covariate_viewer(QtGui.QGroupBox):


    def __init__(self, name, values, df, include=False, layer_fname=None,
                 parent=None, cm=mpl.cm.coolwarm, label='', matchax=None,
                 previous_verts=None):

        super(covariate_viewer, self).__init__(name, parent=parent)

        self.dpi = 100

        self.name = name
        self.values = values
        self.df = df
        self.include = include
        self.layer_fname = layer_fname
        self.cm = cm
        self.label = label
        self.matchax = matchax
        self.raster_im = False
        self.vertices = previous_verts

        self.main_layout = QtGui.QHBoxLayout()
        self.include_chk = QtGui.QCheckBox("include")
        self.include_chk.setMaximumWidth(100)

        self.chart_panel = QtGui.QWidget()
        self.chart_layout = QtGui.QHBoxLayout()
        self.chart_panel.setMinimumSize(QtCore.QSize(100, 100))

        self.include_chk.setChecked(self.include)
        self.main_layout.addWidget(self.include_chk)
        self.connect(self.include_chk, QtCore.SIGNAL('stateChanged(int)'), self.include_changed)
        spacer_item = QtGui.QSpacerItem(0, 0, QtGui.QSizePolicy.Expanding, QtGui.QSizePolicy.Minimum)
        self.main_layout.addItem(spacer_item)


        self.fig_hsc = Figure((5.0, 4.0), dpi=self.dpi)
        self.canvas_hsc = FigureCanvas(self.fig_hsc)
        self.canvas_hsc.setParent(self.chart_panel)
        self.ax_hsc = self.fig_hsc.add_subplot(111)
        self.ax_hist = self.ax_hsc.twinx()
        self.ax_hist.yaxis.set_major_locator(mpl.pyplot.NullLocator())

        self.chart_layout.addWidget(self.canvas_hsc)

        self.fig_map = Figure((5.0, 4.0), dpi=self.dpi)
        self.canvas_map = FigureCanvas(self.fig_map)
        self.canvas_map.setParent(self)
        self.canvas_map.mpl_connect('scroll_event', self.wheel_zoom)


        if self.matchax is None:
            self.ax_map = self.fig_map.add_axes((0.05, 0.1, .95, 0.8))
        else:
            self.ax_map = self.fig_map.add_axes((0.05, 0.1, .95, 0.8),
                             sharex=self.matchax, sharey=self.matchax)

        self.ax_map_cb = self.fig_map.add_axes((0.01, 0.1, 0.02, 0.8))

        self.ax_map.xaxis.set_major_locator(mpl.pyplot.NullLocator())

        self.ax_map.yaxis.set_major_locator(mpl.pyplot.NullLocator())


        norm = mpl.colors.Normalize(vmin=0.0, vmax=1.0)
        cb1 = mpl.colorbar.ColorbarBase(self.ax_map_cb, cmap=self.cm, norm=norm, orientation='vertical')

        self.chart_layout.addWidget(self.canvas_map)
        self.chart_panel.setLayout(self.chart_layout)

        self.main_layout.addWidget(self.chart_panel)


        self.setLayout(self.main_layout)

        self.init_hsc_chart()
        self.init_map(self.interactor.vertices)
        self.repaint()

    def wheel_zoom(self, event):
        #  zoom in or out centered on the current cursor position

        inv = self.ax_map.transData.inverted()
        curX, curY = inv.transform((event.x, event.y))

        curL, curR = self.ax_map.get_xlim()
        curB, curT = self.ax_map.get_ylim()
        width = curR - curL
        height = curT - curB
        steps = -1 * event.step / 0.25
        factor = steps / 35
        #  sanity check
        if factor > 0.5:
            factor = 0.5
        if factor < -0.5:
            factor = -0.5

        newWidth = width * (1.0 - factor)
        newHeight = height * (1.0 - factor)
        dWidth = width - newWidth
        dHeight = height - newHeight

        pcntLofX = 1 - (width - (curX - curL)) / width
        pcntUnderTop = (height - (curT - curY)) / height

        newL = curL + (dWidth * pcntLofX)
        newR = curR - (dWidth * (1 - pcntLofX))
        newB = curB + (dHeight * pcntUnderTop)
        newT = curT - (dWidth * (1 - pcntUnderTop))
        self.ax_map.set_xlim((newL, newR))
        self.ax_map.set_ylim((newB, newT))

        self.rasterdisplay_layer.ax_update(self.ax_map)
        self.canvas_map.draw()
        self.repaint()

#          self.display_map(vertices=self.interactor.vertices,
#                           xlim=(newL, newR), ylim=(newB, newT))

    def init_hsc_chart(self):

        min = self.values.min()
        max = self.values.max()
        mean = self.values.mean()


        bins = np.max([10, len(self.df) / 10])
        data = []
        colors = []
        labels = []
        for val, c, label in [('0', 'b', 'absence'),
                       ('1', 'r', 'presence'),
                       ('-9999', 'black', 'background'),
                       ('-9998', 'black', 'background'),
                       (0, 'b', 'absence'),
                       (1, 'r', 'presence'),
                       (-9999, 'black', 'background'),
                       (-9998, 'black', 'background')]:
            try:

                x = self.values.loc[self.df['responseBinary'] == val]
                x = [float(x) for x in x.values]
                data.append(x)
                colors.append(c)
                labels.append(label)
            except:
                pass
        self.ax_hist.hist(data, bins=bins, stacked=True, color=colors,
                        label=labels, alpha=0.4, histtype='stepfilled', lw=0.1)
#          plt.legend()



        if self.vertices is None:
            x, y = [min, mean, max], [1.0, 1.0, 1.0]
        else:
            x, y = zip(*self.vertices)

        self.interactor = PathInteractor(self.ax_hsc, x, y, parent=self)
        self.ax_hsc.set_ylim((0, 1.05))
        self.ax_hsc.set_title(self.label + "\nCurve and Distribution")

    def init_map(self, vertices):
#          self.ax_map.plot([1, 2], [3, 4])
        self.rasterdisplay_layer = RasterDisplay()
        self.rasterdisplay_layer.setDims(self.ax_map)
        self.rasterdisplay_layer.switch_raster({'raster_file':self.layer_fname})

        self.display_map(vertices)
        
        self.mpl_toolbar = NavigationToolbar2QT(self.canvas_map, None)
        self.mpl_toolbar.pan()
        
        self.ax_map.end_pan = self.end_pan

    def end_pan(self, *args, **kwargs):
        self.rasterdisplay_layer.ax_update(self.ax_map)
        self.canvas_map.draw()
        self.repaint()


    def display_map(self, vertices, xlim=None, ylim=None):
        '''refresh the given raster
        '''
        self.vertices = vertices
        max_xlim = (self.rasterdisplay_layer.raster.west, self.rasterdisplay_layer.raster.east)
        max_ylim = (self.rasterdisplay_layer.raster.south, self.rasterdisplay_layer.raster.north)

        if xlim is None:
            xlim = max_xlim
        if ylim is None:
            ylim = max_ylim



        rmax = self.rasterdisplay_layer.display_max
        rmin = self.rasterdisplay_layer.display_min

        r0, g0, b0, a = self.cm(vertices[0][1])


        r = [(0.0, r0, r0)]
        g = [(0.0, g0, g0)]
        b = [(0.0, b0, b0)]

        xi = np.linspace(rmin, rmax, 1000)
        yi = np.interp(xi, *zip(*vertices))
        for v in zip(xi, yi):
            if v[0] >= rmin and v[0] <= rmax:
                ratio = linear(v[0], rmin, rmax, new_min=0.0, new_max=1.0)
                if ratio > 1.0:
                    ratio = 1.0
                if ratio < 0.0:
                    ratio = 0.0

                ri, gi, bi, a = self.cm(v[1])

                r.append((ratio, ri, ri))
                g.append((ratio, gi, gi))
                b.append((ratio, bi, bi))

        
        r0, g0, b0, a = self.cm(vertices[-1][1])
        r.append((1.0, r0, r0))
        g.append((1.0, g0, g0))
        b.append((1.0, b0, b0))

        cdict = {'red': r,
                'green': g,
                'blue': b}

#          cdict = {'blue': [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0)], 'green': [(0.0, 1.0, 1.0), (0.0036880400000000001, 1.0, 1.0), (0.51891997144155255, 0.00984375000000004, 0.00984375000000004), (0.99509499999999995, 1.0, 1.0), (1.0, 1.0, 1.0)], 'red': [(0.0, 0.0, 0.0), (0.0036880400000000001, 0.0, 0.0), (0.51891997144155255, 0.99015624999999996, 0.99015624999999996), (0.99509499999999995, 0.0, 0.0), (1.0, 0.0, 0.0)]}

        cmap = colors.LinearSegmentedColormap('my_colormap', cdict, 256)


        if not self.raster_im:
            self.map_data = self.rasterdisplay_layer(xlim, ylim)
            self.raster_im = self.ax_map.imshow(self.map_data ,
                                            interpolation="nearest",

                                            cmap=cmap,
                                            origin='upper',
                                            extent=xlim + ylim)
            for val, c, label in [(0, 'b', 'absence'),
                       (1, 'r', 'presence'),
                       (-9999, 'black', 'background'),
                       (-9998, 'black', 'background')]:
#              try:
                x = self.df.loc[self.df['responseBinary'] == val]['X']
                x = [float(x) for x in x.values]
                y = self.df.loc[self.df['responseBinary'] == val]['Y']
                y = [float(y) for y in y.values]
                self.ax_map.scatter(x, y, color=c, label=label, alpha=0.8, s=5)
#              except:
#                  pass

                self.ax_map.set_title(self.label + "\n corresponding map")

        else:
            self.raster_im.set_cmap(cmap)

#          self.fig_map.colorbar(self.raster_im,
#                          ticks=[0.0, 0.5, 1.0], orientation='vertical',
#                          pad=0.01, shrink=.9, fraction=.3, aspect=15)


        self.ax_map.set_xlim(xlim)
        self.ax_map.set_ylim(ylim)
        self.canvas_map.draw()
        self.repaint()

    def include_changed(self):
        self.include = self.include_chk.isChecked()
        self.on_draw()

    def on_draw(self):
#          self.fig_hsc.
#          self.fig_hsc.set_visible(self.include)
#          self.fig_map.set_visible(self.include)
        if self.include:
            self.chart_panel.show()
#              self.canvas_hsc.show()
#              self.canvas_map.show()
        else:
            self.chart_panel.hide()
#              self.canvas_hsc.hide()
#              self.canvas_map.hide()
        self.update()



################################################################################
################################################################################
################################################################################
#  ##                interactive line
#  ## Based off of: http://matplotlib.org/examples/event_handling/poly_editor.html
################################################################################
################################################################################
################################################################################
class PathInteractor(object):
    """
    An path editor.

    Key-bindings

      't' toggle vertex markers on and off.  When vertex markers are on,
          you can move them, delete them


    """

    showverts = True
    epsilon = 8  #  max pixel distance to count as a vertex hit

    def __init__(self, ax, x, y, parent=None):

        self.parent = parent

        self.changed_vertices = False
        self.vertices = zip(x, y)
        self.ax_hsc = ax
        canvas = ax.figure.canvas

        self.line, = ax.plot(*zip(*self.vertices),
                             marker="o", mec="b", mfc="w", lw=5, mew=3, ms=10,
                             animated=True)

#          shadow.set_rasterized(True)
        self.add_shadow()
        self.add_fill()


        self._ind = None  #  the active vert

        canvas.mpl_connect('draw_event', self.draw_callback)
        canvas.mpl_connect('button_press_event', self.button_press_callback)
        canvas.mpl_connect('key_press_event', self.key_press_callback)
        canvas.mpl_connect('button_release_event', self.button_release_callback)
        canvas.mpl_connect('motion_notify_event', self.motion_notify_callback)
        canvas.mpl_connect('scroll_event', self.wheel_zoom)
        canvas.mpl_connect('axes_leave_event', self.leave_axes)
        self.canvas_hsc = canvas

    def leave_axes(self, event):
        if not self.showverts: return
        if self._ind is None: return
        if event.inaxes is None: return
        if event.button != 1: return
        x, y = event.xdata, event.ydata


        if self.ax_hsc != event.inaxes:
            ax_x, ax_y = self.parent.ax_hist.transData.transform((x, y))
            x, y = self.ax_hsc.transData.inverted().transform([ax_x, ax_y])
        elif self.ax_hist == event.inaxes:
            x, y = event.xdata, event.ydata
        else:
            return

        if y > 0.8:
            y = 1.0
        if y < 0.2:
            y = 0.0

        self.vertices[self._ind] = x, y

        no_dupes = []
        for v in self.vertices:
            if v not in no_dupes:
                no_dupes.append(v)
        self.vertices = no_dupes
        self.vertices.sort()
        self._ind = self.vertices.index((x, y))

        self.line.set_data(*zip(*self.vertices))
        self.add_fill()
        self.shadow.set_data(*zip(*self.vertices))

        self.canvas_hsc.restore_region(self.background)
#          self.ax_hsc.draw_artist(self.pathpatch)
        self.ax_hsc.draw_artist(self.line)
        self.canvas_hsc.blit(self.ax_hsc.bbox)

        self.canvas_hsc.draw()
        self.changed_vertices = True

    def wheel_zoom(self, event):
        #  zoom in or out centered on the current cursor position

        inv = self.ax_hsc.transData.inverted()
        curX, curY = inv.transform((event.x, event.y))

        curL, curR = self.ax_hsc.get_xlim()
        curB, curT = self.ax_hsc.get_ylim()
        width = curR - curL
        height = curT - curB
        steps = -1 * event.step / 0.25
        factor = steps / 35
        #  sanity check
        if factor > 0.5:
            factor = 0.5
        if factor < -0.5:
            factor = -0.5

        newWidth = width * (1.0 - factor)
        newHeight = height * (1.0 - factor)
        dWidth = width - newWidth
        dHeight = height - newHeight

        pcntLofX = 1 - (width - (curX - curL)) / width
        pcntUnderTop = (height - (curT - curY)) / height

        newL = curL + (dWidth * pcntLofX)
        newR = curR - (dWidth * (1 - pcntLofX))
        newB = curB + (dHeight * pcntUnderTop)
        newT = curT - (dWidth * (1 - pcntUnderTop))
        self.ax_hsc.set_xlim((newL, newR))
        self.add_fill()
        self.canvas_hsc.draw()
#          self.axes.set_ylim((newB, newT))

    def add_fill(self):

        for coll in (self.ax_hsc.collections):
            self.ax_hsc.collections.remove(coll)

        xlim = self.ax_hsc.get_xlim()
        x, y = [list(t) for t in zip(*self.vertices)]

        if xlim[0] < x[0]:
            x.insert(0, xlim[0])
            y.insert(0, y[0])
        elif x[0] < xlim[0]:
            x[0] = xlim[0]

        if xlim[1] > x[-1]:
            x.append(xlim[1])
            y.append(y[-1])
        elif x[-1] < xlim[-1]:
            x[-1] = xlim[-1]

        self.fill = self.ax_hsc.fill_between(x, 0, y, facecolor='green', alpha=0.5)

    def add_shadow(self):
        self.shadow, = self.ax_hsc.plot(*zip(*self.vertices))
        self.shadow.update_from(self.line)

        #  offset transform
        ot = mtransforms.offset_copy(self.line.get_transform(),
                                self.ax_hsc.figure, x=4.0, y=-6.0, units='points')

        self.shadow.set_transform(ot)

        #  adjust zorder of the shadow lines so that it is drawn below the
        #  original lines
        self.shadow.set_zorder(self.line.get_zorder() - 0.5)
        gauss = DropShadowFilter(4)
        self.shadow.set_agg_filter(gauss)

    def draw_callback(self, event):
        self.background = self.canvas_hsc.copy_from_bbox(self.ax_hsc.bbox)
#          self.ax_hsc.draw_artist(self.pathpatch)
        self.ax_hsc.draw_artist(self.line)
#          self.canvas_hsc.blit(self.ax_hsc.bbox)
        self.canvas_hsc.blit(self.ax_hsc.figure.bbox)

    def get_ind_under_point(self, event):
        'get the index of the vertex under point if within epsilon tolerance'

        #  display coords
        xy = np.asarray(self.vertices)
        xyt = self.ax_hsc.transData.transform(xy)
        xt, yt = xyt[:, 0], xyt[:, 1]
        d = np.sqrt((xt - event.x) ** 2 + (yt - event.y) ** 2)
        ind = d.argmin()

        if d[ind] >= self.epsilon:
            ind = None

        return ind

    def get_line_under_point(self, event):
        'get the index of the line under point if within epsilon tolerance'
        p = event.x, event.y  #  display coords
        for i in range(len(self.vertices) - 1):
            s0 = self.ax_hsc.transData.transform(self.vertices[i])
            s1 = self.ax_hsc.transData.transform(self.vertices[i + 1])
            d = dist_point_to_segment(p, s0, s1)
            if d <= self.epsilon:
                return i
        return None


    def button_press_callback(self, event):
        'whenever a mouse button is pressed'
        if not self.showverts: return
        if event.inaxes == None: return
        if event.button != 1: return
        self._ind = self.get_ind_under_point(event)

        if event.dblclick:
            point = self.get_ind_under_point(event)
            line = self.get_line_under_point(event)
            if point is not None:
                del self.vertices[point]
                self.line.set_data(*zip(*self.vertices))
                self.shadow.set_data(*zip(*self.vertices))
                self.add_fill()
                self.canvas_hsc.draw()
                self.changed_vertices = True
            else:
                x, y = self.ax_hsc.transData.inverted().transform((event.x, event.y))
                if y > 1.0:
                    y = 1.0
                if (line is not None and point is None) or \
                    x > self.vertices[-1][0] or x < self.vertices[0][0]:

                    if line is not None:
                        line_i = line + 1
                    elif x > self.vertices[-1][0]:
                        line_i = len(self.vertices)
                    else:
                        line_i = 0

                    self.vertices.insert(line_i, (x, y))
                    self.line.set_data(*zip(*self.vertices))
                    self.shadow.set_data(*zip(*self.vertices))
                    self.add_fill()
                    self.canvas_hsc.draw()
                    self.changed_vertices = True

                
                

    def button_release_callback(self, event):
        'whenever a mouse button is released'
        if not self.showverts: return
        if event.button != 1: return
        self._ind = None

        if self.changed_vertices:
            cur_xlim = self.parent.ax_map.get_xlim()
            cur_ylim = self.parent.ax_map.get_ylim()
            self.parent.display_map(self.vertices, xlim=cur_xlim, ylim=cur_ylim)
            self.changed_vertices = False

    def key_press_callback(self, event):
        'whenever a key is pressed'
        if not event.inaxes: return
        if event.key == 't':
            self.showverts = not self.showverts
            self.line.set_visible(self.showverts)
            if not self.showverts: self._ind = None

        elif event.key == 'd':
            ind = self.get_ind_under_point(event)
            if ind is not None:
                del self.vertices[ind]
                self.line.set_data(*zip(*self.vertices))
                self.shadow.set_data(*zip(*self.vertices))
        elif event.key == 'i':
            i = self.get_line_under_point(event)
            if i:
                self.vertices.insert(i + 1,
                        self.ax_hsc.transData.inverted().transform((event.x, event.y)()))
                self.line.set_data(*zip(*self.vertices))
                self.shadow.set_data(*zip(*self.vertices))

        self.canvas_hsc.draw()

    def motion_notify_callback(self, event):
        'on mouse movement'
        if not self.showverts: return
        if self._ind is None: return
        if event.inaxes is None: return
        if event.button != 1: return
        x, y = event.xdata, event.ydata


        if self.ax_hsc != event.inaxes:
            ax_x, ax_y = self.parent.ax_hist.transData.transform((x, y))
            x, y = self.ax_hsc.transData.inverted().transform([ax_x, ax_y])
        elif self.ax_hist == event.inaxes:
            x, y = event.xdata, event.ydata
        else:
            return

        if y > 1.0:
            y = 1.0

        self.vertices[self._ind] = x, y

        no_dupes = []
        for v in self.vertices:
            if v not in no_dupes:
                no_dupes.append(v)
        self.vertices = no_dupes
        self.vertices.sort()
        self._ind = self.vertices.index((x, y))

        self.line.set_data(*zip(*self.vertices))
        self.add_fill()
        self.shadow.set_data(*zip(*self.vertices))

        self.canvas_hsc.restore_region(self.background)
#          self.ax_hsc.draw_artist(self.pathpatch)
        self.ax_hsc.draw_artist(self.line)
        self.canvas_hsc.blit(self.ax_hsc.bbox)

        self.canvas_hsc.draw()
        self.changed_vertices = True

################################################################################
################################################################################
################################################################################
#  ##                fancy matplotlib lines
################################################################################
################################################################################
################################################################################
def smooth1d(x, window_len):
    #  copied from http://www.scipy.org/Cookbook/SignalSmooth

    s = np.r_[2 * x[0] - x[window_len:1:-1], x, 2 * x[-1] - x[-1:-window_len:-1]]
    w = np.hanning(window_len)
    y = np.convolve(w / w.sum(), s, mode='same')
    return y[window_len - 1:-window_len + 1]

def smooth2d(A, sigma=3):

    window_len = max(int(sigma), 3) * 2 + 1
    A1 = np.array([smooth1d(x, window_len) for x in np.asarray(A)])
    A2 = np.transpose(A1)
    A3 = np.array([smooth1d(x, window_len) for x in A2])
    A4 = np.transpose(A3)

    return A4

class BaseFilter(object):
    def prepare_image(self, src_image, dpi, pad):
        ny, nx, depth = src_image.shape
        #  tgt_image = np.zeros([pad*2+ny, pad*2+nx, depth], dtype="d")
        padded_src = np.zeros([pad * 2 + ny, pad * 2 + nx, depth], dtype="d")
        padded_src[pad:-pad, pad:-pad, :] = src_image[:, :, :]

        return padded_src  #  , tgt_image

    def get_pad(self, dpi):
        return 0

    def __call__(self, im, dpi):
        pad = self.get_pad(dpi)
        padded_src = self.prepare_image(im, dpi, pad)
        tgt_image = self.process_image(padded_src, dpi)
        return tgt_image, -pad, -pad

class OffsetFilter(BaseFilter):
    def __init__(self, offsets=None):
        if offsets is None:
            self.offsets = (0, 0)
        else:
            self.offsets = offsets

    def get_pad(self, dpi):
        return int(max(*self.offsets) / 72.*dpi)

    def process_image(self, padded_src, dpi):
        ox, oy = self.offsets
        a1 = np.roll(padded_src, int(ox / 72.*dpi), axis=1)
        a2 = np.roll(a1, -int(oy / 72.*dpi), axis=0)
        return a2

class GaussianFilter(BaseFilter):
    "simple gauss filter"
    def __init__(self, sigma, alpha=0.5, color=None):
        self.sigma = sigma
        self.alpha = alpha
        if color is None:
            self.color = (0, 0, 0)
        else:
            self.color = color

    def get_pad(self, dpi):
        return int(self.sigma * 3 / 72.*dpi)


    def process_image(self, padded_src, dpi):
        #  offsetx, offsety = int(self.offsets[0]), int(self.offsets[1])
        tgt_image = np.zeros_like(padded_src)
        aa = smooth2d(padded_src[:, :, -1] * self.alpha,
                      self.sigma / 72.*dpi)
        tgt_image[:, :, -1] = aa
        tgt_image[:, :, :-1] = self.color
        return tgt_image


class DropShadowFilter(BaseFilter):
    def __init__(self, sigma, alpha=0.3, color=None, offsets=None):
        self.gauss_filter = GaussianFilter(sigma, alpha, color)
        self.offset_filter = OffsetFilter(offsets)

    def get_pad(self, dpi):
        return max(self.gauss_filter.get_pad(dpi),
                   self.offset_filter.get_pad(dpi))

    def process_image(self, padded_src, dpi):
        t1 = self.gauss_filter.process_image(padded_src, dpi)
        t2 = self.offset_filter.process_image(t1, dpi)
        return t2
################################################################################
################################################################################
################################################################################
#  ##                fancy interactive matplotlib lines
################################################################################
################################################################################
################################################################################

def linear(value, old_min, old_max, new_min=0, new_max=100):
    return ((value - old_min) / (old_max - old_min)) * (new_max - new_min) + new_min


if __name__ == "__main__":

    app = QtGui.QApplication(sys.argv)


    args = {'inputMDS':r"I:\VisTrails\WorkingFiles\workspace\_HabitatSuitabilityCurves\hsc_BrewersSparrowHSC_1\CovariateCorrelationOutputMDS_BrewersSparrowHSC_initial.csv",
            'output_json':r"I:\VisTrails\WorkingFiles\workspace\_HabitatSuitabilityCurves\hsc_BrewersSparrowHSC_1\hsc.json",
            }

    createPredictorCurves = CreatePredictorCurvesDialog(args)
    retVal = createPredictorCurves.exec_()
