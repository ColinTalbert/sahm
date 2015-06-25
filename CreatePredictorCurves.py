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


from matplotlib.backends.backend_qt4agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.figure import Figure
import matplotlib.transforms as mtransforms
from matplotlib.mlab import dist_point_to_segment
import matplotlib.colors as colors

import pySAHM.SpatialUtilities as SpatialUtilities
import seaborn as sns
sns.set(style="darkgrid")

import json

try:
    _fromUtf8 = QtCore.QString.fromUtf8
except AttributeError:
    _fromUtf8 = lambda s: s

class CreatePredictorCurvesDialog(QtGui.QDialog):

    def __init__(self, kwargs, parent=None):
        self.kwargs = kwargs

        QtGui.QDialog.__init__(self)  #  , parent)

        self.input_mds = kwargs['inputMDS']
        self.output_json = kwargs['output_json']

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

        self.setWindowTitle(_fromUtf8("Create User Defined Predictor Curves"))
        self.btnOK.setText(_fromUtf8("OK"))
        self.btnCancel.setText(_fromUtf8("Cancel"))
        self.resize(650, 1000)

        self.btnCancel.setShortcut('Esc')
        self.connect(self.btnOK, QtCore.SIGNAL('clicked(bool)'),
                     self.okTriggered)
        self.connect(self.btnCancel, QtCore.SIGNAL('clicked(bool)'),
                     self.cancel)

        self.setLayout(self.main_layout)
        self.repaint()

    def okTriggered(self):
        self.SaveMDSFromTreeview()
        self.clear_contents()
        self.done(0)

    def clear_contents(self):
        for cv in self.covariate_viewers:
            cv.fig_udc.delaxes(cv.ax_udc)
            del cv

    def cancel(self):
        self.clear_contents()
        self.done(1)
    
    def PopulateTreeview(self):
        ''' Reads in the input MDS and populates the treeview widget
        with the items in covariate columns.
        Sets the check state to be the same as the 0/1 include flag.
        '''

        if os.path.exists(self.output_json):
            prev_out = json.load(open(self.output_json, 'rb'))
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
                this_viewer = self.create_covartiate_viewer(col,
                previous_verts=prev_out.get(col, None))
                self.covariates_vlayout.addWidget(this_viewer)
                self.covariate_viewers.append(this_viewer)

    def create_covartiate_viewer(self, col, previous_verts=None):
        mds_subset = self.mds_data[list(self.mds_data.columns[:3].values) + [col]][2:]
        mds_subset['responseBinary'] = mds_subset['responseBinary'].astype(dtype='int')
        this_item = covariate_viewer(name=col,
               values=self.mds_data[col][2:].astype(dtype='float'),
               df=mds_subset,
               include=int(self.mds_data[col][0]) == 1,
               parent=self,
               label=col,
               previous_verts=previous_verts,
               categorical=col.endswith("_categorical"))
        this_item.on_draw()
        return this_item

    def SaveMDSFromTreeview(self):
        #  updates the second header line on the input MDS file
        #  to reflect the checked items in the tree view
        #  and saves the results to the output MDS.
    
    
           
        reader = csv.reader(open(self.input_mds[:-4] + "_orig.csv", "r"))
        header = reader.next()  #  store the header
        header2 = reader.next()  #  the 2nd line of the mds with use/don't use
        header3 = reader.next()  #  the 3rd line of the mds with the path
        
        outHeader2 = header2

        output = {}
        for covariate_viewer in self.covariate_viewers:
            if covariate_viewer.include:
                if covariate_viewer.categorical:
                    verts = covariate_viewer.int.vertices
                    labels = covariate_viewer.int.labels
                    output[covariate_viewer.name] = [[l, v[1]] for l, v in zip(labels , verts)]
                else:
                    output[covariate_viewer.name] = covariate_viewer.int.vertices
            
                col_index = header.index(covariate_viewer.name)

                if covariate_viewer.include:
                    outHeader2[col_index] = "1"
                else:
                    outHeader2[col_index] = "0"

        oFile = open(self.input_mds, 'wb')
        writer = csv.writer(oFile, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
        writer.writerow(header)
        writer.writerow(outHeader2)
        writer.writerow(header3)
        for row in reader:
            writer.writerow(row)
        oFile.close

        with open(self.output_json, 'wb') as f:
            json.dump(output, f)
        
    def closeEvent(self, event):
        self.clear_contents()
        self.cancel()

class covariate_viewer(QtGui.QGroupBox):


    def __init__(self, name, values, df, include=False, layer_fname=None,
        parent=None, label='', matchax=None,
        previous_verts=None, categorical=False):

        super(covariate_viewer, self).__init__(name, parent=parent)
    
        self.dpi = 100
    
        self.name = name
        self.values = values
        self.df = df
        self.include = include
        self.categorical = categorical
        self.label = label
        self.matchax = matchax
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
    
    
        self.fig_udc = Figure((5.0, 4.0), dpi=self.dpi)
        self.canvas_udc = FigureCanvas(self.fig_udc)
        self.canvas_udc.setParent(self.chart_panel)
        self.ax_udc = self.fig_udc.add_subplot(111)
    
        self.chart_layout.addWidget(self.canvas_udc)
        self.chart_panel.setLayout(self.chart_layout)
    
        self.main_layout.addWidget(self.chart_panel)
    
    
        self.setLayout(self.main_layout)
    
        self.init_udc_chart()
        self.repaint()


    def init_udc_chart(self):

        if self.categorical:
            uniques = np.unique(self.values)
        
            if self.vertices is None:
                x, y = uniques, np.ones(len(uniques))
            else:
                x, y = zip(*self.vertices)
            
            self.int = BarInteractor(self.ax_udc, x, y, title=self.label,
                                     parent=self)

        else:
            min = self.values.min()
            max = self.values.max()
            mean = self.values.mean()
            colors = []
        
            if self.vertices is None:
                x, y = [min, mean, max], [1.0, 1.0, 1.0]
            else:
                x, y = zip(*self.vertices)
            
            self.int = PathInteractor(self.ax_udc, x, y, title=self.label,
                                      parent=self)
        self.ax_udc.set_ylim((0, 1.05))

        #
    def include_changed(self):
        self.include = self.include_chk.isChecked()
        self.on_draw()

    def on_draw(self):
        self.fig_udc.set_visible(self.include)
        if self.include:
            self.chart_panel.show()
        else:
            self.chart_panel.hide()
            self.update()



################################################################################
################################################################################
################################################################################
#  ##          interactive line
#  ## Based off of: http://matplotlib.org/examples/event_handling/poly_editor.html
################################################################################
################################################################################
################################################################################
class PlotInteractor(object):
    """
    Base class for an interactive bar/line plot

    Key-bindings

     't' toggle vertex markers on and off.  When vertex markers are on,
     you can move them, delete them


    """
    #  max pixel distance to count as a vertex hit
    epsilon = 8
    showverts = True

    def __init__(self, ax, x, y, title=None, parent=None):

        self.parent = parent

        self.title = title

        self.ax_udc = ax
        canvas = ax.figure.canvas
    
        self._ind = None  #  the active vert
    
        canvas.mpl_connect('draw_event', self.draw_callback)
        canvas.mpl_connect('button_press_event', self.button_press_callback)
        canvas.mpl_connect('button_release_event', self.button_release_callback)
        canvas.mpl_connect('motion_notify_event', self.motion_notify_callback)
        canvas.mpl_connect('scroll_event', self.wheel_zoom)
        canvas.mpl_connect('axes_leave_event', self.leave_axes)
        self.canvas_udc = canvas
    
        self.last_x = None

    def get_ind_under_point(self, event):
        'get the index of the vertex under point if within epsilon tolerance'
    
        #  display coords
        xy = np.asarray(self.vertices)
        xyt = self.ax_udc.transData.transform(xy)
        xt, yt = xyt[:, 0], xyt[:, 1]
        d = np.sqrt((xt - event.x) ** 2 + (yt - event.y) ** 2)
        ind = d.argmin()

        if d[ind] >= self.epsilon:
            ind = None
    
        return ind

    def button_release_callback(self, event):
        'whenever a mouse button is released'
        if not self.showverts: return
        if event.button != 1: return
        
        self._ind = None
        
        self.button_pressed = False
        self.last_x = None
        
    def leave_axes(self, event):
        if not self.showverts: return
        if self._ind is None: return
        if event.inaxes is None: return
        if event.button != 1: return
        x, y = event.xdata, event.ydata
        self.last_x = None
        
        if y > 0.8:
            y = 1.0
        if y < 0.2:
            y = 0.0
        
        no_dupes = []
        for v in self.vertices:
            if v not in no_dupes:
                no_dupes.append(v)
        self.vertices = no_dupes
        self.vertices.sort()
        
        if type(self) == BarInteractor:
            self.vertices[self._ind] = self.vertices[self._ind][0], y
            self._ind = self.vertices.index((self.vertices[self._ind][0], y))
        else:
            self.vertices[self._ind] = x, y
            self._ind = self.vertices.index((x, y))

        self.replot()
        


class BarInteractor(PlotInteractor):

    def __init__(self, ax, x, y, title=None, parent=None):

        super(BarInteractor, self).__init__(ax, x, y, title=title, parent=parent)

        self.vertices = zip(np.arange(len(x)), y)
        self.labels = x
        self.plot()


    def plot(self):

        self.ax_udc.cla()
        x, y = [list(t) for t in zip(*self.vertices)]
        self.bar = self.ax_udc.bar(x, y, 0.6, align='center', color='g',
                                   alpha=0.5)
        
        self.ax_udc.set_xticks(x)
        self.ax_udc.set_xticklabels([int(xi) for xi in self.labels])
    

        self.ax_udc.scatter(x, y, marker="o", lw=3, s=110, edgecolors='b',
                            c='white', zorder=10)

        self.ax_udc.set_ylim((0, 1.05))
        self.add_shadow()
        self.ax_udc.set_title(self.title)

    def replot(self):
        self.plot()
        self.canvas_udc.draw()

    def add_shadow(self):

        gauss = DropShadowFilter(4, offsets=(1, 1),)
    
        self.shadow = FilteredArtistList(self.bar, gauss)
        self.shadow.set_agg_filter(gauss)

    def button_press_callback(self, event):
        'whenever a mouse button is pressed'
        if not self.showverts: return
        if event.inaxes == None: return
        if event.button != 1: return
        
        self._ind = self.get_ind_under_point(event)

    def motion_notify_callback(self, event):
        'on mouse movement'
        if event.inaxes is None: return
        
        x, y = event.xdata, event.ydata
        
        if not self._ind is None:
            #  button pressed on a vertex
            if y > 1.0:
                y = 1.0

            self.vertices[self._ind] = (self.vertices[self._ind][0], y)
            
            self.plot()
            self.canvas_udc.blit(self.ax_udc.bbox)

            self.canvas_udc.draw()

    def wheel_zoom(self, event):
    #  zooming or panning the chart does not make sense for categorical variables
        pass

    def draw_callback(self, event):
        self.background = self.canvas_udc.copy_from_bbox(self.ax_udc.bbox)
#          self.ax_udc.draw_artist(self.bar)
        self.canvas_udc.blit(self.ax_udc.figure.bbox)

class PathInteractor(PlotInteractor):

    def __init__(self, ax, x, y, title=None, parent=None):

        super(PathInteractor, self).__init__(ax, x, y, title=title, parent=parent)

        self.vertices = zip(x, y)
        self.plot()

    def plot(self):
        self.line, = self.ax_udc.plot(*zip(*self.vertices),
               marker="o", mec="b", mfc="w", lw=5, mew=3, ms=10,
               animated=True)
        self.add_shadow()
        self.add_fill()
        self.ax_udc.set_title(self.title)

    def replot(self):
        self.line.set_data(*zip(*self.vertices))
        self.add_fill()
        self.shadow.set_data(*zip(*self.vertices))

        self.canvas_udc.restore_region(self.background)
        #     self.ax_udc.draw_artist(self.pathpatch)
        self.ax_udc.draw_artist(self.line)
        self.canvas_udc.blit(self.ax_udc.bbox)

        self.canvas_udc.draw()

    def add_fill(self):

        for coll in (self.ax_udc.collections):
            self.ax_udc.collections.remove(coll)

        xlim = self.ax_udc.get_xlim()
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

        self.fill = self.ax_udc.fill_between(x, 0, y, facecolor='green', alpha=0.5)

    def add_shadow(self):
        self.shadow, = self.ax_udc.plot(*zip(*self.vertices))
        self.shadow.update_from(self.line)

        #  offset transform
        ot = mtransforms.offset_copy(self.line.get_transform(),
                     self.ax_udc.figure, x=4.0, y=-6.0, units='points')
    
        self.shadow.set_transform(ot)
    
        #  adjust zorder of the shadow lines so that it is drawn below the
        #  original lines
        self.shadow.set_zorder(self.line.get_zorder() - 0.5)
        gauss = DropShadowFilter(4)
        self.shadow.set_agg_filter(gauss)
    
    def get_line_under_point(self, event):
        'get the index of the line under point if within epsilon tolerance'
        p = event.x, event.y  #  display coords
        for i in range(len(self.vertices) - 1):
            s0 = self.ax_udc.transData.transform(self.vertices[i])
            s1 = self.ax_udc.transData.transform(self.vertices[i + 1])
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
        
        self.button_pressed = True
        
        if event.dblclick:
            point = self.get_ind_under_point(event)
            line = self.get_line_under_point(event)
            if point is not None:
                del self.vertices[point]
                self.line.set_data(*zip(*self.vertices))
                self.shadow.set_data(*zip(*self.vertices))
                self.add_fill()
                self.canvas_udc.draw()
            else:
                x, y = self.ax_udc.transData.inverted().transform((event.x, event.y))
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
                    self.canvas_udc.draw()


    def motion_notify_callback(self, event):
        'on mouse movement'
        if not self.showverts: return
        if event.inaxes is None: return
        if event.button != 1: return
    
        x, y = event.xdata, event.ydata

        if not self._ind is None:
            #  button pressed on a vertex
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
            
            self.replot()
            
        elif  event.button == 1:
        #  button pressed but not on a vertex == Pan!!
            if self.last_x is None:
                self.last_x = x
            else:
                delta_x = (self.last_x - x) * 10
                xlim = self.ax_udc.get_xlim()
                self.ax_udc.set_xlim((xlim[0] + delta_x, xlim[1] + delta_x))
                self.canvas_udc.draw()
                self.last_x = None


    def wheel_zoom(self, event):
        #  zoom in or out centered on the current cursor position
    
        inv = self.ax_udc.transData.inverted()
        curX, curY = inv.transform((event.x, event.y))
    
        curL, curR = self.ax_udc.get_xlim()
        curB, curT = self.ax_udc.get_ylim()
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
        self.ax_udc.set_xlim((newL, newR))
        self.add_fill()
        self.canvas_udc.draw()

    def draw_callback(self, event):
        self.background = self.canvas_udc.copy_from_bbox(self.ax_udc.bbox)
        self.ax_udc.draw_artist(self.line)
        self.canvas_udc.blit(self.ax_udc.figure.bbox)

################################################################################
################################################################################
################################################################################
#  ##          fancy matplotlib lines
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
    
from matplotlib.artist import Artist
class FilteredArtistList(Artist):
    """
    A simple container to draw filtered artist.
    """
    def __init__(self, artist_list, filter):
        self._artist_list = artist_list
        self._filter = filter
        Artist.__init__(self)
 
    def draw(self, renderer):
        renderer.start_rasterizing()
        renderer.start_filter()
        for a in self._artist_list:
            a.draw(renderer)
        renderer.stop_filter(self._filter)
        renderer.stop_rasterizing()

################################################################################
################################################################################
################################################################################
#  ##          fancy interactive matplotlib lines
################################################################################
################################################################################
################################################################################

def linear(value, old_min, old_max, new_min=0, new_max=100):
    return ((value - old_min) / (old_max - old_min)) * (new_max - new_min) + new_min


if __name__ == "__main__":

    app = QtGui.QApplication(sys.argv)


    args = {'inputMDS':r"I:\VisTrails\WorkingFiles\workspace\_HabitatSuitabilityCurves\udc_test2\CovariateCorrelationOutputMDS_BrewersSparrowudc_initial.csv",
       'output_json':r"I:\VisTrails\WorkingFiles\workspace\_HabitatSuitabilityCurves\udc_BrewersSparrowudc_1\udc.json",
       }

    createPredictorCurves = CreatePredictorCurvesDialog(args)
    retVal = createPredictorCurves.exec_()
