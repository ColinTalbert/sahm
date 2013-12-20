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
try:
    from vistrails.core.modules.basic_modules import String
except ImportError:
    from core.modules.basic_modules import String
    
from enum_widget import build_enum_widget

import pylab

class ResponseType(String):
    '''
    This module is a required class for other modules and scripts within the
    SAHM package. It is not intended for direct use or incorporation into
    the VisTrails workflow by the user.
    '''
    _input_ports = [('value', '(gov.usgs.sahm:ResponseType:Other)')]
    _output_ports = [('value_as_string', '(edu.utah.sci.vistrails.basic:String)', True)]
    _widget_class = build_enum_widget('ResponseType', 
                                      ['Presence(Absence)',
                                       'Count'])

    @staticmethod
    def get_widget_class():
        return ResponseType._widget_class        

    
class AggregationMethod(String):
    '''
    This module is a required class for other modules and scripts within the
    SAHM package. It is not intended for direct use or incorporation into
    the VisTrails workflow by the user.
    '''
    _input_ports = [('value', '(gov.usgs.sahm:AggregationMethod:Other)')]
    _output_ports = [('value_as_string', '(edu.utah.sci.vistrails.basic:String)', True)]
    _widget_class = build_enum_widget('AggregationMethod', 
                                      ['Mean', 'Max', 'Min', 'STD', 'Majority', 'None'])

    @staticmethod
    def get_widget_class():
        return AggregationMethod._widget_class

class ResampleMethod(String):
    '''
    This module is a required class for other modules and scripts within the
    SAHM package. It is not intended for direct use or incorporation into
    the VisTrails workflow by the user.
    '''
    _input_ports = [('value', '(gov.usgs.sahm:ResampleMethod:Other)')]
    _output_ports = [('value_as_string', '(edu.utah.sci.vistrails.basic:String)', True)]
    _widget_class = build_enum_widget('ResampleMethod', 
                                      ['NearestNeighbor', 'Bilinear', 'Cubic', 'CubicSpline', 'Lanczos'])

    @staticmethod
    def get_widget_class():
        return ResampleMethod._widget_class
    
class PointAggregationMethod(String):
    '''
    This module is a required class for other modules and scripts within the
    SAHM package. It is not intended for direct use or incorporation into
    the VisTrails workflow by the user.
    '''
    _input_ports = [('value', '(gov.usgs.sahm:PointAggregationMethod:Other)')]
    _output_ports = [('value_as_string', '(edu.utah.sci.vistrails.basic:String)', True)]
    _widget_class = build_enum_widget('PointAggregationMethod', 
                                      ['Collapse In Pixel', 
                                       'Weight Per Pixel'])

    @staticmethod
    def get_widget_class():
        return PointAggregationMethod._widget_class

class ModelOutputType(String):
    '''
    This module is a required class for other modules and scripts within the
    SAHM package. It is not intended for direct use or incorporation into
    the VisTrails workflow by the user.
    '''
    _input_ports = [('value', '(gov.usgs.sahm:ModelOutputType:Other)')]
    _output_ports = [('value_as_string', '(edu.utah.sci.vistrails.basic:String)', True)]
    _widget_class = build_enum_widget('ModelOutputType', 
                                      ['Text',
                                       'Response Curves', 
                                       'AUC', 
                                       'Calibration', 
                                       'Confusion',
                                       'Residuals'])

    @staticmethod
    def get_widget_class():
        return ModelOutputType._widget_class
    
class RandomPointType(String):
    '''
    This module is a required class for other modules and scripts within the
    SAHM package. It is not intended for direct use or incorporation into
    the VisTrails workflow by the user.
    '''
    _input_ports = [('value', '(gov.usgs.sahm:RandomPointType:Other)')]
    _output_ports = [('value_as_string', '(edu.utah.sci.vistrails.basic:String)', True)]
    _widget_class = build_enum_widget('RandomPointType', 
                                      ['Background',
                                       'Pseudo-absence (for R models)'])

    @staticmethod
    def get_widget_class():
        return RandomPointType._widget_class
    
    
class OutputRaster(String):
    '''
    This module is a required class for other modules and scripts within the
    SAHM package. It is not intended for direct use or incorporation into
    the VisTrails workflow by the user.
    '''
    _input_ports = [('value', '(gov.usgs.sahm:OutputRaster:Other)')]
    _output_ports = [('value_as_string', '(edu.utah.sci.vistrails.basic:String)', True)]
    _widget_class = build_enum_widget('OutputRaster', 
                                      ['Probability',
                                       'Binary Probability',
                                       'Residuals',
                                       'Mess',
                                       'MoD'])

    @staticmethod
    def get_widget_class():
        return OutputRaster._widget_class
    
    
class mpl_colormap(String):
    '''
    This module is a required class for other modules and scripts within the
    SAHM package. It is not intended for direct use or incorporation into
    the VisTrails workflow by the user.
    '''
    _input_ports = [('value', '(gov.usgs.sahm:mpl_colormap:Other)')]
    _output_ports = [('value_as_string', '(edu.utah.sci.vistrails.basic:String)', True)]
    maps=[m for m in pylab.cm.datad if not m.endswith("_r")]
    maps.sort()
    _widget_class = build_enum_widget('mpl_colormap', 
                                      maps)

    @staticmethod
    def get_widget_class():
        return mpl_colormap._widget_class
    
class T_O_M(String):
    '''
    This module is a required class for other modules and scripts within the
    SAHM package. It is not intended for direct use or incorporation into
    the VisTrails workflow by the user.
    '''
    _input_ports = [('value', '(gov.usgs.sahm:T_O_M:Other)')]
    _output_ports = [('value_as_string', '(edu.utah.sci.vistrails.basic:String)', True)]
    _widget_class = build_enum_widget('T_O_M', 
                                      ["Threshold=0.5",
                                       "Sensitivity=Specificity",
                                       "Maximizes (sensitivity+specificity)/2",
                                       "Maximizes Cohen's Kappa",
                                       "Maximizes PCC (percent correctly classified)",
                                       "Predicted prevalence=observed prevalence",
                                       "Threshold=observed prevalence",
                                       "Mean predicted probability",
                                       "Minimizes distance between ROC plot and (0,1)",
                                       ])

    @staticmethod
    def get_widget_class():
        return T_O_M._widget_class