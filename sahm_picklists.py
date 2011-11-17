
from core.modules.basic_modules import String
from enum_widget import build_enum_widget

class ResponseType(String):
    '''
    This module is a required class for other modules and scripts within the
    SAHM package. It is not intended for direct use or incorporation into
    the VisTrails workflow by the user.
    '''
    _input_ports = [('value', '(gov.usgs.sahm:ResponseType:Other)')]
    _output_ports = [('value_as_string', '(edu.utah.sci.vistrails.basic:String)', True)]
    _widget_class = build_enum_widget('ResponseType', 
                                      ['Presence(Absence)','Count'])

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
                                      ['Mean', 'Max', 'Min', 'Majority', 'None'])

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