from core.configuration import ConfigurationObject

name = "SAHM"
identifier = "gov.usgs.sahm"
version = '0.0.4'

sahm_path = None
models_path = None
configuration = \
    ConfigurationObject(models_path='I:\VisTrails\Central_VisTrailsInstall_debug\vistrails\packages\sahm\pySAHM\Resources\R_Modules',
                        layers_path = 'I:\VisTrails\Central_VisTrailsInstall_debug\VisTrails\vistrails\packages\sahm\layers.csv',
                        r_path = 'I:\VisTrails\Central_VisTrailsInstall\Central_R\R-2.12.1\bin',
                        color_breaks_csv = 'I:\VisTrails\Central_VisTrailsInstall_debug\VisTrails\vistrails\packages\sahm\ColorBreaks.csv',
                        output_dir='I:\VisTrails\WorkingFiles\workspace',
                        verbose = 'true')
