from core.configuration import ConfigurationObject

name = "SAHM"
identifier = "gov.usgs.sahm"
version = '0.0.5'

sahm_path = None
models_path = None
configuration = \
    ConfigurationObject(output_dir= r'I:\VisTrails\WorkingFiles\workspace',
                        r_path = r'I:\VisTrails\Central_VisTrailsInstall\Central_R\R-2.12.1\bin',
                        gdal_path = r'I:\VisTrails\Central_VisTrailsInstall\Central_GDAL',
                        maxent_path = r'I:\VisTrails\Central_VisTrailsInstall\Central_Maxent',
                        projection_layers_path = r'I:\WorldClim_Future_Climate\RenamedBILs',
                        verbose = 'True')
