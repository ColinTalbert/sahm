from core.configuration import ConfigurationObject

name = "SAHM"
identifier = "gov.usgs.sahm"
version = '1.0.0'

configuration = \
    ConfigurationObject(output_dir= r'C:\temp\SAHM_workspace',
                        r_path = r'..\\..\\Central_R\R-2.14.1\bin',
                        gdal_path = r'..\\..\\Central_GDAL',
                        maxent_path = r'..\\..\\Central_Maxent',
                        cur_session_folder = r"C:\temp\SAHM_workspace",
                        verbose = 'True')
