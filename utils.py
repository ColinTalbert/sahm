'''
Created on Nov 18, 2010

@author: talbertc
'''
import os, shutil
import time
import tempfile


from core.modules.basic_modules import File, Directory, new_constant, Constant


_roottempdir = ""
_temp_files = []
_temp_dirs = []

def mktempfile(*args, **kwargs):
    global _temp_files
    global _roottempdir
    kwargs["dir"] = _roottempdir
    (fd, fname) = tempfile.mkstemp(*args, **kwargs)
    os.close(fd)
    _temp_files.append(fname)
    return fname

def mktempdir(*args, **kwargs):
    global _temp_dirs
    global _roottempdir
    kwargs["dir"] = _roottempdir
    dname = tempfile.mkdtemp(*args, **kwargs)
    _temp_dirs.append(dname)
    return dname

def createrootdir():
    global _roottempdir
    _roottempdir = tempfile.mkdtemp(prefix='sahmrun_' + 
                    time.strftime("%Y%m%dT%H%M%S") +"_")
    
    print ("*"*60 + "\n")*2 + ("*"*60 )
    print "temp directory location is " + _roottempdir
    print ("*"*60 + "\n")*3

def cleantemps():
    global _temp_files, _temp_dirs, _roottempdir
    for file in _temp_files:
        os.remove(file)
    for dir in _temp_dirs:
        shutil.rmtree(dir)
    shutil.rmtree(_roottempdir)

def map_ports(module, port_map):
    args = {}
    for port, (flag, access, required) in port_map.iteritems():
        if required or module.hasInputFromPort(port):
            value = module.getInputFromPort(port)
            if access is not None:
                value = access(value)
            args[flag] = value
    return args

def path_value(value):
    return value.name

def dir_path_value(value):
    val = value.name
    sep = os.path.sep
    return val.replace("/", sep)
    

def create_file_module(fname, f=None):
    if f is None:
        f = File()
    f.name = fname
    f.upToDate = True
    return f

def create_dir_module(dname, d=None):
    if d is None:
        d = Directory()
    d.name = dname
    d.upToDate = True
    return d

def collapse_dictionary(dict):
    list = []
    for k,v in dict.items():
        list.append(k)
        list.append(v)
    return list

if __name__ == '__main__':
    pass