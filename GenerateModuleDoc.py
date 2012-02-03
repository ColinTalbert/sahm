'''
Due to the complexity of the documentation we wish to provide to the
SAHM package users it became necessary to store our module and port
level documentation in an external xml file.
This file is arranged in a specific manner with detailed information about
each of our modules, the input and output ports, options, defaults, requirements
references, etc.

'''

from xml.etree import ElementTree as ET
import textwrap

textwidth = 100

def load_documentation(xml_file):
    global doc_tree
    doc_tree = ET.parse(xml_file)
    
#    doc_tree = xml.dom.minidom.parse(xml_file)
#    global nodes
#    nodes = {}
#    module_nodes = doc_tree.getElementsByTagName("Module")
#    for module_node in module_nodes:
#        title = module_node.getElementsByTagName("Title")
#        nodes[title[0].firstChild.data] = module_node


def construct_module_doc(module_name):
    global doc_tree
    
    for elem in doc_tree.findall("Module"):

        title = elem.find("Title")
        if title.text == module_name:
            title = elem.find("Title")
            description = elem.find("Description")
            
            doc_string = module_name + "\n\n"
            doc_string += "Description:\n" + cleanupstring(description.text, 8, 4) + "\n\n"
            doc_string += constructInPortsText(elem)
            doc_string += constructOutPortsText(elem)      
                    
            doc_string += constructRefs(elem)
            
    return doc_string

def construct_port_doc(cls, port_name, direction):
    global doc_tree
    
    for elem in doc_tree.findall("Module"):

        title = elem.find("Title")
        if title.text == cls.__name__:
            if direction == "in":
                ports = elem.findall("InputPorts/Port")
            else:
                ports = elem.findall("OutputPorts/Port")
            for port in ports:
                name = port.find("PortName").text
                if name == port_name:
                    return construct_port_msg(port, 0)

                        
def constructInPortsText(module):
    inports = module.findall("InputPorts/Port")
    portsText = "Input Ports:\n"
    if not inports:
        portsText += "    None\n\n"
        return portsText
    
    for port in inports:
        portsText += construct_port_msg(port, 8) + "\n\n"
    return portsText

def constructOutPortsText(module):
    outports = module.findall("OutputPorts/Port")
    portsText = "Output Ports:\n"
    if not outports:
        portsText += "    None"
        return portsText
    
    for port in outports:
        portsText += construct_port_msg(port, 8) + "\n\n"
    return portsText

def constructRefs(module):
    refs = module.findall("References/Reference")
    refsText = "References:\n"
    if not refs:
        return ""
    else:
        refsText = "References:"
        for ref in refs:
            cleanref = cleanupstring(ref.text, 0, 8)
            refsText += "\n\n    " + cleanref
        return refsText

def construct_port_msg(port, indent):
    nl = "\n" + " "*indent
    Portname = port.find("PortName").text
    Definition = cleanupstring(port.find("Definition").text, indent, indent)
    Mandatory = port.find("Manditory").text
    Default = port.find("Default").text
    Options = port.findall("Options/Option")
    Connections = port.findall("Connections/Connection")
    msg = " "*(indent -4) + Portname + ":  "
    if Mandatory.lower() == "true":
        msg += "(mandatory)"
    elif Mandatory.lower() == "false":
        msg += "(optional)"
    ":"
    msg += "\n" + Definition
    
        
    if Default != "NA":
        msg += nl + nl + "Default value = " + Default
        
    if Options:
        msg += nl + "Options are:"
        for Option in Options:
            msg += "\n" + cleanupstring(Option.text, indent+4, indent+12)
            
    if Connections:
        msg += nl +nl + "Common connections:"
        for Connection in Connections:
            msg += "\n"+ cleanupstring(Connection.text, indent+4, indent+12)

    return msg

def cleanupstring(str, indent1, indent2):
    if str is None:
        return ""
    lines = str.split("\n")
    cleanstr = ""
    for line in lines:
        cleanstr += textwrap.fill(line, initial_indent=' '*indent1, subsequent_indent=' '*indent2, width = textwidth) +"\n"
#    str = textwrap.dedent(str)
    cleanstr = cleanstr[:-1]
    return cleanstr


