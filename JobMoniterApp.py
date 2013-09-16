'''Original from:
http://stackoverflow.com/questions/8786136/pyqt-how-to-detect-and-close-ui-if-its-already-running
'''
import os, sys
from PyQt4 import QtGui, QtCore, QtNetwork

class SingleApplication(QtGui.QApplication):
    def __init__(self, argv, key):
        QtGui.QApplication.__init__(self, argv)
        self._memory = QtCore.QSharedMemory(self)
        self._memory.setKey(key)
        if self._memory.attach():
            self._running = True
        else:
            self._running = False
            if not self._memory.create(1):
                raise RuntimeError(
                    self._memory.errorString().toLocal8Bit().data())

    def isRunning(self):
        return self._running

class Window(QtGui.QWidget):

    def __init__(self, workspace):
    
        QtGui.QWidget.__init__(self)
        try:
            #Try to set the window icon.  This might not be cross-platform
            vistrailsdir = os.path.dirname(os.path.dirname(sys.executable))
            app_icon = QtGui.QIcon(vistrailsdir +
                '/gui/resources/images/vistrails_icon_small.png')
            self.setWindowIcon(app_icon)
        except:
            pass
        
        self.sessionDir = workspace
        
        self.treeView = QtGui.QTreeWidget()
        self.treeView.setAlternatingRowColors(True)
        self.treeView.setColumnCount(2)
        self.treeView.headerItem().setText(0, "Model")
        self.treeView.setColumnWidth(0,150)
        self.treeView.headerItem().setText(1, "Status")
        self.treeView.setColumnWidth(1, 550)
        
        self.data = self.loadData()
        
        #add a timer so that we can update the contents every 5 sec
        self.timer = QtCore.QTimer()
        self.timer.timeout.connect(self.updateContents)
        self.timer.start(500)
        
        layout = QtGui.QVBoxLayout()
        layout.addWidget(self.treeView)
        self.setLayout(layout)
    
    def addItems(self, parent, elements):
    
        for text, children in elements:
            item = QtGui.QStandardItem(text)
            parent.appendRow(item)
            if children:
                self.addItems(item, children)
    
    def loadData(self):
        
        
        subFolders = [x[0] for x in os.walk(self.sessionDir)]
        models = ["brt", "rf_", "mar", "glm", "Max", "App"]
        self.data = []
        
        expandedNodes = []
        for index in range(self.treeView.topLevelItemCount()):
            if self.treeView.topLevelItem(index).isExpanded():
                expandedNodes.append(index)
        
        self.treeView.clear()
        for subFolder in subFolders:
            if os.path.split(subFolder)[1][:3] in models:
                itemName = (os.path.split(subFolder)[1])
                result = self.checkIfModelFinished(subFolder)
                
                errorText = self.getText(subFolder, "stdErr.txt").strip()
                warningCount = errorText.count("Warning")
                
                if warningCount == 0:
                    resultText = result
                else:
                    resultText = result + "    (with " + str(warningCount) + " warnings)"
                child_item = QtGui.QTreeWidgetItem([itemName, resultText])
                if result.startswith("Completed successfully"):
                    child_item.setBackgroundColor(1, QtGui.QColor(188,220, 157))
                elif result == "Error in model":
                    child_item.setBackgroundColor(1, QtGui.QColor(223, 131, 125))
                    
                errorText = self.getText(subFolder, "stdErr.txt").strip()
                errorText += "\n" +  self.getText(subFolder, "stdErr_R.txt").strip()
                if itemName.startswith("Maxent"):
                    errorText += "\n" +  self.getText(subFolder, "stdErr_max.txt").strip()  
                    
                error_item = QtGui.QTreeWidgetItem(["stdError", errorText])
                error_item.setTextAlignment(0, QtCore.Qt.AlignJustify)
                error_item.setTextAlignment(1, QtCore.Qt.AlignJustify)
#                out_item = QTreeWidgetItem(["stdOut", self.getText(subFolder, "stdOut.txt")])
#                out_item.setTextAlignment(Qt.AlignTop, Qt.AlignCenter)
                child_item.addChild(error_item)
#                child_item.addChild(out_item)
                
                self.treeView.addTopLevelItem(child_item)

        for index in expandedNodes:
            self.treeView.topLevelItem(index).setExpanded(True)
       
       
    def getText(self, subfolder, fname):
        try:
            fullName = os.path.join(subfolder, fname)
            return "".join(open(fullName, "r").readlines())
        except IOError:
            return ""
                    
    def checkIfModelFinished(self, model_dir):
        try:
            for err_fname in ['stdErr.txt', 'stdErr_R.txt', 'stdErr_max.txt']:
                out_err = os.path.join(model_dir, err_fname)
                stdErrLines = "\n".join(open(out_err, "r").readlines())
                if "Error" in stdErrLines:
                    return "Error in model"
        except:
            pass
    
        try:
            outText = self.find_file(model_dir, "_output.txt")
        except RuntimeError:
            return "Starting ..."
        
        model_text = os.path.join(model_dir, outText)
        try:
            lastLine = open(model_text, 'r').readlines()[-2]
        except IndexError:
            return "Running ..."
         
        if lastLine.startswith("Total time"):
            return "Completed successfully in " + lastLine[lastLine.find(" = ")+3:]
        elif lastLine.startswith("Model Failed"):
            return "Error in model"
        else:
            return "Running ..."
    
    def find_file(self, model_dir, suffix):
        try:
            return [file_name for file_name in os.listdir(model_dir)
                                     if file_name.endswith(suffix)][0]
        except IndexError:
            raise RuntimeError('The expected model output '
                                   + suffix + ' was not found in the model output directory')
        
    def updateContents(self,):
        self.loadData()   



if __name__ == '__main__':

    import sys

    key = 'SAHM_JobMonitorApplication'

    app = SingleApplication(sys.argv, key)
    if app.isRunning():
        print('SAHM_JobMonitorApplication is already running')
        sys.exit(1)

    window = Window(sys.argv[1])
    window.setWindowTitle("Asynchronous Model Run Monitor")
    window.resize(750, 800)
    window.show()
    sys.exit(app.exec_())