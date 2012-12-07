import sys, os
from PyQt4.QtCore import *
from PyQt4.QtGui import *

data = [
    ("Alice", [
        ("Keys", []),
        ("Purse", [
            ("Cellphone", [])
            ])
        ]),
    ("Bob", [
        ("Wallet", [
            ("Credit cardalkdfjdlas;kjf\n\njklhfl;dkfjh\nakudfh\n\nklajhfklds\nakljfld;skj\nalkufhiaurtyeiuh", []),
            ("Money", [])
            ])
        ])
    ]

class Window(QWidget):

    def __init__(self, workspace):
    
        QWidget.__init__(self)
        
        self.sessionDir = workspace
        
        self.treeView = QTreeWidget()
        self.treeView.setAlternatingRowColors(True)
        self.treeView.setColumnCount(2)
        self.treeView.headerItem().setText(0, "Model")
        self.treeView.setColumnWidth(0,150)
        self.treeView.headerItem().setText(1, "Status")
        self.treeView.setColumnWidth(1, 550)
        
        self.data = self.loadData()
        
        #add a timer so that we can update the contents every 5 sec
        self.timer = QTimer()
        self.timer.timeout.connect(self.updateContents)
        self.timer.start(5000)
        
        layout = QVBoxLayout()
        layout.addWidget(self.treeView)
        self.setLayout(layout)
    
    def addItems(self, parent, elements):
    
        for text, children in elements:
            item = QStandardItem(text)
            parent.appendRow(item)
            if children:
                self.addItems(item, children)
    
    def loadData(self):
        
        
        subFolders = [x[0] for x in os.walk(self.sessionDir)]
        models = ["brt", "rf_", "mar", "glm"]
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
                warningCount = errorText.count("Warning:")
                
                if warningCount == 0:
                    resultText = result
                else:
                    resultText = result + "    (with " + str(warningCount) + " warnings)"
                child_item = QTreeWidgetItem([itemName, resultText])
                if result == "Completed successfully":
                    child_item.setBackgroundColor(1, QColor(188,220, 157))
                elif result == "Error in model":
                    child_item.setBackgroundColor(1, QColor(223, 131, 125))
                    
                errorText = self.getText(subFolder, "stdErr.txt").strip()
                    
                error_item = QTreeWidgetItem(["stdError", errorText])
                error_item.setTextAlignment(0, Qt.AlignJustify)
                error_item.setTextAlignment(1, Qt.AlignJustify)
#                out_item = QTreeWidgetItem(["stdOut", self.getText(subFolder, "stdOut.txt")])
#                out_item.setTextAlignment(Qt.AlignTop, Qt.AlignCenter)
                child_item.addChild(error_item)
#                child_item.addChild(out_item)
                
                self.treeView.addTopLevelItem(child_item)

        for index in expandedNodes:
            self.treeView.topLevelItem(index).setExpanded(True)
       
       
    def getText(self, subfolder, fname):
        fullName = os.path.join(subfolder, fname)
        return "".join(open(fullName, "r").readlines())
                    
    def checkIfModelFinished(self, model_dir):
    
        try:
            outText = self.find_file(model_dir, "_output.txt")
        except RuntimeError:
            return "Running ..."
        
        model_text = os.path.join(model_dir, outText)
        try:
            lastLine = open(model_text, 'r').readlines()[-2]
        except IndexError:
            return "Running ..."
         
        if lastLine.startswith("Total time"):
            return "Completed successfully"
        elif lastLine.startswith("Model failed"):
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




if __name__ == "__main__":

    app = QApplication(sys.argv)
    window = Window(sys.argv[1])
    window.setWindowTitle("Asynchronous Model Run Monitor")
    window.resize(750, 800)
    window.show()
    sys.exit(app.exec_())