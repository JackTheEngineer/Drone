import serial # import Serial Library
import pyqtgraph as pg
import numpy as np
from time import sleep
from collections import deque
from functools import partial as fp
from collections import deque
from pyqtgraph.Qt import QtGui, QtCore

class Plot(object):
      def __init__(self, window, title):
            self.plot = window.addPlot(title=title)
            self.plot.showGrid(x=False, y=True)
            self.plot.enableAutoRange('xy', True)
            self.curves = []
            self.data = []

      def addCurveToPlot(self, data, color):
            self.curves.append(self.plot.plot(np.array(data), pen=color))
            self.data.append([])

      def setData(self, curve_index, data):
            if(curve_index < self.numOfCurves()):
                  self.curves[curve_index].setData(np.array(data))

      def numOfCurves(self):
            return len(self.curves)

      def addDataPoint(self, curve_index, value):
            if(curve_index < self.numOfCurves()):
                  self.data[curve_index].append(value)
                  self.setData(curve_index, self.data[curve_index])
                  while(len(self.data[curve_index]) > 200):
                        del self.data[curve_index][0]

def readSerialAndUpdatePlot(ser_port, plots):
      bytes_per_value = 4

      while(ser_port.in_waiting != 0):
        values = []
        bytess = ser_port.readline()
        if(len(bytess) == 37):
          for i in range(9): # 9 Integer Numbers, using 36 Bytes , last byte is "\n"
            values.append(int.from_bytes(bytess[(bytes_per_value*i):(bytes_per_value*(i+1))],
                                         'little', signed=True))
          for i in range(3):
            for j in range(3):
                  plots[i].addDataPoint(j, values[i*3 + j])
            
def main():
      #ser_port = serial.Serial('/dev/ttyUSB0', 460800)
      ser_port = serial.Serial('/dev/ttyUSB0', 460800)
      #ser_port = serial.Serial('/dev/ttyUSB0', 115200)

      app = QtGui.QApplication([])

      win = pg.GraphicsWindow(title="Basic plotting examples")
      win.resize(1200,800)
      win.setWindowTitle('Plotting')
      pg.setConfigOptions(antialias=True)

      plotnames = ("Acceleration", "AngularMotion", "Magnetic Field")
      colors = (((255, 0, 0),(0,255,0), (0,0,255)),
                ((255,255,0),(255,0,255),(0,255,255)),
                ((255,100,100),(100,100,255),(100,255,100)))
      plots = []

      for i in range(3):
            newplot = Plot(win, plotnames[i])
            for color in colors[i]: # colors[i] is a color triple
                  newplot.addCurveToPlot([], color)
            plots.append(newplot)

      win.nextRow()


  
      timer = pg.QtCore.QTimer()
      timer.timeout.connect(fp(readSerialAndUpdatePlot,
                               ser_port, plots))

      timer.start(20)
      
      QtGui.QApplication.instance().exec_()
  
      ser_port.flush()
      ser_port.close()

    
if __name__ == "__main__":
      main()
            




  
