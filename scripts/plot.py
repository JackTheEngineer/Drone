import serial # import Serial Library
import pyqtgraph as pg
import numpy as np
from time import sleep
from collections import deque
from functools import partial as fp
from collections import deque
from pyqtgraph.Qt import QtGui, QtCore

def readSerialAndUpdatePlot(ser_port, curves, data):
      bytes_per_value = 4

      while(ser_port.in_waiting != 0):
        values = []
        bytess = ser_port.readline()
        if(len(bytess) == 37):
          for i in range(9): # 9 Integer Numbers, using 36 Bytes , last byte is "\n"
            values.append(int.from_bytes(bytess[(bytes_per_value*i):(bytes_per_value*(i+1))],
                                         'little', signed=True))
          print(values)
          for i, oneArray in enumerate(data):
            value = values[i]
            if(value > 2**15):
              value = values[i] - 2**16 

            oneArray.append(value)
            curves[i].setData(np.array(oneArray))
            while(len(oneArray) > 200):
              del oneArray[0]

          
            
def main():
      #ser_port = serial.Serial('/dev/ttyUSB0', 460800)
      ser_port = serial.Serial('/dev/ttyUSB0', 460800)
      #ser_port = serial.Serial('/dev/ttyUSB1', 115200)

      app = QtGui.QApplication([])

      win = pg.GraphicsWindow(title="Basic plotting examples")
      win.resize(1200,800)
      win.setWindowTitle('Plotting')
      pg.setConfigOptions(antialias=True)
      
      p1 = win.addPlot(title="Acceleration")
      p1.showGrid(x=False, y=True)
      p1.enableAutoRange('xy', True)

      p2 = win.addPlot(title="AngularMotion")
      p2.showGrid(x=False, y=True)
      p2.enableAutoRange('xy', True)

      p3 = win.addPlot(title="Magnetic Field")
      p3.showGrid(x=False, y=True)
      p3.enableAutoRange('xy', True)

      colors = [(255, 0, 0),(0,255,0), (0,0,255),
                (255,255,0),(255,0,255),(0,255,255),
                (255,255,255),(255,255,255),(255,255,255)]
  
      data = []
      curves = []
      for i in range(3):
            data.append([])
            curves.append(p1.plot(np.array(data[i]), pen=colors[i]))

      for i in range(3,6):
            data.append([])
            # appending to plot p2
            curves.append(p2.plot(np.array(data[i]), pen=colors[i]))

      for i in range(6,9):
            data.append([])
            # appending to plot p3
            curves.append(p2.plot(np.array(data[i]), pen=colors[i]))

      timer = pg.QtCore.QTimer()
      timer.timeout.connect(fp(readSerialAndUpdatePlot,
                               ser_port,
                               curves,
                               data))
      timer.start(20)
      
      QtGui.QApplication.instance().exec_()
  
      ser_port.flush()
      ser_port.close()

    
if __name__ == "__main__":
      main()
            




  
