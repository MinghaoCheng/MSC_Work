import pyaudio
import numpy as np
from pyqtgraph.Qt import QtGui, QtCore
import pyqtgraph as pg
import threading
import sys
import time
"""
    defined a class to refresh the signal waveform (time domain and frequency domain) using a dedicated thread
    in comparison to matplotlib, pyqtgraph is super fast
"""
class UI:

    """
        initialise function
    """
    def __init__(self, Chunksize, SamplingRate, Max_voltage):
        self.fs = SamplingRate
        self.chunksize = Chunksize
        self.vmax = Max_voltage
        self.t = np.arange(0, Chunksize, 1)         # allocate time array
        self.refresh_flag = True                    # UI refreshing flag, when flag is set to true, the data can be written into the display buffer

        self.original = np.zeros(Chunksize)         # allocate an array to store the signal in time domain
        self.filtered = np.zeros(Chunksize)
    
    """
        write the data into the display buffer
    """
    def UI_wrtite_data(self, original_in, filtered_in):
        if(self.refresh_flag):
            self.original = original_in
            self.filtered = filtered_in
    
    """
        Create a thread to handle the UI and start it
    """
    def UI_start(self):
        self.p = threading.Thread(target = self.UI_task)
        self.p.start()

    """
        refresh the UI by setting the data of curve to the buffer, then the qtgraph will update the data automatically
    """
    def UI_refresh(self):
        self.refresh_flag = False
        self.original_curve.setData(self.t, self.original)
        self.filtered_curve.setData(self.t, self.filtered)
        self.refresh_flag = True

    """
        the task function, initialise the QT graph UI and set a timer which would trigger an event 60 times per second
        thus the UI would run at 60fps
        ps: basicly, the function is modified from the pyqtgraph official example
    """ 
    def UI_task(self):
        # qt 
        app = QtGui.QApplication([])
        pg.setConfigOptions(antialias = True)
        win = pg.GraphicsWindow(title = "Plotting")
        win.resize(800, 800)
        win.setWindowTitle('Plotting')

        # create axis& lable setting
        xlabels = [
            (self.chunksize*0.1, str(self.chunksize*0.1/self.fs)),
            (self.chunksize*0.2, str(self.chunksize*0.2/self.fs)),
            (self.chunksize*0.3, str(self.chunksize*0.3/self.fs)),
            (self.chunksize*0.4, str(self.chunksize*0.4/self.fs)),
            (self.chunksize*0.5, str(self.chunksize*0.5/self.fs)),
            (self.chunksize*0.6, str(self.chunksize*0.6/self.fs)),
            (self.chunksize*0.7, str(self.chunksize*0.7/self.fs)),
            (self.chunksize*0.8, str(self.chunksize*0.8/self.fs)),
            (self.chunksize*0.9, str(self.chunksize*0.9/self.fs)),
            (self.chunksize*1,   str(self.chunksize*1/self.fs))
        ]
        ylabels = [
            (self.vmax*0.2, str(self.vmax*0.2)),
            (self.vmax*0.4, str(self.vmax*0.4)),
            (self.vmax*0.6, str(self.vmax*0.6)),
            (self.vmax*0.8, str(self.vmax*0.8)),
            (self.vmax*1,   str(self.vmax*1))
        ]

        original_xaxis = pg.AxisItem(orientation='bottom')
        original_xaxis.setTicks([xlabels])
        original_yaxis = pg.AxisItem(orientation='left')
        original_yaxis.setTicks([ylabels])

        filtered_xaxis = pg.AxisItem(orientation='bottom')
        filtered_xaxis.setTicks([xlabels])
        filtered_yaxis = pg.AxisItem(orientation='left')
        filtered_yaxis.setTicks([ylabels])
        
        win.setBackground('w')
        original_plot = win.addPlot(title='Original signal', row=1, col=1, axisItems={'bottom': original_xaxis, 'left': original_yaxis},)
        filtered_plot = win.addPlot(title='Filtered signal', row=2, col=1, axisItems={'bottom': filtered_xaxis, 'left': filtered_yaxis},)

        self.original_curve = original_plot.plot(pen = 'r')
        self.filtered_curve = filtered_plot.plot(pen = 'b')  

        original_plot.plot(pen = 'c', width = 3)
        original_plot.setYRange(0, self.vmax, padding=0)
        original_plot.setXRange(0, self.chunksize, padding=0.005)

        filtered_plot.plot(pen = 'c', width = 3)
        filtered_plot.setYRange(0, self.vmax, padding=0)
        filtered_plot.setXRange(0, self.chunksize, padding=0.005)

        # start a timer to trigger the refresh event
        timer = QtCore.QTimer()
        timer.timeout.connect(self.UI_refresh)
        timer.start(1/60)
        app.exec_()