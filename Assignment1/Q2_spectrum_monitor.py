import pyaudio
import numpy as np
from pyqtgraph.Qt import QtGui, QtCore
import pyqtgraph as pg
import struct
import threading
import sys
import time
"""
    define parameters of the whole system
"""
SamplingRate = 44100
Channel = 1
DataFormat = pyaudio.paInt16
Chunk = 1024                                                    # in order to maxmise the speed of fft, the chunk size shoud be the power of n

Data_max_value = 32768

"""
    defined a class to refresh the signal waveform (time domain and frequency domain) using a dedicated thread
    in comparison to matplotlib, pyqtgraph is super fast
"""
class UI:

    """
        initialise function
    """
    def __init__(self):
        self.t = np.arange(0, Chunk, 1)                         # allocate time array
        self.f = np.linspace(0, SamplingRate / 2, Chunk / 2)    # allocate frequency array which would be the x axis of the spectrum plot

        self.refresh_flag = True                                # UI refreshing flag, when flag is set to true, the data can be written into the display buffer

        self.wv_data = np.zeros(Chunk)                          # allocate an array to store the signal in time domain
        self.sp_data = np.zeros(Chunk>>1)                       # allocate an array to store the signal in frequency domain
    
    """
        write the data into the display buffer
        param: 
            wv_data_in: time domain data
            sp_data_in: frequency domain data
    """
    def UI_wrtite_data(self, wv_data_in, sp_data_in):
        if(self.refresh_flag):
            self.wv_data = wv_data_in
            self.sp_data = sp_data_in
    
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
        self.wv_curve.setData(self.t, self.wv_data)
        self.sp_curve.setData(self.f, self.sp_data)
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
        # waveform lable setting
        wf_xlabels = []
        wf_xaxis = pg.AxisItem(orientation='bottom')
        wf_xaxis.setTicks([wf_xlabels])

        wf_ylabels = []
        wf_yaxis = pg.AxisItem(orientation='left')
        wf_yaxis.setTicks([wf_ylabels])

        sp_xlabels = [
            (np.log10(40), '40'), (np.log10(100), '100'),
            (np.log10(500), '500'), (np.log10(1000), '1k'),
            (np.log10(2000), '2k'), (np.log10(3000), '3k'),
            (np.log10(4000), '4k'), (np.log10(5000), '5k'),
            (np.log10(6000), '6k'), (np.log10(7000), '7k'),
            (np.log10(8000), '8k'), (np.log10(9000), '9k'),
            (np.log10(10000), '10k'), (np.log10(22050), '22.05k')
        ]
        sp_yaxis = pg.AxisItem(orientation='left')
        sp_xaxis = pg.AxisItem(orientation='bottom')
        sp_xaxis.setTicks([sp_xlabels])
        
        win.setBackground('w')
        waveform = win.addPlot(title='Time domain', row=1, col=1, axisItems={'bottom': wf_xaxis, 'left': wf_yaxis},)
        spectrum = win.addPlot(title='Frequency domain', row=2, col=1, axisItems={'bottom': sp_xaxis, 'left': sp_yaxis},)

        self.wv_curve = waveform.plot(pen = 'r')
        self.sp_curve = spectrum.plot(pen = 'b')  

        waveform.plot(pen = 'c', width = 3)
        waveform.setYRange(0, Data_max_value<<1, padding=0)
        waveform.setXRange(0, Chunk, padding=0.005)

        spectrum.setYRange(0, 1, padding=0)
        # spectrum.setXRange(0, SamplingRate / 2, padding=0.005)
        spectrum.setLogMode(x=True, y=False)
        # from 40 to 22050, log mode cannot diplay dc and the frequency resolution is (samplingrate / 2) / (chunksize / 2), roughly 43hz in this case
        spectrum.setXRange(np.log10(40), np.log10(SamplingRate/2), padding=0.005)
        spectrum.enableAutoRange(enable = False, x=False, y=False)

        # start a timer to trigger the refresh event
        timer = QtCore.QTimer()
        timer.timeout.connect(self.UI_refresh)
        timer.start(1/60)
        app.exec_()


"""
    main function
    entrance of the program
"""
def main():
    # open an audio input stream using pyaudio
    audio_handler = pyaudio.PyAudio()
    stream = audio_handler.open(format = DataFormat,
                    channels = Channel,
                    rate = SamplingRate,
                    input = True)
    # implement an ui instance
    ui = UI()
    ui.UI_start()

    while True:
        # read a chunk from stream
        raw_data = stream.read(Chunk)
        # unpack the data from binary stream to int16 array
        count = len(raw_data)/2
        format = "%dh"%(count)
        shorts = struct.unpack(format, raw_data)
        wv_data = np.array(shorts) + Data_max_value
        # perform fft to get the spectrum data
        sp_data = abs(np.fft.fft(wv_data))[0:int(len(wv_data) / 2)] * 2 / Chunk / Data_max_value

        # hand over all the data to the UI
        ui.UI_wrtite_data(wv_data, sp_data)

if __name__ == '__main__':
    main()