import numpy as np
import scipy.signal as sig
import matplotlib.pyplot as plt
import serial
import struct
import multiprocessing
import time

import iir_filter
import iir_UI
import iir_MCUinterface


def plt_plot_task(w,h):
    plt.figure(1)
    plt.subplot(2,1,1)
    plt.title('Amplitude response')
    plt.plot(w/np.pi/2 * 200, 20 * np.log10(np.abs(h)))
    plt.subplot(2,1,2)
    plt.title('Phase response')
    plt.plot(w/np.pi/2 * 200,np.angle(h))
    plt.show()

def main():
    # specify parameters
    time_length = 5         # how much time of data would be shown
    Samplingrate = 200
    v_max = 3300

    # filter design
    bandstop = sig.cheby2(N = 8, Wn = [48/100, 53/100], rs = 40, btype = 'bandstop', output = 'sos')
    # highpass = sig.cheby2(N = 16, Wn = 1/100, rs = 40, btype = 'highpass', output = 'sos')
    
    # sos = np.append(highpass, bandstop)
    # sos = sos.reshape(int(len(sos)/6), 6)
    sos = bandstop

    # plot the frequency response
    w, h = sig.sosfreqz(sos)
    freq_response_process = multiprocessing.Process(target = plt_plot_task(w,h), name = "freq_response_plot")
    freq_response_process.start()
    
    # realtime viewer
    iirUI = iir_UI.UI(time_length * Samplingrate, Samplingrate, v_max)
    iirUI.UI_start()
    # instance of iirfilter
    iir = iir_filter.IIRFilter(sos)

    # open protocol handler
    port = iir_MCUinterface.MCU_interface("com3", 115200)

    buffer_original = np.zeros(time_length * Samplingrate)
    buffer_filtered = np.zeros(time_length * Samplingrate)
    i = 0
    while (True):
        time.sleep(0.001)
        while(port.Is_buffer_empty() == False):
            x = port.Read_one_packet()
            buffer_original[i] = x
            buffer_filtered[i] = iir.filter(x)
            i += 1
            if (i == Samplingrate * time_length):
                i = 0
            iirUI.UI_wrtite_data(buffer_original, buffer_filtered)
            if(port.Packet_lost() != 0):
                print(port.Packet_lost())


if __name__ == "__main__":
    main()