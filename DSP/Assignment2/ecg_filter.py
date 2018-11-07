import numpy as np
import matplotlib.pyplot as plt
import scipy.signal as sig
import ctypes

#for linux
import os

class fir_lfilter:

    def __init__(self, coefficients):
        self.__tabs = len(coefficients)
        # vars for python lfilter
        self.__buffer = np.zeros(self.__tabs)
        self.__coefficients = coefficients
        self.__pointer = 0

        # import c lfilter
            # for windows
        # self.fir_c_lib = ctypes.CDLL.LoadLibrary("DSP_FIR.dll")
            # for linux
        self.fir_c_lib = ctypes.CDLL(os.path.abspath("DSP_FIR.so"))
        self.fir_c_lib.dofilter.restype = ctypes.c_float
        self.fir_c_lib.dofilter.argtypes = [ctypes.c_float]

        b_c = (ctypes.c_float * self.__tabs)()
        for i in range(self.__tabs):
            b_c[i] = self.__coefficients[i]

        self.fir_c_lib.init(b_c, self.__tabs)

    # extremely slow, I cannot endure the speed!!!!!!
    def dofilter(self, v):
        self.__buffer[self.__pointer] = v
        output = 0
        coefficient_index = 0
        
        for i in range(self.__pointer, self.__tabs):
            output += self.__coefficients[coefficient_index] * self.__buffer[i]
            coefficient_index += 1
        for i in range(0, self.__pointer):
            output += self.__coefficients[coefficient_index] * self.__buffer[i]
            coefficient_index += 1
        
        self.__pointer -= 1
        if(self.__pointer < 0):
            self.__pointer = self.__tabs - 1
        
        return output

class fir_filter:

    def __init__(self, sampling_frequency, cutoff_frequency, pass_zero, window):

        self.__nyq = sampling_frequency / 2
        self.__tabs = self.__calculate_tabs(cutoff_frequency)
        self.__coefficients = self.__firwin(cutoff_frequency, pass_zero, window)

        self.lfilter = fir_lfilter(self.__coefficients)

    def __calculate_tabs(self, cutoff_frequency):
        cutoff_temp = np.append(0, cutoff_frequency)
        cutoff_temp = np.append(cutoff_temp, self.__nyq)
        diff = []
        for i in range(len(cutoff_temp) - 1):
            diff.append(cutoff_temp[i + 1] - cutoff_temp[i])
        freq_res = min(diff)

        ntabs = int(self.__nyq * 2 / freq_res)
        ntabs = ntabs * 2 + 1
        return ntabs

    def __firwin(self, cutoff_frequency, pass_zero, window):

        w0 = np.linspace(0, 2 * np.pi - 2 * np.pi / self.__tabs, self.__tabs)
        normalised_cutoff_radian = np.array(cutoff_frequency) * np.pi / self.__nyq
        hwj = np.zeros(self.__tabs)
        for i in range(len(normalised_cutoff_radian)):

            if (i == 0):
                for j in range(len(w0)):
                    if (w0[j] <= normalised_cutoff_radian[i] or w0[j] >= 2 * np.pi - normalised_cutoff_radian[i]):
                        hwj[j] = 1
            else:
                if (i % 2):
                    for j in range(len(w0)):
                        if (w0[j] >= normalised_cutoff_radian[i] and w0[j] <= np.pi and w0[j] >= normalised_cutoff_radian[i - 1]):
                            hwj[j] = 1
                        if (w0[j] <= 2 * np.pi - normalised_cutoff_radian[i] and w0[j] >= np.pi and w0[j] <= 2 * np.pi - normalised_cutoff_radian[i - 1]):
                            hwj[j] = 1
                    
                else:
                    for j in range(len(w0)):
                        if (w0[j] >= normalised_cutoff_radian[i] and w0[j] <= np.pi and w0[j] >= normalised_cutoff_radian[i - 1]):
                            hwj[j] = 0
                        if (w0[j] <= 2 * np.pi - normalised_cutoff_radian[i] and w0[j] >= np.pi and w0[j] <= 2 * np.pi - normalised_cutoff_radian[i - 1]):
                            hwj[j] = 0
        if(pass_zero == False):
            for i in range(len(hwj)):
                if(hwj[i] == 0):
                    hwj[i] = 1
                else:
                    hwj[i] = 0

        ph = int(self.__tabs / 2) * w0

        h = np.fft.ifft(hwj * np.exp( -1j * ph))
        h = np.real(h) * sig.get_window(window, self.__tabs)

        return h

def main():

    # read ECG file
    # ecg = open("2293577c.dat")
    ecg = open("2359434c.dat", mode = 'r')

    time = []
    ch0 = []
    ch1 = []
    ch2 = []
 
    for line in (ecg):
        temp = line.split(' ')
        time.append(np.float32(temp[0]))
        ch0.append(np.float32(temp[1]))
        ch1.append(np.float32(temp[2]))
        ch2.append(np.float32(temp[3]))

    time = np.array(time)
    ch0 = np.array(ch0)
    ch1 = np.array(ch1)
    ch2 = np.array(ch2)
    ecg.close()

    # initialise filter
    fs = 1000
    fir = fir_filter(sampling_frequency = fs, cutoff_frequency = [1, 45, 55], pass_zero = False, window = 'hamming')

    # plot original data
    plt.figure(1)
    plt.plot(time, ch0)
    plt.xlabel("time s")
    plt.ylabel("original ch0 magnitude")
    # do filter
    ch0_filtered = []
    for i in range(len(ch0)):
        ch0_filtered.append(fir.lfilter.fir_c_lib.dofilter(ch0[i]))
    fir.lfilter.fir_c_lib.reset_buffer()

    # plot filtered data
    plt.figure(2)
    plt.plot(time, ch0_filtered)
    plt.xlabel("time s")
    plt.ylabel("filtered ch0 magnitude")

    # plot spectrum
    plt.figure(3)
    sp = np.abs(np.fft.fft(ch0_filtered))[0:len(ch0)>>1]
    freq = np.linspace(0, fs/2, len(sp))
    plt.plot(freq, sp)
    plt.xlabel("frequency hz")
    plt.ylabel("filtered ch0 magnitude")

    plt.show()

if __name__ == "__main__":
    main()