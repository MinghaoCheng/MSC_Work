import numpy as np
import matplotlib.pyplot as plt
import scipy.signal as sig

class FIR_filter:

    def __init__(self, sampling_frequency):
        self.__nyq = sampling_frequency / 2

    def Lowpass(self, tabs, cutoff_frequency):

        normalised_cutoff_radian = cutoff_frequency * np.pi / self.__nyq
        w0 = np.linspace(0, 2 * np.pi - 2 * np.pi / tabs, tabs)
        hwj = np.zeros(tabs)
        # calculate coeffecient of magnitude
        for i in range(len(w0)):
            if (w0[i] <= normalised_cutoff_radian) or (w0[i] >= 2 * np.pi - normalised_cutoff_radian):
                hwj[i] = 1
        # linear phase
        ph = int(tabs / 2) * w0

        h = np.fft.ifft(hwj * np.exp( -1j * ph))
        h = np.real(h) * np.hamming(tabs)

        return h

    def Highpass(self, tabs, cutoff_frequency):
        normalised_cutoff_radian = cutoff_frequency * np.pi / self.__nyq
        w0 = np.linspace(0, 2 * np.pi - 2 * np.pi / tabs, tabs)
        hwj = np.zeros(tabs)
        # calculate coeffecient of magnitude
        for i in range(len(w0)):
            if (w0[i] >= normalised_cutoff_radian) and (w0[i] <= 2 * np.pi - normalised_cutoff_radian):
                hwj[i] = 1
        # linear phase
        ph = int(tabs / 2) * w0

        h = np.fft.ifft(hwj * np.exp( -1j * ph))
        h = np.real(h) * np.blackman(tabs)

        return h

    def Bandpass(self, tabs, start_frequency, stop_frequency):
        normalised_start_radian =  start_frequency * np.pi / self.__nyq
        normalised_stop_radian = stop_frequency * np.pi / self.__nyq
        w0 = np.linspace(0, 2 * np.pi - 2 * np.pi / tabs, tabs)
        hwj = np.zeros(tabs)
        # calculate coeffecient of magnitude
        for i in range(len(w0)):
            if (w0[i] >= normalised_start_radian) and (w0[i] <= normalised_stop_radian):
                hwj[i] = 1
            if (w0[i] >= 2 * np.pi - normalised_stop_radian) and (w0[i] <= 2 * np.pi - normalised_start_radian):
                hwj[i] = 1

        # linear phase
        ph = int(tabs / 2) * w0

        h = np.fft.ifft(hwj * np.exp( -1j * ph))
        h = np.real(h) * np.blackman(tabs)

        return h

    def Bandstop(self, tabs, start_frequency, stop_frequency):
        normalised_start_radian =  start_frequency * np.pi / self.__nyq
        normalised_stop_radian = stop_frequency * np.pi / self.__nyq
        w0 = np.linspace(0, 2 * np.pi - 2 * np.pi / tabs, tabs)
        hwj = np.ones(tabs)
        # calculate coeffecient of magnitude
        for i in range(len(w0)):
            if (w0[i] >= normalised_start_radian) and (w0[i] <= normalised_stop_radian):
                hwj[i] = 0
            if (w0[i] >= 2 * np.pi - normalised_stop_radian) and (w0[i] <= 2 * np.pi - normalised_start_radian):
                hwj[i] = 0

        # linear phase
        ph = int(tabs / 2) * w0

        h = np.fft.ifft(hwj * np.exp( -1j * ph))
        h = np.real(h) * np.blackman(tabs)

        return h

def convolve(input1, input2):
    input1_prime = np.append(input1, np.zeros(len(input2)))
    output = np.zeros(len(input1) + len(input2))

    for i in range(len(input1) + len(input2)):
        for j in range(len(input2)):
            output[i] += input2[j] * input1_prime[i - j]

    return output

def main():

    fs = 1024
    
    fir = FIR_filter(fs)
    
    t = np.arange(0, 1, 1/fs)
    x = np.sin(2 * np.pi * 10 * t) + np.sin(2 * np.pi * 50 * t) + np.sin(2 * np.pi * 100 * t)

    h = fir.Lowpass(401, 15)
    y = convolve(h, x)

    x_p = np.sin(2 * np.pi * 10 * t)

    plt.plot(y)
    plt.plot(x_p)
    plt.show()


if __name__ == "__main__":
    main()