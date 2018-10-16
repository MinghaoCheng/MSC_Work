import numpy as np
import matplotlib.pyplot as plt
import scipy.signal as sig


def FIR_lp_ifft(N, stop_freq):

    w = np.linspace(0, 2 * np.pi, N)
    hwj = np.zeros(N)

    for i in range(len(w)):
        if (w[i] <= stop_freq) or (w[i] >= 2 * np.pi - stop_freq):
            hwj[i] = 1

    ph = (N / 2 + 1) * w

    h = np.real(np.fft.ifft(hwj * np.exp( - 1j * ph)))
    
    return h

def FIR_hp_ifft(N, stop_freq):

    w = np.linspace(0, 2 * np.pi, N)
    hwj = np.zeros(N)

    for i in range(len(w)):
        if (w[i] >= stop_freq) and (w[i] <= 2 * np.pi - stop_freq):
            hwj[i] = 1

    ph = (N / 2 + 1) * w

    h = np.real(np.fft.ifft(hwj * np.exp( - 1j * ph)))
    
    return h

def FIR_lp(N, stop_freq):

    n = np.arange(-N/2, N/2 +1)

    h = 1/(n * np.pi) * np.sin(stop_freq *n)

    h[N>>1] = stop_freq / np.pi

    return h

def main():

    sampling_freq = 1000
    cut_off_freq = 55
    
    h = FIR_hp_ifft(101, 2 * np.pi * cut_off_freq / sampling_freq )

    t = np.arange(0, 1, 1/sampling_freq)
    freq_arr = np.arange(0, sampling_freq >> 1, 1)
    
    
    x = np.sin(2*np.pi*50*t) + np.sin(2*np.pi*60*t)
    
    xsp = abs(np.fft.fft(x))[0:sampling_freq >> 1]

    plt.figure(1)
    plt.subplot(2, 1, 1)
    plt.plot(freq_arr,xsp)


    y = sig.lfilter(h, 1, x)
    ysp = abs(np.fft.fft(y))[0:sampling_freq >> 1]
    plt.subplot(2, 1, 2)
    plt.plot(freq_arr,ysp)

    plt.figure(2)
    w = np.linspace(0, 2 * np.pi, 1000)
    w, z = sig.freqz(h, 1, w)
    plt.plot(w, abs(z))

    plt.show()








if __name__ == "__main__":
    main()