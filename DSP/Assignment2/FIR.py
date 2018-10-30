import numpy as np
import matplotlib.pyplot as plt
import scipy.signal as sig


def FIR_lp_ifft(N, stop_freq):

    w = np.linspace(0, 2 * np.pi - 2 * np.pi / N, N)
    
    hwj = np.zeros(N)

    for i in range(len(w)):
        if (w[i] <= stop_freq) or (w[i] >= 2 * np.pi - stop_freq):
            hwj[i] = 1

    ph = int(N / 2) * w

    h = np.real(np.fft.ifft(hwj * np.exp( - 1j * ph)))

    return h

def FIR_hp_ifft(N, stop_freq):

    w = np.linspace(0, 2 * np.pi - 2 * np.pi / N, N)
    hwj = np.zeros(N)

    for i in range(len(w)):
        if (w[i] >= stop_freq) and (w[i] <= 2 * np.pi - stop_freq):
            hwj[i] = 1

    ph = int(N / 2) * w

    h = np.real(np.fft.ifft(hwj * np.exp( - 1j * ph)))
    
    return h

def FIR_lp(N, stop_freq):

    n = np.arange(int(-N/2), int(N/2 +1))

    h = 1/(n * np.pi) * np.sin(stop_freq *n)

    h[N>>1] = stop_freq / np.pi

    return h

def main():

    sampling_freq = 1000
    cut_off_freq = 100
    
    b = FIR_lp(401, 2 * np.pi * cut_off_freq / sampling_freq) * np.hamming(401)

    f = sig.firwin(401, 100, pass_zero = True, fs = 1000 , window = 'hamming')

    plt.plot(b)
    plt.plot(f)

    plt.show()


if __name__ == "__main__":
    main()