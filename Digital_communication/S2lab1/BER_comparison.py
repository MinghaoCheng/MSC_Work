import numpy as np
from PIL import Image
import matplotlib.pyplot as plt
import scipy.special
import komm

def main():

    tx_im = Image.open("DC4_150x100.pgm")
    tx_bin = np.unpackbits(np.array(tx_im))

    # make sure every modulation has the same energy per symbol
    bpsk = komm.PSKModulation(2)
    qam_16 = komm.QAModulation(16, base_amplitudes=1/np.sqrt(10))
    qam_256 = komm.QAModulation(256, base_amplitudes=1/np.sqrt(170))

    snr = np.arange(0,40,2)
    ber_bpsk = snr_ber_simulation(tx_bin, snr, bpsk)
    ber_qam_16 = snr_ber_simulation(tx_bin, snr, qam_16)
    ber_qam_256 = snr_ber_simulation(tx_bin, snr, qam_256)

    # plot all snr vs ber curves
    plt.figure()    
    plt.scatter(snr, ber_bpsk)
    plt.plot(snr, ber_bpsk, label = 'BPSK')
    plt.scatter(snr, ber_qam_16)
    plt.plot(snr, ber_qam_16, label = '16QAM')
    plt.scatter(snr, ber_qam_256)
    plt.plot(snr, ber_qam_256, label = '256QAM')

    plt.xlabel('SNR dB')
    plt.ylabel('BER percentage')
    plt.title('SNR vs BER')
    plt.legend()
    plt.yscale('log')

    # image recovery
    # plt.figure()
    # rx_im = np.packbits(rx_bin).reshape(tx_im.size[1],tx_im.size[0])

    plt.show()

def snr_ber_simulation(tx_bin, snr, modulate):

    BER = []
    tx_data = modulate.modulate(tx_bin)

    for i in range (0, len(snr)):
        
        awgn = komm.AWGNChannel(snr=10**(snr[i]/10.))
        rx_data = awgn(tx_data)
        rx_bin = modulate.demodulate(rx_data)
        BER.append(BER_cal(tx_bin, rx_bin))
        # plt.figure()
        # plt.title("constellation diagram")
        # plt.scatter(rx_data.real,rx_data.imag,s=1,marker=".")
        # plt.axes().set_aspect('equal')
    return BER

def BER_cal(tx_bin, rx_bin):

    total_bits = len(tx_bin)
    temp = tx_bin ^ rx_bin
    error_bits = np.sum(temp)

    return error_bits / total_bits

if __name__ == "__main__":
    main()