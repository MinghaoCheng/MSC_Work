import numpy as np
from PIL import Image
import matplotlib.pyplot as plt
import scipy.special
import komm

def main():

    tx_im = Image.open("DC4_300x200.pgm")
    tx_bin = np.unpackbits(np.array(tx_im))

    modulation = komm.PSKModulation(2)
    # modulation = komm.QAModulation(256, base_amplitudes= 1/np.sqrt(170))

    SNR = np.arange(0,20,1)
    BER = snr_ber_simulation(tx_bin, SNR, modulation)

    plt.figure()
    plt.scatter(SNR, BER)
    plt.plot(SNR, BER, label = 'simulation')

    # theoraticall curve
    BER_theoratical = 1/2 * scipy.special.erfc(np.sqrt(10**(SNR/10)/modulation.bits_per_symbol))
    plt.plot(SNR, BER_theoratical, label = 'theoratical')
    
    plt.title('SNR vs BER')
    plt.yscale('log')
    plt.legend()
    plt.show()
    # image recovery
    # plt.figure()
    # rx_im = np.packbits(rx_bin).reshape(tx_im.size[1],tx_im.size[0])

def snr_ber_simulation(tx_bin, snr, modulate):

    BER = []
    tx_data = modulate.modulate(tx_bin)

    for i in range (0, len(snr)):

        awgn = komm.AWGNChannel(snr=10**(snr[i]/10.))
        rx_data = awgn(tx_data)
        rx_bin = modulate.demodulate(rx_data)
        BER.append(BER_cal(tx_bin, rx_bin))

        # constellation diagram
        # plt.figure()
        # plt.title("constellation diagram")
        # plt.scatter(rx_data.real,rx_data.imag,s=1,marker=".")
        # plt.ylim(bottom = -1.2, top = 1.2)
        # plt.axes().set_aspect('equal')

    return BER

def BER_cal(tx_bin, rx_bin):

    total_bits = len(tx_bin)
    temp = tx_bin ^ rx_bin
    error_bits = np.sum(temp)

    return error_bits / total_bits

if __name__ == "__main__":
    main()