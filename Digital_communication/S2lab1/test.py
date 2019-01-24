import numpy as np
from PIL import Image
import matplotlib.pyplot as plt
import komm

def main():
    tx_im = Image.open("DC4_150x100.pgm")
    # Npixels = tx_im.size[1]*tx_im.size[0]
    # plt.figure()
    # plt.imshow(np.array(tx_im), cmap='gray', vmin=0,vmax=255)
    # plt.show()
    tx_bin = np.unpackbits(np.array(tx_im))

    psk = komm.PSKModulation(2)
    awgn = komm.AWGNChannel(snr=10**(6./10.))
    tx_data = psk.modulate(tx_bin)
    rx_data = awgn(tx_data)
    rx_bin = psk.demodulate(rx_data)

    plt.figure()
    plt.plot(tx_data)
    plt.show()
    print(rx_bin==tx_bin)

if __name__ == "__main__":
    main()