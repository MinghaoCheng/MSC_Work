import komm
import numpy as np
import matplotlib.pyplot as plt
import multiprocessing
from BCH_simulation import *

def main():

    SNR = np.arange(-5, 10, 1)
    modulation = komm.PSKModulation(4)
    tx_bin = np.random.randint(2, size = 1200)
    # new figure
    plt.figure()

    P = multiprocessing.Pool(len(SNR))
    encoder = komm.BCHCode(3, 1)   # (7,4) BCH
    param = []
    for i in range(0,len(SNR)):
        param.append(sim_param(tx_bin, SNR[i], modulation, encoder))
    BER = P.map(sim_BCH, param)
    plt.plot(SNR, BER, label = '(7,4) BCH code')
    P.close()
    P.join()

    P = multiprocessing.Pool(len(SNR))
    encoder = komm.BCHCode(4, 3)   # (15,5) BCH
    param = []
    for i in range(0,len(SNR)):
        param.append(sim_param(tx_bin, SNR[i], modulation, encoder))
    BER = P.map(sim_BCH, param)
    plt.plot(SNR, BER, label = '(15,5) BCH code')
    P.close()
    P.join()

    P = multiprocessing.Pool(len(SNR))
    encoder = komm.BCHCode(5, 7)   # (31,6) BCH
    param = []
    for i in range(0,len(SNR)):
        param.append(sim_param(tx_bin, SNR[i], modulation, encoder))
    BER = P.map(sim_BCH, param)
    plt.plot(SNR, BER, label = '(31,6) BCH code')
    P.close()
    P.join()

    P = multiprocessing.Pool(len(SNR))
    encoder = komm.BCHCode(6, 13)   # (63,10) BCH
    param = []
    for i in range(0,len(SNR)):
        param.append(sim_param(tx_bin, SNR[i], modulation, encoder))
    BER = P.map(sim_BCH, param)
    plt.plot(SNR, BER, label = '(63,10) BCH code')
    P.close()
    P.join()

    P = multiprocessing.Pool(len(SNR))
    encoder = -1
    param = []
    for i in range(0,len(SNR)):
        param.append(sim_param(tx_bin, SNR[i], modulation, encoder))
    BER = P.map(sim_BCH, param)
    plt.plot(SNR, BER, label = 'QPSK without FEC')
    P.close()
    P.join()
    
    plt.xlabel("SNR")
    plt.ylabel("BER")
    plt.yscale('log')
    plt.legend()
    plt.title("Comparison among different BCH codes")
    plt.show()









if __name__ == "__main__":
    main()