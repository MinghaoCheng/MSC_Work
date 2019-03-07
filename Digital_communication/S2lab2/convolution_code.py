import komm
import numpy as np
import matplotlib.pyplot as plt
from convolution_simulation import *
import multiprocessing.dummy

def main():
    # param specification
    SNR = np.arange(-5, 10, 1)
    modulation = komm.PSKModulation(4)
    tx_bin = np.random.randint(2, size = 1200)
    tblen = 36

    # hard decision simulation
    decision_method = "hard"
    code = komm.ConvolutionalCode(feedforward_polynomials=[[0o7, 0o5]])
    BER = []
    param = []
    p = multiprocessing.dummy.Pool(len(SNR))
    for i in range(0,len(SNR)):
        temp = sim_param(tx_bin, SNR[i], modulation, code, tblen, decision_method)
        param.append(temp)
    BER = p.map(CCODE_simulation, param)
    p.close()
    p.join()
    plt.figure()
    plt.title("Comparison between Ccode with soft and hard decision")
    plt.yscale('log')
    plt.plot(SNR, BER, label = '(0o7,0o5) ccode with hard decision')
    
    # soft decision simulation
    decision_method = "soft"
    code = komm.ConvolutionalCode(feedforward_polynomials=[[0o7, 0o5]])
    BER = []
    param = []
    p = multiprocessing.dummy.Pool(len(SNR))
    for i in range(0,len(SNR)):
        temp = sim_param(tx_bin, SNR[i], modulation, code, tblen, decision_method)
        param.append(temp)
    BER = p.map(CCODE_simulation, param)
    p.close()
    p.join()
    plt.plot(SNR, BER, label = '(0o7,0o5) ccode with soft decision')
    plt.legend()
    plt.xlabel("SNR")
    plt.ylabel("BER")

    plt.figure()
    plt.yscale('log')
    plt.title("Comparison among different Ccode with soft decision")
    plt.plot(SNR, BER, label = '(0o7,0o5) ccode')
    # (0o155,0o117) simulation
    decision_method = "soft"
    code = komm.ConvolutionalCode(feedforward_polynomials=[[0o155, 0o117]])
    BER = []
    param = []
    p = multiprocessing.dummy.Pool(len(SNR))
    for i in range(0,len(SNR)):
        temp = sim_param(tx_bin, SNR[i], modulation, code, tblen, decision_method)
        param.append(temp)
    BER = p.map(CCODE_simulation, param)
    p.close()
    p.join()
    plt.plot(SNR, BER, label = '(0o155,0o117) ccode')

    # (0o155,0o117,0o127) simulation
    decision_method = "soft"
    code = komm.ConvolutionalCode(feedforward_polynomials=[[0o155, 0o117, 0o127]])
    BER = []
    param = []
    p = multiprocessing.dummy.Pool(len(SNR))
    for i in range(0,len(SNR)):
        temp = sim_param(tx_bin, SNR[i], modulation, code, tblen, decision_method)
        param.append(temp)
    BER = p.map(CCODE_simulation, param)
    p.close()
    p.join()
    plt.plot(SNR, BER, label = '(0o155,0o117,0o127) ccode')
    plt.xlabel("SNR")
    plt.ylabel("BER")
    plt.legend()
    plt.show()





if __name__ == "__main__":
    main()