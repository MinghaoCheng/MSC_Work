import komm
import numpy as np
import matplotlib.pyplot as plt
from condenated_simulation import *


def main():
    # param specification
    SNR = 0
    modulation = komm.PSKModulation(4)
    tx_bin = np.random.randint(2, size = 1200)
    decision_method = "soft"
    param = []
    
    # Convolution param (0o7,0o5)
    tblen = 18
    C_code = komm.ConvolutionalCode(feedforward_polynomials=[[0o7, 0o5]])
    BCH_code = komm.BCHCode(3, 1)
    param.append(con_param(tx_bin, SNR, modulation, BCH_code, C_code, tblen, decision_method))
    BCH_code = komm.BCHCode(4, 3)
    param.append(con_param(tx_bin, SNR, modulation, BCH_code, C_code, tblen, decision_method))
    BCH_code = komm.BCHCode(5, 7)
    param.append(con_param(tx_bin, SNR, modulation, BCH_code, C_code, tblen, decision_method))
    BCH_code = komm.BCHCode(6, 13)
    param.append(con_param(tx_bin, SNR, modulation, BCH_code, C_code, tblen, decision_method))

    # Convolution param (0o155,0o117)
    tblen = 36
    C_code = komm.ConvolutionalCode(feedforward_polynomials=[[0o155, 0o117]])
    BCH_code = komm.BCHCode(3, 1)
    param.append(con_param(tx_bin, SNR, modulation, BCH_code, C_code, tblen, decision_method))
    BCH_code = komm.BCHCode(4, 3)
    param.append(con_param(tx_bin, SNR, modulation, BCH_code, C_code, tblen, decision_method))
    BCH_code = komm.BCHCode(5, 7)
    param.append(con_param(tx_bin, SNR, modulation, BCH_code, C_code, tblen, decision_method))
    BCH_code = komm.BCHCode(6, 13)
    param.append(con_param(tx_bin, SNR, modulation, BCH_code, C_code, tblen, decision_method))

    # Convolution param (0o155,0o117,0o127)
    tblen = 36
    C_code = komm.ConvolutionalCode(feedforward_polynomials=[[0o155, 0o117, 0o127]])
    BCH_code = komm.BCHCode(3, 1)
    param.append(con_param(tx_bin, SNR, modulation, BCH_code, C_code, tblen, decision_method))
    BCH_code = komm.BCHCode(4, 3)
    param.append(con_param(tx_bin, SNR, modulation, BCH_code, C_code, tblen, decision_method))
    BCH_code = komm.BCHCode(5, 7)
    param.append(con_param(tx_bin, SNR, modulation, BCH_code, C_code, tblen, decision_method))
    BCH_code = komm.BCHCode(6, 13)
    param.append(con_param(tx_bin, SNR, modulation, BCH_code, C_code, tblen, decision_method))


    BER = []
    for i in range (0, len(param), 4):
        temp = [format(sim_CON(param[i]),'.4f'),
                format(sim_CON(param[i+1]),'.4f'),
                format(sim_CON(param[i+2]), '.4f'),
                format(sim_CON(param[i+3]), '.4f')]
        BER.append(temp)

    columns = ['(7,4)BCH','(15,5)BCH','(31,6)BCH','(63,10)BCH']
    rows = ['(0o7,0o5)','(0o155,0o117)','(0o155,0o117,0o127)']

    plt.table(cellText = BER, rowLabels = rows, colLabels = columns, loc='center')
    plt.axis('tight')
    plt.axis('off')
    plt.title("Comparison among different combination of BCH code and Conv code")
    plt.show()









if __name__ == "__main__":
    main()