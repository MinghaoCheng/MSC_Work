import numpy as np
import matplotlib.pyplot as plt
import komm
import BCH_simulation
import convolution_simulation
import ARQ_simulation


def main():
    Result = []
    SNR = 3
    modulation = komm.PSKModulation(4)
    tx_bin = np.random.randint(2, size = 1200)

    # convolutional code
    Ccode = komm.ConvolutionalCode(feedforward_polynomials=[[0o7, 0o5]])
    tblen = 18
    method = "soft"
    Cparam = convolution_simulation.sim_param(tx_bin, SNR, modulation, Ccode, tblen, method)
    BER = format(convolution_simulation.CCODE_simulation(Cparam), '.4f')
    Result.append([BER,'1/2'])

    Ccode = komm.ConvolutionalCode(feedforward_polynomials=[[0o155, 0o117]])
    tblen = 36
    method = "soft"
    Cparam = convolution_simulation.sim_param(tx_bin, SNR, modulation, Ccode, tblen, method)
    BER = format(convolution_simulation.CCODE_simulation(Cparam), '.4f')
    Result.append([BER,'1/2'])

    Ccode = komm.ConvolutionalCode(feedforward_polynomials=[[0o155, 0o117, 0o127]])
    tblen = 36
    method = "soft"
    Cparam = convolution_simulation.sim_param(tx_bin, SNR, modulation, Ccode, tblen, method)
    BER = format(convolution_simulation.CCODE_simulation(Cparam), '.4f')
    Result.append([BER,'1/3'])

    # BCH code
    BCH_encoder = komm.BCHCode(3, 1)
    BCH_param = BCH_simulation.sim_param(tx_bin, SNR, modulation, BCH_encoder)
    BER = format(BCH_simulation.sim_BCH(BCH_param), '.4f')
    Result.append([BER,'4/7'])

    BCH_encoder = komm.BCHCode(4, 3)
    BCH_param = BCH_simulation.sim_param(tx_bin, SNR, modulation, BCH_encoder)
    BER = format(BCH_simulation.sim_BCH(BCH_param), '.4f')
    Result.append([BER,'5/15'])

    BCH_encoder = komm.BCHCode(5, 7)
    BCH_param = BCH_simulation.sim_param(tx_bin, SNR, modulation, BCH_encoder)
    BER = format(BCH_simulation.sim_BCH(BCH_param), '.4f')
    Result.append([BER,'6/31'])

    BCH_encoder = komm.BCHCode(6, 13)
    BCH_param = BCH_simulation.sim_param(tx_bin, SNR, modulation, BCH_encoder)
    BER = format(BCH_simulation.sim_BCH(BCH_param), '.4f')
    Result.append([BER,'10/63'])

    # ARQ
    BER = format(ARQ_simulation.ARQ_simulation(tx_bin, modulation, SNR), '.4f')
    Result.append([BER, '7/8'])


    columns = ['BER','Code rate']
    rows = ['(0o7,0o5)','(0o155,0o117)','(0o155,0o117,0o127)','(7,4)BCH','(15,5)BCH','(31,6)BCH','(63,10)BCH','ARQ']

    plt.table(cellText = Result, rowLabels = rows, colLabels = columns, loc='center')
    plt.axis('tight')
    plt.axis('off')
    plt.title("Comparison among different FEC code")
    plt.show()


if __name__ == "__main__":
    main()