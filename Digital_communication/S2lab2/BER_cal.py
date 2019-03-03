import numpy as np

def BER_cal(tx_bin, rx_bin):

    total_bits = len(tx_bin)
    temp = tx_bin ^ rx_bin
    error_bits = np.sum(temp)
    BER = error_bits / total_bits

    return BER