import numpy as np
import matplotlib.pyplot as plt
import komm
from BER_cal import BER_cal

class Parity:
    def __init__(self, parity_format):
        if(parity_format == 'odd'):
            self.parity_format = 'odd'
        else:
            self.parity_format = 'even'

    def parity_cal(self, data):
        temp = np.sum(data) & 1
        if(self.parity_format == 'even'):
            return temp
        else:
            return not(temp)


def ARQ_simulation(tx_bin, modulator, snr):
    parity = Parity('odd')
    awgn = komm.AWGNChannel(snr=10**(snr/10.))

    # make the data a multiple of 7
    if(len(tx_bin)%7 != 0):
        tx_bin = np.int8(np.append(tx_bin, np.zeros(7 - len(tx_bin)%7)))

    # transmission
    rx_bin = []
    ARQ_counter = 0
    for i in range(0, len(tx_bin), 7):
        temp = tx_bin[i:i + 7]
        temp = np.append(temp, parity.parity_cal(temp))
        tx_data = modulator.modulate(temp)

        rx_data = awgn(tx_data)
        rx_byte = modulator.demodulate(rx_data)
        
        while(parity.parity_cal(rx_byte[0:7]) != rx_byte[7]):
            # parity check fail, need resend
            rx_data = awgn(tx_data)
            rx_byte = modulator.demodulate(rx_data)
            ARQ_counter += 1
        
        rx_bin = np.int8(np.append(rx_bin, rx_byte[0:7]))

    return BER_cal(tx_bin, rx_bin)