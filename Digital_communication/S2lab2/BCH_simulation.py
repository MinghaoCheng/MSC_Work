import komm
import numpy as np
from BER_cal import BER_cal
from multiprocessing.dummy import Pool as ThreadPool

class sim_param:
    def __init__(self,TX_BIN,SNR,MODULATION,ENCODER):
        self.tx_bin = TX_BIN
        self.snr = SNR
        self.modulation = MODULATION
        self.encoder = ENCODER

def sim_BCH(param):
    return BCH_snr_ber_simulation(param.tx_bin, param.snr, param.modulation, param.encoder)

def BCH_snr_ber_simulation(tx_bin, snr, modulate, encoder):

    if (encoder != -1):
        # encode
        tx_bin_rearrange =  np.reshape(tx_bin, (int(len(tx_bin) / encoder.dimension),encoder.dimension))
        pool = ThreadPool(8) 
        tx_encoded_data = pool.map(encoder.encode, tx_bin_rearrange)
        pool.close()
        pool.join()
        # for i in range (0, len(tx_bin_rearrange)):
        #     tx_encoded_data.append(encoder.encode(tx_bin_rearrange[i]))
    else:
        tx_encoded_data = tx_bin
    
    # modulate
    tx_data = modulate.modulate(np.array(tx_encoded_data).flatten())

    # transaction
    awgn = komm.AWGNChannel(snr=10**(snr/10.))
    rx_data = awgn(tx_data)

    # demodulate
    rx_encoded_data = modulate.demodulate(rx_data)

    # decode
    if (encoder != -1):
        rx_encoded_data = np.reshape(rx_encoded_data, (int(len(rx_encoded_data) / encoder.length), encoder.length))
        pool = ThreadPool(8) 
        rx_bin = pool.map(encoder.decode, rx_encoded_data)
        pool.close()
        pool.join()
        # for j in range(0, len(rx_encoded_data)):
        #     rx_bit_stream.append(encoder.decode(rx_encoded_data[j]))
    else:
        rx_bin = rx_encoded_data

    rx_bin = np.array(rx_bin).flatten()

    BER = BER_cal(tx_bin, rx_bin)

    return BER