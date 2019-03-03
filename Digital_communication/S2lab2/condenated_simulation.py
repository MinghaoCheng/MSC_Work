import komm
import numpy as np
from multiprocessing.dummy import Pool as ThreadPool
from BER_cal import BER_cal

class con_param:
    def __init__(self, TX_BIN, SNR, MODULATION, BCH_CODE, CCODE, TBLEN, METHOD):
        self.tx_bin = TX_BIN
        self.snr = SNR
        self.modulation = MODULATION
        self.BCH_code = BCH_CODE
        self.C_encoder = komm.ConvolutionalStreamEncoder(CCODE)
        self.C_decoder = komm.ConvolutionalStreamDecoder(CCODE, traceback_length=TBLEN, input_type=METHOD)
        self.tblen = TBLEN
        self.method = METHOD

def sim_CON(param):
    return CON_snr_ber_simulation(param.tx_bin, param.snr, param.modulation, param.BCH_code, param.C_encoder, param.C_decoder, param.tblen, param.method)

def CON_snr_ber_simulation(tx_bin, snr, modulate, BCH_encoder, C_encoder, C_decoder, tblen, method):
    # BCH encode
    tx_bin_rearrange =  np.reshape(tx_bin, (int(len(tx_bin) / BCH_encoder.dimension), BCH_encoder.dimension))
    pool = ThreadPool(8) 
    tx_encoded_data = pool.map(BCH_encoder.encode, tx_bin_rearrange)
    tx_encoded_data = np.array(tx_encoded_data).flatten()
    pool.close()
    pool.join()
    
    # convolution encode
    tx_enc_c = np.uint8(C_encoder(np.append(tx_encoded_data,np.zeros(tblen))))

    # modulate
    tx_data = modulate.modulate(tx_enc_c)

    # transaction
    awgn = komm.AWGNChannel(snr=10**(snr/10.))
    rx_data = awgn(tx_data)

    # demodulate
    rx_enc_c = modulate.demodulate(rx_data, decision_method = method)


    # Convolution decode
    rx_encoded_data = C_decoder(rx_enc_c)[tblen:]

    # BCH decode
    rx_encoded_data = np.reshape(rx_encoded_data, (int(len(rx_encoded_data) / BCH_encoder.length), BCH_encoder.length))
    pool = ThreadPool(8)
    rx_bin = pool.map(BCH_encoder.decode, rx_encoded_data)
    pool.close()
    pool.join()
    rx_bin = np.array(rx_bin).flatten()
    
    return BER_cal(tx_bin, rx_bin)

