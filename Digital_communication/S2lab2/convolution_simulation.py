import komm
import numpy as np
from BER_cal import BER_cal

class sim_param:
    def __init__(self, TX_BIN, SNR, MODULATION, CODE, TBLEN, METHOD):
        self.tx_bin = TX_BIN
        self.snr = SNR
        self.modulation = MODULATION
        self.encoder = komm.ConvolutionalStreamEncoder(CODE)
        self.decoder = komm.ConvolutionalStreamDecoder(CODE, traceback_length=TBLEN, input_type=METHOD)
        self.tblen = TBLEN
        self.method = METHOD

def CCODE_simulation(param):
    return CCODE_snr_ber_simulation(param.tx_bin, param.snr, param.modulation, param.encoder, param.decoder, param.tblen, param.method)

def CCODE_snr_ber_simulation(tx_bin, snr, modulation, encoder, decoder, tblen, method):
    # encode
    tx_enc = np.uint8(encoder(np.append(tx_bin,np.zeros(tblen))))
    # modulate
    tx_data = modulation.modulate(tx_enc)

    # transaction
    awgn = komm.AWGNChannel(snr=10**(snr/10.))
    rx_data = awgn(tx_data)

    # demodulate
    rx_enc = modulation.demodulate(rx_data, decision_method = method)

    # decode
    rx_bin = np.uint8(decoder(rx_enc))[tblen:]

    return BER_cal(tx_bin, rx_bin)