from ecg_filter import fir_lfilter
from ecg_filter import fir_filter
import matplotlib.pyplot as plt
import numpy as np

class matched_filter:
    def __init__(self, template):
        self.__coefficient = template[::-1]
        self.lfilter = fir_lfilter(self.__coefficient)

def main():
    # load template
    template_file = open("template", mode = 'r')
    for line in template_file:
        temp = line.split()
    template = []
    for i in range (len(temp) - 1):
        template.append(float(temp[i]))
    
    # load ecg
    ecg = open("2359434c.dat", mode = 'r')

    time = []
    ch0 = []
    ch1 = []
    ch2 = []
 
    for line in (ecg):
        temp = line.split(' ')
        time.append(np.float32(temp[0]))
        ch0.append(np.float32(temp[1]))
        ch1.append(np.float32(temp[2]))
        ch2.append(np.float32(temp[3]))

    time = np.array(time)
    ch0 = np.array(ch0)
    ch1 = np.array(ch1)
    ch2 = np.array(ch2)
    ecg.close()

    # filter the original data
    fir = fir_filter(sampling_frequency = 1000, cutoff_frequency = [1,45,55], pass_zero = False, window = 'hamming')

    ch0_filtered = []
    for i in range(len(ch0)):
        ch0_filtered.append(fir.lfilter.fir_c_lib.dofilter(ch0[i]))
    fir.lfilter.fir_c_lib.reset_all()
    
    # implement matched filter
    matched = matched_filter(template)
    matched_ch0 = []

    # filter the data
    for i in range (len(ch0)):
        matched_ch0.append(matched.lfilter.fir_c_lib.dofilter(ch0_filtered[i]))
    matched.lfilter.fir_c_lib.reset_buffer()

    # square the data
    matched_ch0 = np.array(matched_ch0)
    for i in range (len(matched_ch0)):
        matched_ch0[i] = matched_ch0[i] ** 2

    plt.figure(1)
    plt.plot(time, matched_ch0)

    # apply threshold
    for i in range (len(matched_ch0)):
        if matched_ch0[i] >= 0.7:
            matched_ch0[i] = 1
        else:
            matched_ch0[i] = 0
    plt.figure(2)
    plt.plot(time, matched_ch0)

    # generate heart rate
    heart_rate = np.zeros(len(time))
    p = 0
    for i in range (1, len(matched_ch0)):
        if (matched_ch0[i] == 1 and matched_ch0[i - 1] == 0):
            if(p != 0):
                heart_rate[i] =  60000 / (i - p)
            p = i
            if(heart_rate[i] < 40 and heart_rate[i] >200):
                heart_rate[i] = heart_rate[i - 1]
        else:
            heart_rate[i] =  heart_rate[i - 1]
        
    # plot time vs heart rate
    plt.figure(3)
    plt.plot(time, heart_rate)
    plt.show()

if __name__ == "__main__":
    main()