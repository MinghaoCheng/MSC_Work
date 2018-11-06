from ecg_filter import fir_lfilter
from ecg_filter import fir_filter
import matplotlib.pyplot as plt
import numpy as np

class matched_filter:
    def __init__(self, template):
        self.__coefficient = template[::-1]
        self.lfilter = fir_lfilter(self.__coefficient)

    def filter(self, data_in):
        output = self.lfilter.fir_c_lib.dofilter(data_in)
        output = output ** 2
        return output

    
class heart_rate_detector:
    def __init__(self, samping_rate):
        self.__fs = samping_rate
        self.__buffer = 0
        self.__counter = 0
        self.__hr_buffer = 0

    def calculate(self, data_in):
        if (data_in == 1 and self.__buffer == 0):
            output =  60 * self.__fs / self.__counter
            self.__counter = 0
            if(output < 40 or output > 200):
                output = self.__hr_buffer
            else:
                self.__hr_buffer = output
        else:
            output = self.__hr_buffer
        self.__counter += 1
        self.__buffer = data_in
        return output
        

def main():
    fs = 1000

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
    fir = fir_filter(sampling_frequency = fs, cutoff_frequency = [1,45,55], pass_zero = False, window = 'hamming')

    ch0_filtered = []
    for i in range(len(ch0)):
        ch0_filtered.append(fir.lfilter.fir_c_lib.dofilter(ch0[i]))
    fir.lfilter.fir_c_lib.reset_all()

    # implement matched filter and heart rate decector
    matched = matched_filter(template)
    hr_calculator = heart_rate_detector(fs)

    ch0_matched = []
    ch0_threshold = []
    heart_rate = []

    # filter the data and calculate heart rate
    for i in range (len(ch0)):
        temp = matched.filter(ch0_filtered[i])
        ch0_matched.append(temp)
        if temp >= 0.7:
            temp = 1
        else:
            temp = 0
        ch0_threshold.append(temp)
        heart_rate.append(hr_calculator.calculate(temp))

    matched.lfilter.fir_c_lib.reset_all()

    plt.figure(1)
    plt.plot(time, ch0_filtered)
    plt.xlabel("time s")
    plt.ylabel("magnitude")
    plt.figure(2)
    plt.plot(time, ch0_matched)
    plt.xlabel("time s")
    plt.ylabel("magnitude")
    plt.figure(3)
    plt.plot(time, ch0_threshold)
    plt.xlabel("time s")
    plt.ylabel("magnitude")
    plt.figure(4)
    plt.plot(time, heart_rate)
    plt.xlabel("time s")
    plt.ylabel("heart rate bpm")
    plt.show()

if __name__ == "__main__":
    main()