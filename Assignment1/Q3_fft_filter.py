import numpy as np
import matplotlib.pyplot as plt
import scipy.io.wavfile as wav_io

"""
    define parameters of the whole system
"""
SamplingRate = 44100
Data_max_value = 32768

file_name = "Q3_sample.wav"
Output_file_name = "Q3_output.wav"

Start_frequency = 1000
Stop_frequency = 2000
Gain = 0
"""
    define a filter using fft&ifft to gain (could be >1 or <1) the certain frequency component
"""
def filter(data_in, sampling_rate, start_freq, stop_freq, gain):
    
    # buffer the input data
    td_sig = np.array(data_in)

    # do the fft to get frequency domain data
    fft = np.fft.fft(td_sig)

    # allocate the frequency array
    freq_array = np.linspace(0 , sampling_rate >> 1, len(fft) >> 1)

    # mirror the frequency array
    # for i in range(len(freq_array)):
    #     freq_array = np.append(freq_array, freq_array[(len(fft) >> 1) - i - 1])

    # gain the data with specified frequency
    for i in range(len(freq_array)):
        if(freq_array[i]>start_freq and freq_array[i]<stop_freq):
            fft[i] *= gain
            fft[len(fft) - i - 1] *= gain
    
    # do the ifft of filterd data
    ifft = np.fft.ifft(fft)
    
    return ifft

"""
    main function
    entrance of the program
"""
def main():

    # read the sample
    fs, data_in = wav_io.read(file_name)
    data_in = np.array(data_in)
    
    # shift the data to all posetive
    data_in = data_in + Data_max_value

    # plot the spectrum of original sample
    sp_in = abs(np.fft.fft(data_in))[0:len(data_in)>>1] * 2 / len(data_in) / Data_max_value
        # drop dc
    sp_in[0] = 0

    freq_arr = np.linspace(0, SamplingRate / 2, len(sp_in))
    plt.subplot(2, 1, 1)
    plt.plot(freq_arr, sp_in)

    # apply filter
    data_out = np.real(filter(data_in, SamplingRate, Start_frequency, Stop_frequency, Gain))

    # plot the spectrum of filtered sample
    sp_out = abs(np.fft.fft(data_out))[0:len(data_out)>>1] * 2 / len(data_out) / Data_max_value 
        # drop dc
    sp_out[0] = 0
    plt.subplot(2, 1, 2)
    plt.plot(freq_arr, sp_out)

    data_out = data_out - Data_max_value
    data_out = np.array(data_out, dtype = np.int16)

    # save the file
    wav_io.write(Output_file_name, SamplingRate, data_out)

    print("done!")

    plt.show()

if __name__ == "__main__":
    main()