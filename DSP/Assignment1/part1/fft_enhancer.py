import numpy as np
import matplotlib.pyplot as plt
import scipy.io.wavfile as wav_io

class fft_enhancer:

    #private functions:
    def __init__(self, sampling_rate, dtype, original_file_name, output_file_name, start_frequency, stop_frequency, gain, show_plot):

        self.SamplingRate = sampling_rate
        self.dtype = dtype
        self.data_input = []
        self.data_output = []
        self.frequency_array = []
        self.original_file_name = original_file_name
        self.output_file_name = output_file_name
        self.start_frequency = start_frequency
        self.stop_frequency = stop_frequency
        self.gain = gain
        self.show_plot = show_plot

    def __load_original_wav(self):
        # read the sample
        fs, raw_data = wav_io.read(self.original_file_name)
        self.data_input = np.array(raw_data)

        if(self.show_plot):
            time_line = 1 / self.SamplingRate * np.arange(0, len(self.data_input), 1) * 1000
            plt.figure(1)
            plt.plot(time_line, self.data_input)
            plt.xlabel('Time (ms)')
            plt.ylabel('Amplitude')
            plt.title('Original data in time domain')

        # allocate frequency array, which would be the x axis of the spectrum plot
        self.frequency_array = np.linspace(0, self.SamplingRate >> 1, len(self.data_input) >> 1)

    def __enhance(self):
        # buffer the input data and add make it all posetive
        td_data_in = self.data_input

        # do the fft to get frequency domain data
        sp_data_in = np.fft.fft(td_data_in)
        
        # plot the spectrum if needed
        if(self.show_plot):
            spectrum_in = abs(sp_data_in)[0:len(td_data_in)>>1] * 2 / len(td_data_in)
            spectrum_in[0] = 0
            plt.figure(2)
            plt.semilogx(self.frequency_array, spectrum_in)
            plt.xlabel('Frequency (Hz)')
            plt.ylabel('Amplitude')
            plt.title('Original data in frequency domain')

        # gain the data with specified frequency
        for i in range(len(self.frequency_array)):
            if(self.frequency_array[i] > self.start_frequency and self.frequency_array[i] < self.stop_frequency):
                sp_data_in[i] *= self.gain
                sp_data_in[len(sp_data_in) - i] *= self.gain
        
        # do the ifft of filterd data
        self.data_output = np.real(np.fft.ifft(sp_data_in))
        self.data_output = np.array(self.data_output, dtype = np.int16)

        # plot the spectrum if needed
        if(self.show_plot):
            spectrum_enhanced = abs(np.fft.fft(self.data_output))[0:len(self.data_output)>>1] * 2 / len(self.data_output)
            spectrum_enhanced[0] = 0

            plt.figure(3)
            plt.semilogx(self.frequency_array, spectrum_enhanced)
            plt.xlabel('Frequency (Hz)')
            plt.ylabel('Amplitude')
            plt.title('Enhanced data in frequency domain')

    def __save_enhanced_data(self):
        wav_io.write(self.output_file_name, self.SamplingRate, self.data_output)

    # public functions:
    def process(self):
        self.__load_original_wav()
        self.__enhance()
        self.__save_enhanced_data()

def main():

    # specify parameters
    sampling_rate = 44100
    dtype = np.int16
    input_file_name = "original.wav"
    output_file_name = "improved.wav"
    start_freq = 2000
    stop_freq = 4000
    gain = 3
    show_plot = True
    
    # do the enhancement
    enhancer = fft_enhancer(sampling_rate, dtype, input_file_name, output_file_name, start_freq, stop_freq, gain, show_plot)
    enhancer.process()

    print("enhance done")
    plt.show()


if __name__ == "__main__":
    main()