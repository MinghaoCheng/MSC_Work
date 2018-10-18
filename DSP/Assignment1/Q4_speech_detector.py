import pyaudio
import numpy as np
import scipy.io.wavfile as wav_io
import matplotlib.pyplot as plt
import struct

"""
    define a class of speech detector
"""
class speech_detector:
    
    """
        initialise function
    """
    def __init__(self):

        # specify the parameters
        self.SamplingRate = 44100
        self.Channel = 1
        self.DataFormat = pyaudio.paInt16
        self.DataMax = 32768
        self.Samples_per_frame = 1024           # 23.21995ms per frame, (1024 / 44100), to make the fft faster, also known as the length of the window

        # allocate frequency array
        self.fft_frequency = np.linspace(0, self.SamplingRate >> 1, self.Samples_per_frame >> 1)

        # state all the sample data arrays and model data arrays
        self.sample = []
        self.sample_frames = []
        self.model_data = []
        self.model_data_frames = []
        self.model_data_frames_sp = []
        self.model_names = []

        # load the model voice data
        self.load_model()
    
    """
        load the model voice data
    """
    def load_model(self):

        # use scipy wav io to read the two models ("hello" and "action"), and append the names to the model_names array
        fs, temp = wav_io.read("hello.wav")
        self.model_names.append("hello")
        self.model_data.append(temp + self.DataMax)
        self.model_data_frames.append([])
        self.model_data_frames_sp.append([])

        fs, temp = wav_io.read("action.wav")
        self.model_names.append("action")
        self.model_data.append(temp + self.DataMax)
        self.model_data_frames.append([])
        self.model_data_frames_sp.append([])

        # apply window to the model data, save the frames into model_data_frames array
        for i in range(len(self.model_data)):
            num_of_frames = int(len(self.model_data[i]) / self.Samples_per_frame)
            temp = []
            j = 0
            while j <= (num_of_frames - 1):
                temp = self.model_data[i][j * (self.Samples_per_frame ) : (j + 1) * (self.Samples_per_frame)]
                self.model_data_frames[i].append(np.array(temp))
                self.model_data_frames_sp[i].append([])
                j += 1

        # calculate fft of model i, frame j and save the result into model_data_frames_sp array
        for i in range(len(self.model_data_frames)):
            for j in range(len(self.model_data_frames[i])):
                    temp = abs(np.fft.fft(self.model_data_frames[i][j]))[0 : self.Samples_per_frame >> 1] / self.Samples_per_frame / self.DataMax
                    self.model_data_frames_sp[i][j] = np.append(self.model_data_frames_sp[i][j], temp)
    
    """
        apply rectangular window to the sample data, and return the frames
    """
    def apply_window(self, data_in):
        temp = []
        # rectangular window
        num_of_frames = int(len(data_in) / self.Samples_per_frame)
        i = 0
        while i <= (num_of_frames - 1):
            temp.append(data_in[i * (self.Samples_per_frame ) : (i + 1) * (self.Samples_per_frame)])
            i += 1
        return temp

    """
        compare the data with model frame by frame in terms of spectrum
    """
    def compare_with_model(self, sample, model):
        # since the length of the sample and the model might be different, here the length of the shorter one is used
        length = min(len(sample), len(model))
        # calculate the difference between sample and model and return the sum of the abs difference
        difference = 0
        for i in range (length):
            model_frame = model[i]
            sp_sample_frame = abs(np.fft.fft(sample[i]))[0 : self.Samples_per_frame >> 1] / self.Samples_per_frame / self.DataMax
            for j in range (self.Samples_per_frame >> 1):
                difference += abs(sp_sample_frame[j] - model_frame[j])

        return difference
    
    """
        detect the voice by calculating the difference with model, the less the difference, the higher likelyhood the voice is the word
    """
    def detect(self, data_input):
        self.sample = data_input + self.DataMax
        self.sample_frames = self.apply_window(self.sample)
        score = []
        for i in range(len(self.model_data_frames_sp)):
            score.append(self.compare_with_model(self.sample_frames, self.model_data_frames_sp[i]))
            # print(score)
        # the data has been compared with two model, find the one with smallest difference and return the name of the model
        min_score = min(score)

        for i in range(len(score)):
            if(score[i] == min_score):
                print(self.model_names[i])

"""
    main function
    entrance of the program
"""
def main():

    # implement speech detector and threshold
    vd = speech_detector()
    threshold = 0.35 * vd.DataMax

    # raw_data is used to store the raw data from pyaudio input audio stream, and shorts is used to store the unpacked voice data (in int16)
    raw_data = []
    shorts = []
    format = "%dh"%(vd.Samples_per_frame)

    # open the audio stream
    audio_handler = pyaudio.PyAudio()
    stream = audio_handler.open(format = vd.DataFormat,
                    channels = vd.Channel,
                    rate = vd.SamplingRate,
                    input = True,
                    )

    print("please say a word")
    # infinite loop
    while True:
        th_data = struct.unpack(format, stream.read(vd.Samples_per_frame))
        # if the volume of the sound is greater than threshold, the while loop will record it until it is less than threshold
        if(max(th_data) > threshold):
            raw_data = []
            shorts = []
            # print("start recording")
            # read 2 chunks and see if the voice is still greater than threshold
            chunks_remaining = 2
            while(chunks_remaining >= 0):
                raw_data.append(stream.read(vd.Samples_per_frame))
                shorts.append(struct.unpack(format, raw_data[len(raw_data) - 1]))
                chunks_remaining -= 1
                if(max(shorts[len(shorts) - 1]) > threshold):
                    chunks_remaining += 2
            # print("recording done")
            # the volume is less than threshold, which indicates the speech is over
            wave = np.array(shorts, dtype = np.int16).flatten()
            # see if the length of the sound is greater than 0.2s, if not, it is a short noise, otherwise, hand the data over to voice detector
            if(len(wave) >= 0.2 * vd.SamplingRate):
                vd.detect(wave)

if __name__ == "__main__":
    main()
