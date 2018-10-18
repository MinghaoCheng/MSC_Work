import pyaudio
import numpy as np
import scipy.io.wavfile as wav_io
import struct
import matplotlib.pyplot as plt

"""
    define parameters of the whole system
"""
SamplingRate = 44100
Channel = 1
DataFormat = pyaudio.paInt16
Chunk = 1024
Output_file_name = "hello1.wav"
Data_max_value = 32768
threshold = 0.35 * Data_max_value

"""
    main function
    entrance of the program
    this program is used for model data recording
"""
def main():

    # raw_data is used to store the raw data from pyaudio input voice stream, and shorts is used to store the unpacked voice data (in int16)
    raw_data = []
    shorts = []
    format = "%dh"%(Chunk)
    # open audiopy input audio stream
    audio_handler = pyaudio.PyAudio()
    stream = audio_handler.open(format = DataFormat,
                    channels = Channel,
                    rate = SamplingRate,
                    input = True,
                    )

    print("please say a word")
    # infinite loop
    while True:
        th_data = struct.unpack(format, stream.read(Chunk))
        # if volume of the sound is greater than threshold, the while loop will record it until the it is less than threshold
        if(max(th_data) > threshold):
            print("start recording")
            chunks_remaining = 2
            while(chunks_remaining >= 0):
                raw_data.append(stream.read(Chunk))
                shorts.append(struct.unpack(format, raw_data[len(raw_data) - 1]))
                chunks_remaining -= 1
                if(max(shorts[len(shorts) - 1]) > threshold):
                    chunks_remaining += 2
            # the volume is less than threshold, recording done, break the infinite while loop
            break
    print("recording done")
    # recording done, close the stream
    stream.stop_stream()
    stream.close()
    audio_handler.terminate()

    wave = np.array(shorts, dtype = np.int16)
    wave = wave.flatten()

    # write the sound data into wav file
    wav_io.write(Output_file_name, SamplingRate, wave)

    print("done")

if __name__ == "__main__":
    main()
