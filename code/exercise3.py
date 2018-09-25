import matplotlib
import matplotlib.pyplot as pl
import numpy

Fm = 2                                                                              #base signal frequency
Fc = 100                                                                            #carrier frequency
Mi = 40                                                                             #Modulation index

sampling_period = 0.0001
simulation_time = 1

Sm = numpy.zeros(int(simulation_time / sampling_period), dtype=float)               #base signal sequence
Sc = numpy.zeros(int(simulation_time / sampling_period), dtype=float)               #carrier sequence
Sout = numpy.zeros(int(simulation_time / sampling_period), dtype=float)             #output signal sequence
St = numpy.arange(0,simulation_time,sampling_period, dtype=float)                   #time sequence

for i in range(int(simulation_time / sampling_period)):
    Sm[i] = numpy.sin(2 * numpy.pi * Fm * St[i])
    Sc[i] = numpy.sin(2 * numpy.pi * Fc * St[i])

# sin(2*pi*Fc*t + Mi*sin(2*pi*Fm*t))
for i in range(int(simulation_time / sampling_period)):
    Sout[i] = numpy.sin(2 * numpy.pi * Fc * St[i] + Mi * numpy.sin(2 * numpy.pi * Fm * St[i] - numpy.pi / 2))

# fourier = numpy.fft.fft(Sm)
# dm = numpy.fft.fftfreq(Sm.size, d=sampling_period)

pl.figure(1)
pl.subplot(311)
pl.plot(St, Sm)
pl.subplot(312)
pl.plot(St, Sc)
pl.subplot(313)
pl.plot(St, Sout)

# pl.figure(2)
# pl.plot(St,dm)

pl.show()
