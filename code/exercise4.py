import math
import matplotlib
import matplotlib.pyplot as pl
import numpy

Fm = 2      #base frequency
Fc = 20     #carrier frequency

number_of_samples = 1000

Swm = []    #base radian sequence
Swc = []    #carrier radian sequence

Sm = []     #base sequence
Sc = []     #carrier sequence
St = []     #time sequence
Sout = []   #output sequence

number_of_samples_fm = number_of_samples / Fm
number_of_samples_fc = number_of_samples / Fc

Swm = numpy.empty([number_of_samples])
Swc = numpy.empty([number_of_samples]) 
Sm = numpy.empty([number_of_samples])
Sc = numpy.empty([number_of_samples])
St = numpy.empty([number_of_samples])
Sout = numpy.empty([number_of_samples])


step = 2 * math.pi / (number_of_samples_fm)
for i in range(Fm):
    for j in range(int(number_of_samples_fm)):
        Swm[j + int(i*number_of_samples_fm)] = j * step

step = 2 * math.pi / (number_of_samples_fc)
for i in range(Fc):
    for j in range(int(number_of_samples_fc)):
        Swc[j + int(i*number_of_samples_fc)] = j * step

St = numpy.arange(0, 1, 1/number_of_samples)

for i in range(number_of_samples):
    Sm[i] = math.sin(Swm[i])
    Sc[i] = math.sin(Swc[i])

for i in range(number_of_samples):
    Sout[i] = math.cos(Swc[i] + (Fc - Fm) / Fm * math.sin(Swm[i]))

pl.figure(1)
pl.subplot(211)
pl.plot(St, Sc)
pl.subplot(212)
pl.plot(St, Sm)

pl.figure(2)
pl.plot(St, Sout)

pl.show()