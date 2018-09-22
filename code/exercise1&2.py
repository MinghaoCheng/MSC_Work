import math
import matplotlib
import matplotlib.pyplot as pl
import numpy

w=[]
out=[]
t=[]

number_of_samples = 8000
number_of_repeat = 5

step = 2 * math.pi / number_of_samples
w.append(0)
for i in range (number_of_samples):
    w.append(i * step)

for i in range (number_of_repeat):
    for j in range (number_of_samples): 
        out.append(math.sin(w[j]))

t=numpy.arange(0, number_of_samples*number_of_repeat ,1)

pl.plot(t, out)
pl.grid()
pl.show()