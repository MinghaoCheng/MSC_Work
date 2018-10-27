import numpy as np
import matplotlib.pyplot as plt


def convolve(input1, input2):
    input1mir = np.zeros(len(input2))
    input1mir = np.append(input1, input1mir)
    output = np.zeros(len(input1) + len(input2))

    

    for i in range(len(input1) + len(input2)):
        for j in range(len(input2)):
            output[i] += input2[j] * input1mir[i - j - 1]

    return output



def main():
    t = np.linspace(0,1,100)
    x = np.sin(2*np.pi*1*t)

    g = np.zeros(300)

    g[100] = 1
    g[200] = 1

    h = np.convolve(x, g)
    plt.plot(h)
    plt.show()

if __name__ == "__main__":
    main()