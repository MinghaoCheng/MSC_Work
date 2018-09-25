import numpy as np
import matplotlib.pyplot as pl
class FM:

    def modulation(self, sampling_period, simulation_time, Fm, Fc, Mi):
        """
        FM modulation function
        """
        St = np.arange(0, simulation_time, sampling_period)
        Sout = np.zeros(int(simulation_time / sampling_period), dtype = float)
        for i in range(int(simulation_time / sampling_period)):
            Sout[i] = np.sin(2 * np.pi * Fc * St[i] + Mi * np.sin(2 * np.pi * Fm * St[i] - np.pi / 2))
        return St, Sout

def main():
    fm = FM()
    St, Sout = fm.modulation(0.0001, 1, 2, 200, 80)
    pl.plot(St, Sout)
    pl.show()


if __name__ == "__main__":
    main()