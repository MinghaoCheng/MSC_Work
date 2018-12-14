# implementation of direct form II bioquad filter
class IIR2Filter:
    def __init__(self, coefficients):
        self.__b0 = coefficients[0]
        self.__b1 = coefficients[1]
        self.__b2 = coefficients[2]
        self.__a0 = coefficients[3]
        self.__a1 = coefficients[4]
        self.__a2 = coefficients[5]
        self.__buffer0 = 0
        self.__buffer1 = 0

    def filter(self, x):
        # calculate the IIR part
        input_sum = x - self.__buffer0 * self.__a1 - self.__buffer1 * self.__a2
        # calculate the FIR part
        output_sum = input_sum * self.__b0 + self.__buffer0 * self.__b1 + self.__buffer1 * self.__b2

        # delay steps
        self.__buffer1 = self.__buffer0
        self.__buffer0 = input_sum

        return output_sum

class IIRFilter:
    def __init__(self, sos_input):
        self.__filter_chain = []
        for sos_row in sos_input:
            self.__filter_chain.append(IIR2Filter(sos_row))
        self.__length = len(self.__filter_chain)

    def filter(self, x):
        for i in range (0, self.__length):
            if (i == 0):
                output = self.__filter_chain[i].filter(x)
            else:
                output = self.__filter_chain[i].filter(output)
        
        return output
