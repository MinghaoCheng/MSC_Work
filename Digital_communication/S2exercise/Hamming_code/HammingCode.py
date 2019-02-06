import numpy as np

class hamming_encoder:
    def __init__(self,r):

        # k => block length
        # n => message length
        k = np.power(2,r) - 1
        n = k - r

        # find the index of data bits and parity bits
        Parity_index = np.zeros(r,dtype = np.uint8)
        Data_index = np.zeros(n,dtype = np.uint8)
        p = []
        for i in range(0,r):
            p.append(1<<i)

        ii_d = 0
        ii_p = 0
        for i in range(1,k+1):
            if (i in p):
                Parity_index[ii_p] = i
                ii_p += 1
            else:
                Data_index[ii_d] = i
                ii_d += 1

        # bits to calculate the parity 
        ip = []
        for i in range(0,r):
            ip.append([1<<i])
            for j in range(1,k + 1):
                if (j & ip[i][0] == ip[i][0]) and (j != ip[i][0]):
                    ip[i].append(j)
        ip = np.matrix(ip,dtype = np.uint8)

        # check matrice
        self.H = np.zeros((r,k),dtype = np.uint8)
        for i in range (0,r):
            for j in range(0,ip[i].size):
                self.H[i,ip[i,j]-1] = 1

        # generate matrice
        self.G = np.zeros((n,k),dtype = np.uint8)
        for j in range(0,k):
            ii_p = self.where(j+1,Parity_index)
            if(ii_p != -1): # this is a parity bit
                for i in range (0,ip[0].size):
                    ii_d = self.where(ip[ii_p][0,i], Data_index)
                    if (ii_d != -1):
                        self.G[ii_d,j] = 1
            else: # this is a data bit
                ii_d = self.where(j+1,Data_index)
                self.G[ii_d,j] = 1
        self.n = n
        self.k = k
        self.r = r
        self.Data_index = Data_index

    def where(self,x,A):
        A = np.array(A).flatten()
        for i in range (0,len(np.array(A))):
            if A[i] == x:
                return i
        
        return -1

    def encode(self,data):
        return np.array(data*self.G % 2).flatten()

    def decode(self,word):
        temp = self.correct(word)
        data = []
        for i in range(self.n):
            data.append(temp[self.Data_index[i]-1])
        return data


    def check(self,word):
        return word * np.matrix.transpose(self.H) % 2
    def correct(self,word):
        temp = word
        p = np.matrix(word) * np.matrix.transpose(self.H) % 2
        t = 0
        for i in range(0,self.r):
            t += p[0,i] << i

        if(temp[t-1] == 0):
            temp[t-1] = 1
        else:
            temp[t-1] = 0
        return temp
    


def main():
    Encoder = hamming_encoder(4)
    data = np.matrix([1,0,1,0,1,0,1,0,1,0,0])
    word = Encoder.encode(data)
    word[4] = 1
    word = Encoder.decode(word)

    print()







if __name__ == "__main__":
    main()