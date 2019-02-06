class lwz_encoder:
    def __init__(self):
        self.dict = ["&","a"]
        for i in range (1,26):
            self.dict.append(chr(ord(self.dict[i]) + 1))
    
    def insert_symbol(self, symbol):
        self.dict.append(symbol)
    
    def encode(self,sequence):
        result = []
        size = len(sequence)
        i = 0
        while (i < size):
            current_symb = sequence[i]
            next_symb = sequence[i+1]
            while (current_symb + next_symb in self.dict):
                i = i + 1
                current_symb = current_symb + next_symb
                if(i + 1 < size):
                    next_symb = sequence[i + 1]

            result.append(self.dict.index(current_symb))
            self.dict.append(current_symb + next_symb)
            i = i + 1
        return result
    
    def decode(self, sequence):
        result = []
        size = len(sequence)
        for i in range (size - 1):
            current_symb = self.dict[sequence[i]]
            next_symb = self.dict[sequence[i+1]][0]
            self.dict.append(current_symb + next_symb)
            result.append(current_symb)
        result.append(self.dict[sequence[size-1]])
        return result

