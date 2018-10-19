function [modulated_sig] = BPSK_Modulate(data_in,carrier_in,carrier_frequency,cycles_per_symbol,sampling_rate)

    modulated_sig = [];
    samples_per_symbol = 1/carrier_frequency*sampling_rate*cycles_per_symbol;
    
    
    for i=0:length(data_in)-1
        temp = [];
        for j = i*samples_per_symbol+1 : (i+1)*samples_per_symbol
            temp = [temp data_in(i+1)*carrier_in(j)];
        end
        modulated_sig = [modulated_sig temp];
    end

end
