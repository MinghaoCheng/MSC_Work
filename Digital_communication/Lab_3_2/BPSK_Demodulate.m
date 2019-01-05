function [demodulated_sig] = BPSK_Demodulate(input_sig,carrier,n_dummy_symbols,samples_per_symbol)
    mixer = input_sig .* carrier;
    b = fir1(7,0.1);
    filtered = filter(b,1,mixer);
    compared = ones(1,length(filtered));
    for i=1:length(filtered)
        if(filtered(i) < 0)
            compared(i) = 0;
        end
    end
    % remove dummy symbols
    compared = compared(1,n_dummy_symbols*samples_per_symbol+1:length(compared));
    demodulated_sig = ones(1,length(compared) / samples_per_symbol);
    for i=1:length(demodulated_sig) - 1
        if(compared((i - 1)*samples_per_symbol + samples_per_symbol/2) == 0)
            demodulated_sig(i) = 0;
        end
    end
    
end

