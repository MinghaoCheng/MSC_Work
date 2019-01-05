function modulated_sig = BPSK_Modulate(input_sig,carrier,samples_per_symbol)
    % convert 0 to -1
    stream = input_sig;
    for i=1:length(stream)
        if(stream(i) == 0)
            stream(i) = -1;
        end
    end
    % modulation
    modulated_sig = carrier;
    for i=1:length(input_sig)
        for j=(i-1)*samples_per_symbol + 1:i*samples_per_symbol
            modulated_sig(j) = modulated_sig(j)*stream(i);
        end
    end
end
