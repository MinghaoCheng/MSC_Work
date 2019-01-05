function demodulated_sig = IQ_Demodulate(input_sig,n_dummy_symbols,i_carrier,q_carrier,samples_per_symbol)
    % get i and q component
    i_mixer = input_sig .* i_carrier;
    q_mixer = input_sig .* q_carrier;
    % filtering
    b = fir1(7,0.1);
    i_filtered = filter(b,1,i_mixer);
    q_filtered = filter(b,1,q_mixer);
    % apply threshold
    i_compared = ones(1,length(i_filtered));
    q_compared = ones(1,length(q_filtered));
    
    for i=1:length(i_filtered)
        if(i_filtered(i) < 0)
            i_compared(i) = 0;
        end
        if(q_filtered(i) < 0)
            q_compared(i) = 0;
        end
    end
    % remove the reference carrier
    i_compared = i_compared(1,samples_per_symbol*n_dummy_symbols+1:length(i_compared));
    q_compared = q_compared(1,samples_per_symbol*n_dummy_symbols+1:length(q_compared));
    % goes back to bit stream
    i_demodulated_sig = ones(1,length(i_compared) / samples_per_symbol);
    q_demodulated_sig = ones(1,length(q_compared) / samples_per_symbol);
    for i=1:length(i_demodulated_sig) - 1
        if(i_compared((i - 1)*samples_per_symbol + samples_per_symbol/2) == 0)
            i_demodulated_sig(i) = 0;
        end
        if(q_compared((i - 1)*samples_per_symbol + samples_per_symbol/2) == 0)
            q_demodulated_sig(i) = 0;
        end
    end
    % combine i and q
    demodulated_sig = ones(1,2*length(i_demodulated_sig));
    for i=1:length(i_demodulated_sig)
        demodulated_sig(i*2-1) = i_demodulated_sig(i);
        demodulated_sig(i*2) = q_demodulated_sig(i);
    end

end

