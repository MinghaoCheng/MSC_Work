function modulated_sig = IQ_Modulate(input_sig,i_carrier,q_carrier,samples_per_symbol)
    % parameters
    len = length(input_sig) / 2;
    % convert 0 to -1
    stream = input_sig;
    for i=1:length(stream)
        if(stream(i) == 0)
            stream(i) = -1;
        end
    end
    % reshape array
    sig = zeros(len,2);
    for i=1:len
        sig(i,1) = stream(i*2-1);
        sig(i,2) = stream(i*2);
    end
    % modulate
    i_modulated = i_carrier;
    q_modulated = q_carrier;
    for i=1:length(sig)
        for j=(i-1)*samples_per_symbol + 1:i*samples_per_symbol
            i_modulated(j) = i_modulated(j)*sig(i,1);
            q_modulated(j) = q_modulated(j)*sig(i,2);
        end
    end
    modulated_sig = i_modulated + q_modulated;
end

