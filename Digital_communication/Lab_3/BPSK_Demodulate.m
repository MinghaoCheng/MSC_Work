function [Out_bit_stream] = BPSK_Demodulate(data_in,carrier_in,carrier_frequency,cycles_per_symbol,sampling_rate)
    Out_bit_stream = []
    samples_per_symbol = 1/carrier_frequency*sampling_rate*cycles_per_symbol;

    % demodulate
    mixer = data_in.*carrier_in*2;
    b = fir1(7,0.01);
    lp_demod = filter(b,1,mixer);

    % comparing
    demodulated_data = []
    for i=1:length(lp_demod)
        if(lp_demod(i) > 0)
            demodulated_data = [demodulated_data 0];
        else
            demodulated_data = [demodulated_data 1];
        end
    end
    % go back to bit streams
    bit_stream_length = length(demodulated_data) / samples_per_symbol;
    for i=0:bit_stream_length-1
        if(demodulated_data(i*samples_per_symbol + samples_per_symbol/2) > 0)
            Out_bit_stream = [Out_bit_stream 1];
        else
            Out_bit_stream = [Out_bit_stream 0];
        end
    end

end

