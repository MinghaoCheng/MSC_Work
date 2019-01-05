function payload = BPSK_Demodulate(input_sig,fc,syncword,samples_per_symbol)
    payload = 0;

    len = length(input_sig);
    sig_filtered_plot = zeros(1,len);
    b = fir1(51,fc);
    z = zeros(51,1);
    z_lock_detector = zeros(51,1);
    % VCO vars
    k = 0.005;
    v = 0;
    c = 1;
    s = 0;
    c_buffer = 0;
    s_buffer = 0;
    voltage = zeros(1,len);
    lock_detector = zeros(1,len);
    ld = 0;
    % PLL parameters
    lock_counter = 0;
    th = 0.005;
    lock_th = samples_per_symbol*2;
    lock = 0;
    w0 = fc;
    b_st = fir1(51,fc/2);
    zf_st = zeros(51,1);
    % demodulator parameters
    b_dmod = fir1(101,fc/2);
    zf_dmod = zeros(1,101);
    zf_i_dmod = zeros(1,101);
    demodulated_bits = [];
    % initial value
    base_band_sampling_T = samples_per_symbol/2;
    constallation = [];
    sync_word_found = 0;
    payload_length_found = 0;
    
    % for loop
    for i=1:len
        c_buffer = c;
        s_buffer = s;
        c = c_buffer*cos(w0) - s_buffer*sin(w0);
        s = s_buffer*cos(w0) + c_buffer*sin(w0);

        % pll lock detector
        if(lock == 0)
            [lp_out,z] = filter(b,1,-s*input_sig(i),z);
            [st,zf_st] = filter(b_st,1,input_sig(i)*c,zf_st);
            v = st * lp_out;
            [ld,z_lock_detector] = filter(b,1,c*input_sig(i),z_lock_detector);
            lock_detector(i) = ld;
            voltage(i) = v;
            w0 = 2*pi*(fc+k*v);
            if(abs(v) < th) && (abs(ld) >0.1)
                lock_counter = lock_counter+1;
            else
                lock_counter = 0;
            end
            if(lock_counter > lock_th)
                lock = 1;
                w0 = 2*pi*fc;
            end
        else
            % PLL is locked and freezed, we can start to demodulate
            i_mixer = input_sig(i) * c;
            
            [i_filtered,zf_i_dmod] = filter(b_dmod,1,i_mixer,zf_i_dmod);
            i_filtered_plot(i) = i_filtered;
            base_band_sampling_T = base_band_sampling_T - 1;
            % baseband sampling time!
            if(base_band_sampling_T == 0)
                base_band_sampling_T = samples_per_symbol;
                if(i_filtered < -0.1)
                    demodulated_bits = [demodulated_bits 0];
                else
                    demodulated_bits = [demodulated_bits 1];
                end
                constallation = [constallation i_filtered];
            end
            % try to find syncword
            if(sync_word_found == 0)
                if(length(demodulated_bits) > length(syncword))
                    % the number of bits here is certainly even
                    for ii=1:2:(length(demodulated_bits) - length(syncword))
                        if(demodulated_bits(ii:ii+length(syncword)-1) == syncword)
                            % syncword found, we can discard the useless bits
                            demodulated_bits = demodulated_bits(1,ii:length(demodulated_bits));
                            constallation = constallation(1,ii:length(constallation));
                            sync_word_found = 1;
                            break;
                        end
                    end
                end
            else
                % sync_word is found, get payload_length and payload, length of len is 8 bits
                if (payload_length_found == 0)
                    if(length(demodulated_bits) == length(syncword) + 8)
                        temp = demodulated_bits(1,length(syncword)+1:length(syncword)+8);
                        payload_length = 0;
                        for ii=1:8
                            payload_length = payload_length + 2^(ii-1) * temp(9-ii);
                        end
                            payload_length_found = 1;
                    end
                else
                    % payload_length has found, read payload
                    if(length(demodulated_bits) >= length(syncword) + 8 + payload_length)
                        payload = demodulated_bits(1,length(syncword) + 9:length(demodulated_bits));
                        figure();
                        plot(i_filtered_plot);
                        figure();
                        q_cons = zeros(1,length(constallation));
                        plot(constallation,q_cons,'.');
                        figure();
                        subplot(2,1,1)
                        plot(voltage);
                        subplot(2,1,2);
                        plot(lock_detector);
                        return ;
                    end
                end
            end
        end
    end
    figure();
    subplot(2,1,1)
    plot(voltage);
    subplot(2,1,2);
    plot(lock_detector);
end
