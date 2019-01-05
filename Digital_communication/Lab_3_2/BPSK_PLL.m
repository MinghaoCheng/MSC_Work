function i_carrier = BPSK_PLL(input_sig,f0,samples_per_symbol)
    len = length(input_sig);
    i_carrier = zeros(1,len);
    q_carrier = zeros(1,len);
    b_lf = fir1(51,f0);
    zf_lf = zeros(51,1);
    b_st = fir1(51,f0);
    zf_st = zeros(51,1);
    % VCO vars
    k = 0.005;
    v = 0;
    c = 1;
    s = 0;
    c_buffer = 0;
    s_buffer = 0;
    voltage = [];
    
    lock_counter = 0;
    th = 0.0005;
    lock_th = samples_per_symbol/2;
    lock = 0;
    
    for i=1:len
        % pll lock detector
        if(lock == 0)
            phi_factor = 2*pi*(f0+k*v);
            if(abs(v) < th)
                lock_counter = lock_counter+1;
            else
                lock_counter = 0;
            end
            if(lock_counter > lock_th)
                 lock = 1;
                 phi_factor = 2*pi*f0;
            end
        end
        c_buffer = c;
        s_buffer = s;
        c = c_buffer*cos(phi_factor) - s_buffer*sin(phi_factor);
        s = s_buffer*cos(phi_factor) + c_buffer*sin(phi_factor);
        i_carrier(i) = c;
        q_carrier(i) = s;
        mixer = -s*input_sig(i);
        [lp_out,zf_lf] = filter(b_lf,1,mixer,zf_lf);
        [st,zf_st] = filter(b_st,1,input_sig(i) * c,zf_st);
        v = lp_out*st;
        voltage = [voltage v];
    end
    plot(voltage);
end

