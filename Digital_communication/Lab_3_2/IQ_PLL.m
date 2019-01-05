function [i_carrier,q_carrier] = IQ_PLL(input_sig,f0,samples_per_symbol)
    len = length(input_sig);
    i_carrier = zeros(1,len);
    q_carrier = zeros(1,len);
    b = fir1(41,f0*1.5);
    zf_lf1 = zeros(41,1);
    zf_lf2 = zeros(41,1);
    zf_lp = zeros(41,1);
    % VCO vars
    k = 0.005;
    v = 0;
    c = 1;
    s = 0;
    c_buffer = 0;
    s_buffer = 0;
    voltage = [];
    
    lock_counter = 0;
    th = 0.0002;
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
        mixer1 = c*input_sig(i);
        mixer2 = s*input_sig(i);
        [lp_out1,zf_lf1] = filter(b,1,mixer1,zf_lf1);
        [lp_out2,zf_lf2] = filter(b,1,mixer2,zf_lf2);
        if(lp_out1 < -0.2)
            si = -1;
        else
            si = 1;
        end
        if(lp_out2 < -0.2)
            sq = -1;
        else
            sq = 1;
        end
        [v,zf_lp] = filter(b,1,lp_out1*sq-lp_out2*si,zf_lp);
        voltage = [voltage v];
    end
    plot(voltage);
end

