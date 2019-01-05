function [i_carrier,q_carrier] = IQ_PLL(input_sig,f0,samples_per_symbol)
    len = length(input_sig);
    i_carrier = zeros(1,len);
    q_carrier = zeros(1,len);
    b = fir1(51,f0);
    z = zeros(51,1);
    z_lock_detector = zeros(51,1);
    % VCO vars
    k = 0.005;
    v = 0;
    c = 1;
    s = 0;
    c_buffer = 0;
    s_buffer = 0;
    voltage = [];
    lock_detector = [];
    ld = 0;
    
    lock_counter = 0;
    th = 0.005;
    lock_th = samples_per_symbol*2;
    lock = 0;
    
    for i=1:len
        % pll lock detector
        if(lock == 0)
            phi_factor = 2*pi*(f0+k*v);
            if(abs(v) < th) && (abs(ld) >0.1)
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
        [v,z] = filter(b,1,mixer,z);
        [ld,z_lock_detector] = filter(b,1,c*input_sig(i),z_lock_detector);
        lock_detector = [lock_detector ld];
        voltage = [voltage v];
    end
    plot(voltage);
end

