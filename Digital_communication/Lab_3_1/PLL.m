function [i_carrier,q_carrier] = PLL(input_sig,f0)
    len = length(input_sig);
    i_carrier = zeros(1,len);
    q_carrier = zeros(1,len);
    b = fir1(51,f0);
    z = zeros(51,1);
    % VCO vars
    k = 0.004;
    v = 0;
    c = 1;
    s = 0;
    c_buffer = 0;
    s_buffer = 0;
    voltage = [];
    
    lock_counter = 0;
    th = 0.001;
    lock_th = 50;
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
        [v,z] = filter(b,1,mixer,z);

        voltage = [voltage v];
    end
%     plot(voltage);
end

