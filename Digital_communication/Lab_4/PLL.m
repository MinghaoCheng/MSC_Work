function [v_out,sin_out,cos_out] = PLL(carrier_frequency,reference_carrier)
    lock = 0;
    v_out = [];
    sin_out = [];
    cos_out = [];
    c = 1;
    s = 0;
    v = 0;
    b = fir1(51,0.1);
    zf = zeros(numel(b)-1,1);
    for i = 1:length(reference_carrier)
        % VCO
        w0 = 2*pi*(carrier_frequency + v*0.005);
        delayed_c = c;
        delayed_s = s;
        c = delayed_c * cos(w0) - delayed_s * sin(w0);
        s = delayed_s * cos(w0) + delayed_c * sin(w0);
        if(lock == 0)
            % phase detector
            phi = reference_carrier(i)*(-c);

            % loop filter
            [v,zf] = filter(b,1,phi,zf);

            if(v == 0)
                lock = 1;
            end
        end
        
        sin_out = [sin_out -s];
        cos_out = [cos_out -c];
        v_out = [v_out v];
    end
end

