function [v_out,sin_out,cos_out] = PLL(reference_carrier,t_sequence)
    v_out = [];
    sin_out = [];
    cos_out = [];
    c = 1;
    s = 0;
    v = 0;
    b = fir1(51,0.1);
    zf = zeros(numel(b)-1,1);
    for i = 1:length(t_sequence)

        % VCO
        w0 = 2*pi*(0.1+v*0.002);
        delayed_c = c;
        delayed_s = s;
        c = delayed_c * cos(w0) - delayed_s * sin(w0);
        s = delayed_s * cos(w0) + delayed_c * sin(w0);

        % phase detector
        phi = reference_carrier(i)*(-c);
        
        % loop filter
        [v,zf] = filter(b,1,phi,zf);
        
        sin_out = [sin_out s];
        cos_out = [cos_out c];
        v_out = [v_out v];
        

    end


end

