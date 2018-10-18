function [sin_out,cos_out] = VCO(v_in,t_sequence)
    w0 = 0.01 + v_in;

    sin_out = [];
    cos_out = [];
    
    delayed_c = 0;
    delayed_s = 0;
    c = 1;
    s = 0;

    for i = 1:length(t_sequence)
        delayed_c = c;
        delayed_s = s;

        c = delayed_c * cos(w0) - delayed_s * sin(w0);
        s = delayed_s * cos(w0) + delayed_c * sin(w0);
        
        sin_out = [sin_out s];
        cos_out = [cos_out c];
    end
end

