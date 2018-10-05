function [t_sequence,modulated_sig] = BSPK_Modulate(input_sig)
    sampling_rate = 1;
    cycles_per_loop = 8;
    carrier_T = 10;
    
    t_cycle = 0:sampling_rate:(carrier_T*cycles_per_loop - sampling_rate);
    modulated_sig = [];
    for i=1:length(input_sig)
        temp = sin(2*pi*0.1*t_cycle + input_sig(i) * pi);
        modulated_sig = [modulated_sig,temp];
    end
    t_sequence = 0:sampling_rate:(carrier_T*cycles_per_loop*length(input_sig) - sampling_rate);
    
end

