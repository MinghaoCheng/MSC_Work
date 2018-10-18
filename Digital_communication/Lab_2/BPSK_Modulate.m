function [t_sequence,modulated_sig] = BSPK_Modulate(input_sig)
    sampling_T = 1;
    sampling_f = 1/sampling_T;
    
    cycles_per_loop = 8;
    carrier_T = 10;
    carrier_f = 1/carrier_T;
    
    t_cycle = 0:sampling_period:(carrier_T*cycles_per_loop - sampling_period);
    modulated_sig = [];
    for i=1:length(input_sig)
        temp = sin(2*pi*carrier_f*t_cycle + input_sig(i)  pi);
        modulated_sig = [modulated_sig,temp];
    end
    t_sequence = 0:sampling_period:(carrier_T*cycles_per_loop*length(input_sig) - sampling_period);
end
