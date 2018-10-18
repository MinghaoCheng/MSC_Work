% BPSK simulation

% Specify modulation parameters
carrier_frequency = 0.1;
sampling_rate = 1;
cycles_per_symbol = 8;

% Specify digital signal to be transmitt
imag = [1,1,1,0,0,1,1,1;
        1,1,0,0,0,0,1,1;
        1,0,0,0,1,0,0,1;
        0,0,0,0,0,0,1,1;
        0,0,0,0,0,1,1,1;
        1,0,0,0,0,0,0,1;
        1,1,0,0,0,0,1,1;
        1,1,1,0,0,1,1,1];
    
N=8*8;
bit_stream = [];
for j=1:8
    bit_stream = [bit_stream imag(j,:)];
end


% Let bit be 1 or -1 in order to modulate

for j=1:length(bit_stream)
    if(bit_stream(j) == 0)
        bit_stream(j) = -1;
    end
end

% Create time sequence and modulate the bit stream

t_sequence = 0:1/sampling_rate:(length(bit_stream)*cycles_per_symbol*(1/carrier_frequency)*(1/sampling_rate)-1);
carrier_tx = sin(2*pi*carrier_frequency*t_sequence);

tx_data = BPSK_Modulate(bit_stream,carrier_tx,carrier_frequency,cycles_per_symbol,sampling_rate);

% trasmission

rx_data = tx_data;


% Carrier recovery
sin_out = [];
cos_out = [];
[sin_out,cos_out] = VCO(0.1, rx_data);

plot(sin_out)


% Demodulate data


% demodulated_bit_stream = BPSK_Demodulate(rx_data,carrier_rx,carrier_frequency,cycles_per_symbol,sampling_rate);
% 
% % Plot data
% 
% demodulated_imag = reshape(demodulated_bit_stream,8,8);
% figure(4);
% imshow(demodulated_imag);