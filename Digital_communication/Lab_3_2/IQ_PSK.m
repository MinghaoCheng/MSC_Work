clear all;
% pacman
imag = [1,1,1,0,0,1,1,1;
        1,1,0,0,0,0,1,1;
        1,0,0,0,1,0,0,1;
        0,0,0,0,0,0,1,1;
        0,0,0,0,0,1,1,1;
        1,0,0,0,0,0,0,1;
        1,1,0,0,0,0,1,1;
        1,1,1,0,0,1,1,1];
N = 8*8;
bit_stream = reshape(imag,1,N);
% parameters
fc = 0.1;
samples_per_cycle = 1/fc;
cycles_per_symbol = 40;
samples_per_symbol = samples_per_cycle*cycles_per_symbol;

% dummy symbols, preamble
n_dummy_symbols = 10;
dummy_stream = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1];
bit_stream = [dummy_stream bit_stream];
% modulate
t = 0 : 1 : (length(bit_stream) * samples_per_symbol)/2 - 1;
i_carrier = cos(2*pi*fc*t);
q_carrier = sin(2*pi*fc*t);
x = IQ_Modulate(bit_stream,i_carrier,q_carrier,samples_per_symbol);

% transmission
y = x;
% demodulate
[d_carrier_i,d_carrier_q] = IQ_PLL(y,fc,samples_per_symbol);
demodulated_bit_stream = IQ_Demodulate(y,n_dummy_symbols,d_carrier_i,d_carrier_q,samples_per_symbol);
demodulated_imag = reshape(demodulated_bit_stream,[8,8]);
figure();
imshow(demodulated_imag);
