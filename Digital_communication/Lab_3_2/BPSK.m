clear;
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
samples_per_cycle = 10;
cycles_per_symbol = 20;
samples_per_symbol = samples_per_cycle*cycles_per_symbol;
t = 0 : 1 : length(bit_stream) * samples_per_symbol - 1;
carrier = cos(2*pi*fc*t);

% modulate
x = BPSK_Modulate(bit_stream,carrier,samples_per_symbol);
figure()
% dummy symbols, preamble
n_dummy_symbols = 10;
dummy_stream = [1,-1,1,-1,1,-1,1,-1,1,-1];
dummy_t = 0:1:n_dummy_symbols*cycles_per_symbol*samples_per_cycle-1;
dummy_carrier = cos(2*pi*fc*dummy_t);
dummy = BPSK_Modulate(dummy_stream,dummy_carrier,samples_per_symbol);
% add dummy symbols with payload
x = [dummy x];
plot(x);

% transmission
y = x;
% demodulate
carrier_prime = BPSK_PLL(y,fc,samples_per_cycle*cycles_per_symbol);
demodulated_bit_stream = BPSK_Demodulate(y,carrier_prime,n_dummy_symbols,samples_per_symbol);

demodulated_imag = reshape(demodulated_bit_stream,8,8);
figure();
imshow(demodulated_imag);