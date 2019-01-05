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
samples_per_cycle = 10;
cycles_per_symbol = 8;
samples_per_symbol = samples_per_cycle * cycles_per_symbol;
t = 0 : 1 : length(bit_stream) * samples_per_symbol - 1;
carrier = sin(2*pi*fc*t);

% modulate
x = BPSK_Modulate(bit_stream,carrier,samples_per_symbol);
figure()
plot(t,x);
figure();
f = abs(fft(x));
plot(f);
% transmission
y = x;
% demodulate

demodulated_bit_stream = BPSK_Demodulate(y,carrier,samples_per_symbol);

demodulated_imag = reshape(demodulated_bit_stream,8,8);
figure();
imshow(demodulated_imag);