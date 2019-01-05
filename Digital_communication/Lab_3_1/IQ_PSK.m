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
cycles_per_symbol = 8;
samples_per_symbol = samples_per_cycle*cycles_per_symbol;
t = 0 : 1 : (length(bit_stream) * samples_per_symbol)/2 - 1;
i_carrier = cos(2*pi*fc*t);
q_carrier = sin(2*pi*fc*t);

% modulate
x = IQ_Modulate(bit_stream,i_carrier,q_carrier,samples_per_symbol);
% reference carrier
n_reference_symbols = 10;
reference_c = cos(2*pi*fc *(0 : 1 : samples_per_symbol*n_reference_symbols - 1));
x = [reference_c x];
figure();
plot(x);

% transmission
y = x;

% demodulate
[i_prime,q_prime] = PLL(y,fc);


demodulated_bit_stream = IQ_Demodulate(y,n_reference_symbols,i_prime,q_prime,samples_per_symbol);
demodulated_imag = reshape(demodulated_bit_stream,[8,8]);
figure();
imshow(demodulated_imag);
