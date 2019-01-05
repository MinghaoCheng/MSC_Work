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
payload = reshape(imag,1,N);

% parameters
fc = 0.1;
samples_per_cycle = 1/fc;
cycles_per_symbol = 40;
samples_per_symbol = samples_per_cycle*cycles_per_symbol;

% reference carrier
reference_t = 0:1:samples_per_symbol*10 - 1;
reference = cos(2*pi*fc*reference_t);

% modulate
syncword = [1,1,0,1,0,1,0,0];   %0xd4
payload_length = [0,1,0,0,0,0,0,0]; %0x40 = 64 here
bit_stream = [syncword payload_length payload];
t = 0 : 1 : (length(bit_stream) * samples_per_symbol)/2 - 1;
i_carrier = cos(2*pi*fc*t);
q_carrier = sin(2*pi*fc*t);

x = IQ_Modulate(bit_stream,i_carrier,q_carrier,samples_per_symbol);
x = [reference x];
% transmission
filename = "QPSK_SIGNAL_TX.wav";
FS = 8000;
%audiowrite(filename,x,FS);
% demodulate
demodulated_bit_stream = IQ_Demodulate(x,fc,syncword,samples_per_symbol);
demodulated_imag = reshape(demodulated_bit_stream,[8,8]);
figure();
imshow(demodulated_imag);
