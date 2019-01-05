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
samples_per_cycle = 1/fc;
cycles_per_symbol = 40;
samples_per_symbol = samples_per_cycle*cycles_per_symbol;

% preamble & syncword
preamble = [1,1,1,1,1,1,1,1,1,1];
syncword = [1,1,0,1,0,1,0,0];   %0xd4
payload_length = [0,1,0,0,0,0,0,0]; %0x40 = 64 here
bit_stream = [preamble syncword payload_length bit_stream];
% modulate
t = 0 : 1 : length(bit_stream) * samples_per_symbol - 1;
carrier = cos(2*pi*fc*t);
x = BPSK_Modulate(bit_stream,carrier,samples_per_symbol);
% give the signal some extra numbers so the last symbol won't get stuck in the filter
temp = zeros(1,samples_per_symbol);
x = [x temp];

% transmission

filename = 'BPSK_SIGNAL_TX.wav';
FS = 8000;
audiowrite(filename,x,FS);


% demodulate

demodulated_bit_stream = BPSK_Demodulate(x,fc,syncword,samples_per_symbol);
demodulated_imag = reshape(demodulated_bit_stream,8,8);
figure();
imshow(demodulated_imag);