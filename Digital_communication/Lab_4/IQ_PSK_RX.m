clear;
% parameters
fc = 0.1;
samples_per_cycle = 1/fc;
cycles_per_symbol = 40;
samples_per_symbol = samples_per_cycle*cycles_per_symbol;
syncword = [1,1,0,1,0,1,0,0];   %0xd4

FS = 8000;
filename = "QPSK_SIGNAL_RX.wav";
%filename = "qam50ms_rx.wav";
y = audioread(filename);
y = reshape(y,1,length(y));
y = y / max(y);
% demodulate
demodulated_bit_stream = IQ_Demodulate(y,fc,syncword,samples_per_symbol);
demodulated_imag = reshape(demodulated_bit_stream,[8,8]);
figure();
imshow(demodulated_imag);


