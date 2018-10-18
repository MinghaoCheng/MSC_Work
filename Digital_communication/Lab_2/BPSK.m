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

[t,x] = BSPK_Modulate(bit_stream);
figure(1)
plot(t,x);
% demodulate
carrier = sin(2*pi*0.1*t);
mixer = x.*carrier*2;
figure(2);
plot(t,mixer);

b = fir1(7,0.01);
lp_demod = filter(b,1,mixer);

figure(3);
plot(t,lp_demod);
% comparing
demodulated_data = []
for i=1:length(lp_demod)
    if(lp_demod(i) > 0)
        demodulated_data = [demodulated_data 0];
    else
        demodulated_data = [demodulated_data 1];
    end
end

% go back to bit streams
demodulated_bit_stream = []
for i=1:length(bit_stream)
    if(demodulated_data(i*80 - 40) > 0)
        demodulated_bit_stream = [demodulated_bit_stream 1];
    else
        demodulated_bit_stream = [demodulated_bit_stream 0];
    end
end

demodulated_imag = reshape(demodulated_bit_stream,8,8);
figure(4);
imshow(demodulated_imag);