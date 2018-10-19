clear all;
close all;
t = 0:1:999;

carrier_frequency = 0.1;
reference_carrier = sin(2*pi*carrier_frequency*t - pi/2.3);

[v,sin_out,cos_out] = PLL(carrier_frequency, reference_carrier);

subplot(2,1,1);
plot(t,reference_carrier);

subplot(2,1,2);
plot(t,sin_out);

figure(2);
plot(t,v);
hold on;
t = zeros(1000);
plot(t);