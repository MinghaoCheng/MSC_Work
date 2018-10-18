t = 0:1:10000;

reference_carrier = sin(2*pi*0.1*t + pi/2.3);

[v,sin_out,cos_out] = PLL(reference_carrier,t);

subplot(2,1,1);
plot(t,reference_carrier);

subplot(2,1,2);
plot(t,sin_out);

figure(2);
plot(t,v);