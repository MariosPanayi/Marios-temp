%Find start and end of Lights in avg luminance videos

avg_luminancediff = avg_luminance(2:end) - avg_luminance(1:end-1);
[~,cueStart] = findpeaks(avg_luminancediff, 'MinPeakHeight',40);
[~,cueEnd] = findpeaks(avg_luminancediff*-1, 'MinPeakHeight',40);

figure
subplot(2,1,1)
plot(avg_luminance)
hold on
plot(cueStart, ones(size(cueStart,1))*45, 'r.')
plot(cueEnd, ones(size(cueEnd,1))*45, 'r.')
hold off


subplot(2,1,2)
plot(avg_luminancediff)
hold on
plot(cueStart, ones(size(cueStart,1)), 'r.')
plot(cueEnd, ones(size(cueEnd,1)), 'r.')
hold off
